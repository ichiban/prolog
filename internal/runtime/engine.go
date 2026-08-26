package runtime

import (
	"context"
	_ "embed"
	"errors"
	"fmt"
	"io"
	"iter"
	"math"

	"github.com/ichiban/prolog/v2/internal/db"
	"github.com/ichiban/prolog/v2/internal/ir"
	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
	"github.com/ichiban/prolog/v2/internal/wam"
)

//go:embed bootstrap.pl
var bootstrap string

type unknownAction int8

const (
	unknownError unknownAction = iota
	unknownFail
	unknownWarning
)

var unknowActionNames = [...]string{
	unknownError:   "error",
	unknownFail:    "fail",
	unknownWarning: "warning",
}

func (u unknownAction) String() string {
	return unknowActionNames[u]
}

type loadedKey struct {
	fsName   term.Atom
	filename string
}

type Engine struct {
	*term.Arena
	TempArena *term.Arena // Used for findall/3, etc.
	wam.Image
	BuiltinSet *BuiltinSet

	FSs FSSet

	Loaded map[loadedKey]struct{}

	Module         term.Atom
	DoubleQuotes   syntax.DoubleQuotes
	Ops            syntax.OperatorSet
	CharConversion syntax.CharConversion
	DB             db.DB
	CurrentTime    wam.LogicalTime

	Input  term.Handle
	Output term.Handle

	debug    bool
	unknown  unknownAction
	Warn     func(error)
	Halt     func(code int)
	location term.Functor
}

func (e *Engine) Predicate(bpi term.Functor) (wam.Predicate, bool, error) {
	p, ok := e.Predicates[bpi]
	if !ok {
		pi := term.NewFunctor(bpi.Name(), bpi.Arity()-1)
		culprit, err := e.PutFunctor(pi)
		if err != nil {
			return wam.Predicate{}, false, err
		}

		err = &ExistenceError{
			ObjectType: term.NewAtom("procedure"),
			Culprit:    syntax.Serialize(e.Arena, culprit),
			Location:   term.NewFunctor(term.NewAtom("user"), 0),
		}
		switch e.unknown {
		case unknownError:
			return wam.Predicate{}, false, err
		case unknownWarning:
			if e.Warn == nil {
				e.Warn = func(error) {}
			}
			e.Warn(err)
			fallthrough
		case unknownFail:
			return wam.Predicate{}, false, nil
		}
	}
	return p, true, nil
}

func (e *Engine) Inspect(t term.Handle) string {
	return fmt.Sprintf("%s", &syntax.Formatter{Arena: e.Arena, Term: t})
}

var _ = (*Engine)(nil).Inspect

func (e *Engine) ExpandTerm(_ context.Context, t term.Handle) iter.Seq2[term.Handle, error] {
	// TODO: Implement this!
	return func(yield func(term.Handle, error) bool) {
		_ = yield(t, nil)
	}
}

func (e *Engine) ExpandGoal(_ context.Context, t term.Handle) (term.Handle, error) {
	return t, nil // TODO: Implement this!
}

func (e *Engine) LoadSystem(ctx context.Context) error {
	var (
		c = Compiler{Engine: e}
		m ir.Module
	)
	if err := c.CompileSystem(ctx, &m); err != nil {
		return err
	}
	if err := c.CompileText(ctx, &m, bootstrap); err != nil {
		return err
	}
	if err := e.LoadModule(ctx, &m); err != nil {
		return err
	}

	for pi, p := range e.Predicates {
		p.BuiltIn = true
		e.Predicates[pi] = p
	}

	return nil
}

// ReadFile reads a Prolog text from file via FS.
func (e *Engine) ReadFile(fsName term.Atom, filename string) (string, error) {
	fs, ok := e.FSs.Get(fsName)
	if !ok {
		return "", errors.New("file not found")
	}

	f, err := fs.Open(filename)
	if err != nil {
		return "", err
	}
	defer func() {
		_ = f.Close()
	}()

	r, ok := f.(io.Reader)
	if !ok {
		return "", errors.New("file does not implement io.Reader")
	}

	b, err := io.ReadAll(r)
	if err != nil {
		return "", err
	}

	return string(b), nil
}

func (e *Engine) LoadFile(ctx context.Context, fsName term.Atom, filename string) error {
	text, err := e.ReadFile(fsName, filename)
	if err != nil {
		return err
	}

	var (
		c = Compiler{Engine: e}
		m ir.Module
	)
	if err := c.CompileText(ctx, &m, text); err != nil {
		return err
	}
	if err := e.LoadModule(ctx, &m); err != nil {
		return err
	}

	if e.Loaded == nil {
		e.Loaded = map[loadedKey]struct{}{}
	}
	e.Loaded[loadedKey{
		fsName:   fsName,
		filename: filename,
	}] = struct{}{}

	return nil
}

func (e *Engine) LoadModule(ctx context.Context, module *ir.Module) error {
	if e.Code == nil {
		e.Predicates = map[term.Functor]wam.Predicate{
			term.NewFunctor(term.NewAtom("true"), 0): {Offset: 0},
		}
		if err := e.emit(wam.OpProceed, 0, 0); err != nil {
			return err
		}
	}

	var (
		current term.Functor
		last    int
		defined = map[term.Functor]struct{}{}
	)
	for _, clause := range module.Clauses {
		bpi := clause.PI

		switch p, _ := e.Predicates[bpi]; {
		case bpi == current: // A subsequent clause of the current chunk.
			if err := e.rewriteN(last, len(e.Code)); err != nil {
				return err
			}
			last = len(e.Code)
			if err := e.emit(wam.OpRetryMeElse, bpi.Arity(), 0); err != nil {
				return err
			}
		case p.Offset == 0: // The 1st clause of the predicate.
			// The current chunk needs to be closed.
			if err := e.closePredicate(current, last); err != nil {
				return err
			}

			current = bpi
			p.Offset = len(e.Code)
			if e.Predicates == nil {
				e.Predicates = map[term.Functor]wam.Predicate{}
			}
			e.Predicates[bpi] = p

			fid := e.EmbedFunctor(bpi)
			if err := e.emit(wam.OpSwitch, 0, fid); err != nil {
				return err
			}
			last = len(e.Code)
			if err := e.emit(wam.OpTryMeElse, bpi.Arity(), 0); err != nil {
				return err
			}
		default: // A new chunk of an already defined predicate.
			if err := e.closePredicate(current, last); err != nil {
				return err
			}

			if e.Warn == nil {
				e.Warn = func(error) {}
			}
			pi := term.NewFunctor(bpi.Name(), bpi.Arity()-1)
			if _, ok := defined[bpi]; ok {
				if !p.Discontiguous {
					e.Warn(fmt.Errorf("discontiguous: %s", pi))
				}
			} else if !p.Multifile {
				e.Warn(fmt.Errorf("multifile: %s", pi))
			}

			// Reopen the predicate's last alternative and link it to this chunk.
			switch e.Code[p.LastChoice].Op {
			case wam.OpNop: // A closed single-clause chunk, i.e. the predicate has one clause so far.
				e.Code[p.LastChoice].Op = wam.OpTryMeElse
				e.Code[p.LastChoice].I = uint16(bpi.Arity())
				// First-argument dispatch pays off again with multiple clauses.
				// The switch was disabled by closePredicate with its operand kept,
				// unless the sole clause wasn't indexable in the first place.
				if len(p.FirstArgIndex) > 0 {
					e.Code[p.Offset].Op = wam.OpSwitch
				}
			case wam.OpTrustMe:
				e.Code[p.LastChoice].Op = wam.OpRetryMeElse
			default:
				return errors.New("invalid instruction")
			}
			if err := e.rewriteN(p.LastChoice, len(e.Code)); err != nil {
				return err
			}

			current = bpi
			last = len(e.Code)
			if err := e.emit(wam.OpRetryMeElse, bpi.Arity(), 0); err != nil {
				return err
			}
		}
		defined[bpi] = struct{}{}

		if clause.MaxRegs >= maxRegisters {
			return errors.New("not enough registers")
		}

		// First argument index.
		fa := clause.FirstArg
		key := wam.FirstArgKey{
			Term:  fa.Term,
			Arity: fa.Arity,
		}
		p, _ := e.Predicates[bpi]
		if _, ok := p.FirstArgIndex[key]; ok || fa == (ir.Index{}) {
			e.Code[p.Offset] = wam.Instruction{Op: wam.OpNondet}
		} else {
			if p.FirstArgIndex == nil {
				p.FirstArgIndex = map[wam.FirstArgKey]int{}
			}
			p.FirstArgIndex[key] = len(e.Code)
			e.Predicates[bpi] = p
		}

		for _, inst := range clause.Code {
			switch op := convertOp(inst); op {
			case wam.OpUnifyVoid, wam.OpWriteVoid:
				if err := e.emit(op, 0, 0); err != nil {
					return err
				}
			case wam.OpPutVariable, wam.OpGetValue:
				if err := e.emit(op, inst.A.Index, inst.B.Index); err != nil {
					return err
				}
			case wam.OpGetStructure, wam.OpPutStructure, wam.OpPushStructure:
				fid := e.EmbedFunctor(inst.A.Functor)
				if err := e.emit(op, inst.B.Index, fid); err != nil {
					return err
				}
			case wam.OpUnifyVariable, wam.OpUnifyValue, wam.OpWriteVariable, wam.OpWriteValue:
				if err := e.emit(op, inst.B.Index, 0); err != nil {
					return err
				}
			case wam.OpGetConstant, wam.OpPutConstant, wam.OpUnifyConstant, wam.OpWriteConstant:
				c := inst.B.Term
				c = e.Deref(c)
				cid := e.EmbedConstants(c)
				if err := e.emit(op, inst.A.Index, cid); err != nil {
					return err
				}
			case wam.OpGetVariable:
				if err := e.emit(wam.OpMove, inst.A.Index, inst.B.Index); err != nil {
					return err
				}
			case wam.OpPutValue:
				if err := e.emit(wam.OpMove, inst.B.Index, inst.A.Index); err != nil {
					return err
				}
			default:
				if err := e.emit(op, inst.A.Index, 0); err != nil {
					return err
				}
			}
		}

		fid := e.EmbedFunctor(clause.Execute)
		if err := e.emit(wam.OpExecute, 0, fid); err != nil {
			return err
		}
	}

	if err := e.closePredicate(current, last); err != nil {
		return err
	}

	for _, g := range module.Initialization {
		for err := range e.Call(ctx, g) {
			if err != nil {
				return err
			}
			break
		}
	}

	return nil
}

func (e *Engine) emit(op wam.OpCode, i, n int) error {
	if i > math.MaxUint16 {
		return fmt.Errorf("operand out of range: i=%d", i)
	}
	if n > math.MaxUint32 {
		return fmt.Errorf("operand out of range: n=%d", n)
	}
	e.Code = append(e.Code, wam.Instruction{
		Op: op,
		I:  uint16(i),
		N:  uint32(n),
	})
	return nil
}

func (e *Engine) rewriteN(addr, n int) error {
	if n > math.MaxUint32 {
		return fmt.Errorf("operand out of range: n=%d", n)
	}
	e.Code[addr].N = uint32(n)
	return nil
}

func (e *Engine) closePredicate(pi term.Functor, last int) error {
	if last == 0 {
		return nil
	}
	p := e.Predicates[pi]
	switch e.Code[last].Op {
	case wam.OpTryMeElse: // The chunk has only one clause.
		// Keep the instruction patchable so that a later chunk can link itself here.
		e.Code[last] = wam.Instruction{Op: wam.OpNop}
		// A single clause needs no first-argument dispatch: hit or miss, the switch
		// would end up right past the nop anyway. Disable it, keeping its operand.
		e.Code[p.Offset].Op = wam.OpNondet
	case wam.OpRetryMeElse:
		e.Code[last].Op = wam.OpTrustMe
	default:
		return errors.New("invalid instruction")
	}
	p.LastChoice = last
	e.Predicates[pi] = p
	return nil
}

func convertOp(inst ir.Instruction) wam.OpCode {
	type key struct {
		op  ir.OpCode
		typ ir.Type
	}
	switch (key{op: inst.OpCode, typ: inst.Type}) {
	case key{op: ir.OpPut, typ: ir.TypeVariable}:
		return wam.OpPutVariable
	case key{op: ir.OpPut, typ: ir.TypeValue}:
		return wam.OpPutValue
	case key{op: ir.OpPut, typ: ir.TypeStructure}:
		return wam.OpPutStructure
	case key{op: ir.OpPut, typ: ir.TypeConstant}:
		return wam.OpPutConstant
	case key{op: ir.OpPut, typ: ir.TypeCut}:
		return wam.OpPutCut
	case key{op: ir.OpGet, typ: ir.TypeVariable}:
		return wam.OpGetVariable
	case key{op: ir.OpGet, typ: ir.TypeValue}:
		return wam.OpGetValue
	case key{op: ir.OpGet, typ: ir.TypeStructure}:
		return wam.OpGetStructure
	case key{op: ir.OpGet, typ: ir.TypeConstant}:
		return wam.OpGetConstant
	case key{op: ir.OpGet, typ: ir.TypeCut}:
		return wam.OpGetCut
	case key{op: ir.OpUnify, typ: ir.TypeVariable}:
		return wam.OpUnifyVariable
	case key{op: ir.OpUnify, typ: ir.TypeValue}:
		return wam.OpUnifyValue
	case key{op: ir.OpUnify, typ: ir.TypeConstant}:
		return wam.OpUnifyConstant
	case key{op: ir.OpUnify, typ: ir.TypeVoid}:
		return wam.OpUnifyVoid
	case key{op: ir.OpWrite, typ: ir.TypeVariable}:
		return wam.OpWriteVariable
	case key{op: ir.OpWrite, typ: ir.TypeValue}:
		return wam.OpWriteValue
	case key{op: ir.OpWrite, typ: ir.TypeConstant}:
		return wam.OpWriteConstant
	case key{op: ir.OpWrite, typ: ir.TypeVoid}:
		return wam.OpWriteVoid
	case key{op: ir.OpPush, typ: ir.TypeVariable}:
		return wam.OpWriteVariable
	case key{op: ir.OpPush, typ: ir.TypeStructure}:
		return wam.OpPushStructure
	case key{op: ir.OpPush, typ: ir.TypeCut}:
		return wam.OpPushCut
	case key{op: ir.OpBuiltin, typ: ir.TypeNotApplicable}, key{op: ir.OpInline, typ: ir.TypeVariable}:
		return wam.OpBuiltin0 + wam.OpCode(inst.A.Index)
	default:
		return wam.OpNop
	}
}

func (e *Engine) DefineBuiltin0(name term.Atom, fn func(context.Context) iter.Seq[error]) {

}

func (e *Engine) Call(ctx context.Context, goal term.Handle) iter.Seq[error] {
	// FIXME: iter.Seq[error] is a code smell since each error isn't an element of the sequence but the error of the sequence itself.
	bpi := term.NewFunctor(term.NewAtom("call"), 2)
	cont, err := e.PutAtom(term.NewAtom("true"))
	if err != nil {
		return func(yield func(error) bool) {
			_ = yield(err)
		}
	}
	p, ok, err := e.Predicate(bpi)
	if err != nil {
		return func(yield func(error) bool) {
			_ = yield(err)
		}
	}
	if !ok {
		return func(yield func(error) bool) {
		}
	}
	exec := Execution{
		Engine:         e,
		programPointer: p.Offset,
	}
	exec.tempVars[1] = goal
	exec.tempVars[2] = cont
	return func(yield func(error) bool) {
		// The last solution's bindings are still trailed on exec when run
		// terminates; undo them so they don't leak into the caller.
		trailTop := len(exec.trail)
		defer func() {
			_ = exec.unwindTrail(trailTop)
		}()
		for err := range exec.run(ctx) {
			if !yield(err) {
				return
			}
		}
	}
}
