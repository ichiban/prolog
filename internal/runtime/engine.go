package runtime

import (
	"context"
	_ "embed"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"iter"
	"math"
	"slices"

	"github.com/ichiban/prolog/v2/internal/ir"
	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
	"github.com/ichiban/prolog/v2/internal/wam"
)

//go:embed bootstrap.pl
var bootstrap string

type Engine struct {
	*term.Arena
	wam.Image
	BuiltinSet *BuiltinSet

	SourceFS fs.FS
	Loaded   map[string]struct{}

	Module       term.Atom
	DoubleQuotes syntax.DoubleQuotes
	Ops          *syntax.OperatorSet

	OnDiscontiguous func(pi term.Functor) error
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
	if e.Code == nil {
		e.Predicates = map[term.Functor]wam.Predicate{
			term.NewFunctor(term.NewAtom("true"), 0): {Offset: 0},
		}
		if err := e.emit(wam.OpProceed, 0, 0); err != nil {
			return err
		}
	}

	var (
		c = Compiler{Engine: e}
		m ir.Module
	)
	if err := c.CompileSystem(&m); err != nil {
		return err
	}
	if err := c.CompileText(ctx, &m, bootstrap); err != nil {
		return err
	}
	return e.LoadModule(&m)
}

func (e *Engine) LoadFile(ctx context.Context, filename string) error {
	f, err := e.SourceFS.Open(filename)
	if err != nil {
		return err
	}
	defer func() {
		_ = f.Close()
	}()

	b, err := io.ReadAll(f)
	if err != nil {
		return err
	}

	var (
		c = Compiler{Engine: e}
		m ir.Module
	)
	if err := c.CompileText(ctx, &m, string(b)); err != nil {
		return err
	}
	if err := e.LoadModule(&m); err != nil {
		return err
	}

	if e.Loaded == nil {
		e.Loaded = map[string]struct{}{}
	}
	e.Loaded[filename] = struct{}{}

	return nil
}

func (e *Engine) LoadModule(module *ir.Module) error {
	if e.OnDiscontiguous == nil {
		e.OnDiscontiguous = func(pi term.Functor) error {
			return nil
		}
	}

	var (
		current term.Functor
		last    int
	)
	for i, clause := range module.Clauses {
		pi := clause.PI
		switch _, ok := e.Predicates[pi]; {
		case !ok: // The 1st clause.
			if i > 0 {
				// The last predicate needs to be closed.
				if err := e.closePredicate(last); err != nil {
					return err
				}
			}

			current = pi
			if e.Predicates == nil {
				e.Predicates = map[term.Functor]wam.Predicate{}
			}
			e.Predicates[pi] = wam.Predicate{
				Offset: len(e.Code),
			}

			fid := e.EmbedFunctor(pi)
			if err := e.emit(wam.OpSwitch, 0, fid); err != nil {
				return err
			}
			last = len(e.Code)
			if err := e.emit(wam.OpTryMeElse, pi.Arity(), 0); err != nil {
				return err
			}
		case pi != current: // A discontiguous clause.
			if err := e.OnDiscontiguous(pi); err != nil {
				return err
			}
			current = pi
			// TODO: Overwrite the previous chunk's `execute P` to an unconditional jump to this chunk.
			fallthrough
		default:
			if last > 0 {
				if err := e.rewriteN(last, len(e.Code)); err != nil {
					return err
				}
				last = len(e.Code)
				if err := e.emit(wam.OpRetryMeElse, pi.Arity(), 0); err != nil {
					return err
				}
			}
		}

		if clause.MaxRegs >= maxRegisters {
			return errors.New("not enough registers")
		}

		// First argument index.
		fa := clause.FirstArg
		key := wam.FirstArgKey{
			PI:    pi,
			Term:  fa.Term,
			Arity: fa.Arity,
		}
		if _, ok := e.FirstArgIndex[key]; ok || fa == (ir.Index{}) {
			e.Code[e.Predicates[pi].Offset] = wam.Instruction{
				Op: wam.OpNondet,
			}
		} else {
			if e.FirstArgIndex == nil {
				e.FirstArgIndex = map[wam.FirstArgKey]int{}
			}
			e.FirstArgIndex[key] = len(e.Code)
		}

		for _, inst := range clause.Code {
			switch op := convertOp(inst); op {
			case wam.OpUnifyVoid, wam.OpWriteVoid:
				if err := e.emit(op, 0, 0); err != nil {
					return err
				}
			case wam.OpLoadVariable, wam.OpPutVariable, wam.OpGetValue, wam.OpLoadValue:
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
			case wam.OpLoadConstant, wam.OpGetConstant, wam.OpPutConstant, wam.OpUnifyConstant, wam.OpWriteConstant:
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

	return e.closePredicate(last)
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

func (e *Engine) closePredicate(last int) error {
	if last == 0 {
		return nil
	}
	switch e.Code[last].Op {
	case wam.OpTryMeElse: // The last predicate has only one clause.
		// TODO: What would happen if it's discontiguous?
		e.Code = slices.Delete(e.Code, last-1, last+1)
	case wam.OpRetryMeElse:
		e.Code[last].Op = wam.OpTrustMe
	default:
		return errors.New("invalid instruction")
	}
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
	case key{op: ir.OpLoad, typ: ir.TypeVariable}:
		return wam.OpLoadVariable
	case key{op: ir.OpLoad, typ: ir.TypeValue}:
		return wam.OpLoadValue
	case key{op: ir.OpLoad, typ: ir.TypeConstant}:
		return wam.OpLoadConstant
	case key{op: ir.OpPush, typ: ir.TypeVariable}:
		return wam.OpPushVariable
	case key{op: ir.OpPush, typ: ir.TypeStructure}:
		return wam.OpPushStructure
	case key{op: ir.OpPush, typ: ir.TypeCut}:
		return wam.OpPushCut
	case key{op: ir.OpBuiltin, typ: ir.TypeNotApplicable}, key{op: ir.OpInline, typ: ir.TypeVariable}, key{op: ir.OpArithmetic, typ: ir.TypeNotApplicable}:
		return wam.OpBuiltin0 + wam.OpCode(inst.A.Index)
	default:
		return wam.OpNop
	}
}

func (e *Engine) DefineBuiltin0(name term.Atom, fn func(context.Context) iter.Seq[error]) {

}

func (e *Engine) Call(ctx context.Context, goal term.Handle) iter.Seq[error] {
	pi := term.NewFunctor(term.NewAtom("call"), 2)
	t, err := e.PutAtom(term.NewAtom("true"))
	if err != nil {
		return func(yield func(error) bool) {
			_ = yield(err)
		}
	}
	p, ok := e.Predicates[pi]
	if !ok {
		return func(yield func(error) bool) {
			pi := term.NewFunctor(pi.Name(), pi.Arity()-1)
			culprit, err := e.PutFunctor(pi)
			if err != nil {
				_ = yield(err)
				return
			}
			_ = yield(&ExistenceError{
				ErrorContext: ErrorContext{
					Location: term.NewFunctor(term.NewAtom("user"), 0),
				},
				ObjectType: "procedure",
				Culprit:    culprit,
			})
		}
	}
	exec := Execution{
		Engine:         e,
		location:       term.NewFunctor(term.NewAtom("call"), 1),
		programPointer: p.Offset,
	}
	exec.tempVars[0] = goal
	exec.tempVars[1] = t
	return exec.run(ctx)
}
