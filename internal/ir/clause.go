package ir

import (
	"errors"
	"fmt"
	"math"
	"slices"
	"strings"

	"github.com/ichiban/prolog/v2/internal/runtime"
	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
)

type LifeTime struct {
	Birth int
	Death int
}

func (l LifeTime) String() string {
	return fmt.Sprintf("%d-%d", l.Birth, l.Death)
}

func (l LifeTime) Precedes(o LifeTime) bool {
	return l.Death < o.Birth
}

func (l LifeTime) Contains(o LifeTime) bool {
	return l.Birth <= o.Birth && o.Death <= l.Death
}

type Variables map[int]Variable

func (v Variables) String() string {
	type Var struct {
		varID int
		Variable
	}
	elems := make([]Var, 0, len(v))
	for varID, elem := range v {
		elems = append(elems, Var{varID: varID, Variable: elem})
	}
	slices.SortFunc(elems, func(i, j Var) int {
		return i.varID - j.varID
	})
	var sb strings.Builder
	for _, elem := range elems {
		_, _ = fmt.Fprintf(&sb, "%4d: %s\n", elem.varID, elem.String())
	}
	return sb.String()
}

type Variable struct {
	Count int
	Reg   int
	LifeTime
}

func (v Variable) String() string {
	return fmt.Sprintf("%d reg(%d) %s", v.Count, v.Reg, v.LifeTime)
}

type Argument struct {
	HeadVarID int
	BodyVarID int
	LifeTime
}

func (a Argument) String() string {
	return fmt.Sprintf("var(%d) var(%d) %s", a.HeadVarID, a.BodyVarID, a.LifeTime)
}

// Index represents a first argument index.
// TODO: What should we do here?
type Index struct {
	Term  term.Handle
	Arity int
}

func (i Index) String() string {
	return fmt.Sprintf("%s/%d", &syntax.Formatter{Term: i.Term}, i.Arity)
}

type Clause struct {
	PI       term.Functor
	FirstArg Index
	MaxRegs  int
	Code     []Instruction
	Execute  term.Functor
}

func (c *Clause) Compile(compiler *Compiler, head, body term.Handle) error {
	h, ok := compiler.Functor(head)
	if !ok {
		return errUnhandled
	}
	b, ok := compiler.Functor(body)
	if !ok {
		return errUnhandled
	}

	// Turns the first argument into a functor for indexing.
	fa := compiler.Arg(head, 0)
	index, err := index(compiler, fa)
	if err != nil {
		return err
	}

	c.PI = h
	c.FirstArg = index

	if err := c.compileHead(compiler, head); err != nil {
		return err
	}
	if err := c.compileBody(compiler, body); err != nil {
		return err
	}

	var (
		maxN = max(h.Arity(), b.Arity())

		vars = Variables{}
		args = make([]Argument, maxN)
	)

	for i := range args {
		args[i].HeadVarID = -1
		args[i].BodyVarID = -1
		args[i].Death = math.MaxInt
	}

	// Replace variables with its variable occurrence.
	// This is where we diverge from the original binprolog.
	// Instead of recording variable occurrences first and deriving lifetime from it later,
	// we record lifetimes at the same time.
	c.findOccurrences(compiler, vars)

	if err := c.fillInfo(compiler, args, vars); err != nil {
		return err
	}

	c.collapseArgs(args, vars)
	c.allocateRegs(compiler, args, vars)
	c.compact()

	return nil
}

func index(compiler *Compiler, t term.Handle) (Index, error) {
	if _, ok := compiler.Variable(t); ok {
		a, err := compiler.PutAtom(term.NewAtomRune('_'))
		if err != nil {
			return Index{}, err
		}
		return Index{
			Term:  a,
			Arity: 0,
		}, nil
	}
	if f, ok := compiler.Functor(t); ok {
		a, err := compiler.PutAtom(f.Name())
		if err != nil {
			return Index{}, err
		}
		return Index{
			Term:  a,
			Arity: f.Arity(),
		}, nil
	}
	return Index{
		Term: t,
	}, nil
}

func (c *Clause) compileHead(compiler *Compiler, head term.Handle) error {
	f, _ := compiler.Functor(head)

	pi := term.NewFunctor(f.Name(), f.Arity()-1)
	if i, ok := compiler.BuiltinIndex[pi]; ok {
		b := compiler.Builtins[i]
		if b.Type == runtime.BuiltinTypeInHead {
			cont := compiler.Arg(head, f.Arity()-1)
			c.emit(Instruction{
				OpCode: OpBuiltin,
				Type:   TypeNotApplicable,
				A:      Operand{Kind: OperandKindBuiltin, Index: i},
				B:      Operand{Kind: OperandKindTerm, Term: cont},
			})
			return nil
		}
	}

	ct, err := compiler.PutCompoundWithFreshVars(f)
	if err != nil {
		return err
	}

	if err := c.emitTopArgs(compiler, OpGet, head, ct); err != nil {
		return err
	}

	return c.compileTopArg(compiler, OpGet, head, ct)
}

func (c *Clause) emitTopArgs(compiler *Compiler, op OpCode, t, ct term.Handle) error {
	f, ok := compiler.Functor(t)
	if !ok {
		return errUnhandled
	}
	for i := 0; i < f.Arity(); i++ {
		a, x := compiler.Arg(t, i), compiler.Arg(ct, i)

		typ, err := classifyArg(compiler, x, a)
		if err != nil {
			return err
		}

		c.emit(Instruction{
			OpCode: op,
			Type:   typ,
			A:      Operand{Kind: OperandKindArgument, Index: i},
			B:      Operand{Kind: OperandKindTerm, Term: x},
		})
	}
	return nil
}

func (c *Clause) compileTopArg(compiler *Compiler, op OpCode, t, ct term.Handle) error {
	f, ok := compiler.Functor(t)
	if !ok {
		return errUnhandled
	}
	for i := 0; i < f.Arity(); i++ {
		a, x := compiler.Arg(t, i), compiler.Arg(ct, i)
		if err := c.compileTopTerm(compiler, op, x, a); err != nil {
			return err
		}
	}
	return nil
}

func (c *Clause) compileTopTerm(compiler *Compiler, op OpCode, x, t term.Handle) error {
	if _, ok := compiler.Variable(t); ok {
		return compiler.Bind(x, t)
	}

	f, ok := compiler.Functor(t)
	if !ok {
		return compiler.Bind(x, t)
	}

	c.emit(Instruction{
		OpCode: op,
		Type:   TypeStructure,
		A:      Operand{Kind: OperandKindFunctor, Functor: f},
		B:      Operand{Kind: OperandKindTerm, Term: x},
	})

	ct, err := compiler.PutCompoundWithFreshVars(f)
	if err != nil {
		return err
	}

	if err := c.emitArgs(compiler, op, t, ct); err != nil {
		return err
	}

	return c.compileArgs(compiler, op, t, ct)
}

func (c *Clause) emitArgs(compiler *Compiler, op OpCode, t, ct term.Handle) error {
	f, _ := compiler.Functor(t)
	for i := range f.Arity() {
		a, x := compiler.Arg(t, i), compiler.Arg(ct, i)
		typ, err := classifyArg(compiler, x, a)
		if err != nil {
			return err
		}

		switch op {
		case OpGet:
			op = OpUnify
		case OpPut:
			if _, ok := compiler.Functor(a); ok {
				op = OpPush
			} else {
				op = OpWrite
			}
		default:
			// Do nothing.
		}

		c.emit(Instruction{
			OpCode: op,
			Type:   typ,
			A:      Operand{Kind: OperandKindGet},
			B:      Operand{Kind: OperandKindTerm, Term: x},
		})
	}
	return nil
}

func (c *Clause) compileArgs(compiler *Compiler, op OpCode, t, ct term.Handle) error {
	f, _ := compiler.Functor(t)
	for i := 0; i < f.Arity(); i++ {
		if err := c.compileTerm(compiler, op, compiler.Arg(ct, i), compiler.Arg(t, i)); err != nil {
			return err
		}
	}
	return nil
}

func (c *Clause) compileTerm(compiler *Compiler, op OpCode, x, t term.Handle) error {
	if _, ok := compiler.Variable(t); ok {
		return compiler.Bind(x, t)
	}

	f, ok := compiler.Functor(t)
	if !ok {
		return compiler.Bind(x, t)
	}

	newOp := op
	if newOp == OpPut {
		newOp = OpPush
	}
	c.emit(Instruction{
		OpCode: newOp,
		Type:   TypeStructure,
		A:      Operand{Kind: OperandKindFunctor, Functor: f},
		B:      Operand{Kind: OperandKindTerm, Term: x},
	})

	ct, err := compiler.PutCompoundWithFreshVars(f)
	if err != nil {
		return err
	}

	return c.emitArgs(compiler, op, t, ct)
}

func (c *Clause) compileBody(compiler *Compiler, body term.Handle) error {
	if _, ok := compiler.Variable(body); ok {
		var err error
		body, err = compiler.PutCompound(term.NewAtom("true"), body)
		if err != nil {
			return err
		}
	}

	if a, ok := compiler.Atom(body); ok && a == term.NewAtom("true") {
		return nil
	}

	f, ok := compiler.Functor(body)
	if !ok {
		return errUnhandled
	}

	switch f {
	case term.NewFunctor(term.NewAtom("$cut_to"), 2):
		cut, cont := compiler.Arg(body, 0), compiler.Arg(body, 1)

		c.emit(Instruction{
			OpCode: OpPut,
			A:      Operand{Kind: OperandKindCutArg, Index: 1},
			B:      Operand{Kind: OperandKindTerm, Term: cut}, // Always `$cut`
		})
		return c.compileBody(compiler, cont)
	case term.NewFunctor(term.NewAtomRune('='), 3):
		a, b, cont := compiler.Arg(body, 0), compiler.Arg(body, 1), compiler.Arg(body, 2)
		if err := c.compileEqual(compiler, a, b); err != nil {
			return err
		}
		return c.compileBody(compiler, cont)
	}

	pi := term.NewFunctor(f.Name(), f.Arity()-1)
	if i, ok := compiler.BuiltinIndex[pi]; ok {
		var (
			b = compiler.Builtins[i]
		)
		switch b.Type {
		case runtime.BuiltinTypeInHead:
			break
		case runtime.BuiltinTypeArithmetic0:
			var (
				cont = compiler.Arg(body, f.Arity()-1)
				pi   = term.NewFunctor(f.Name(), f.Arity()-1)
			)
			newOpArgs, err := compiler.PutCompoundWithFreshVars(pi)
			if err != nil {
				return err
			}
			for i := range pi.Arity() {
				a, x := compiler.Arg(body, i), compiler.Arg(newOpArgs, i)
				typ, err := c.classifyLoad(compiler, x, a)
				if err != nil {
					return err
				}
				c.emit(Instruction{
					OpCode: OpLoad,
					Type:   typ,
					A:      Operand{Kind: OperandKindArgument, Index: i},
					B:      Operand{Kind: OperandKindTerm, Term: x},
				})
			}
			zero, err := compiler.PutInteger(0)
			if err != nil {
				return err
			}
			c.emit(Instruction{
				OpCode: OpArithmetic,
				A:      Operand{Kind: OperandKindBuiltin, Index: i},
				B:      Operand{Kind: OperandKindTerm, Term: zero},
			})
			return c.compileBody(compiler, cont)
		case runtime.BuiltinTypeArithmetic1:
			var (
				cont = compiler.Arg(body, f.Arity()-1)
				pi   = term.NewFunctor(f.Name(), f.Arity()-2)
				res  = compiler.Arg(body, f.Arity()-2)
			)
			varRes, err := compiler.PutVariable()
			if err != nil {
				return err
			}
			if err := c.handleConstantRes(compiler, varRes, res); err != nil {
				return err
			}
			newOpArgs, err := compiler.PutCompoundWithFreshVars(pi)
			if err != nil {
				return err
			}
			for i := range pi.Arity() {
				a, x := compiler.Arg(body, i), compiler.Arg(newOpArgs, i)
				typ, err := c.classifyLoad(compiler, x, a)
				if err != nil {
					return err
				}
				c.emit(Instruction{
					OpCode: OpLoad,
					Type:   typ,
					A:      Operand{Kind: OperandKindArgument, Index: i},
					B:      Operand{Kind: OperandKindTerm, Term: x},
				})
			}
			c.emit(Instruction{
				OpCode: OpArithmetic,
				A:      Operand{Kind: OperandKindBuiltin, Index: i},
				B:      Operand{Kind: OperandKindTerm, Term: varRes},
			})
			return c.compileBody(compiler, cont)
		case runtime.BuiltinTypeInline:
			var cont term.Handle
			switch f.Arity() {
			case 1:
				cont = compiler.Arg(body, 0)
			case 2:
				cont = compiler.Arg(body, 1)
				arg := compiler.Arg(body, 0)
				v, err := compiler.PutVariable()
				if err != nil {
					return err
				}
				if err := c.compileTopTerm(compiler, OpPut, v, arg); err != nil {
					return err
				}
				c.emit(Instruction{
					OpCode: OpPut,
					A:      Operand{Kind: OperandKindTemp, Index: 0},
					B:      Operand{Kind: OperandKindTerm, Term: v},
				})
			default:
				return errors.New("can't inline a builtin with arity more than 1")
			}
			x, err := compiler.PutVariable()
			if err != nil {
				return err
			}
			c.emit(Instruction{
				OpCode: OpInline,
				A:      Operand{Kind: OperandKindBuiltin, Index: i},
				B:      Operand{Kind: OperandKindTerm, Term: x},
			})
			return c.compileBody(compiler, cont)
		}
	}

	c.Execute = f

	ct, err := compiler.PutCompoundWithFreshVars(f)
	if err != nil {
		return err
	}
	return c.emitBodyTopTerm(compiler, body, ct)
}

func (c *Clause) compileEqual(compiler *Compiler, a, b term.Handle) error {
	if _, ok := compiler.Variable(b); ok {
		if _, ok := compiler.Functor(a); !ok {
			a, b = b, a
		}
	}

	v1, err := compiler.PutVariable()
	if err != nil {
		return err
	}

	v2, err := compiler.PutVariable()
	if err != nil {
		return err
	}

	if err := c.compileTopTerm(compiler, OpGet, v1, a); err != nil {
		return err
	}

	c.emit(Instruction{
		OpCode: OpPut,
		A:      Operand{Kind: OperandKindTemp},
		B:      Operand{Kind: OperandKindTerm, Term: v1},
	})
	c.emit(Instruction{
		OpCode: OpGet,
		A:      Operand{Kind: OperandKindTemp},
		B:      Operand{Kind: OperandKindTerm, Term: v2},
	})

	return c.compileTopTerm(compiler, OpPut, v2, b)
}

func (c *Clause) emitBodyTopTerm(compiler *Compiler, t, ct term.Handle) error {
	if err := c.compileTopArg(compiler, OpPut, t, ct); err != nil {
		return err
	}
	return c.emitTopArgs(compiler, OpPut, t, ct)
}

func (c *Clause) emit(inst Instruction) {
	c.Code = append(c.Code, inst)
}

func classifyArg(compiler *Compiler, x, a term.Handle) (Type, error) {
	if _, ok := compiler.Variable(a); ok {
		err := compiler.Bind(x, a)
		return TypeUnknown, err
	}

	if _, ok := compiler.Functor(a); !ok {
		err := compiler.Bind(x, a)
		return TypeConstant, err
	}

	return TypeUnknown, nil
}

func (c *Clause) findOccurrences(compiler *Compiler, vars Variables) {
	for i := range c.Code {
		inst := &c.Code[i]

		if inst.B.Kind != OperandKindTerm {
			continue
		}

		t := inst.B.Term
		t = compiler.Deref(t)
		varID, ok := compiler.Variable(t)
		if !ok {
			continue
		}

		v, ok := vars[varID]
		if !ok {
			v = Variable{
				Reg: -1,
				LifeTime: LifeTime{
					Birth: i,
				},
			}
		}
		v.Count++
		v.Death = i
		vars[varID] = v

		inst.B = Operand{Kind: OperandKindOccurrence, Term: t, Index: v.Count}
	}
}

func (c *Clause) fillInfo(compiler *Compiler, args []Argument, vars map[int]Variable) error {
	for i := range c.Code {
		inst := &c.Code[i]
		c.fillVarType(compiler, inst, vars)

		if inst.A.Kind != OperandKindArgument {
			continue
		}

		a := &args[inst.A.Index]
		switch inst.OpCode {
		case OpGet:
			a.Birth = i
		case OpPut:
			a.Death = i
		default:
			// Do nothing.
		}

		if inst.B.Kind != OperandKindOccurrence {
			continue
		}

		varID, _ := compiler.Variable(inst.B.Term)
		switch inst.OpCode {
		case OpGet:
			a.HeadVarID = varID
		case OpPut:
			a.BodyVarID = varID
		default:
			// Do nothing.
		}
	}
	return nil
}

func (c *Clause) fillVarType(compiler *Compiler, inst *Instruction, vars map[int]Variable) {
	if inst.Type != TypeUnknown {
		return
	}

	defer func() {
		if inst.Type != TypeUnknown {
			return
		}
		inst.Type = TypeConstant
	}()

	if inst.B.Kind != OperandKindOccurrence {
		return
	}

	o := inst.B
	t := o.Term
	varID, ok := compiler.Variable(t)
	if !ok {
		return
	}
	v := vars[varID]
	switch {
	case o.Index == 1 && v.Count == 1 && (inst.OpCode == OpUnify || inst.OpCode == OpWrite):
		inst.Type = TypeVoid
	case o.Index == 1:
		inst.Type = TypeVariable
	default:
		inst.Type = TypeValue
	}
}

func (c *Clause) collapseArgs(args []Argument, vars map[int]Variable) {
	for i := range args {
		a := &args[i]

		var (
			h = Variable{Reg: -1, LifeTime: LifeTime{Birth: 0, Death: math.MaxInt}}
			b = Variable{Reg: -1, LifeTime: LifeTime{Birth: 0, Death: math.MaxInt}}
		)
		if a.HeadVarID >= 0 {
			h = vars[a.HeadVarID]
		}
		if a.BodyVarID >= 0 {
			b = vars[a.BodyVarID]
		}

		switch {
		case a.LifeTime.Contains(h.LifeTime) && h.LifeTime.Precedes(b.LifeTime) && a.LifeTime.Contains(b.LifeTime) && h.Reg < 0 && b.Reg < 0:
			h.Reg = i
			b.Reg = i
			vars[a.HeadVarID] = h
			vars[a.BodyVarID] = b
		case a.LifeTime.Contains(h.LifeTime) && h.Reg < 0:
			h.Reg = i
			vars[a.HeadVarID] = h
		case a.LifeTime.Contains(b.LifeTime) && b.Reg < 0:
			b.Reg = i
			vars[a.BodyVarID] = b
		}
	}
}

func (c *Clause) allocateRegs(compiler *Compiler, args []Argument, vars map[int]Variable) {
	var (
		n        = len(args)
		freeList []int
	)
	for i := range c.Code {
		inst := &c.Code[i]

		if inst.B.Kind != OperandKindOccurrence {
			continue
		}

		o := inst.B
		t := o.Term
		varID, ok := compiler.Variable(t)
		if !ok {
			continue
		}
		v := vars[varID]
		if o.Index == 1 && v.Reg < 0 {
			v.Reg = getReg(&n, &freeList)
			vars[varID] = v
		}
		if o.Index == v.Count && v.Reg >= len(args) {
			freeList = append(freeList, v.Reg)
		}
		inst.B = Operand{Kind: OperandKindRegister, Index: v.Reg}
	}
	c.MaxRegs = n
}

func getReg(n *int, freeList *[]int) int {
	if len(*freeList) > 0 {
		var (
			r int
			l = len(*freeList)
		)
		r, *freeList = (*freeList)[l-1], (*freeList)[:l]
		return r
	}
	r := *n
	*n++
	return r
}

func (c *Clause) compact() {
	c.Code = slices.DeleteFunc(c.Code, func(inst Instruction) bool {
		var (
			getVariable  = inst.OpCode == OpGet && inst.Type == TypeVariable
			putValue     = inst.OpCode == OpPut && inst.Type == TypeValue
			sameRegister = inst.A.Kind == OperandKindArgument && inst.B.Kind == OperandKindRegister && inst.A.Index == inst.B.Index
		)
		return (getVariable || putValue) && sameRegister
	})
}

func (c *Clause) classifyLoad(compiler *Compiler, x, a term.Handle) (Type, error) {
	if _, ok := compiler.Variable(a); ok {
		return TypeUnknown, compiler.Bind(x, a)
	}

	if _, ok := compiler.Functor(a); !ok {
		return TypeConstant, compiler.Bind(x, a)
	}

	return TypeUnknown, c.compileTopTerm(compiler, OpPut, x, a)
}

func (c *Clause) handleConstantRes(compiler *Compiler, x, res term.Handle) error {
	if _, ok := compiler.Variable(res); ok {
		return compiler.Bind(x, res)
	}
	if _, ok := compiler.Functor(res); !ok {
		c.emit(Instruction{
			OpCode: OpPut,
			A:      Operand{Kind: OperandKindTemp, Index: 0},
			B:      Operand{Kind: OperandKindTerm, Term: res},
		})
		c.emit(Instruction{
			OpCode: OpGet,
			A:      Operand{Kind: OperandKindTemp, Index: 0},
			B:      Operand{Kind: OperandKindTerm, Term: x},
		})
		return nil
	}
	return c.compileTopTerm(compiler, OpPut, x, res)
}

type ClauseStringer struct {
	Arena *term.Arena
	Clause
}

func (c ClauseStringer) String() string {
	var sb strings.Builder
	_, _ = fmt.Fprintf(&sb, "\tPI: %s\n", c.PI)
	_, _ = fmt.Fprintf(&sb, "\tfirst_arg: %s\n", c.FirstArg)
	_, _ = fmt.Fprintf(&sb, "\tmax_regs: %d\n", c.MaxRegs)
	_, _ = fmt.Fprintf(&sb, "\tcode:\n")
	for i, inst := range c.Code {
		inst := InstructionStringer{
			Arena:       c.Arena,
			Instruction: inst,
		}
		_, _ = fmt.Fprintf(&sb, "\t%4d: %s\n", i, inst.String())
	}
	_, _ = fmt.Fprintf(&sb, "\texecute: %s\n", c.Execute)
	return sb.String()
}
