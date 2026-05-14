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

func (c *Clause) String() string {
	var sb strings.Builder
	_, _ = fmt.Fprintf(&sb, "\tPI: %s\n", c.PI)
	_, _ = fmt.Fprintf(&sb, "\tfirst_arg: %s\n", c.FirstArg)
	_, _ = fmt.Fprintf(&sb, "\tmax_regs: %d\n", c.MaxRegs)
	_, _ = fmt.Fprintf(&sb, "\tcode:\n")
	for i, inst := range c.Code {
		_, _ = fmt.Fprintf(&sb, "\t%4d: %s\n", i, inst.String())
	}
	_, _ = fmt.Fprintf(&sb, "\texecute: %s\n", c.Execute)
	return sb.String()
}

func (c *Clause) Compile(engine *runtime.Engine, head, body term.Handle) error {
	h, ok := head.Functor()
	if !ok {
		return errUnhandled
	}
	b, ok := body.Functor()
	if !ok {
		return errUnhandled
	}

	// Turns the first argument into a functor for indexing.
	fa := head.Arg(0)
	index, err := index(engine, fa)
	if err != nil {
		return err
	}

	c.PI = h
	c.FirstArg = index

	if err := c.compileHead(engine, head); err != nil {
		return err
	}
	if err := c.compileBody(engine, body); err != nil {
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

	fmt.Printf("raw:\n%s\n", c)
	fmt.Printf("vars:\n%s\n", vars)
	fmt.Printf("args:\n")
	for i, arg := range args {
		fmt.Printf("%4d: %s\n", i, arg)
	}
	fmt.Printf("\n")

	// Replace variables with its variable occurrence.
	// This is where we diverge from the original binprolog.
	// Instead of recording variable occurrences first and deriving lifetime from it later,
	// we record lifetimes at the same time.
	c.findOccurrences(vars)

	fmt.Printf("findOccurrences:\n%s\n", c)
	fmt.Printf("vars:\n%s\n", vars)
	fmt.Printf("args:\n")
	for i, arg := range args {
		fmt.Printf("%4d: %s\n", i, arg)
	}
	fmt.Printf("\n")

	if err := c.fillInfo(args, vars); err != nil {
		return err
	}

	fmt.Printf("fillInfo:\n%s\n", c)
	fmt.Printf("vars:\n%s\n", vars)
	fmt.Printf("args:\n")
	for i, arg := range args {
		fmt.Printf("%4d: %s\n", i, arg)
	}
	fmt.Printf("\n")

	c.collapseArgs(args, vars)

	fmt.Printf("collapseArgs:\n%s\n", c)
	fmt.Printf("vars:\n%s\n", vars)
	fmt.Printf("args:\n")
	for i, arg := range args {
		fmt.Printf("%4d: %s\n", i, arg)
	}
	fmt.Printf("\n")

	c.allocateRegs(args, vars)

	fmt.Printf("allocateRegs:\n%s\n", c)
	fmt.Printf("vars:\n%s\n", vars)
	fmt.Printf("args:\n")
	for i, arg := range args {
		fmt.Printf("%4d: %s\n", i, arg)
	}
	fmt.Printf("\n")

	c.compact()

	return nil
}

func index(engine *runtime.Engine, t term.Handle) (Index, error) {
	if _, ok := t.Variable(); ok {
		a, err := engine.PutAtom(term.NewAtomRune('_'))
		if err != nil {
			return Index{}, err
		}
		return Index{
			Term:  a,
			Arity: 0,
		}, nil
	}
	if f, ok := t.Functor(); ok {
		a, err := engine.PutAtom(f.Name())
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

func (c *Clause) compileHead(engine *runtime.Engine, head term.Handle) error {
	f, _ := head.Functor()

	pi := term.NewFunctor(f.Name(), f.Arity()-1)
	if i, ok := engine.BuiltinIndex[pi]; ok {
		b := engine.Builtins[i]
		if b.Type == runtime.BuiltinTypeInHead {
			cont := head.Arg(f.Arity() - 1)
			c.emit(Instruction{
				OpCode: OpBuiltin,
				Type:   TypeNotApplicable,
				A:      Operand{Kind: OperandKindBuiltin, Index: i},
				B:      Operand{Kind: OperandKindTerm, Term: cont},
			})
			return nil
		}
	}

	ct, err := engine.PutCompoundWithFreshVars(f)
	if err != nil {
		return err
	}

	if err := c.emitTopArgs(OpGet, head, ct); err != nil {
		return err
	}

	return c.compileTopArg(OpGet, engine, head, ct)
}

func (c *Clause) emitTopArgs(op OpCode, t, ct term.Handle) error {
	f, ok := t.Functor()
	if !ok {
		return errUnhandled
	}
	for i := 0; i < f.Arity(); i++ {
		a, x := t.Arg(i), ct.Arg(i)

		typ, err := classifyArg(x, a)
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

func (c *Clause) compileTopArg(op OpCode, engine *runtime.Engine, t, ct term.Handle) error {
	f, ok := t.Functor()
	if !ok {
		return errUnhandled
	}
	for i := 0; i < f.Arity(); i++ {
		a, x := t.Arg(i), ct.Arg(i)
		if err := c.compileTopTerm(engine, op, x, a); err != nil {
			return err
		}
	}
	return nil
}

func (c *Clause) compileTopTerm(engine *runtime.Engine, op OpCode, x, t term.Handle) error {
	if _, ok := t.Variable(); ok {
		return x.Bind(t)
	}

	f, ok := t.Functor()
	if !ok {
		return x.Bind(t)
	}

	c.emit(Instruction{
		OpCode: op,
		Type:   TypeStructure,
		A:      Operand{Kind: OperandKindFunctor, Functor: f},
		B:      Operand{Kind: OperandKindTerm, Term: x},
	})

	ct, err := engine.PutCompoundWithFreshVars(f)
	if err != nil {
		return err
	}

	if err := c.emitArgs(op, t, ct); err != nil {
		return err
	}

	return c.compileArgs(engine, op, t, ct)
}

func (c *Clause) emitArgs(op OpCode, t, ct term.Handle) error {
	f, _ := t.Functor()
	for i := range f.Arity() {
		a, x := t.Arg(i), ct.Arg(i)
		typ, err := classifyArg(x, a)
		if err != nil {
			return err
		}

		switch op {
		case OpGet:
			op = OpUnify
		case OpPut:
			if _, ok := a.Functor(); ok {
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

func (c *Clause) compileArgs(engine *runtime.Engine, op OpCode, t, ct term.Handle) error {
	f, _ := t.Functor()
	for i := 0; i < f.Arity(); i++ {
		if err := c.compileTerm(engine, op, ct.Arg(i), t.Arg(i)); err != nil {
			return err
		}
	}
	return nil
}

func (c *Clause) compileTerm(engine *runtime.Engine, op OpCode, x, t term.Handle) error {
	if _, ok := t.Variable(); ok {
		return x.Bind(t)
	}

	f, ok := t.Functor()
	if !ok {
		return x.Bind(t)
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

	ct, err := engine.PutCompoundWithFreshVars(f)
	if err != nil {
		return err
	}

	return c.emitArgs(op, t, ct)
}

func (c *Clause) compileBody(engine *runtime.Engine, body term.Handle) error {
	if _, ok := body.Variable(); ok {
		var err error
		body, err = engine.PutCompound(term.NewAtom("true"), body)
		if err != nil {
			return err
		}
	}

	if a, ok := body.Atom(); ok && a == term.NewAtom("true") {
		return nil
	}

	f, ok := body.Functor()
	if !ok {
		return errUnhandled
	}

	switch f {
	case term.NewFunctor(term.NewAtom("$cut_to"), 2):
		cut, cont := body.Arg(0), body.Arg(1)

		c.emit(Instruction{
			OpCode: OpPut,
			A:      Operand{Kind: OperandKindCutArg, Index: 1},
			B:      Operand{Kind: OperandKindTerm, Term: cut}, // Always `$cut`
		})
		return c.compileBody(engine, cont)
	case term.NewFunctor(term.NewAtomRune('='), 3):
		a, b, cont := body.Arg(0), body.Arg(1), body.Arg(2)
		if err := c.compileEqual(engine, a, b); err != nil {
			return err
		}
		return c.compileBody(engine, cont)
	}

	pi := term.NewFunctor(f.Name(), f.Arity()-1)
	if i, ok := engine.BuiltinIndex[pi]; ok {
		var (
			b = engine.Builtins[i]
		)
		switch b.Type {
		case runtime.BuiltinTypeInHead:
			break
		case runtime.BuiltinTypeArithmetic0:
			var (
				cont = body.Arg(f.Arity() - 1)
				pi   = term.NewFunctor(f.Name(), f.Arity()-1)
			)
			newOpArgs, err := engine.PutCompoundWithFreshVars(pi)
			if err != nil {
				return err
			}
			for i := range pi.Arity() {
				a, x := body.Arg(i), newOpArgs.Arg(i)
				typ, err := c.classifyLoad(engine, x, a)
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
			zero, err := engine.PutInteger(0)
			if err != nil {
				return err
			}
			c.emit(Instruction{
				OpCode: OpArithmetic,
				A:      Operand{Kind: OperandKindBuiltin, Index: i},
				B:      Operand{Kind: OperandKindTerm, Term: zero},
			})
			return c.compileBody(engine, cont)
		case runtime.BuiltinTypeArithmetic1:
			var (
				cont = body.Arg(f.Arity() - 1)
				pi   = term.NewFunctor(f.Name(), f.Arity()-2)
				res  = body.Arg(f.Arity() - 2)
			)
			varRes, err := engine.PutVariable()
			if err != nil {
				return err
			}
			if err := c.handleConstantRes(engine, varRes, res); err != nil {
				return err
			}
			newOpArgs, err := engine.PutCompoundWithFreshVars(pi)
			if err != nil {
				return err
			}
			for i := range pi.Arity() {
				a, x := body.Arg(i), newOpArgs.Arg(i)
				typ, err := c.classifyLoad(engine, x, a)
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
			return c.compileBody(engine, cont)
		case runtime.BuiltinTypeInline:
			var cont term.Handle
			switch f.Arity() {
			case 1:
				cont = body.Arg(0)
			case 2:
				cont = body.Arg(1)
				arg := body.Arg(0)
				v, err := engine.PutVariable()
				if err != nil {
					return err
				}
				if err := c.compileTopTerm(engine, OpPut, v, arg); err != nil {
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
			x, err := engine.PutVariable()
			if err != nil {
				return err
			}
			c.emit(Instruction{
				OpCode: OpInline,
				A:      Operand{Kind: OperandKindBuiltin, Index: i},
				B:      Operand{Kind: OperandKindTerm, Term: x},
			})
			return c.compileBody(engine, cont)
		}
	}

	c.Execute = f

	ct, err := engine.PutCompoundWithFreshVars(f)
	if err != nil {
		return err
	}
	return c.emitBodyTopTerm(engine, body, ct)
}

func (c *Clause) compileEqual(engine *runtime.Engine, a, b term.Handle) error {
	if _, ok := b.Variable(); ok {
		if _, ok := a.Functor(); !ok {
			a, b = b, a
		}
	}

	v1, err := engine.PutVariable()
	if err != nil {
		return err
	}

	v2, err := engine.PutVariable()
	if err != nil {
		return err
	}

	if err := c.compileTopTerm(engine, OpGet, v1, a); err != nil {
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

	return c.compileTopTerm(engine, OpPut, v2, b)
}

func (c *Clause) emitBodyTopTerm(engine *runtime.Engine, t, ct term.Handle) error {
	if err := c.compileTopArg(OpPut, engine, t, ct); err != nil {
		return err
	}
	return c.emitTopArgs(OpPut, t, ct)
}

func (c *Clause) emit(inst Instruction) {
	c.Code = append(c.Code, inst)
}

func classifyArg(x, a term.Handle) (Type, error) {
	if _, ok := a.Variable(); ok {
		err := x.Bind(a)
		return TypeUnknown, err
	}

	if _, ok := a.Functor(); !ok {
		err := x.Bind(a)
		return TypeConstant, err
	}

	return TypeUnknown, nil
}

func (c *Clause) findOccurrences(vars Variables) {
	for i := range c.Code {
		inst := &c.Code[i]

		if inst.B.Kind != OperandKindTerm {
			continue
		}

		t := inst.B.Term
		t = t.Deref()
		varID, ok := t.Variable()
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

func (c *Clause) fillInfo(args []Argument, vars map[int]Variable) error {
	for i := range c.Code {
		inst := &c.Code[i]
		c.fillVarType(inst, vars)

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

		varID, _ := inst.B.Term.Variable()
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

func (c *Clause) fillVarType(inst *Instruction, vars map[int]Variable) {
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
	varID, ok := t.Variable()
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

func (c *Clause) allocateRegs(args []Argument, vars map[int]Variable) {
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
		varID, ok := t.Variable()
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

func (c *Clause) classifyLoad(engine *runtime.Engine, x, a term.Handle) (Type, error) {
	if _, ok := a.Variable(); ok {
		return TypeUnknown, x.Bind(a)
	}

	if _, ok := a.Functor(); !ok {
		return TypeConstant, x.Bind(a)
	}

	return TypeUnknown, c.compileTopTerm(engine, OpPut, x, a)
}

func (c *Clause) handleConstantRes(engine *runtime.Engine, x, res term.Handle) error {
	if _, ok := res.Variable(); ok {
		return x.Bind(res)
	}
	if _, ok := res.Functor(); !ok {
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
	return c.compileTopTerm(engine, OpPut, x, res)
}
