package ir

import (
	"fmt"
	"math"
	"slices"
	"strings"

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

func (c *Clause) Compile(heap *term.Heap, head, body term.Handle) error {
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
	index, err := index(heap, fa)
	if err != nil {
		return err
	}

	c.PI = h
	c.FirstArg = index
	c.Execute = b

	if err := c.compileHead(heap, head); err != nil {
		return err
	}
	if err := c.compileBody(heap, body); err != nil {
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

	//
	if err := c.fillInfo(heap, args, vars); err != nil {
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

func index(heap *term.Heap, t term.Handle) (Index, error) {
	if _, ok := t.Variable(); ok {
		a, err := heap.PutAtom(term.NewAtomRune('_'))
		if err != nil {
			return Index{}, err
		}
		return Index{
			Term:  a,
			Arity: 0,
		}, nil
	}
	if f, ok := t.Functor(); ok {
		a, err := heap.PutAtom(f.Name())
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

func (c *Clause) compileHead(heap *term.Heap, head term.Handle) error {
	// TODO: builtins

	f, _ := head.Functor()
	ct, err := heap.PutCompoundWithFreshVars(f)
	if err != nil {
		return err
	}

	if err := c.emitTopArgs(OpGet, head, ct); err != nil {
		return err
	}

	return c.compileTopArg(OpGet, heap, head, ct)
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

func (c *Clause) compileTopArg(op OpCode, heap *term.Heap, t, ct term.Handle) error {
	f, ok := t.Functor()
	if !ok {
		return errUnhandled
	}
	for i := 0; i < f.Arity(); i++ {
		a, x := t.Arg(i), ct.Arg(i)
		if err := c.compileTopTerm(heap, op, x, a); err != nil {
			return err
		}
	}
	return nil
}

func (c *Clause) compileTopTerm(heap *term.Heap, op OpCode, x, t term.Handle) error {
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

	ct, err := heap.PutCompoundWithFreshVars(f)
	if err != nil {
		return err
	}

	if err := c.emitArgs(op, t, ct); err != nil {
		return err
	}

	return c.compileArgs(heap, op, t, ct)
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

func (c *Clause) compileArgs(heap *term.Heap, op OpCode, t, ct term.Handle) error {
	f, _ := t.Functor()
	for i := 0; i < f.Arity(); i++ {
		if err := c.compileTerm(heap, op, ct.Arg(i), t.Arg(i)); err != nil {
			return err
		}
	}
	return nil
}

func (c *Clause) compileTerm(heap *term.Heap, op OpCode, x, t term.Handle) error {
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

	ct, err := heap.PutCompoundWithFreshVars(f)
	if err != nil {
		return err
	}

	return c.emitArgs(op, t, ct)
}

func (c *Clause) compileBody(heap *term.Heap, body term.Handle) error {
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
		return c.compileBody(heap, cont)
	case term.NewFunctor(term.NewAtomRune('='), 3):
		a, b, cont := body.Arg(0), body.Arg(1), body.Arg(2)
		if err := c.compileEqual(heap, a, b); err != nil {
			return err
		}
		return c.compileBody(heap, cont)
	}

	// TODO: builtins

	ct, err := heap.PutCompoundWithFreshVars(f)
	if err != nil {
		return err
	}
	return c.emitBodyTopTerm(heap, body, ct)
}

func (c *Clause) compileEqual(heap *term.Heap, a, b term.Handle) error {
	if _, ok := b.Variable(); ok {
		if _, ok := a.Functor(); !ok {
			a, b = b, a
		}
	}

	v1, err := heap.PutVariable()
	if err != nil {
		return err
	}

	v2, err := heap.PutVariable()
	if err != nil {
		return err
	}

	if err := c.compileTopTerm(heap, OpGet, v1, a); err != nil {
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

	return c.compileTopTerm(heap, OpPut, v2, b)
}

func (c *Clause) emitBodyTopTerm(heap *term.Heap, t, ct term.Handle) error {
	if err := c.compileTopArg(OpPut, heap, t, ct); err != nil {
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

func (c *Clause) fillInfo(heap *term.Heap, args []Argument, vars map[int]Variable) error {
	for i := range c.Code {
		inst := &c.Code[i]

		c.fillVarType(inst, vars)

		if inst.A.Kind != OperandKindArgument || inst.B.Kind != OperandKindOccurrence {
			continue
		}

		t := inst.B.Term

		var varID int
		if v, ok := t.Variable(); ok {
			varID = v
		} else {
			v, err := heap.PutVariable()
			if err != nil {
				return err
			}
			varID, _ = v.Variable()
			vars[varID] = Variable{
				Count:    1,
				Reg:      -1,
				LifeTime: LifeTime{Birth: i, Death: i},
			}
		}

		a := &args[inst.A.Index]
		switch inst.OpCode {
		case OpGet:
			a.HeadVarID = varID
			a.Birth = i
		case OpPut:
			a.BodyVarID = varID
			a.Death = i
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
