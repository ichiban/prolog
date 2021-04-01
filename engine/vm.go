package engine

import (
	"errors"
	"fmt"
	"io"

	"github.com/sirupsen/logrus"
)

const (
	opVoid byte = iota
	opEnter
	opCall
	opExit
	opConst
	opVar
	opFunctor
	opPop
)

// VM is the core of a Prolog interpreter. The zero value for VM is a valid VM without any builtin predicates.
type VM struct {
	// BeforeHalt is a hook which gets triggered right before halt/0 or halt/1.
	BeforeHalt []func()

	operators       Operators
	procedures      map[procedureIndicator]procedure
	streams         map[Term]*Stream
	input, output   *Stream
	charConversions map[rune]rune
	charConvEnabled bool
	debug           bool
	unknown         unknownAction
}

func (vm *VM) SetUserInput(r io.Reader) {
	const userInput = Atom("user_input")
	if vm.streams == nil {
		vm.streams = map[Term]*Stream{}
	}
	vm.streams[userInput] = &Stream{
		source: r,
		mode:   streamModeRead,
		alias:  userInput,
	}
}

func (vm *VM) SetUserOutput(w io.Writer) {
	const userOutput = Atom("user_output")
	if vm.streams == nil {
		vm.streams = map[Term]*Stream{}
	}
	vm.streams[userOutput] = &Stream{
		sink:  w,
		mode:  streamModeWrite,
		alias: userOutput,
	}
}

// Register0 registers a predicate of arity 0.
func (vm *VM) Register0(name string, p func(func() Promise) Promise) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: Atom(name), arity: 0}] = predicate0(p)
}

// Register1 registers a predicate of arity 1.
func (vm *VM) Register1(name string, p func(Term, func() Promise) Promise) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: Atom(name), arity: 1}] = predicate1(p)
}

// Register2 registers a predicate of arity 2.
func (vm *VM) Register2(name string, p func(Term, Term, func() Promise) Promise) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: Atom(name), arity: 2}] = predicate2(p)
}

// Register3 registers a predicate of arity 3.
func (vm *VM) Register3(name string, p func(Term, Term, Term, func() Promise) Promise) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: Atom(name), arity: 3}] = predicate3(p)
}

// Register4 registers a predicate of arity 4.
func (vm *VM) Register4(name string, p func(Term, Term, Term, Term, func() Promise) Promise) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: Atom(name), arity: 4}] = predicate4(p)
}

// Register5 registers a predicate of arity 5.
func (vm *VM) Register5(name string, p func(Term, Term, Term, Term, Term, func() Promise) Promise) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: Atom(name), arity: 5}] = predicate5(p)
}

type unknownAction int

const (
	unknownError unknownAction = iota
	unknownFail
	unknownWarning
)

func (u unknownAction) String() string {
	switch u {
	case unknownError:
		return "error"
	case unknownFail:
		return "fail"
	case unknownWarning:
		return "warning"
	default:
		return fmt.Sprintf("unknown(%d)", u)
	}
}

type procedure interface {
	Call(*VM, Term, func() Promise) Promise
}

func (vm *VM) arrive(pi procedureIndicator, args Term, k func() Promise) Promise {
	p := vm.procedures[pi]
	if p == nil {
		switch vm.unknown {
		case unknownError:
			return Error(existenceErrorProcedure(&Compound{
				Functor: "/",
				Args:    []Term{pi.name, pi.arity},
			}))
		case unknownWarning:
			logrus.WithField("procedure", pi).Warn("unknown procedure")
			fallthrough
		case unknownFail:
			return Bool(false)
		default:
			return Error(systemError(fmt.Errorf("unknown unknown: %s", vm.unknown)))
		}
	}

	return Delay(func() Promise {
		return p.Call(vm, args, k)
	})
}

func (vm *VM) exec(pc bytecode, xr []Term, vars []*Variable, k func() Promise, args, astack Term) Promise {
	for len(pc) != 0 {
		switch pc[0] {
		case opVoid:
			pc = pc[1:]
		case opConst:
			x := xr[pc[1]]
			var arest Variable
			cons := Compound{
				Functor: ".",
				Args:    []Term{x, &arest},
			}
			if !args.Unify(&cons, false) {
				return Bool(false)
			}
			pc = pc[2:]
			args = &arest
		case opVar:
			v := vars[pc[1]]
			var arest Variable
			cons := Compound{
				Functor: ".",
				Args:    []Term{v, &arest},
			}
			if !args.Unify(&cons, false) {
				return Bool(false)
			}
			pc = pc[2:]
			args = &arest
		case opFunctor:
			x := xr[pc[1]]
			var arg, arest Variable
			cons1 := Compound{
				Functor: ".",
				Args:    []Term{&arg, &arest},
			}
			if !args.Unify(&cons1, false) {
				return Bool(false)
			}
			pf, ok := x.(procedureIndicator)
			if !ok {
				return Error(errors.New("not a principal functor"))
			}
			ok, err := Functor(&arg, pf.name, pf.arity, Done).Force()
			if err != nil {
				return Error(err)
			}
			if !ok {
				return Bool(false)
			}
			pc = pc[2:]
			args = &Variable{}
			cons2 := Compound{
				Functor: ".",
				Args:    []Term{pf.name, args},
			}
			ok, err = Univ(&arg, &cons2, Done).Force()
			if err != nil {
				return Error(err)
			}
			if !ok {
				return Bool(false)
			}
			astack = Cons(&arest, astack)
		case opPop:
			if !args.Unify(List(), false) {
				return Bool(false)
			}
			pc = pc[1:]
			var a, arest Variable
			cons := Compound{
				Functor: ".",
				Args:    []Term{&a, &arest},
			}
			if !astack.Unify(&cons, false) {
				return Bool(false)
			}
			args = &a
			astack = &arest
		case opEnter:
			if !args.Unify(List(), false) {
				return Bool(false)
			}
			if !astack.Unify(List(), false) {
				return Bool(false)
			}
			pc = pc[1:]
			var v Variable
			args = &v
			astack = &v
		case opCall:
			x := xr[pc[1]]
			if !args.Unify(List(), false) {
				return Bool(false)
			}
			pc = pc[2:]
			pf, ok := x.(procedureIndicator)
			if !ok {
				return Error(errors.New("not a principal functor"))
			}
			return Delay(func() Promise {
				return vm.arrive(pf, astack, func() Promise {
					var v Variable
					return Delay(func() Promise {
						return vm.exec(pc, xr, vars, k, &v, &v)
					})
				})
			})
		case opExit:
			return Delay(k)
		default:
			return Error(fmt.Errorf("unknown(%d)", pc[0]))
		}
	}
	return Error(errors.New("non-exit end of bytecode"))
}

type clauses []clause

func (cs clauses) Call(e *VM, args Term, k func() Promise) Promise {
	if len(cs) == 0 {
		return Bool(false)
	}

	fvs := FreeVariables(args)
	ks := make([]func() Promise, len(cs))
	for i := range cs {
		c := cs[i]
		ks[i] = func() Promise {
			ResetVariables(fvs...)
			vars := make([]*Variable, len(c.vars))
			for i := range c.vars {
				vars[i] = &Variable{}
			}
			return e.exec(c.bytecode, c.xrTable, vars, k, args, List())
		}
	}
	return Delay(ks...)
}

type clause struct {
	pf       procedureIndicator
	raw      Term
	xrTable  []Term
	vars     []*Variable
	bytecode bytecode
}

func (c *clause) compile(t Term) error {
	t = Resolve(t)
	c.raw = t
	switch t := t.(type) {
	case Atom:
		return c.compileClause(t, nil)
	case *Compound:
		if t.Functor == ":-" {
			return c.compileClause(t.Args[0], t.Args[1])
		}
		return c.compileClause(t, nil)
	default:
		return typeErrorCallable(t)
	}
}

func (c *clause) compileClause(head Term, body Term) error {
	switch head := head.(type) {
	case Atom:
	case *Compound:
		for _, a := range head.Args {
			if err := c.compileArg(a); err != nil {
				return err
			}
		}
	default:
		return typeErrorCallable(head)
	}
	if body != nil {
		c.bytecode = append(c.bytecode, opEnter)
		for {
			p, ok := body.(*Compound)
			if !ok || p.Functor != "," || len(p.Args) != 2 {
				break
			}
			if err := c.compilePred(p.Args[0]); err != nil {
				return err
			}
			body = p.Args[1]
		}
		if err := c.compilePred(body); err != nil {
			return err
		}
	}
	c.bytecode = append(c.bytecode, opExit)
	return nil
}

func (c *clause) compilePred(p Term) error {
	switch p := p.(type) {
	case Atom:
		c.bytecode = append(c.bytecode, opCall, c.xrOffset(procedureIndicator{name: p, arity: 0}))
		return nil
	case *Compound:
		for _, a := range p.Args {
			if err := c.compileArg(a); err != nil {
				return err
			}
		}
		c.bytecode = append(c.bytecode, opCall, c.xrOffset(procedureIndicator{name: p.Functor, arity: Integer(len(p.Args))}))
		return nil
	default:
		return typeErrorCallable(p)
	}
}

func (c *clause) compileArg(a Term) error {
	switch a := a.(type) {
	case *Variable:
		c.bytecode = append(c.bytecode, opVar, c.varOffset(a))
	case Float, Integer, Atom:
		c.bytecode = append(c.bytecode, opConst, c.xrOffset(a))
	case *Compound:
		c.bytecode = append(c.bytecode, opFunctor, c.xrOffset(procedureIndicator{name: a.Functor, arity: Integer(len(a.Args))}))
		for _, n := range a.Args {
			if err := c.compileArg(n); err != nil {
				return err
			}
		}
		c.bytecode = append(c.bytecode, opPop)
	default:
		return systemError(fmt.Errorf("unknown argument: %s", a))
	}
	return nil
}

func (c *clause) xrOffset(o Term) byte {
	for i, r := range c.xrTable {
		if r.Unify(o, false) {
			return byte(i)
		}
	}
	c.xrTable = append(c.xrTable, o)
	return byte(len(c.xrTable) - 1)
}

func (c *clause) varOffset(o *Variable) byte {
	for i, v := range c.vars {
		if v == o {
			return byte(i)
		}
	}
	o.Name = ""
	c.vars = append(c.vars, o)
	return byte(len(c.vars) - 1)
}

type bytecode []byte

type predicate0 func(func() Promise) Promise

func (p predicate0) Call(e *VM, args Term, k func() Promise) Promise {
	if !args.Unify(List(), false) {
		return Error(errors.New("wrong number of arguments"))
	}

	return p(func() Promise {
		return e.exec([]byte{opExit}, nil, nil, k, nil, nil)
	})
}

type predicate1 func(Term, func() Promise) Promise

func (p predicate1) Call(e *VM, args Term, k func() Promise) Promise {
	var v1 Variable
	if !args.Unify(List(&v1), false) {
		return Error(fmt.Errorf("wrong number of arguments: %s", args))
	}

	return p(&v1, func() Promise {
		return e.exec([]byte{opExit}, nil, nil, k, nil, nil)
	})
}

type predicate2 func(Term, Term, func() Promise) Promise

func (p predicate2) Call(e *VM, args Term, k func() Promise) Promise {
	var v1, v2 Variable
	if !args.Unify(List(&v1, &v2), false) {
		return Error(errors.New("wrong number of arguments"))
	}

	return p(&v1, &v2, func() Promise {
		return e.exec([]byte{opExit}, nil, nil, k, nil, nil)
	})
}

type predicate3 func(Term, Term, Term, func() Promise) Promise

func (p predicate3) Call(e *VM, args Term, k func() Promise) Promise {
	var v1, v2, v3 Variable
	if !args.Unify(List(&v1, &v2, &v3), false) {
		return Error(errors.New("wrong number of arguments"))
	}

	return p(&v1, &v2, &v3, func() Promise {
		return e.exec([]byte{opExit}, nil, nil, k, nil, nil)
	})
}

type predicate4 func(Term, Term, Term, Term, func() Promise) Promise

func (p predicate4) Call(e *VM, args Term, k func() Promise) Promise {
	var v1, v2, v3, v4 Variable
	if !args.Unify(List(&v1, &v2, &v3, &v4), false) {
		return Error(errors.New("wrong number of arguments"))
	}

	return p(&v1, &v2, &v3, &v4, func() Promise {
		return e.exec([]byte{opExit}, nil, nil, k, nil, nil)
	})
}

type predicate5 func(Term, Term, Term, Term, Term, func() Promise) Promise

func (p predicate5) Call(e *VM, args Term, k func() Promise) Promise {
	var v1, v2, v3, v4, v5 Variable
	if !args.Unify(List(&v1, &v2, &v3, &v4, &v5), false) {
		return Error(errors.New("wrong number of arguments"))
	}

	return p(&v1, &v2, &v3, &v4, &v5, func() Promise {
		return e.exec([]byte{opExit}, nil, nil, k, nil, nil)
	})
}

func FreeVariables(ts ...Term) []*Variable {
	var fvs []*Variable
	for _, t := range ts {
		fvs = appendFreeVariables(fvs, t)
	}
	return fvs
}

func appendFreeVariables(fvs []*Variable, t Term) []*Variable {
	switch t := t.(type) {
	case *Variable:
		if t.Ref != nil {
			return appendFreeVariables(fvs, t.Ref)
		}
		for _, v := range fvs {
			if v == t {
				return fvs
			}
		}
		return append(fvs, t)
	case *Compound:
		for _, arg := range t.Args {
			fvs = appendFreeVariables(fvs, arg)
		}
	}
	return fvs
}

func ResetVariables(vs ...*Variable) {
	for _, v := range vs {
		v.Ref = nil
	}
}

// Done terminates a continuation chain.
func Done() Promise {
	return Bool(true)
}
