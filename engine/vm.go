package engine

import (
	"bytes"
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

	opCut
)

// VM is the core of a Prolog interpreter. The zero value for VM is a valid VM without any builtin predicates.
type VM struct {
	// OnHalt is a hook which gets triggered right before halt/0 or halt/1.
	OnHalt func()

	// OnArrive is a hook which gets triggered when the execution reached to a procedure.
	OnArrive                       func(goal Term)
	OnExec                         func(op string, arg Term, env Env)
	OnCall, OnExit, OnFail, OnRedo func(pi string, args Term, env Env)
	OnPanic                        func(r interface{})

	operators       Operators
	procedures      map[procedureIndicator]procedure
	streams         map[Term]*Stream
	input, output   *Stream
	charConversions map[rune]rune
	charConvEnabled bool
	debug           bool
	unknown         unknownAction
}

// SetUserInput sets the given reader as a stream with an alias of user_input.
func (vm *VM) SetUserInput(r io.Reader) {
	const userInput = Atom("user_input")

	s := Stream{
		source: r,
		mode:   streamModeRead,
		alias:  userInput,
	}

	if vm.streams == nil {
		vm.streams = map[Term]*Stream{}
	}
	vm.streams[userInput] = &s

	vm.input = &s
}

// SetUserOutput sets the given writer as a stream with an alias of user_output.
func (vm *VM) SetUserOutput(w io.Writer) {
	const userOutput = Atom("user_output")

	s := Stream{
		sink:  w,
		mode:  streamModeWrite,
		alias: userOutput,
	}

	if vm.streams == nil {
		vm.streams = map[Term]*Stream{}
	}
	vm.streams[userOutput] = &s

	vm.output = &s
}

func (vm *VM) DescribeTerm(t Term, env Env) string {
	var buf bytes.Buffer
	_ = t.WriteTerm(&buf, WriteTermOptions{
		Quoted:      true,
		Ops:         vm.operators,
		Descriptive: true,
	}, env)
	return buf.String()
}

// Register0 registers a predicate of arity 0.
func (vm *VM) Register0(name string, p func(func(Env) *Promise, *Env) *Promise) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: Atom(name), arity: 0}] = predicate0(p)
}

// Register1 registers a predicate of arity 1.
func (vm *VM) Register1(name string, p func(Term, func(Env) *Promise, *Env) *Promise) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: Atom(name), arity: 1}] = predicate1(p)
}

// Register2 registers a predicate of arity 2.
func (vm *VM) Register2(name string, p func(Term, Term, func(Env) *Promise, *Env) *Promise) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: Atom(name), arity: 2}] = predicate2(p)
}

// Register3 registers a predicate of arity 3.
func (vm *VM) Register3(name string, p func(Term, Term, Term, func(Env) *Promise, *Env) *Promise) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: Atom(name), arity: 3}] = predicate3(p)
}

// Register4 registers a predicate of arity 4.
func (vm *VM) Register4(name string, p func(Term, Term, Term, Term, func(Env) *Promise, *Env) *Promise) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: Atom(name), arity: 4}] = predicate4(p)
}

// Register5 registers a predicate of arity 5.
func (vm *VM) Register5(name string, p func(Term, Term, Term, Term, Term, func(Env) *Promise, *Env) *Promise) {
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
	Call(*VM, Term, func(Env) *Promise, *Env) *Promise
}

func (vm *VM) arrive(pi procedureIndicator, args Term, k func(Env) *Promise, env *Env) *Promise {
	if vm.OnArrive == nil {
		vm.OnArrive = func(goal Term) {}
	}
	var as []Term
	if err := Each(args, func(elem Term) error {
		as = append(as, elem)
		return nil
	}, *env); err != nil {
		return Error(systemError(err))
	}
	if len(as) == 0 {
		vm.OnArrive(pi.name)
	} else {
		vm.OnArrive(&Compound{Functor: pi.name, Args: as})
	}

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

	return Delay(func() *Promise {
		env := *env
		return p.Call(vm, args, k, &env)
	})
}

type cont struct {
	exit func(Env) *Promise
	fail func(Env) *Promise
}

func (vm *VM) exec(pc bytecode, xr []Term, vars []Variable, k cont, args, astack Term, env *Env, cutParent *Promise) *Promise {
	if cutParent == nil {
		cutParent = &Promise{}
	}
	if vm.OnExec == nil {
		vm.OnExec = func(op string, arg Term, env Env) {}
	}
	for len(pc) != 0 {
		switch pc[0] {
		case opVoid:
			vm.OnExec("void", nil, *env)
			pc = pc[1:]
		case opConst:
			x := xr[pc[1]]
			vm.OnExec("const", x, *env)
			arest := NewVariable()
			cons := Compound{
				Functor: ".",
				Args:    []Term{x, arest},
			}
			if !args.Unify(&cons, false, env) {
				return k.fail(*env)
			}
			pc = pc[2:]
			args = arest
		case opVar:
			v := vars[pc[1]]
			vm.OnExec("var", v, *env)
			arest := NewVariable()
			cons := Compound{
				Functor: ".",
				Args:    []Term{v, arest},
			}
			if !args.Unify(&cons, false, env) {
				return k.fail(*env)
			}
			pc = pc[2:]
			args = arest
		case opFunctor:
			x := xr[pc[1]]
			vm.OnExec("functor", x, *env)
			arg, arest := NewVariable(), NewVariable()
			cons1 := Compound{
				Functor: ".",
				Args:    []Term{arg, arest},
			}
			if !args.Unify(&cons1, false, env) {
				return k.fail(*env)
			}
			pf, ok := x.(procedureIndicator)
			if !ok {
				return Error(errors.New("not a principal functor"))
			}
			ok, err := Functor(arg, pf.name, pf.arity, func(e Env) *Promise {
				env = &e
				return Bool(true)
			}, env).Force()
			if err != nil {
				return Error(err)
			}
			if !ok {
				return k.fail(*env)
			}
			pc = pc[2:]
			args = NewVariable()
			cons2 := Compound{
				Functor: ".",
				Args:    []Term{pf.name, args},
			}
			ok, err = Univ(arg, &cons2, func(e Env) *Promise {
				env = &e
				return Bool(true)
			}, env).Force()
			if err != nil {
				return Error(err)
			}
			if !ok {
				return k.fail(*env)
			}
			astack = Cons(arest, astack)
		case opPop:
			vm.OnExec("pop", nil, *env)
			if !args.Unify(List(), false, env) {
				return k.fail(*env)
			}
			pc = pc[1:]
			a, arest := NewVariable(), NewVariable()
			cons := Compound{
				Functor: ".",
				Args:    []Term{a, arest},
			}
			if !astack.Unify(&cons, false, env) {
				return k.fail(*env)
			}
			args = a
			astack = arest
		case opEnter:
			vm.OnExec("enter", nil, *env)
			if !args.Unify(List(), false, env) {
				return k.fail(*env)
			}
			if !astack.Unify(List(), false, env) {
				return k.fail(*env)
			}
			pc = pc[1:]
			v := NewVariable()
			args = v
			astack = v
		case opCall:
			x := xr[pc[1]]
			vm.OnExec("call", x, *env)
			if !args.Unify(List(), false, env) {
				return k.fail(*env)
			}
			pc = pc[2:]
			pi, ok := x.(procedureIndicator)
			if !ok {
				return Error(errors.New("not a principal functor"))
			}
			return Delay(func() *Promise {
				env := *env
				return vm.arrive(pi, astack, func(env Env) *Promise {
					v := NewVariable()
					return vm.exec(pc, xr, vars, k, v, v, &env, cutParent)
				}, &env)
			})
		case opExit:
			vm.OnExec("exit", nil, *env)
			return k.exit(*env)
		case opCut:
			vm.OnExec("cut", nil, *env)
			pc = pc[1:]
			return Cut(Delay(func() *Promise {
				env := *env
				return vm.exec(pc, xr, vars, k, args, astack, &env, cutParent)
			}), cutParent)
		default:
			return Error(fmt.Errorf("unknown(%d)", pc[0]))
		}
	}
	return Error(errors.New("non-exit end of bytecode"))
}

type clauses []clause

func (cs clauses) Call(vm *VM, args Term, k func(Env) *Promise, env *Env) *Promise {
	if len(cs) == 0 {
		return Bool(false)
	}

	if vm.OnCall == nil {
		vm.OnCall = func(pi string, args Term, env Env) {}
	}
	if vm.OnExit == nil {
		vm.OnExit = func(pi string, args Term, env Env) {}
	}
	if vm.OnFail == nil {
		vm.OnFail = func(pi string, args Term, env Env) {}
	}
	if vm.OnRedo == nil {
		vm.OnRedo = func(pi string, args Term, env Env) {}
	}

	var p *Promise
	ks := make([]func() *Promise, len(cs))
	for i := range cs {
		i, c := i, cs[i]
		ks[i] = func() *Promise {
			if i == 0 {
				vm.OnCall(c.pi.String(), args, *env)
			} else {
				vm.OnRedo(c.pi.String(), args, *env)
			}
			vars := make([]Variable, len(c.vars))
			for i := range c.vars {
				vars[i] = NewVariable()
			}
			return Delay(func() *Promise {
				env := *env
				return vm.exec(c.bytecode, c.xrTable, vars, cont{
					exit: func(env Env) *Promise {
						vm.OnExit(c.pi.String(), args, env)
						return k(env)
					},
					fail: func(env Env) *Promise {
						vm.OnFail(c.pi.String(), args, env)
						return Bool(false)
					},
				}, args, List(), &env, p)
			})
		}
	}
	p = Delay(ks...)
	return p
}

type clause struct {
	pi       procedureIndicator
	raw      Term
	xrTable  []Term
	vars     []Variable
	bytecode bytecode
}

func (c *clause) compile(t Term, env Env) error {
	t = env.Resolve(t)
	c.raw = t
	switch t := t.(type) {
	case Variable:
		return instantiationError(t)
	case Atom:
		return c.compileClause(t, nil, env)
	case *Compound:
		if t.Functor == ":-" {
			return c.compileClause(t.Args[0], t.Args[1], env)
		}
		return c.compileClause(t, nil, env)
	default:
		return typeErrorCallable(t)
	}
}

func (c *clause) compileClause(head Term, body Term, env Env) error {
	switch head := env.Resolve(head).(type) {
	case Variable:
		return instantiationError(head)
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
			if err := c.compilePred(p.Args[0], env); err != nil {
				return err
			}
			body = p.Args[1]
		}
		if err := c.compilePred(body, env); err != nil {
			return err
		}
	}
	c.bytecode = append(c.bytecode, opExit)
	return nil
}

func (c *clause) compilePred(p Term, env Env) error {
	switch p := env.Resolve(p).(type) {
	case Variable:
		return instantiationError(p)
	case Atom:
		if p == "!" {
			c.bytecode = append(c.bytecode, opCut)
			return nil
		}
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
	case Variable:
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
		if r.Unify(o, false, nil) {
			return byte(i)
		}
	}
	c.xrTable = append(c.xrTable, o)
	return byte(len(c.xrTable) - 1)
}

func (c *clause) varOffset(o Variable) byte {
	for i, v := range c.vars {
		if v == o {
			return byte(i)
		}
	}
	c.vars = append(c.vars, o)
	return byte(len(c.vars) - 1)
}

type bytecode []byte

type predicate0 func(func(Env) *Promise, *Env) *Promise

func (p predicate0) Call(e *VM, args Term, k func(Env) *Promise, env *Env) *Promise {
	if !args.Unify(List(), false, env) {
		return Error(errors.New("wrong number of arguments"))
	}

	return p(func(env Env) *Promise {
		return e.exec([]byte{opExit}, nil, nil, cont{
			exit: k,
			fail: Failure,
		}, nil, nil, &env, nil)
	}, env)
}

type predicate1 func(Term, func(Env) *Promise, *Env) *Promise

func (p predicate1) Call(e *VM, args Term, k func(Env) *Promise, env *Env) *Promise {
	v1 := NewVariable()
	if !args.Unify(List(v1), false, env) {
		return Error(fmt.Errorf("wrong number of arguments: %s", args))
	}

	return p(v1, func(env Env) *Promise {
		return e.exec([]byte{opExit}, nil, nil, cont{
			exit: k,
			fail: Failure,
		}, nil, nil, &env, nil)
	}, env)
}

type predicate2 func(Term, Term, func(Env) *Promise, *Env) *Promise

func (p predicate2) Call(e *VM, args Term, k func(Env) *Promise, env *Env) *Promise {
	v1, v2 := NewVariable(), NewVariable()
	if !args.Unify(List(v1, v2), false, env) {
		return Error(errors.New("wrong number of arguments"))
	}

	return p(v1, v2, func(env Env) *Promise {
		return e.exec([]byte{opExit}, nil, nil, cont{
			exit: k,
			fail: Failure,
		}, nil, nil, &env, nil)
	}, env)
}

type predicate3 func(Term, Term, Term, func(Env) *Promise, *Env) *Promise

func (p predicate3) Call(e *VM, args Term, k func(Env) *Promise, env *Env) *Promise {
	v1, v2, v3 := NewVariable(), NewVariable(), NewVariable()
	if !args.Unify(List(v1, v2, v3), false, env) {
		return Error(errors.New("wrong number of arguments"))
	}

	return p(v1, v2, v3, func(env Env) *Promise {
		return e.exec([]byte{opExit}, nil, nil, cont{
			exit: k,
			fail: Failure,
		}, nil, nil, &env, nil)
	}, env)
}

type predicate4 func(Term, Term, Term, Term, func(Env) *Promise, *Env) *Promise

func (p predicate4) Call(e *VM, args Term, k func(Env) *Promise, env *Env) *Promise {
	v1, v2, v3, v4 := NewVariable(), NewVariable(), NewVariable(), NewVariable()
	if !args.Unify(List(v1, v2, v3, v4), false, env) {
		return Error(errors.New("wrong number of arguments"))
	}

	return p(v1, v2, v3, v4, func(env Env) *Promise {
		return e.exec([]byte{opExit}, nil, nil, cont{
			exit: k,
			fail: Failure,
		}, nil, nil, &env, nil)
	}, env)
}

type predicate5 func(Term, Term, Term, Term, Term, func(Env) *Promise, *Env) *Promise

func (p predicate5) Call(e *VM, args Term, k func(Env) *Promise, env *Env) *Promise {
	v1, v2, v3, v4, v5 := NewVariable(), NewVariable(), NewVariable(), NewVariable(), NewVariable()
	if !args.Unify(List(v1, v2, v3, v4, v5), false, env) {
		return Error(errors.New("wrong number of arguments"))
	}

	return p(v1, v2, v3, v4, v5, func(env Env) *Promise {
		return e.exec([]byte{opExit}, nil, nil, cont{
			exit: k,
			fail: Failure,
		}, nil, nil, &env, nil)
	}, env)
}

func Success(_ Env) *Promise {
	return Bool(true)
}

func Failure(_ Env) *Promise {
	return Bool(false)
}
