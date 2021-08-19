package engine

import (
	"bytes"
	"errors"
	"fmt"
	"io"

	"github.com/ichiban/prolog/nondet"
	"github.com/ichiban/prolog/term"
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
	OnCall, OnExit, OnFail, OnRedo func(pi string, args term.Interface, env term.Env)

	Panic          func(r interface{})
	UnknownWarning func(procedure string)

	Operators       term.Operators
	CharConversions map[rune]rune

	procedures      map[procedureIndicator]procedure
	streams         map[term.Interface]*term.Stream
	input, output   *term.Stream
	charConvEnabled bool
	debug           bool
	unknown         unknownAction
}

// SetUserInput sets the given reader as a stream with an alias of user_input.
func (vm *VM) SetUserInput(r io.Reader) {
	const userInput = term.Atom("user_input")

	s := term.Stream{
		Source: r,
		Mode:   term.StreamModeRead,
		Alias:  userInput,
	}

	if vm.streams == nil {
		vm.streams = map[term.Interface]*term.Stream{}
	}
	vm.streams[userInput] = &s

	vm.input = &s
}

// SetUserOutput sets the given writer as a stream with an alias of user_output.
func (vm *VM) SetUserOutput(w io.Writer) {
	const userOutput = term.Atom("user_output")

	s := term.Stream{
		Sink:  w,
		Mode:  term.StreamModeWrite,
		Alias: userOutput,
	}

	if vm.streams == nil {
		vm.streams = map[term.Interface]*term.Stream{}
	}
	vm.streams[userOutput] = &s

	vm.output = &s
}

func (vm *VM) DescribeTerm(t term.Interface, env term.Env) string {
	var buf bytes.Buffer
	_ = t.WriteTerm(&buf, term.WriteTermOptions{
		Quoted:      true,
		Ops:         vm.Operators,
		Descriptive: true,
	}, env)
	return buf.String()
}

// Register0 registers a predicate of arity 0.
func (vm *VM) Register0(name string, p func(func(term.Env) *nondet.Promise, *term.Env) *nondet.Promise) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: term.Atom(name), arity: 0}] = predicate0(p)
}

// Register1 registers a predicate of arity 1.
func (vm *VM) Register1(name string, p func(term.Interface, func(term.Env) *nondet.Promise, *term.Env) *nondet.Promise) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: term.Atom(name), arity: 1}] = predicate1(p)
}

// Register2 registers a predicate of arity 2.
func (vm *VM) Register2(name string, p func(term.Interface, term.Interface, func(term.Env) *nondet.Promise, *term.Env) *nondet.Promise) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: term.Atom(name), arity: 2}] = predicate2(p)
}

// Register3 registers a predicate of arity 3.
func (vm *VM) Register3(name string, p func(term.Interface, term.Interface, term.Interface, func(term.Env) *nondet.Promise, *term.Env) *nondet.Promise) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: term.Atom(name), arity: 3}] = predicate3(p)
}

// Register4 registers a predicate of arity 4.
func (vm *VM) Register4(name string, p func(term.Interface, term.Interface, term.Interface, term.Interface, func(term.Env) *nondet.Promise, *term.Env) *nondet.Promise) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: term.Atom(name), arity: 4}] = predicate4(p)
}

// Register5 registers a predicate of arity 5.
func (vm *VM) Register5(name string, p func(term.Interface, term.Interface, term.Interface, term.Interface, term.Interface, func(term.Env) *nondet.Promise, *term.Env) *nondet.Promise) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: term.Atom(name), arity: 5}] = predicate5(p)
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
	Call(*VM, term.Interface, func(term.Env) *nondet.Promise, *term.Env) *nondet.Promise
}

func (vm *VM) arrive(pi procedureIndicator, args term.Interface, k func(term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
	if vm.UnknownWarning == nil {
		vm.UnknownWarning = func(string) {}
	}

	p := vm.procedures[pi]
	if p == nil {
		switch vm.unknown {
		case unknownError:
			return nondet.Error(existenceErrorProcedure(&term.Compound{
				Functor: "/",
				Args:    []term.Interface{pi.name, pi.arity},
			}))
		case unknownWarning:
			vm.UnknownWarning(pi.String())
			fallthrough
		case unknownFail:
			return nondet.Bool(false)
		default:
			return nondet.Error(systemError(fmt.Errorf("unknown unknown: %s", vm.unknown)))
		}
	}

	return nondet.Delay(func() *nondet.Promise {
		env := *env
		return p.Call(vm, args, k, &env)
	})
}

type cont struct {
	exit func(term.Env) *nondet.Promise
	fail func(term.Env) *nondet.Promise
}

func (vm *VM) exec(pc bytecode, xr []term.Interface, vars []term.Variable, k cont, args, astack term.Interface, env *term.Env, cutParent *nondet.Promise) *nondet.Promise {
	if cutParent == nil {
		cutParent = &nondet.Promise{}
	}
	for len(pc) != 0 {
		switch pc[0] {
		case opVoid:
			pc = pc[1:]
		case opConst:
			x := xr[pc[1]]
			arest := term.NewVariable()
			cons := term.Compound{
				Functor: ".",
				Args:    []term.Interface{x, arest},
			}
			if !args.Unify(&cons, false, env) {
				return k.fail(*env)
			}
			pc = pc[2:]
			args = arest
		case opVar:
			v := vars[pc[1]]
			arest := term.NewVariable()
			cons := term.Compound{
				Functor: ".",
				Args:    []term.Interface{v, arest},
			}
			if !args.Unify(&cons, false, env) {
				return k.fail(*env)
			}
			pc = pc[2:]
			args = arest
		case opFunctor:
			x := xr[pc[1]]
			arg, arest := term.NewVariable(), term.NewVariable()
			cons1 := term.Compound{
				Functor: ".",
				Args:    []term.Interface{arg, arest},
			}
			if !args.Unify(&cons1, false, env) {
				return k.fail(*env)
			}
			pf, ok := x.(procedureIndicator)
			if !ok {
				return nondet.Error(errors.New("not a principal functor"))
			}
			ok, err := Functor(arg, pf.name, pf.arity, func(e term.Env) *nondet.Promise {
				env = &e
				return nondet.Bool(true)
			}, env).Force()
			if err != nil {
				return nondet.Error(err)
			}
			if !ok {
				return k.fail(*env)
			}
			pc = pc[2:]
			args = term.NewVariable()
			cons2 := term.Compound{
				Functor: ".",
				Args:    []term.Interface{pf.name, args},
			}
			ok, err = Univ(arg, &cons2, func(e term.Env) *nondet.Promise {
				env = &e
				return nondet.Bool(true)
			}, env).Force()
			if err != nil {
				return nondet.Error(err)
			}
			if !ok {
				return k.fail(*env)
			}
			astack = term.Cons(arest, astack)
		case opPop:
			if !args.Unify(term.List(), false, env) {
				return k.fail(*env)
			}
			pc = pc[1:]
			a, arest := term.NewVariable(), term.NewVariable()
			cons := term.Compound{
				Functor: ".",
				Args:    []term.Interface{a, arest},
			}
			if !astack.Unify(&cons, false, env) {
				return k.fail(*env)
			}
			args = a
			astack = arest
		case opEnter:
			if !args.Unify(term.List(), false, env) {
				return k.fail(*env)
			}
			if !astack.Unify(term.List(), false, env) {
				return k.fail(*env)
			}
			pc = pc[1:]
			v := term.NewVariable()
			args = v
			astack = v
		case opCall:
			x := xr[pc[1]]
			if !args.Unify(term.List(), false, env) {
				return k.fail(*env)
			}
			pc = pc[2:]
			pi, ok := x.(procedureIndicator)
			if !ok {
				return nondet.Error(errors.New("not a principal functor"))
			}
			return nondet.Delay(func() *nondet.Promise {
				env := *env
				return vm.arrive(pi, astack, func(env term.Env) *nondet.Promise {
					v := term.NewVariable()
					return vm.exec(pc, xr, vars, k, v, v, &env, cutParent)
				}, &env)
			})
		case opExit:
			return k.exit(*env)
		case opCut:
			pc = pc[1:]
			return nondet.Cut(nondet.Delay(func() *nondet.Promise {
				env := *env
				return vm.exec(pc, xr, vars, k, args, astack, &env, cutParent)
			}), cutParent)
		default:
			return nondet.Error(fmt.Errorf("unknown(%d)", pc[0]))
		}
	}
	return nondet.Error(errors.New("non-exit end of bytecode"))
}

type clauses []clause

func (cs clauses) Call(vm *VM, args term.Interface, k func(term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
	if len(cs) == 0 {
		return nondet.Bool(false)
	}

	if vm.OnCall == nil {
		vm.OnCall = func(pi string, args term.Interface, env term.Env) {}
	}
	if vm.OnExit == nil {
		vm.OnExit = func(pi string, args term.Interface, env term.Env) {}
	}
	if vm.OnFail == nil {
		vm.OnFail = func(pi string, args term.Interface, env term.Env) {}
	}
	if vm.OnRedo == nil {
		vm.OnRedo = func(pi string, args term.Interface, env term.Env) {}
	}

	var p *nondet.Promise
	ks := make([]func() *nondet.Promise, len(cs))
	for i := range cs {
		i, c := i, cs[i]
		ks[i] = func() *nondet.Promise {
			if i == 0 {
				vm.OnCall(c.pi.String(), args, *env)
			} else {
				vm.OnRedo(c.pi.String(), args, *env)
			}
			vars := make([]term.Variable, len(c.vars))
			for i := range c.vars {
				vars[i] = term.NewVariable()
			}
			return nondet.Delay(func() *nondet.Promise {
				env := *env
				return vm.exec(c.bytecode, c.xrTable, vars, cont{
					exit: func(env term.Env) *nondet.Promise {
						vm.OnExit(c.pi.String(), args, env)
						return k(env)
					},
					fail: func(env term.Env) *nondet.Promise {
						vm.OnFail(c.pi.String(), args, env)
						return nondet.Bool(false)
					},
				}, args, term.List(), &env, p)
			})
		}
	}
	p = nondet.Delay(ks...)
	return p
}

type clause struct {
	pi       procedureIndicator
	raw      term.Interface
	xrTable  []term.Interface
	vars     []term.Variable
	bytecode bytecode
}

func (c *clause) compile(t term.Interface, env term.Env) error {
	t = env.Resolve(t)
	c.raw = t
	switch t := t.(type) {
	case term.Variable:
		return instantiationError(t)
	case term.Atom:
		return c.compileClause(t, nil, env)
	case *term.Compound:
		if t.Functor == ":-" {
			return c.compileClause(t.Args[0], t.Args[1], env)
		}
		return c.compileClause(t, nil, env)
	default:
		return typeErrorCallable(t)
	}
}

func (c *clause) compileClause(head term.Interface, body term.Interface, env term.Env) error {
	switch head := env.Resolve(head).(type) {
	case term.Variable:
		return instantiationError(head)
	case term.Atom:
	case *term.Compound:
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
			p, ok := body.(*term.Compound)
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

func (c *clause) compilePred(p term.Interface, env term.Env) error {
	switch p := env.Resolve(p).(type) {
	case term.Variable:
		return instantiationError(p)
	case term.Atom:
		if p == "!" {
			c.bytecode = append(c.bytecode, opCut)
			return nil
		}
		c.bytecode = append(c.bytecode, opCall, c.xrOffset(procedureIndicator{name: p, arity: 0}))
		return nil
	case *term.Compound:
		for _, a := range p.Args {
			if err := c.compileArg(a); err != nil {
				return err
			}
		}
		c.bytecode = append(c.bytecode, opCall, c.xrOffset(procedureIndicator{name: p.Functor, arity: term.Integer(len(p.Args))}))
		return nil
	default:
		return typeErrorCallable(p)
	}
}

func (c *clause) compileArg(a term.Interface) error {
	switch a := a.(type) {
	case term.Variable:
		c.bytecode = append(c.bytecode, opVar, c.varOffset(a))
	case term.Float, term.Integer, term.Atom:
		c.bytecode = append(c.bytecode, opConst, c.xrOffset(a))
	case *term.Compound:
		c.bytecode = append(c.bytecode, opFunctor, c.xrOffset(procedureIndicator{name: a.Functor, arity: term.Integer(len(a.Args))}))
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

func (c *clause) xrOffset(o term.Interface) byte {
	for i, r := range c.xrTable {
		if r.Unify(o, false, nil) {
			return byte(i)
		}
	}
	c.xrTable = append(c.xrTable, o)
	return byte(len(c.xrTable) - 1)
}

func (c *clause) varOffset(o term.Variable) byte {
	for i, v := range c.vars {
		if v == o {
			return byte(i)
		}
	}
	c.vars = append(c.vars, o)
	return byte(len(c.vars) - 1)
}

type bytecode []byte

type predicate0 func(func(term.Env) *nondet.Promise, *term.Env) *nondet.Promise

func (p predicate0) Call(e *VM, args term.Interface, k func(term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
	if !args.Unify(term.List(), false, env) {
		return nondet.Error(errors.New("wrong number of arguments"))
	}

	return p(func(env term.Env) *nondet.Promise {
		return e.exec([]byte{opExit}, nil, nil, cont{
			exit: k,
			fail: Failure,
		}, nil, nil, &env, nil)
	}, env)
}

type predicate1 func(term.Interface, func(term.Env) *nondet.Promise, *term.Env) *nondet.Promise

func (p predicate1) Call(e *VM, args term.Interface, k func(term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
	v1 := term.NewVariable()
	if !args.Unify(term.List(v1), false, env) {
		return nondet.Error(fmt.Errorf("wrong number of arguments: %s", args))
	}

	return p(v1, func(env term.Env) *nondet.Promise {
		return e.exec([]byte{opExit}, nil, nil, cont{
			exit: k,
			fail: Failure,
		}, nil, nil, &env, nil)
	}, env)
}

type predicate2 func(term.Interface, term.Interface, func(term.Env) *nondet.Promise, *term.Env) *nondet.Promise

func (p predicate2) Call(e *VM, args term.Interface, k func(term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
	v1, v2 := term.NewVariable(), term.NewVariable()
	if !args.Unify(term.List(v1, v2), false, env) {
		return nondet.Error(errors.New("wrong number of arguments"))
	}

	return p(v1, v2, func(env term.Env) *nondet.Promise {
		return e.exec([]byte{opExit}, nil, nil, cont{
			exit: k,
			fail: Failure,
		}, nil, nil, &env, nil)
	}, env)
}

type predicate3 func(term.Interface, term.Interface, term.Interface, func(term.Env) *nondet.Promise, *term.Env) *nondet.Promise

func (p predicate3) Call(e *VM, args term.Interface, k func(term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
	v1, v2, v3 := term.NewVariable(), term.NewVariable(), term.NewVariable()
	if !args.Unify(term.List(v1, v2, v3), false, env) {
		return nondet.Error(errors.New("wrong number of arguments"))
	}

	return p(v1, v2, v3, func(env term.Env) *nondet.Promise {
		return e.exec([]byte{opExit}, nil, nil, cont{
			exit: k,
			fail: Failure,
		}, nil, nil, &env, nil)
	}, env)
}

type predicate4 func(term.Interface, term.Interface, term.Interface, term.Interface, func(term.Env) *nondet.Promise, *term.Env) *nondet.Promise

func (p predicate4) Call(e *VM, args term.Interface, k func(term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
	v1, v2, v3, v4 := term.NewVariable(), term.NewVariable(), term.NewVariable(), term.NewVariable()
	if !args.Unify(term.List(v1, v2, v3, v4), false, env) {
		return nondet.Error(errors.New("wrong number of arguments"))
	}

	return p(v1, v2, v3, v4, func(env term.Env) *nondet.Promise {
		return e.exec([]byte{opExit}, nil, nil, cont{
			exit: k,
			fail: Failure,
		}, nil, nil, &env, nil)
	}, env)
}

type predicate5 func(term.Interface, term.Interface, term.Interface, term.Interface, term.Interface, func(term.Env) *nondet.Promise, *term.Env) *nondet.Promise

func (p predicate5) Call(e *VM, args term.Interface, k func(term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
	v1, v2, v3, v4, v5 := term.NewVariable(), term.NewVariable(), term.NewVariable(), term.NewVariable(), term.NewVariable()
	if !args.Unify(term.List(v1, v2, v3, v4, v5), false, env) {
		return nondet.Error(errors.New("wrong number of arguments"))
	}

	return p(v1, v2, v3, v4, v5, func(env term.Env) *nondet.Promise {
		return e.exec([]byte{opExit}, nil, nil, cont{
			exit: k,
			fail: Failure,
		}, nil, nil, &env, nil)
	}, env)
}

func Success(_ term.Env) *nondet.Promise {
	return nondet.Bool(true)
}

func Failure(_ term.Env) *nondet.Promise {
	return nondet.Bool(false)
}

// Each iterates over list.
func Each(list term.Interface, f func(elem term.Interface) error, env term.Env) error {
	whole := list
	for {
		switch l := env.Resolve(list).(type) {
		case term.Variable:
			return instantiationError(whole)
		case term.Atom:
			if l != "[]" {
				return typeErrorList(l)
			}
			return nil
		case *term.Compound:
			if l.Functor != "." || len(l.Args) != 2 {
				return typeErrorList(l)
			}
			if err := f(l.Args[0]); err != nil {
				return err
			}
			list = l.Args[1]
		default:
			return typeErrorList(l)
		}
	}
}

// procedureIndicator is a specialized variant of Compound.
type procedureIndicator struct {
	name  term.Atom
	arity term.Integer
}

func (p procedureIndicator) String() string {
	var buf bytes.Buffer
	_ = p.WriteTerm(&buf, term.DefaultWriteTermOptions, nil)
	return buf.String()
}

func (p procedureIndicator) WriteTerm(w io.Writer, _ term.WriteTermOptions, _ term.Env) error {
	_, err := fmt.Fprintf(w, "%s/%d", p.name, p.arity)
	return err
}

func (p procedureIndicator) Unify(t term.Interface, _ bool, _ *term.Env) bool {
	pf, ok := t.(procedureIndicator)
	return ok && p.name == pf.name && p.arity == pf.arity
}

func piArgs(t term.Interface, env term.Env) (procedureIndicator, term.Interface, error) {
	switch f := env.Resolve(t).(type) {
	case term.Variable:
		return procedureIndicator{}, nil, instantiationError(t)
	case term.Atom:
		return procedureIndicator{name: f, arity: 0}, term.List(), nil
	case *term.Compound:
		return procedureIndicator{name: f.Functor, arity: term.Integer(len(f.Args))}, term.List(f.Args...), nil
	default:
		return procedureIndicator{}, nil, typeErrorCallable(t)
	}
}
