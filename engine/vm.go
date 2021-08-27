package engine

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"io"

	"github.com/ichiban/prolog/nondet"
	"github.com/ichiban/prolog/term"
)

type bytecode []instruction

type instruction struct {
	opcode  opcode
	operand byte
}

type opcode byte

const (
	opVoid opcode = iota
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
	// OnCall is a callback that is triggered when the VM reaches to the predicate.
	OnCall func(pi string, args term.Interface, env term.Env)

	// OnExit is a callback that is triggered when the predicate succeeds and the VM continues.
	OnExit func(pi string, args term.Interface, env term.Env)

	// OnFail is a callback that is triggered when the predicate fails and the VM backtracks.
	OnFail func(pi string, args term.Interface, env term.Env)

	// OnRedo is a callback that is triggered when the VM retries the predicate as a result of backtrack.
	OnRedo func(pi string, args term.Interface, env term.Env)

	// OnUnknown is a callback that is triggered when the VM reaches to an unknown predicate and also current_prolog_flag(unknown, warning).
	OnUnknown func(pi string, args term.Interface, env term.Env)

	// Core
	procedures map[procedureIndicator]procedure
	unknown    unknownAction

	// Internal/external expression
	operators       term.Operators
	charConversions map[rune]rune
	charConvEnabled bool

	// I/O
	streams       map[term.Interface]*term.Stream
	input, output *term.Stream

	// Misc
	debug bool
}

func (vm *VM) Parser(r io.Reader) *term.Parser {
	br, ok := r.(*bufio.Reader)
	if !ok {
		br = bufio.NewReader(r)
	}
	return term.NewParser(br, &vm.operators, vm.charConversions)
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
		Ops:         vm.operators,
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
	if vm.OnUnknown == nil {
		vm.OnUnknown = func(string, term.Interface, term.Env) {}
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
			vm.OnUnknown(pi.String(), args, *env)
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

type registers struct {
	pc           bytecode
	xr           []term.Interface
	vars         []term.Variable
	cont         func(term.Env) *nondet.Promise
	args, astack term.Interface

	pi        []procedureIndicator
	env       *term.Env
	cutParent *nondet.Promise
}

func (vm *VM) exec(r registers) *nondet.Promise {
	if r.cutParent == nil {
		r.cutParent = &nondet.Promise{}
	}
	jumpTable := [256]func(r *registers) *nondet.Promise{
		opVoid:    vm.execVoid,
		opConst:   vm.execConst,
		opVar:     vm.execVar,
		opFunctor: vm.execFunctor,
		opPop:     vm.execPop,
		opEnter:   vm.execEnter,
		opCall:    vm.execCall,
		opExit:    vm.execExit,
		opCut:     vm.execCut,
	}
	for len(r.pc) != 0 {
		op := jumpTable[r.pc[0].opcode]
		if op == nil {
			return nondet.Error(systemError(fmt.Errorf("unknown opcode: %d", r.pc[0].opcode)))
		}
		p := op(&r)
		if p != nil {
			return p
		}
	}
	return nondet.Error(systemError(errors.New("non-exit end of bytecode")))
}

func (*VM) execVoid(r *registers) *nondet.Promise {
	r.pc = r.pc[1:]
	return nil
}

func (*VM) execConst(r *registers) *nondet.Promise {
	x := r.xr[r.pc[0].operand]
	arest := term.NewVariable()
	cons := term.Compound{
		Functor: ".",
		Args:    []term.Interface{x, arest},
	}
	if !r.args.Unify(&cons, false, r.env) {
		return nondet.Bool(false)
	}
	r.pc = r.pc[1:]
	r.args = arest
	return nil
}

func (*VM) execVar(r *registers) *nondet.Promise {
	v := r.vars[r.pc[0].operand]
	arest := term.NewVariable()
	cons := term.Compound{
		Functor: ".",
		Args:    []term.Interface{v, arest},
	}
	if !r.args.Unify(&cons, false, r.env) {
		return nondet.Bool(false)
	}
	r.pc = r.pc[1:]
	r.args = arest
	return nil
}

func (*VM) execFunctor(r *registers) *nondet.Promise {
	pi := r.pi[r.pc[0].operand]
	arg, arest := term.NewVariable(), term.NewVariable()
	cons1 := term.Compound{
		Functor: ".",
		Args:    []term.Interface{arg, arest},
	}
	if !r.args.Unify(&cons1, false, r.env) {
		return nondet.Bool(false)
	}
	ok, err := Functor(arg, pi.name, pi.arity, func(e term.Env) *nondet.Promise {
		r.env = &e
		return nondet.Bool(true)
	}, r.env).Force()
	if err != nil {
		return nondet.Error(err)
	}
	if !ok {
		return nondet.Bool(false)
	}
	r.pc = r.pc[1:]
	r.args = term.NewVariable()
	cons2 := term.Compound{
		Functor: ".",
		Args:    []term.Interface{pi.name, r.args},
	}
	ok, err = Univ(arg, &cons2, func(e term.Env) *nondet.Promise {
		r.env = &e
		return nondet.Bool(true)
	}, r.env).Force()
	if err != nil {
		return nondet.Error(err)
	}
	if !ok {
		return nondet.Bool(false)
	}
	r.astack = term.Cons(arest, r.astack)
	return nil
}

func (*VM) execPop(r *registers) *nondet.Promise {
	if !r.args.Unify(term.List(), false, r.env) {
		return nondet.Bool(false)
	}
	r.pc = r.pc[1:]
	a, arest := term.NewVariable(), term.NewVariable()
	cons := term.Compound{
		Functor: ".",
		Args:    []term.Interface{a, arest},
	}
	if !r.astack.Unify(&cons, false, r.env) {
		return nondet.Bool(false)
	}
	r.args = a
	r.astack = arest
	return nil
}

func (*VM) execEnter(r *registers) *nondet.Promise {
	if !r.args.Unify(term.List(), false, r.env) {
		return nondet.Bool(false)
	}
	if !r.astack.Unify(term.List(), false, r.env) {
		return nondet.Bool(false)
	}
	r.pc = r.pc[1:]
	v := term.NewVariable()
	r.args = v
	r.astack = v
	return nil
}

func (vm *VM) execCall(r *registers) *nondet.Promise {
	pi := r.pi[r.pc[0].operand]
	if !r.args.Unify(term.List(), false, r.env) {
		return nondet.Bool(false)
	}
	r.pc = r.pc[1:]
	return nondet.Delay(func() *nondet.Promise {
		env := *r.env
		return vm.arrive(pi, r.astack, func(env term.Env) *nondet.Promise {
			v := term.NewVariable()
			return vm.exec(registers{
				pc:        r.pc,
				xr:        r.xr,
				vars:      r.vars,
				cont:      r.cont,
				args:      v,
				astack:    v,
				pi:        r.pi,
				env:       &env,
				cutParent: r.cutParent,
			})
		}, &env)
	})
}

func (*VM) execExit(r *registers) *nondet.Promise {
	return r.cont(*r.env)
}

func (vm *VM) execCut(r *registers) *nondet.Promise {
	r.pc = r.pc[1:]
	return nondet.Cut(nondet.Delay(func() *nondet.Promise {
		env := *r.env
		return vm.exec(registers{
			pc:        r.pc,
			xr:        r.xr,
			vars:      r.vars,
			cont:      r.cont,
			args:      r.args,
			astack:    r.astack,
			pi:        r.pi,
			env:       &env,
			cutParent: r.cutParent,
		})
	}), r.cutParent)
}

type predicate0 func(func(term.Env) *nondet.Promise, *term.Env) *nondet.Promise

func (p predicate0) Call(_ *VM, args term.Interface, k func(term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
	if !args.Unify(term.List(), false, env) {
		return nondet.Error(errors.New("wrong number of arguments"))
	}

	return p(k, env)
}

type predicate1 func(term.Interface, func(term.Env) *nondet.Promise, *term.Env) *nondet.Promise

func (p predicate1) Call(_ *VM, args term.Interface, k func(term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
	v1 := term.NewVariable()
	if !args.Unify(term.List(v1), false, env) {
		return nondet.Error(fmt.Errorf("wrong number of arguments: %s", args))
	}

	return p(v1, k, env)
}

type predicate2 func(term.Interface, term.Interface, func(term.Env) *nondet.Promise, *term.Env) *nondet.Promise

func (p predicate2) Call(_ *VM, args term.Interface, k func(term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
	v1, v2 := term.NewVariable(), term.NewVariable()
	if !args.Unify(term.List(v1, v2), false, env) {
		return nondet.Error(errors.New("wrong number of arguments"))
	}

	return p(v1, v2, k, env)
}

type predicate3 func(term.Interface, term.Interface, term.Interface, func(term.Env) *nondet.Promise, *term.Env) *nondet.Promise

func (p predicate3) Call(_ *VM, args term.Interface, k func(term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
	v1, v2, v3 := term.NewVariable(), term.NewVariable(), term.NewVariable()
	if !args.Unify(term.List(v1, v2, v3), false, env) {
		return nondet.Error(errors.New("wrong number of arguments"))
	}

	return p(v1, v2, v3, k, env)
}

type predicate4 func(term.Interface, term.Interface, term.Interface, term.Interface, func(term.Env) *nondet.Promise, *term.Env) *nondet.Promise

func (p predicate4) Call(_ *VM, args term.Interface, k func(term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
	v1, v2, v3, v4 := term.NewVariable(), term.NewVariable(), term.NewVariable(), term.NewVariable()
	if !args.Unify(term.List(v1, v2, v3, v4), false, env) {
		return nondet.Error(errors.New("wrong number of arguments"))
	}

	return p(v1, v2, v3, v4, k, env)
}

type predicate5 func(term.Interface, term.Interface, term.Interface, term.Interface, term.Interface, func(term.Env) *nondet.Promise, *term.Env) *nondet.Promise

func (p predicate5) Call(_ *VM, args term.Interface, k func(term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
	v1, v2, v3, v4, v5 := term.NewVariable(), term.NewVariable(), term.NewVariable(), term.NewVariable(), term.NewVariable()
	if !args.Unify(term.List(v1, v2, v3, v4, v5), false, env) {
		return nondet.Error(errors.New("wrong number of arguments"))
	}

	return p(v1, v2, v3, v4, v5, k, env)
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

type procedureIndicator struct {
	name  term.Atom
	arity term.Integer
}

func (p *procedureIndicator) String() string {
	return fmt.Sprintf("%s/%d", p.name, p.arity)
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
