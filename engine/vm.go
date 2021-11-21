package engine

import (
	"bufio"
	"bytes"
	"context"
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

	_opLen
)

func (o opcode) String() string {
	return [_opLen]string{
		opVoid:    "void",
		opEnter:   "enter",
		opCall:    "call",
		opExit:    "exit",
		opConst:   "const",
		opVar:     "var",
		opFunctor: "functor",
		opPop:     "pop",
		opCut:     "cut",
	}[o]
}

// VM is the core of a Prolog interpreter. The zero value for VM is a valid VM without any builtin predicates.
type VM struct {
	// OnCall is a callback that is triggered when the VM reaches to the predicate.
	OnCall func(pi ProcedureIndicator, args []term.Interface, env *term.Env)

	// OnExit is a callback that is triggered when the predicate succeeds and the VM continues.
	OnExit func(pi ProcedureIndicator, args []term.Interface, env *term.Env)

	// OnFail is a callback that is triggered when the predicate fails and the VM backtracks.
	OnFail func(pi ProcedureIndicator, args []term.Interface, env *term.Env)

	// OnRedo is a callback that is triggered when the VM retries the predicate as a result of backtrack.
	OnRedo func(pi ProcedureIndicator, args []term.Interface, env *term.Env)

	// OnUnknown is a callback that is triggered when the VM reaches to an unknown predicate and also current_prolog_flag(unknown, warning).
	OnUnknown func(pi ProcedureIndicator, args []term.Interface, env *term.Env)

	// Core
	procedures map[ProcedureIndicator]procedure
	unknown    unknownAction

	// Internal/external expression
	operators       term.Operators
	charConversions map[rune]rune
	charConvEnabled bool
	doubleQuotes    term.DoubleQuotes

	// I/O
	streams       map[term.Interface]*term.Stream
	input, output *term.Stream

	// Misc
	debug bool
}

func (vm *VM) Parser(r io.Reader, vars *[]term.ParsedVariable) *term.Parser {
	br, ok := r.(*bufio.Reader)
	if !ok {
		br = bufio.NewReader(r)
	}
	return term.NewParser(br, vm.charConversions,
		term.WithOperators(&vm.operators),
		term.WithDoubleQuotes(vm.doubleQuotes),
		term.WithParsedVars(vars),
	)
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

func (vm *VM) DescribeTerm(t term.Interface, env *term.Env) string {
	var buf bytes.Buffer
	_ = term.Write(&buf, t, term.WriteTermOptions{
		Ops:         vm.operators,
		Quoted:      true,
		Descriptive: true,
		Priority:    1200,
	}, env)
	return buf.String()
}

// Register0 registers a predicate of arity 0.
func (vm *VM) Register0(name string, p func(func(*term.Env) *nondet.Promise, *term.Env) *nondet.Promise) {
	if vm.procedures == nil {
		vm.procedures = map[ProcedureIndicator]procedure{}
	}
	vm.procedures[ProcedureIndicator{Name: term.Atom(name), Arity: 0}] = predicate0(p)
}

// Register1 registers a predicate of arity 1.
func (vm *VM) Register1(name string, p func(term.Interface, func(*term.Env) *nondet.Promise, *term.Env) *nondet.Promise) {
	if vm.procedures == nil {
		vm.procedures = map[ProcedureIndicator]procedure{}
	}
	vm.procedures[ProcedureIndicator{Name: term.Atom(name), Arity: 1}] = predicate1(p)
}

// Register2 registers a predicate of arity 2.
func (vm *VM) Register2(name string, p func(term.Interface, term.Interface, func(*term.Env) *nondet.Promise, *term.Env) *nondet.Promise) {
	if vm.procedures == nil {
		vm.procedures = map[ProcedureIndicator]procedure{}
	}
	vm.procedures[ProcedureIndicator{Name: term.Atom(name), Arity: 2}] = predicate2(p)
}

// Register3 registers a predicate of arity 3.
func (vm *VM) Register3(name string, p func(term.Interface, term.Interface, term.Interface, func(*term.Env) *nondet.Promise, *term.Env) *nondet.Promise) {
	if vm.procedures == nil {
		vm.procedures = map[ProcedureIndicator]procedure{}
	}
	vm.procedures[ProcedureIndicator{Name: term.Atom(name), Arity: 3}] = predicate3(p)
}

// Register4 registers a predicate of arity 4.
func (vm *VM) Register4(name string, p func(term.Interface, term.Interface, term.Interface, term.Interface, func(*term.Env) *nondet.Promise, *term.Env) *nondet.Promise) {
	if vm.procedures == nil {
		vm.procedures = map[ProcedureIndicator]procedure{}
	}
	vm.procedures[ProcedureIndicator{Name: term.Atom(name), Arity: 4}] = predicate4(p)
}

// Register5 registers a predicate of arity 5.
func (vm *VM) Register5(name string, p func(term.Interface, term.Interface, term.Interface, term.Interface, term.Interface, func(*term.Env) *nondet.Promise, *term.Env) *nondet.Promise) {
	if vm.procedures == nil {
		vm.procedures = map[ProcedureIndicator]procedure{}
	}
	vm.procedures[ProcedureIndicator{Name: term.Atom(name), Arity: 5}] = predicate5(p)
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
	Call(*VM, []term.Interface, func(*term.Env) *nondet.Promise, *term.Env) *nondet.Promise
}

func (vm *VM) arrive(pi ProcedureIndicator, args []term.Interface, k func(*term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
	if vm.OnUnknown == nil {
		vm.OnUnknown = func(ProcedureIndicator, []term.Interface, *term.Env) {}
	}

	p := vm.procedures[pi]
	if p == nil {
		switch vm.unknown {
		case unknownError:
			return nondet.Error(existenceErrorProcedure(pi.Term()))
		case unknownWarning:
			vm.OnUnknown(pi, args, env)
			fallthrough
		case unknownFail:
			return nondet.Bool(false)
		default:
			return nondet.Error(SystemError(fmt.Errorf("unknown unknown: %s", vm.unknown)))
		}
	}

	return nondet.Delay(func(context.Context) *nondet.Promise {
		env := env
		return p.Call(vm, args, k, env)
	})
}

type registers struct {
	pc           bytecode
	xr           []term.Interface
	vars         []term.Variable
	cont         func(*term.Env) *nondet.Promise
	args, astack term.Interface

	pi        []ProcedureIndicator
	env       *term.Env
	cutParent *nondet.Promise
}

func (vm *VM) exec(r registers) *nondet.Promise {
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
			return nondet.Error(fmt.Errorf("unknown opcode: %d", r.pc[0].opcode))
		}
		p := op(&r)
		if p != nil {
			return p
		}
	}
	return nondet.Error(errors.New("non-exit end of bytecode"))
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
	var ok bool
	r.env, ok = r.args.Unify(&cons, false, r.env)
	if !ok {
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
	var ok bool
	r.env, ok = cons.Unify(r.args, false, r.env)
	if !ok {
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
	var ok bool
	r.env, ok = r.args.Unify(&cons1, false, r.env)
	if !ok {
		return nondet.Bool(false)
	}
	ok, err := Functor(arg, pi.Name, pi.Arity, func(e *term.Env) *nondet.Promise {
		r.env = e
		return nondet.Bool(true)
	}, r.env).Force(context.Background())
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
		Args:    []term.Interface{pi.Name, r.args},
	}
	ok, err = Univ(arg, &cons2, func(e *term.Env) *nondet.Promise {
		r.env = e
		return nondet.Bool(true)
	}, r.env).Force(context.Background())
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
	var ok bool
	r.env, ok = r.args.Unify(term.List(), false, r.env)
	if !ok {
		return nondet.Bool(false)
	}
	r.pc = r.pc[1:]
	a, arest := term.NewVariable(), term.NewVariable()
	cons := term.Compound{
		Functor: ".",
		Args:    []term.Interface{a, arest},
	}
	r.env, ok = r.astack.Unify(&cons, false, r.env)
	if !ok {
		return nondet.Bool(false)
	}
	r.args = a
	r.astack = arest
	return nil
}

func (*VM) execEnter(r *registers) *nondet.Promise {
	var ok bool
	r.env, ok = r.args.Unify(term.List(), false, r.env)
	if !ok {
		return nondet.Bool(false)
	}
	r.env, ok = r.astack.Unify(term.List(), false, r.env)
	if !ok {
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
	var ok bool
	r.env, ok = r.args.Unify(term.List(), false, r.env)
	if !ok {
		return nondet.Bool(false)
	}
	r.pc = r.pc[1:]
	return nondet.Delay(func(context.Context) *nondet.Promise {
		env := r.env
		args, err := Slice(r.astack, env)
		if err != nil {
			return nondet.Error(err)
		}
		return vm.arrive(pi, args, func(env *term.Env) *nondet.Promise {
			v := term.NewVariable()
			return vm.exec(registers{
				pc:        r.pc,
				xr:        r.xr,
				vars:      r.vars,
				cont:      r.cont,
				args:      v,
				astack:    v,
				pi:        r.pi,
				env:       env,
				cutParent: r.cutParent,
			})
		}, env)
	})
}

func (*VM) execExit(r *registers) *nondet.Promise {
	return r.cont(r.env)
}

func (vm *VM) execCut(r *registers) *nondet.Promise {
	r.pc = r.pc[1:]
	return nondet.Cut(r.cutParent, func(context.Context) *nondet.Promise {
		env := r.env
		return vm.exec(registers{
			pc:        r.pc,
			xr:        r.xr,
			vars:      r.vars,
			cont:      r.cont,
			args:      r.args,
			astack:    r.astack,
			pi:        r.pi,
			env:       env,
			cutParent: r.cutParent,
		})
	})
}

type predicate0 func(func(*term.Env) *nondet.Promise, *term.Env) *nondet.Promise

func (p predicate0) Call(_ *VM, args []term.Interface, k func(*term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
	if len(args) != 0 {
		return nondet.Error(errors.New("wrong number of arguments"))
	}

	return p(k, env)
}

type predicate1 func(term.Interface, func(*term.Env) *nondet.Promise, *term.Env) *nondet.Promise

func (p predicate1) Call(_ *VM, args []term.Interface, k func(*term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
	if len(args) != 1 {
		return nondet.Error(fmt.Errorf("wrong number of arguments: %s", args))
	}

	return p(args[0], k, env)
}

type predicate2 func(term.Interface, term.Interface, func(*term.Env) *nondet.Promise, *term.Env) *nondet.Promise

func (p predicate2) Call(_ *VM, args []term.Interface, k func(*term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
	if len(args) != 2 {
		return nondet.Error(errors.New("wrong number of arguments"))
	}

	return p(args[0], args[1], k, env)
}

type predicate3 func(term.Interface, term.Interface, term.Interface, func(*term.Env) *nondet.Promise, *term.Env) *nondet.Promise

func (p predicate3) Call(_ *VM, args []term.Interface, k func(*term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
	if len(args) != 3 {
		return nondet.Error(errors.New("wrong number of arguments"))
	}

	return p(args[0], args[1], args[2], k, env)
}

type predicate4 func(term.Interface, term.Interface, term.Interface, term.Interface, func(*term.Env) *nondet.Promise, *term.Env) *nondet.Promise

func (p predicate4) Call(_ *VM, args []term.Interface, k func(*term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
	if len(args) != 4 {
		return nondet.Error(errors.New("wrong number of arguments"))
	}

	return p(args[0], args[1], args[2], args[3], k, env)
}

type predicate5 func(term.Interface, term.Interface, term.Interface, term.Interface, term.Interface, func(*term.Env) *nondet.Promise, *term.Env) *nondet.Promise

func (p predicate5) Call(_ *VM, args []term.Interface, k func(*term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
	if len(args) != 5 {
		return nondet.Error(errors.New("wrong number of arguments"))
	}

	return p(args[0], args[1], args[2], args[3], args[4], k, env)
}

func Success(_ *term.Env) *nondet.Promise {
	return nondet.Bool(true)
}

func Failure(_ *term.Env) *nondet.Promise {
	return nondet.Bool(false)
}

// Each iterates over list.
func Each(list term.Interface, f func(elem term.Interface) error, env *term.Env) error {
	whole := list
	for {
		switch l := env.Resolve(list).(type) {
		case term.Variable:
			return InstantiationError(whole)
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

func Slice(list term.Interface, env *term.Env) ([]term.Interface, error) {
	var ret []term.Interface
	if err := Each(list, func(elem term.Interface) error {
		ret = append(ret, env.Resolve(elem))
		return nil
	}, env); err != nil {
		return nil, err
	}
	return ret, nil
}

// ProcedureIndicator identifies procedure e.g. (=)/2.
type ProcedureIndicator struct {
	Name  term.Atom
	Arity term.Integer
}

func (p ProcedureIndicator) String() string {
	return fmt.Sprintf("%s/%d", p.Name, p.Arity)
}

// Term returns p as term.
func (p ProcedureIndicator) Term() term.Interface {
	return &term.Compound{
		Functor: "/",
		Args: []term.Interface{
			p.Name,
			p.Arity,
		},
	}
}

// Apply applies p to args.
func (p ProcedureIndicator) Apply(args []term.Interface) (term.Interface, error) {
	if p.Arity != term.Integer(len(args)) {
		return nil, errors.New("wrong number of arguments")
	}
	return p.Name.Apply(args...), nil
}

func piArgs(t term.Interface, env *term.Env) (ProcedureIndicator, []term.Interface, error) {
	switch f := env.Resolve(t).(type) {
	case term.Variable:
		return ProcedureIndicator{}, nil, InstantiationError(t)
	case term.Atom:
		return ProcedureIndicator{Name: f, Arity: 0}, nil, nil
	case *term.Compound:
		return ProcedureIndicator{Name: f.Functor, Arity: term.Integer(len(f.Args))}, f.Args, nil
	default:
		return ProcedureIndicator{}, nil, typeErrorCallable(t)
	}
}
