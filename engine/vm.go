package engine

import (
	"bufio"
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
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
	OnCall func(pi ProcedureIndicator, args []Term, env *Env)

	// OnExit is a callback that is triggered when the predicate succeeds and the VM continues.
	OnExit func(pi ProcedureIndicator, args []Term, env *Env)

	// OnFail is a callback that is triggered when the predicate fails and the VM backtracks.
	OnFail func(pi ProcedureIndicator, args []Term, env *Env)

	// OnRedo is a callback that is triggered when the VM retries the predicate as a result of backtrack.
	OnRedo func(pi ProcedureIndicator, args []Term, env *Env)

	// OnUnknown is a callback that is triggered when the VM reaches to an unknown predicate and also current_prolog_flag(unknown, warning).
	OnUnknown func(pi ProcedureIndicator, args []Term, env *Env)

	// Core
	procedures map[ProcedureIndicator]procedure
	unknown    unknownAction

	// Internal/external expression
	operators       Operators
	charConversions map[rune]rune
	charConvEnabled bool
	doubleQuotes    DoubleQuotes

	// I/O
	streams       map[Term]*Stream
	input, output *Stream

	// Misc
	debug bool
}

func (vm *VM) Parser(r io.Reader, vars *[]ParsedVariable) *Parser {
	br, ok := r.(*bufio.Reader)
	if !ok {
		br = bufio.NewReader(r)
	}
	return NewParser(br, vm.charConversions,
		WithOperators(&vm.operators),
		WithDoubleQuotes(vm.doubleQuotes),
		WithParsedVars(vars),
	)
}

// SetUserInput sets the given reader as a stream with an alias of user_input.
func (vm *VM) SetUserInput(r io.Reader) {
	const userInput = Atom("user_input")

	s := Stream{
		Source: r,
		Mode:   StreamModeRead,
		Alias:  userInput,
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
		Sink:  w,
		Mode:  StreamModeWrite,
		Alias: userOutput,
	}

	if vm.streams == nil {
		vm.streams = map[Term]*Stream{}
	}
	vm.streams[userOutput] = &s

	vm.output = &s
}

func (vm *VM) DescribeTerm(t Term, env *Env) string {
	var buf bytes.Buffer
	_ = Write(&buf, t, WriteTermOptions{
		Ops:         vm.operators,
		Quoted:      true,
		Descriptive: true,
		Priority:    1200,
	}, env)
	return buf.String()
}

// Register0 registers a predicate of arity 0.
func (vm *VM) Register0(name string, p func(func(*Env) *Promise, *Env) *Promise) {
	if vm.procedures == nil {
		vm.procedures = map[ProcedureIndicator]procedure{}
	}
	vm.procedures[ProcedureIndicator{Name: Atom(name), Arity: 0}] = predicate0(p)
}

// Register1 registers a predicate of arity 1.
func (vm *VM) Register1(name string, p func(Term, func(*Env) *Promise, *Env) *Promise) {
	if vm.procedures == nil {
		vm.procedures = map[ProcedureIndicator]procedure{}
	}
	vm.procedures[ProcedureIndicator{Name: Atom(name), Arity: 1}] = predicate1(p)
}

// Register2 registers a predicate of arity 2.
func (vm *VM) Register2(name string, p func(Term, Term, func(*Env) *Promise, *Env) *Promise) {
	if vm.procedures == nil {
		vm.procedures = map[ProcedureIndicator]procedure{}
	}
	vm.procedures[ProcedureIndicator{Name: Atom(name), Arity: 2}] = predicate2(p)
}

// Register3 registers a predicate of arity 3.
func (vm *VM) Register3(name string, p func(Term, Term, Term, func(*Env) *Promise, *Env) *Promise) {
	if vm.procedures == nil {
		vm.procedures = map[ProcedureIndicator]procedure{}
	}
	vm.procedures[ProcedureIndicator{Name: Atom(name), Arity: 3}] = predicate3(p)
}

// Register4 registers a predicate of arity 4.
func (vm *VM) Register4(name string, p func(Term, Term, Term, Term, func(*Env) *Promise, *Env) *Promise) {
	if vm.procedures == nil {
		vm.procedures = map[ProcedureIndicator]procedure{}
	}
	vm.procedures[ProcedureIndicator{Name: Atom(name), Arity: 4}] = predicate4(p)
}

// Register5 registers a predicate of arity 5.
func (vm *VM) Register5(name string, p func(Term, Term, Term, Term, Term, func(*Env) *Promise, *Env) *Promise) {
	if vm.procedures == nil {
		vm.procedures = map[ProcedureIndicator]procedure{}
	}
	vm.procedures[ProcedureIndicator{Name: Atom(name), Arity: 5}] = predicate5(p)
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
	Call(*VM, []Term, func(*Env) *Promise, *Env) *Promise
}

func (vm *VM) arrive(pi ProcedureIndicator, args []Term, k func(*Env) *Promise, env *Env) *Promise {
	if vm.OnUnknown == nil {
		vm.OnUnknown = func(ProcedureIndicator, []Term, *Env) {}
	}

	p := vm.procedures[pi]
	if p == nil {
		switch vm.unknown {
		case unknownError:
			return Error(existenceErrorProcedure(pi.Term()))
		case unknownWarning:
			vm.OnUnknown(pi, args, env)
			fallthrough
		case unknownFail:
			return Bool(false)
		default:
			return Error(SystemError(fmt.Errorf("unknown unknown: %s", vm.unknown)))
		}
	}

	return Delay(func(context.Context) *Promise {
		env := env
		return p.Call(vm, args, k, env)
	})
}

type registers struct {
	pc           bytecode
	xr           []Term
	vars         []Variable
	cont         func(*Env) *Promise
	args, astack Term

	pi        []ProcedureIndicator
	env       *Env
	cutParent *Promise
}

func (vm *VM) exec(r registers) *Promise {
	jumpTable := [256]func(r *registers) *Promise{
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
			return Error(fmt.Errorf("unknown opcode: %d", r.pc[0].opcode))
		}
		p := op(&r)
		if p != nil {
			return p
		}
	}
	return Error(errors.New("non-exit end of bytecode"))
}

func (*VM) execVoid(r *registers) *Promise {
	r.pc = r.pc[1:]
	return nil
}

func (*VM) execConst(r *registers) *Promise {
	x := r.xr[r.pc[0].operand]
	arest := NewVariable()
	cons := Compound{
		Functor: ".",
		Args:    []Term{x, arest},
	}
	var ok bool
	r.env, ok = r.args.Unify(&cons, false, r.env)
	if !ok {
		return Bool(false)
	}
	r.pc = r.pc[1:]
	r.args = arest
	return nil
}

func (*VM) execVar(r *registers) *Promise {
	v := r.vars[r.pc[0].operand]
	arest := NewVariable()
	cons := Compound{
		Functor: ".",
		Args:    []Term{v, arest},
	}
	var ok bool
	r.env, ok = cons.Unify(r.args, false, r.env)
	if !ok {
		return Bool(false)
	}
	r.pc = r.pc[1:]
	r.args = arest
	return nil
}

func (*VM) execFunctor(r *registers) *Promise {
	pi := r.pi[r.pc[0].operand]
	arg, arest := NewVariable(), NewVariable()
	cons1 := Compound{
		Functor: ".",
		Args:    []Term{arg, arest},
	}
	var ok bool
	r.env, ok = r.args.Unify(&cons1, false, r.env)
	if !ok {
		return Bool(false)
	}
	ok, err := Functor(arg, pi.Name, pi.Arity, func(e *Env) *Promise {
		r.env = e
		return Bool(true)
	}, r.env).Force(context.Background())
	if err != nil {
		return Error(err)
	}
	if !ok {
		return Bool(false)
	}
	r.pc = r.pc[1:]
	r.args = NewVariable()
	cons2 := Compound{
		Functor: ".",
		Args:    []Term{pi.Name, r.args},
	}
	ok, err = Univ(arg, &cons2, func(e *Env) *Promise {
		r.env = e
		return Bool(true)
	}, r.env).Force(context.Background())
	if err != nil {
		return Error(err)
	}
	if !ok {
		return Bool(false)
	}
	r.astack = Cons(arest, r.astack)
	return nil
}

func (*VM) execPop(r *registers) *Promise {
	var ok bool
	r.env, ok = r.args.Unify(List(), false, r.env)
	if !ok {
		return Bool(false)
	}
	r.pc = r.pc[1:]
	a, arest := NewVariable(), NewVariable()
	cons := Compound{
		Functor: ".",
		Args:    []Term{a, arest},
	}
	r.env, ok = r.astack.Unify(&cons, false, r.env)
	if !ok {
		return Bool(false)
	}
	r.args = a
	r.astack = arest
	return nil
}

func (*VM) execEnter(r *registers) *Promise {
	var ok bool
	r.env, ok = r.args.Unify(List(), false, r.env)
	if !ok {
		return Bool(false)
	}
	r.env, ok = r.astack.Unify(List(), false, r.env)
	if !ok {
		return Bool(false)
	}
	r.pc = r.pc[1:]
	v := NewVariable()
	r.args = v
	r.astack = v
	return nil
}

func (vm *VM) execCall(r *registers) *Promise {
	pi := r.pi[r.pc[0].operand]
	var ok bool
	r.env, ok = r.args.Unify(List(), false, r.env)
	if !ok {
		return Bool(false)
	}
	r.pc = r.pc[1:]
	return Delay(func(context.Context) *Promise {
		env := r.env
		args, err := Slice(r.astack, env)
		if err != nil {
			return Error(err)
		}
		return vm.arrive(pi, args, func(env *Env) *Promise {
			v := NewVariable()
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

func (*VM) execExit(r *registers) *Promise {
	return r.cont(r.env)
}

func (vm *VM) execCut(r *registers) *Promise {
	r.pc = r.pc[1:]
	return Cut(r.cutParent, func(context.Context) *Promise {
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

type predicate0 func(func(*Env) *Promise, *Env) *Promise

func (p predicate0) Call(_ *VM, args []Term, k func(*Env) *Promise, env *Env) *Promise {
	if len(args) != 0 {
		return Error(errors.New("wrong number of arguments"))
	}

	return p(k, env)
}

type predicate1 func(Term, func(*Env) *Promise, *Env) *Promise

func (p predicate1) Call(_ *VM, args []Term, k func(*Env) *Promise, env *Env) *Promise {
	if len(args) != 1 {
		return Error(fmt.Errorf("wrong number of arguments: %s", args))
	}

	return p(args[0], k, env)
}

type predicate2 func(Term, Term, func(*Env) *Promise, *Env) *Promise

func (p predicate2) Call(_ *VM, args []Term, k func(*Env) *Promise, env *Env) *Promise {
	if len(args) != 2 {
		return Error(errors.New("wrong number of arguments"))
	}

	return p(args[0], args[1], k, env)
}

type predicate3 func(Term, Term, Term, func(*Env) *Promise, *Env) *Promise

func (p predicate3) Call(_ *VM, args []Term, k func(*Env) *Promise, env *Env) *Promise {
	if len(args) != 3 {
		return Error(errors.New("wrong number of arguments"))
	}

	return p(args[0], args[1], args[2], k, env)
}

type predicate4 func(Term, Term, Term, Term, func(*Env) *Promise, *Env) *Promise

func (p predicate4) Call(_ *VM, args []Term, k func(*Env) *Promise, env *Env) *Promise {
	if len(args) != 4 {
		return Error(errors.New("wrong number of arguments"))
	}

	return p(args[0], args[1], args[2], args[3], k, env)
}

type predicate5 func(Term, Term, Term, Term, Term, func(*Env) *Promise, *Env) *Promise

func (p predicate5) Call(_ *VM, args []Term, k func(*Env) *Promise, env *Env) *Promise {
	if len(args) != 5 {
		return Error(errors.New("wrong number of arguments"))
	}

	return p(args[0], args[1], args[2], args[3], args[4], k, env)
}

func Success(_ *Env) *Promise {
	return Bool(true)
}

func Failure(_ *Env) *Promise {
	return Bool(false)
}

// Each iterates over list.
func Each(list Term, f func(elem Term) error, env *Env) error {
	whole := list
	for {
		switch l := env.Resolve(list).(type) {
		case Variable:
			return InstantiationError(whole)
		case Atom:
			if l != "[]" {
				return typeErrorList(l)
			}
			return nil
		case *Compound:
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

func Slice(list Term, env *Env) ([]Term, error) {
	var ret []Term
	if err := Each(list, func(elem Term) error {
		ret = append(ret, env.Resolve(elem))
		return nil
	}, env); err != nil {
		return nil, err
	}
	return ret, nil
}

// ProcedureIndicator identifies procedure e.g. (=)/2.
type ProcedureIndicator struct {
	Name  Atom
	Arity Integer
}

func (p ProcedureIndicator) String() string {
	return fmt.Sprintf("%s/%d", p.Name, p.Arity)
}

// Term returns p as term.
func (p ProcedureIndicator) Term() Term {
	return &Compound{
		Functor: "/",
		Args: []Term{
			p.Name,
			p.Arity,
		},
	}
}

// Apply applies p to args.
func (p ProcedureIndicator) Apply(args []Term) (Term, error) {
	if p.Arity != Integer(len(args)) {
		return nil, errors.New("wrong number of arguments")
	}
	return p.Name.Apply(args...), nil
}

func piArgs(t Term, env *Env) (ProcedureIndicator, []Term, error) {
	switch f := env.Resolve(t).(type) {
	case Variable:
		return ProcedureIndicator{}, nil, InstantiationError(t)
	case Atom:
		return ProcedureIndicator{Name: f, Arity: 0}, nil, nil
	case *Compound:
		return ProcedureIndicator{Name: f.Functor, Arity: Integer(len(f.Args))}, f.Args, nil
	default:
		return ProcedureIndicator{}, nil, typeErrorCallable(t)
	}
}
