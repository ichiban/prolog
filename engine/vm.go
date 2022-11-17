package engine

import (
	"context"
	"fmt"
	"strings"
)

type bytecode []instruction

type instruction struct {
	opcode  opcode
	operand byte
}

type opcode byte

const (
	opEnter opcode = iota
	opCall
	opExit
	opConst
	opVar
	opFunctor
	opPop

	opCut
	opList
	opPartial
)

var (
	// Success is a continuation that leads to true.
	Success = func(*Env) *Promise {
		return Bool(true)
	}

	// Failure is a continuation that leads to false.
	Failure = func(*Env) *Promise {
		return Bool(false)
	}
)

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

	procedures map[ProcedureIndicator]procedure
	unknown    unknownAction
}

// Register0 registers a predicate of arity 0.
func (vm *VM) Register0(name string, p func(func(*Env) *Promise, *Env) *Promise) {
	if vm.procedures == nil {
		vm.procedures = map[ProcedureIndicator]procedure{}
	}
	vm.procedures[ProcedureIndicator{Name: NewAtom(name), Arity: 0}] = predicate0(p)
}

// Register1 registers a predicate of arity 1.
func (vm *VM) Register1(name string, p func(Term, func(*Env) *Promise, *Env) *Promise) {
	if vm.procedures == nil {
		vm.procedures = map[ProcedureIndicator]procedure{}
	}
	vm.procedures[ProcedureIndicator{Name: NewAtom(name), Arity: 1}] = predicate1(p)
}

// Register2 registers a predicate of arity 2.
func (vm *VM) Register2(name string, p func(Term, Term, func(*Env) *Promise, *Env) *Promise) {
	if vm.procedures == nil {
		vm.procedures = map[ProcedureIndicator]procedure{}
	}
	vm.procedures[ProcedureIndicator{Name: NewAtom(name), Arity: 2}] = predicate2(p)
}

// Register3 registers a predicate of arity 3.
func (vm *VM) Register3(name string, p func(Term, Term, Term, func(*Env) *Promise, *Env) *Promise) {
	if vm.procedures == nil {
		vm.procedures = map[ProcedureIndicator]procedure{}
	}
	vm.procedures[ProcedureIndicator{Name: NewAtom(name), Arity: 3}] = predicate3(p)
}

// Register4 registers a predicate of arity 4.
func (vm *VM) Register4(name string, p func(Term, Term, Term, Term, func(*Env) *Promise, *Env) *Promise) {
	if vm.procedures == nil {
		vm.procedures = map[ProcedureIndicator]procedure{}
	}
	vm.procedures[ProcedureIndicator{Name: NewAtom(name), Arity: 4}] = predicate4(p)
}

// Register5 registers a predicate of arity 5.
func (vm *VM) Register5(name string, p func(Term, Term, Term, Term, Term, func(*Env) *Promise, *Env) *Promise) {
	if vm.procedures == nil {
		vm.procedures = map[ProcedureIndicator]procedure{}
	}
	vm.procedures[ProcedureIndicator{Name: NewAtom(name), Arity: 5}] = predicate5(p)
}

// Register6 registers a predicate of arity 6.
func (vm *VM) Register6(name string, p func(Term, Term, Term, Term, Term, Term, func(*Env) *Promise, *Env) *Promise) {
	if vm.procedures == nil {
		vm.procedures = map[ProcedureIndicator]procedure{}
	}
	vm.procedures[ProcedureIndicator{Name: NewAtom(name), Arity: 6}] = predicate6(p)
}

// Register7 registers a predicate of arity 7.
func (vm *VM) Register7(name string, p func(Term, Term, Term, Term, Term, Term, Term, func(*Env) *Promise, *Env) *Promise) {
	if vm.procedures == nil {
		vm.procedures = map[ProcedureIndicator]procedure{}
	}
	vm.procedures[ProcedureIndicator{Name: NewAtom(name), Arity: 7}] = predicate7(p)
}

// Register8 registers a predicate of arity 8.
func (vm *VM) Register8(name string, p func(Term, Term, Term, Term, Term, Term, Term, Term, func(*Env) *Promise, *Env) *Promise) {
	if vm.procedures == nil {
		vm.procedures = map[ProcedureIndicator]procedure{}
	}
	vm.procedures[ProcedureIndicator{Name: NewAtom(name), Arity: 8}] = predicate8(p)
}

type unknownAction int

const (
	unknownError unknownAction = iota
	unknownFail
	unknownWarning
)

func (u unknownAction) String() string {
	return [...]string{
		unknownError:   "error",
		unknownFail:    "fail",
		unknownWarning: "warning",
	}[u]
}

type procedure interface {
	Call(*VM, []Term, func(*Env) *Promise, *Env) *Promise
}

// Arrive is the entry point of the VM.
func (vm *VM) Arrive(pi ProcedureIndicator, args []Term, k func(*Env) *Promise, env *Env) *Promise {
	if vm.OnUnknown == nil {
		vm.OnUnknown = func(ProcedureIndicator, []Term, *Env) {}
	}

	p, ok := vm.procedures[pi]
	if !ok {
		switch vm.unknown {
		case unknownWarning:
			vm.OnUnknown(pi, args, env)
			fallthrough
		case unknownFail:
			return Bool(false)
		default:
			return Error(ExistenceError(ObjectTypeProcedure, pi.Term(), env))
		}
	}

	// Bind the special variable to inform the predicate about the context.
	env = env.Bind(varContext, pi.Term())

	return p.Call(vm, args, k, env)
}

type registers struct {
	pc           bytecode
	xr           []Term
	vars         []Variable
	cont         func(*Env) *Promise
	args, astack Term

	env       *Env
	cutParent *Promise
}

func (r *registers) updateEnv(e *Env) *Promise {
	r.env = e
	return Bool(true)
}

func (vm *VM) exec(r registers) *Promise {
	jumpTable := [...]func(r *registers) *Promise{
		opConst:   vm.execConst,
		opVar:     vm.execVar,
		opFunctor: vm.execFunctor,
		opPop:     vm.execPop,
		opEnter:   vm.execEnter,
		opCall:    vm.execCall,
		opExit:    vm.execExit,
		opCut:     vm.execCut,
		opList:    vm.execList,
		opPartial: vm.execPartial,
	}
	for {
		op := jumpTable[r.pc[0].opcode]
		if p := op(&r); p != nil {
			return p
		}
	}
}

func (*VM) execConst(r *registers) *Promise {
	x := r.xr[r.pc[0].operand]
	arest := NewVariable()
	var ok bool
	r.env, ok = r.env.Unify(r.args, Cons(x, arest), false)
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
	var ok bool
	r.env, ok = r.env.Unify(Cons(v, arest), r.args, false)
	if !ok {
		return Bool(false)
	}
	r.pc = r.pc[1:]
	r.args = arest
	return nil
}

func (*VM) execFunctor(r *registers) *Promise {
	pi := r.xr[r.pc[0].operand].(ProcedureIndicator)
	arg, arest := NewVariable(), NewVariable()
	var ok bool
	r.env, ok = r.env.Unify(r.args, Cons(arg, arest), false)
	if !ok {
		return Bool(false)
	}
	ok, err := Functor(arg, pi.Name, pi.Arity, r.updateEnv, r.env).Force(context.Background())
	if err != nil {
		return Error(err)
	}
	if !ok {
		return Bool(false)
	}
	r.pc = r.pc[1:]
	r.args = NewVariable()
	ok, err = Univ(arg, Cons(pi.Name, r.args), r.updateEnv, r.env).Force(context.Background())
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
	r.env, ok = r.env.Unify(r.args, List(), false)
	if !ok {
		return Bool(false)
	}
	r.pc = r.pc[1:]
	a, arest := NewVariable(), NewVariable()
	r.env, ok = r.env.Unify(r.astack, Cons(a, arest), false)
	if !ok {
		return Bool(false)
	}
	r.args = a
	r.astack = arest
	return nil
}

func (*VM) execEnter(r *registers) *Promise {
	var ok bool
	r.env, ok = r.env.Unify(r.args, List(), false)
	if !ok {
		return Bool(false)
	}
	r.env, ok = r.env.Unify(r.astack, List(), false)
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
	pi := r.xr[r.pc[0].operand].(ProcedureIndicator)
	var ok bool
	r.env, ok = r.env.Unify(r.args, List(), false)
	if !ok {
		return Bool(false)
	}
	r.pc = r.pc[1:]
	args, _ := Slice(r.astack, r.env)
	return vm.Arrive(pi, args, func(env *Env) *Promise {
		v := NewVariable()
		return vm.exec(registers{
			pc:        r.pc,
			xr:        r.xr,
			vars:      r.vars,
			cont:      r.cont,
			args:      v,
			astack:    v,
			env:       env,
			cutParent: r.cutParent,
		})
	}, r.env)
}

func (*VM) execExit(r *registers) *Promise {
	return r.cont(r.env)
}

func (vm *VM) execCut(r *registers) *Promise {
	r.pc = r.pc[1:]
	return Cut(r.cutParent, func(context.Context) *Promise {
		return vm.exec(registers{
			pc:        r.pc,
			xr:        r.xr,
			vars:      r.vars,
			cont:      r.cont,
			args:      r.args,
			astack:    r.astack,
			env:       r.env,
			cutParent: r.cutParent,
		})
	})
}

func (vm *VM) execList(r *registers) *Promise {
	l := r.xr[r.pc[0].operand].(Integer)
	arg, arest := NewVariable(), NewVariable()
	var ok bool
	r.env, ok = r.env.Unify(r.args, Cons(arg, arest), false)
	if !ok {
		return Bool(false)
	}
	_, _ = Length(arg, l, r.updateEnv, r.env).Force(context.Background())
	r.pc = r.pc[1:]
	r.args = arg
	r.astack = Cons(arest, r.astack)
	return nil
}

func (vm *VM) execPartial(r *registers) *Promise {
	l := r.xr[r.pc[0].operand].(Integer)
	arg, arest := NewVariable(), NewVariable()
	var ok bool
	r.env, ok = r.env.Unify(r.args, Cons(arg, arest), false)
	if !ok {
		return Bool(false)
	}
	prefix, tail := NewVariable(), NewVariable()
	_, _ = Length(prefix, l, r.updateEnv, r.env).Force(context.Background())
	_, _ = Append(prefix, tail, arg, r.updateEnv, r.env).Force(context.Background())
	r.pc = r.pc[1:]
	r.args = Cons(tail, prefix)
	r.astack = Cons(arest, r.astack)
	return nil
}

type predicate0 func(func(*Env) *Promise, *Env) *Promise

func (p predicate0) Call(_ *VM, args []Term, k func(*Env) *Promise, env *Env) *Promise {
	if len(args) != 0 {
		return Error(&wrongNumberOfArgumentsError{expected: 0, actual: args})
	}

	return p(k, env)
}

type predicate1 func(Term, func(*Env) *Promise, *Env) *Promise

func (p predicate1) Call(_ *VM, args []Term, k func(*Env) *Promise, env *Env) *Promise {
	if len(args) != 1 {
		return Error(&wrongNumberOfArgumentsError{expected: 1, actual: args})
	}

	return p(args[0], k, env)
}

type predicate2 func(Term, Term, func(*Env) *Promise, *Env) *Promise

func (p predicate2) Call(_ *VM, args []Term, k func(*Env) *Promise, env *Env) *Promise {
	if len(args) != 2 {
		return Error(&wrongNumberOfArgumentsError{expected: 2, actual: args})
	}

	return p(args[0], args[1], k, env)
}

type predicate3 func(Term, Term, Term, func(*Env) *Promise, *Env) *Promise

func (p predicate3) Call(_ *VM, args []Term, k func(*Env) *Promise, env *Env) *Promise {
	if len(args) != 3 {
		return Error(&wrongNumberOfArgumentsError{expected: 3, actual: args})
	}

	return p(args[0], args[1], args[2], k, env)
}

type predicate4 func(Term, Term, Term, Term, func(*Env) *Promise, *Env) *Promise

func (p predicate4) Call(_ *VM, args []Term, k func(*Env) *Promise, env *Env) *Promise {
	if len(args) != 4 {
		return Error(&wrongNumberOfArgumentsError{expected: 4, actual: args})
	}

	return p(args[0], args[1], args[2], args[3], k, env)
}

type predicate5 func(Term, Term, Term, Term, Term, func(*Env) *Promise, *Env) *Promise

func (p predicate5) Call(_ *VM, args []Term, k func(*Env) *Promise, env *Env) *Promise {
	if len(args) != 5 {
		return Error(&wrongNumberOfArgumentsError{expected: 5, actual: args})
	}

	return p(args[0], args[1], args[2], args[3], args[4], k, env)
}

type predicate6 func(Term, Term, Term, Term, Term, Term, func(*Env) *Promise, *Env) *Promise

func (p predicate6) Call(_ *VM, args []Term, k func(*Env) *Promise, env *Env) *Promise {
	if len(args) != 6 {
		return Error(&wrongNumberOfArgumentsError{expected: 6, actual: args})
	}

	return p(args[0], args[1], args[2], args[3], args[4], args[5], k, env)
}

type predicate7 func(Term, Term, Term, Term, Term, Term, Term, func(*Env) *Promise, *Env) *Promise

func (p predicate7) Call(_ *VM, args []Term, k func(*Env) *Promise, env *Env) *Promise {
	if len(args) != 7 {
		return Error(&wrongNumberOfArgumentsError{expected: 7, actual: args})
	}

	return p(args[0], args[1], args[2], args[3], args[4], args[5], args[6], k, env)
}

type predicate8 func(Term, Term, Term, Term, Term, Term, Term, Term, func(*Env) *Promise, *Env) *Promise

func (p predicate8) Call(_ *VM, args []Term, k func(*Env) *Promise, env *Env) *Promise {
	if len(args) != 8 {
		return Error(&wrongNumberOfArgumentsError{expected: 8, actual: args})
	}

	return p(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], k, env)
}

// ProcedureIndicator identifies procedure e.g. (=)/2.
type ProcedureIndicator struct {
	Name  Atom
	Arity Integer
}

// NewProcedureIndicator creates a new ProcedureIndicator from a term that matches Name/Arity.
func NewProcedureIndicator(pi Term, env *Env) (ProcedureIndicator, error) {
	switch p := env.Resolve(pi).(type) {
	case Variable:
		return ProcedureIndicator{}, InstantiationError(env)
	case Compound:
		if p.Functor() != atomSlash || p.Arity() != 2 {
			return ProcedureIndicator{}, TypeError(ValidTypePredicateIndicator, pi, env)
		}
		switch f := env.Resolve(p.Arg(0)).(type) {
		case Variable:
			return ProcedureIndicator{}, InstantiationError(env)
		case Atom:
			switch a := env.Resolve(p.Arg(1)).(type) {
			case Variable:
				return ProcedureIndicator{}, InstantiationError(env)
			case Integer:
				pi := ProcedureIndicator{Name: f, Arity: a}
				return pi, nil
			default:
				return ProcedureIndicator{}, TypeError(ValidTypePredicateIndicator, pi, env)
			}
		default:
			return ProcedureIndicator{}, TypeError(ValidTypePredicateIndicator, pi, env)
		}
	default:
		return ProcedureIndicator{}, TypeError(ValidTypePredicateIndicator, pi, env)
	}
}

func (p ProcedureIndicator) String() string {
	var sb strings.Builder
	_ = writeAtom(&sb, p.Name, &WriteOptions{
		Quoted: true,
	})
	_, _ = fmt.Fprintf(&sb, "/%d", p.Arity)
	return sb.String()
}

// Term returns p as term.
func (p ProcedureIndicator) Term() Term {
	return atomSlash.Apply(p.Name, p.Arity)
}

// Apply applies p to args.
func (p ProcedureIndicator) Apply(args ...Term) (Term, error) {
	if p.Arity != Integer(len(args)) {
		return nil, &wrongNumberOfArgumentsError{expected: int(p.Arity), actual: args}
	}
	return p.Name.Apply(args...), nil
}

func PI(t Term, env *Env) (ProcedureIndicator, func(int) Term, error) {
	switch f := env.Resolve(t).(type) {
	case Variable:
		return ProcedureIndicator{}, nil, InstantiationError(env)
	case Atom:
		return ProcedureIndicator{Name: f, Arity: 0}, nil, nil
	case Compound:
		return ProcedureIndicator{Name: f.Functor(), Arity: Integer(f.Arity())}, f.Arg, nil
	default:
		return ProcedureIndicator{}, nil, TypeError(ValidTypeCallable, f, env)
	}
}

type wrongNumberOfArgumentsError struct {
	expected int
	actual   []Term
}

func (e *wrongNumberOfArgumentsError) Error() string {
	return fmt.Sprintf("wrong number of arguments: expected=%d, actual=%s", e.expected, e.actual)
}
