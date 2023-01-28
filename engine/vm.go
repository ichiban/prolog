package engine

import (
	"context"
	"fmt"
	"io/fs"
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

// Success is a continuation that leads to true.
func Success(*Env) *Promise {
	return Bool(true)
}

// Failure is a continuation that leads to false.
func Failure(*Env) *Promise {
	return Bool(false)
}

// VM is the core of a Prolog interpreter. The zero value for VM is a valid VM without any builtin predicates.
type VM struct {
	// Unknown is a callback that is triggered when the VM reaches to an unknown predicate while current_prolog_flag(unknown, warning).
	Unknown func(ctx context.Context, name Atom, args []Term)

	procedures map[procedureIndicator]procedure
	unknown    unknownAction

	// FS is a file system that is referenced when the VM loads Prolog texts e.g. ensure_loaded/1.
	// It has no effect on open/4 nor open/3 which always access the actual file system.
	FS     fs.FS
	loaded map[string]struct{}

	// Internal/external expression
	operators       operators
	charConversions map[rune]rune
	charConvEnabled bool
	doubleQuotes    doubleQuotes

	// I/O
	streams       streams
	input, output *Stream

	// Misc
	debug bool
}

// Register0 registers a predicate of arity 0.
func (vm *VM) Register0(name Atom, p Predicate0) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: name, arity: 0}] = p
}

// Register1 registers a predicate of arity 1.
func (vm *VM) Register1(name Atom, p Predicate1) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: name, arity: 1}] = p
}

// Register2 registers a predicate of arity 2.
func (vm *VM) Register2(name Atom, p Predicate2) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: name, arity: 2}] = p
}

// Register3 registers a predicate of arity 3.
func (vm *VM) Register3(name Atom, p Predicate3) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: name, arity: 3}] = p
}

// Register4 registers a predicate of arity 4.
func (vm *VM) Register4(name Atom, p Predicate4) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: name, arity: 4}] = p
}

// Register5 registers a predicate of arity 5.
func (vm *VM) Register5(name Atom, p Predicate5) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: name, arity: 5}] = p
}

// Register6 registers a predicate of arity 6.
func (vm *VM) Register6(name Atom, p Predicate6) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: name, arity: 6}] = p
}

// Register7 registers a predicate of arity 7.
func (vm *VM) Register7(name Atom, p Predicate7) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: name, arity: 7}] = p
}

// Register8 registers a predicate of arity 8.
func (vm *VM) Register8(name Atom, p Predicate8) {
	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedure{}
	}
	vm.procedures[procedureIndicator{name: name, arity: 8}] = p
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
	call(context.Context, []Term) *Promise
}

// Cont is a continuation.
type Cont func(ctx context.Context) *Promise

// Arrive is the entry point of the VM.
func (vm *VM) Arrive(ctx context.Context, name Atom, args []Term) *Promise {
	if vm.Unknown == nil {
		vm.Unknown = func(context.Context, Atom, []Term) {}
	}

	pi := procedureIndicator{name: name, arity: Integer(len(args))}
	p, ok := vm.procedures[pi]
	if !ok {
		switch vm.unknown {
		case unknownWarning:
			vm.Unknown(ctx, name, args)
			fallthrough
		case unknownFail:
			return Bool(false)
		default:
			return Error(existenceError(ctx, objectTypeProcedure, pi.Term()))
		}
	}

	// bind the special variable to inform the predicate about the context.
	ctx = context.WithValue(ctx, prologCtxKey{}, pi.Term())

	return p.call(ctx, args)
}

type registers struct {
	pc           bytecode
	xr           []Term
	vars         []Variable
	cont         Cont
	args, astack Term

	ctx       context.Context
	cutParent *Promise
}

func (r *registers) updateCtx(ctx context.Context) *Promise {
	r.ctx = ctx
	return Bool(true)
}

func (vm *VM) exec(ctx context.Context, r registers) *Promise {
	jumpTable := [...]func(context.Context, registers) *Promise{
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
		if p := op(ctx, r); p != nil {
			return p
		}
	}
}

func (*VM) execConst(_ context.Context, r registers) *Promise {
	x := r.xr[r.pc[0].operand]
	arest := NewVariable()
	var ok bool
	r.ctx, ok = withUnification(r.ctx, r.args, Cons(x, arest))
	if !ok {
		return Bool(false)
	}
	r.pc = r.pc[1:]
	r.args = arest
	return nil
}

func (*VM) execVar(_ context.Context, r registers) *Promise {
	v := r.vars[r.pc[0].operand]
	arest := NewVariable()
	var ok bool
	r.ctx, ok = withUnification(r.ctx, Cons(v, arest), r.args)
	if !ok {
		return Bool(false)
	}
	r.pc = r.pc[1:]
	r.args = arest
	return nil
}

func (vm *VM) execFunctor(_ context.Context, r registers) *Promise {
	pi := r.xr[r.pc[0].operand].(procedureIndicator)
	arg, arest := NewVariable(), NewVariable()
	var ok bool
	r.ctx, ok = withUnification(r.ctx, r.args, Cons(arg, arest))
	if !ok {
		return Bool(false)
	}
	vars, err := makeSlice[Term](int(pi.arity))
	if err != nil {
		return Error(err)
	}
	for i := range vars {
		vars[i] = NewVariable()
	}
	r.ctx, ok = withUnification(r.ctx, arg, pi.name.Apply(vars...))
	if !ok {
		return Bool(false)
	}
	r.pc = r.pc[1:]
	r.args = NewVariable()
	r.ctx, ok = withUnification(r.ctx, List(vars...), r.args)
	if !ok {
		return Bool(false)
	}
	r.astack = Cons(arest, r.astack)
	return nil
}

func (*VM) execPop(_ context.Context, r registers) *Promise {
	var ok bool
	r.ctx, ok = withUnification(r.ctx, r.args, List())
	if !ok {
		return Bool(false)
	}
	r.pc = r.pc[1:]
	a, arest := NewVariable(), NewVariable()
	r.ctx, ok = withUnification(r.ctx, r.astack, Cons(a, arest))
	if !ok {
		return Bool(false)
	}
	r.args = a
	r.astack = arest
	return nil
}

func (*VM) execEnter(_ context.Context, r registers) *Promise {
	var ok bool
	r.ctx, ok = withUnification(r.ctx, r.args, List())
	if !ok {
		return Bool(false)
	}
	r.ctx, ok = withUnification(r.ctx, r.astack, List())
	if !ok {
		return Bool(false)
	}
	r.pc = r.pc[1:]
	v := NewVariable()
	r.args = v
	r.astack = v
	return nil
}

func (vm *VM) execCall(ctx context.Context, r registers) *Promise {
	pi := r.xr[r.pc[0].operand].(procedureIndicator)
	var ok bool
	r.ctx, ok = withUnification(r.ctx, r.args, List())
	if !ok {
		return Bool(false)
	}
	r.pc = r.pc[1:]
	args, _ := slice(r.ctx, r.astack)

	ctx = WithCont(ctx, func(ctx context.Context) *Promise {
		v := NewVariable()
		return vm.exec(ctx, registers{
			pc:        r.pc,
			xr:        r.xr,
			vars:      r.vars,
			cont:      r.cont,
			args:      v,
			astack:    v,
			ctx:       ctx,
			cutParent: r.cutParent,
		})
	})
	return vm.Arrive(ctx, pi.name, args)
}

func (*VM) execExit(_ context.Context, r registers) *Promise {
	return r.cont(r.ctx)
}

func (vm *VM) execCut(ctx context.Context, r registers) *Promise {
	r.pc = r.pc[1:]
	return cut(r.cutParent, func() *Promise {
		return vm.exec(ctx, registers{
			pc:        r.pc,
			xr:        r.xr,
			vars:      r.vars,
			cont:      r.cont,
			args:      r.args,
			astack:    r.astack,
			ctx:       r.ctx,
			cutParent: r.cutParent,
		})
	})
}

func (vm *VM) execList(_ context.Context, r registers) *Promise {
	l := r.xr[r.pc[0].operand].(Integer)
	arg, arest := NewVariable(), NewVariable()
	var ok bool
	r.ctx, ok = withUnification(r.ctx, r.args, Cons(arg, arest))
	if !ok {
		return Bool(false)
	}
	ctx := WithCont(r.ctx, r.updateCtx)
	_, _ = Length(ctx, arg, l).Force()
	r.pc = r.pc[1:]
	r.args = arg
	r.astack = Cons(arest, r.astack)
	return nil
}

func (vm *VM) execPartial(_ context.Context, r registers) *Promise {
	l := r.xr[r.pc[0].operand].(Integer)
	arg, arest := NewVariable(), NewVariable()
	var ok bool
	r.ctx, ok = withUnification(r.ctx, r.args, Cons(arg, arest))
	if !ok {
		return Bool(false)
	}
	prefix, tail := NewVariable(), NewVariable()
	_, _ = Length(WithCont(r.ctx, r.updateCtx), prefix, l).Force()
	_, _ = Append(WithCont(r.ctx, r.updateCtx), prefix, tail, arg).Force()
	r.pc = r.pc[1:]
	r.args = Cons(tail, prefix)
	r.astack = Cons(arest, r.astack)
	return nil
}

// SetUserInput sets the given stream as user_input.
func (vm *VM) SetUserInput(s *Stream) {
	s.vm = vm
	s.alias = atomUserInput
	vm.streams.add(s)
	vm.input = s
}

// SetUserOutput sets the given stream as user_output.
func (vm *VM) SetUserOutput(s *Stream) {
	s.vm = vm
	s.alias = atomUserOutput
	vm.streams.add(s)
	vm.output = s
}

// Predicate0 is a predicate of arity 0.
type Predicate0 func(context.Context) *Promise

func (p Predicate0) call(ctx context.Context, args []Term) *Promise {
	if len(args) != 0 {
		return Error(&wrongNumberOfArgumentsError{expected: 0, actual: args})
	}

	return p(ctx)
}

// Predicate1 is a predicate of arity 1.
type Predicate1 func(context.Context, Term) *Promise

func (p Predicate1) call(ctx context.Context, args []Term) *Promise {
	if len(args) != 1 {
		return Error(&wrongNumberOfArgumentsError{expected: 1, actual: args})
	}

	return p(ctx, args[0])
}

// Predicate2 is a predicate of arity 2.
type Predicate2 func(context.Context, Term, Term) *Promise

func (p Predicate2) call(ctx context.Context, args []Term) *Promise {
	if len(args) != 2 {
		return Error(&wrongNumberOfArgumentsError{expected: 2, actual: args})
	}

	return p(ctx, args[0], args[1])
}

// Predicate3 is a predicate of arity 3.
type Predicate3 func(context.Context, Term, Term, Term) *Promise

func (p Predicate3) call(ctx context.Context, args []Term) *Promise {
	if len(args) != 3 {
		return Error(&wrongNumberOfArgumentsError{expected: 3, actual: args})
	}

	return p(ctx, args[0], args[1], args[2])
}

// Predicate4 is a predicate of arity 4.
type Predicate4 func(context.Context, Term, Term, Term, Term) *Promise

func (p Predicate4) call(ctx context.Context, args []Term) *Promise {
	if len(args) != 4 {
		return Error(&wrongNumberOfArgumentsError{expected: 4, actual: args})
	}

	return p(ctx, args[0], args[1], args[2], args[3])
}

// Predicate5 is a predicate of arity 5.
type Predicate5 func(context.Context, Term, Term, Term, Term, Term) *Promise

func (p Predicate5) call(ctx context.Context, args []Term) *Promise {
	if len(args) != 5 {
		return Error(&wrongNumberOfArgumentsError{expected: 5, actual: args})
	}

	return p(ctx, args[0], args[1], args[2], args[3], args[4])
}

// Predicate6 is a predicate of arity 6.
type Predicate6 func(context.Context, Term, Term, Term, Term, Term, Term) *Promise

func (p Predicate6) call(ctx context.Context, args []Term) *Promise {
	if len(args) != 6 {
		return Error(&wrongNumberOfArgumentsError{expected: 6, actual: args})
	}

	return p(ctx, args[0], args[1], args[2], args[3], args[4], args[5])
}

// Predicate7 is a predicate of arity 7.
type Predicate7 func(context.Context, Term, Term, Term, Term, Term, Term, Term) *Promise

func (p Predicate7) call(ctx context.Context, args []Term) *Promise {
	if len(args) != 7 {
		return Error(&wrongNumberOfArgumentsError{expected: 7, actual: args})
	}

	return p(ctx, args[0], args[1], args[2], args[3], args[4], args[5], args[6])
}

// Predicate8 is a predicate of arity 8.
type Predicate8 func(context.Context, Term, Term, Term, Term, Term, Term, Term, Term) *Promise

func (p Predicate8) call(ctx context.Context, args []Term) *Promise {
	if len(args) != 8 {
		return Error(&wrongNumberOfArgumentsError{expected: 8, actual: args})
	}

	return p(ctx, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7])
}

// procedureIndicator identifies a procedure e.g. (=)/2.
type procedureIndicator struct {
	name  Atom
	arity Integer
}

func (p procedureIndicator) String() string {
	var sb strings.Builder
	_ = writeAtom(&sb, p.name, &writeOptions{
		quoted: true,
	})
	_, _ = fmt.Fprintf(&sb, "/%d", p.arity)
	return sb.String()
}

// Term returns p as term.
func (p procedureIndicator) Term() Term {
	return atomSlash.Apply(p.name, p.arity)
}

// Apply applies p to args.
func (p procedureIndicator) Apply(args ...Term) (Term, error) {
	if p.arity != Integer(len(args)) {
		return nil, &wrongNumberOfArgumentsError{expected: int(p.arity), actual: args}
	}
	return p.name.Apply(args...), nil
}

func piArg(ctx context.Context, t Term) (procedureIndicator, func(int) Term, error) {
	switch f := Resolve(ctx, t).(type) {
	case Variable:
		return procedureIndicator{}, nil, InstantiationError(ctx)
	case Atom:
		return procedureIndicator{name: f, arity: 0}, nil, nil
	case Compound:
		return procedureIndicator{name: f.Functor(), arity: Integer(f.Arity())}, f.Arg, nil
	default:
		return procedureIndicator{}, nil, typeError(ctx, validTypeCallable, f)
	}
}

type wrongNumberOfArgumentsError struct {
	expected int
	actual   []Term
}

func (e *wrongNumberOfArgumentsError) Error() string {
	return fmt.Sprintf("wrong number of arguments: expected=%d, actual=%s", e.expected, e.actual)
}
