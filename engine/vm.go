package engine

import (
	"context"
	"fmt"
	"io"
	"io/fs"
	"strings"
)

type bytecode []instruction

type instruction struct {
	opcode  opcode
	operand Term
}

type opcode byte

const (
	opEnter opcode = iota
	opCall
	opExit
	opGetConst
	opPutConst
	opGetVar
	opPutVar
	opGetFunctor
	opPutFunctor
	opPop

	opCut
	opGetList
	opPutList
	opGetPartial
	opPutPartial
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
	Unknown func(name Atom, args []Term, env *Env)

	// FS is a file system that is referenced when the VM loads Prolog texts e.g. ensure_loaded/1.
	// It has no effect on open/4 nor open/3 which always access the actual file system.
	FS     fs.FS
	loaded map[string]struct{}

	modules map[Atom]*Module
	system  *Module
	typeIn  *Module

	// I/O
	streams       streams
	input, output *Stream
}

// Module returns the type-in module.
func (vm *VM) Module() *Module {
	if vm.typeIn == nil {
		vm.typeIn = vm.module(atomUser)
	}
	return vm.typeIn
}

func (vm *VM) SystemModule() *Module {
	if vm.system == nil {
		vm.system = vm.module(atomProlog)
	}
	return vm.system
}

func (vm *VM) module(name Atom) *Module {
	m, _ := vm.modules[name]
	if m != nil {
		return m
	}

	m = &Module{name: name}
	// TODO: import from the system module.

	if vm.modules == nil {
		vm.modules = map[Atom]*Module{}
	}
	vm.modules[name] = m
	return m
}

// SetModule sets the type-in module.
func (vm *VM) SetModule(name string) {
	n := NewAtom(name)
	vm.typeIn = vm.module(n)
}

// Cont is a continuation.
type Cont func(*Env) *Promise

// Arrive is the entry point of the VM.
func (vm *VM) Arrive(name Atom, args []Term, k Cont, env *Env) (promise *Promise) {
	defer ensurePromise(&promise)

	if vm.Unknown == nil {
		vm.Unknown = func(Atom, []Term, *Env) {}
	}

	m := vm.Module()
	pi := procedureIndicator{name: name, arity: Integer(len(args))}
	e, ok := m.procedures[pi]
	if !ok {
		switch m.unknown {
		case unknownWarning:
			vm.Unknown(name, args, env)
			fallthrough
		case unknownFail:
			return Bool(false)
		default:
			return Error(existenceError(objectTypeProcedure, pi.Term(), env))
		}
	}

	// bind the special variable to inform the predicate about the context.
	env = env.bind(varContext, pi.Term())

	return e.procedure.call(vm, args, k, env)
}

func (vm *VM) exec(pc bytecode, vars []Variable, cont Cont, args []Term, astack [][]Term, env *Env, cutParent *Promise) *Promise {
	var (
		ok  = true
		op  instruction
		arg Term
	)
	for ok {
		op, pc = pc[0], pc[1:]
		switch opcode, operand := op.opcode, op.operand; opcode {
		case opGetConst:
			arg, args = args[0], args[1:]
			env, ok = env.Unify(arg, operand)
		case opPutConst:
			args = append(args, operand)
		case opGetVar:
			v := vars[operand.(Integer)]
			arg, args = args[0], args[1:]
			env, ok = env.Unify(arg, v)
		case opPutVar:
			v := vars[operand.(Integer)]
			args = append(args, v)
		case opGetFunctor:
			pi := operand.(procedureIndicator)
			arg, astack = env.Resolve(args[0]), append(astack, args[1:])
			args = make([]Term, int(pi.arity))
			for i := range args {
				args[i] = NewVariable()
			}
			env, ok = env.Unify(arg, pi.name.Apply(args...))
		case opPutFunctor:
			pi := operand.(procedureIndicator)
			vs := make([]Term, int(pi.arity))
			arg = pi.name.Apply(vs...)
			args = append(args, arg)
			astack = append(astack, args)
			args = vs[:0]
		case opPop:
			args, astack = astack[len(astack)-1], astack[:len(astack)-1]
		case opEnter:
			break
		case opCall:
			pi := operand.(procedureIndicator)
			return vm.Arrive(pi.name, args, func(env *Env) *Promise {
				return vm.exec(pc, vars, cont, nil, nil, env, cutParent)
			}, env)
		case opExit:
			return cont(env)
		case opCut:
			return cut(cutParent, func(context.Context) *Promise {
				return vm.exec(pc, vars, cont, args, astack, env, cutParent)
			})
		case opGetList:
			l := operand.(Integer)
			arg, astack = args[0], append(astack, args[1:])
			args = make([]Term, int(l))
			for i := range args {
				args[i] = NewVariable()
			}
			env, ok = env.Unify(arg, list(args))
		case opPutList:
			l := operand.(Integer)
			vs := make([]Term, int(l))
			arg = list(vs)
			args = append(args, arg)
			astack = append(astack, args)
			args = vs[:0]
		case opGetPartial:
			l := operand.(Integer)
			arg, astack = args[0], append(astack, args[1:])
			args = make([]Term, int(l+1))
			for i := range args {
				args[i] = NewVariable()
			}
			env, ok = env.Unify(arg, PartialList(args[0], args[1:]...))
		case opPutPartial:
			l := operand.(Integer)
			vs := make([]Term, int(l+1))
			arg = &partial{
				Compound: list(vs[1:]),
				tail:     &vs[0],
			}
			args = append(args, arg)
			astack = append(astack, args)
			args = vs[:0]
		}
	}

	return Bool(false)
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

func (vm *VM) Compile(ctx context.Context, text string) error {
	m := vm.Module()

	// Skip a shebang line if exists.
	if strings.HasPrefix(text, "#!") {
		switch i := strings.IndexRune(text, '\n'); i {
		case -1:
			text = ""
		default:
			text = text[i+1:]
		}
	}

	r := strings.NewReader(text)
	p := NewParser(m, r)
	for p.More() {
		p.Vars = p.Vars[:]
		t, err := p.Term()
		if err != nil {
			return err
		}

		et, err := expand(vm, t, nil)
		if err != nil {
			return err
		}

		pi, arg, err := piArg(et, nil)
		if err != nil {
			return err
		}
		switch pi {
		case procedureIndicator{name: atomIf, arity: 1}: // Directive
			if err := m.flushClauseBuf(); err != nil {
				return err
			}

			ok, err := Call(vm, arg(0), Success, nil).Force(ctx)
			if err != nil {
				return err
			}
			if !ok {
				return errExpectation
			}

			continue
		case procedureIndicator{name: atomIf, arity: 2}: // Rule
			pi, arg, err = piArg(arg(0), nil)
			if err != nil {
				return err
			}

			fallthrough
		default:
			if len(m.buf) > 0 && pi != m.buf[0].pi {
				if err := m.flushClauseBuf(); err != nil {
					return err
				}
			}

			cs, err := compile(et, nil)
			if err != nil {
				return err
			}

			m.buf = append(m.buf, cs...)
		}
	}

	if err := m.flushClauseBuf(); err != nil {
		return err
	}

	for _, g := range m.initGoals {
		ok, err := Call(vm, g, Success, nil).Force(ctx)
		if err != nil {
			return err
		}
		if !ok {
			var sb strings.Builder
			s := NewOutputTextStream(&sb)
			_, _ = WriteTerm(vm, s, g, List(atomQuoted.Apply(atomTrue)), Success, nil).Force(ctx)
			return fmt.Errorf("failed initialization goal: %s", sb.String())
		}
	}
	m.initGoals = m.initGoals[:0]

	return nil
}

func (vm *VM) Load(ctx context.Context, filename string) error {
	f, err := vm.FS.Open(filename)
	if err != nil {
		return err
	}
	defer func() {
		_ = f.Close()
	}()

	b, err := io.ReadAll(f)
	if err != nil {
		return err
	}

	return vm.Compile(ctx, string(b))
}

// Predicate0 is a predicate of arity 0.
type Predicate0 func(*VM, Cont, *Env) *Promise

func (p Predicate0) call(vm *VM, args []Term, k Cont, env *Env) *Promise {
	if len(args) != 0 {
		return Error(&wrongNumberOfArgumentsError{expected: 0, actual: args})
	}

	return p(vm, k, env)
}

// Predicate1 is a predicate of arity 1.
type Predicate1 func(*VM, Term, Cont, *Env) *Promise

func (p Predicate1) call(vm *VM, args []Term, k Cont, env *Env) *Promise {
	if len(args) != 1 {
		return Error(&wrongNumberOfArgumentsError{expected: 1, actual: args})
	}

	return p(vm, args[0], k, env)
}

// Predicate2 is a predicate of arity 2.
type Predicate2 func(*VM, Term, Term, Cont, *Env) *Promise

func (p Predicate2) call(vm *VM, args []Term, k Cont, env *Env) *Promise {
	if len(args) != 2 {
		return Error(&wrongNumberOfArgumentsError{expected: 2, actual: args})
	}

	return p(vm, args[0], args[1], k, env)
}

// Predicate3 is a predicate of arity 3.
type Predicate3 func(*VM, Term, Term, Term, Cont, *Env) *Promise

func (p Predicate3) call(vm *VM, args []Term, k Cont, env *Env) *Promise {
	if len(args) != 3 {
		return Error(&wrongNumberOfArgumentsError{expected: 3, actual: args})
	}

	return p(vm, args[0], args[1], args[2], k, env)
}

// Predicate4 is a predicate of arity 4.
type Predicate4 func(*VM, Term, Term, Term, Term, Cont, *Env) *Promise

func (p Predicate4) call(vm *VM, args []Term, k Cont, env *Env) *Promise {
	if len(args) != 4 {
		return Error(&wrongNumberOfArgumentsError{expected: 4, actual: args})
	}

	return p(vm, args[0], args[1], args[2], args[3], k, env)
}

// Predicate5 is a predicate of arity 5.
type Predicate5 func(*VM, Term, Term, Term, Term, Term, Cont, *Env) *Promise

func (p Predicate5) call(vm *VM, args []Term, k Cont, env *Env) *Promise {
	if len(args) != 5 {
		return Error(&wrongNumberOfArgumentsError{expected: 5, actual: args})
	}

	return p(vm, args[0], args[1], args[2], args[3], args[4], k, env)
}

// Predicate6 is a predicate of arity 6.
type Predicate6 func(*VM, Term, Term, Term, Term, Term, Term, Cont, *Env) *Promise

func (p Predicate6) call(vm *VM, args []Term, k Cont, env *Env) *Promise {
	if len(args) != 6 {
		return Error(&wrongNumberOfArgumentsError{expected: 6, actual: args})
	}

	return p(vm, args[0], args[1], args[2], args[3], args[4], args[5], k, env)
}

// Predicate7 is a predicate of arity 7.
type Predicate7 func(*VM, Term, Term, Term, Term, Term, Term, Term, Cont, *Env) *Promise

func (p Predicate7) call(vm *VM, args []Term, k Cont, env *Env) *Promise {
	if len(args) != 7 {
		return Error(&wrongNumberOfArgumentsError{expected: 7, actual: args})
	}

	return p(vm, args[0], args[1], args[2], args[3], args[4], args[5], args[6], k, env)
}

// Predicate8 is a predicate of arity 8.
type Predicate8 func(*VM, Term, Term, Term, Term, Term, Term, Term, Term, Cont, *Env) *Promise

func (p Predicate8) call(vm *VM, args []Term, k Cont, env *Env) *Promise {
	if len(args) != 8 {
		return Error(&wrongNumberOfArgumentsError{expected: 8, actual: args})
	}

	return p(vm, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], k, env)
}

// procedureIndicator identifies a procedure e.g. (=)/2.
type procedureIndicator struct {
	name  Atom
	arity Integer
}

func (p procedureIndicator) WriteTerm(w io.Writer, opts *WriteOptions, env *Env) error {
	return WriteCompound(w, p, opts, env)
}

func (p procedureIndicator) Compare(t Term, env *Env) int {
	return CompareCompound(p, t, env)
}

func (p procedureIndicator) Functor() Atom {
	return atomSlash
}

func (p procedureIndicator) Arity() int {
	return 2
}

func (p procedureIndicator) Arg(n int) Term {
	if n == 0 {
		return p.name
	}
	return p.arity
}

func (p procedureIndicator) String() string {
	var sb strings.Builder
	_ = p.name.WriteTerm(&sb, &WriteOptions{
		quoted: true,
	}, nil)
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

func piArg(t Term, env *Env) (procedureIndicator, func(int) Term, error) {
	switch f := env.Resolve(t).(type) {
	case Variable:
		return procedureIndicator{}, nil, InstantiationError(env)
	case Atom:
		return procedureIndicator{name: f, arity: 0}, nil, nil
	case Compound:
		return procedureIndicator{name: f.Functor(), arity: Integer(f.Arity())}, f.Arg, nil
	default:
		return procedureIndicator{}, nil, typeError(validTypeCallable, f, env)
	}
}

type wrongNumberOfArgumentsError struct {
	expected int
	actual   []Term
}

func (e *wrongNumberOfArgumentsError) Error() string {
	return fmt.Sprintf("wrong number of arguments: expected=%d, actual=%s", e.expected, e.actual)
}
