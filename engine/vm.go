package engine

import (
	"context"
	"fmt"
	"io"
	"io/fs"
	"strings"
	"time"
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

type loadResult struct {
	module   Atom
	loadedAt time.Time
}

// VM is the core of a Prolog interpreter. The zero value for VM is a valid VM without any builtin predicates.
type VM struct {
	// Unknown is a callback that is triggered when the VM reaches to an unknown predicate while current_prolog_flag(unknown, warning).
	Unknown func(name Atom, args []Term, env *Env)

	// FS is a file system that is referenced when the VM loads Prolog texts e.g. ensure_loaded/1.
	// It has no effect on open/4 nor open/3 which always access the actual file system.
	FS     fs.FS
	loaded map[string]loadResult

	modules map[Atom]*module
	system  Atom
	typeIn  Atom

	// I/O
	streams       streams
	input, output *Stream
}

// Module returns the module.
func (vm *VM) Module(name string) *module {
	return vm.module(NewAtom(name))
}

// TypeInModule returns the type-in module.
func (vm *VM) TypeInModule() *module {
	return vm.module(vm.typeIn)
}

func (vm *VM) module(name Atom) *module {
	if vm.modules == nil {
		vm.modules = map[Atom]*module{}
	}

	if m, ok := vm.modules[name]; ok {
		return m
	}

	m := newModule()
	if vm.modules == nil {
		vm.modules = map[Atom]*module{}
	}
	vm.modules[name] = m

	if s := vm.system; s != 0 {
		vm.importPredicates(name, vm.system, nil)
	}

	return m
}

// SetModule sets the type-in module.
func (vm *VM) SetModule(name Atom) {
	vm.typeIn = name
}

func (vm *VM) SetSystemModule(name Atom) {
	vm.system = name
}

// Cont is a continuation.
type Cont func(*Env) *Promise

// Arrive is the entry point of the VM.
func (vm *VM) Arrive(module, name Atom, args []Term, k Cont, env *Env) (promise *Promise) {
	defer ensurePromise(&promise)

	if vm.Unknown == nil {
		vm.Unknown = func(Atom, []Term, *Env) {}
	}

	m := vm.module(module)
	pi := predicateIndicator{name: name, arity: Integer(len(args))}
	e := m.procedures[pi]
	if e.procedure == nil {
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
			pi := operand.(predicateIndicator)
			arg, astack = env.Resolve(args[0]), append(astack, args[1:])
			args = make([]Term, int(pi.arity))
			for i := range args {
				args[i] = NewVariable()
			}
			env, ok = env.Unify(arg, pi.name.Apply(args...))
		case opPutFunctor:
			pi := operand.(predicateIndicator)
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
			pi := operand.(qualifiedPredicateIndicator)
			return vm.Arrive(pi.module, pi.name, args, func(env *Env) *Promise {
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

type LoadFileOptions struct {
	condition  LoadFileCondition
	importList []ImportSpec
}

// LoadFileCondition denotes a condition to load a file.
type LoadFileCondition int

const (
	// LoadFileConditionTrue is a condition which always loads a file.
	LoadFileConditionTrue LoadFileCondition = iota
	// LoadFileConditionChanged is a condition which loads a file if it's not loaded yet or the timestamp is updated.
	LoadFileConditionChanged
)

type ImportSpec struct {
	Name  string
	Arity int
}

// LoadFileOption specifies how the VM loads the file.
type LoadFileOption func(*LoadFileOptions)

// LoadFileOptionIf specifies the condition of loading the file.
func LoadFileOptionIf(cond LoadFileCondition) func(*LoadFileOptions) {
	return func(opts *LoadFileOptions) {
		opts.condition = cond
	}
}

// LoadFileOptionImports specifies which predicates the type-in module imports from the loaded module.
func LoadFileOptionImports(importList []ImportSpec) func(options *LoadFileOptions) {
	return func(opts *LoadFileOptions) {
		opts.importList = importList
	}
}

// LoadFile loads a Prolog text from a file specified by filename.
func (vm *VM) LoadFile(ctx context.Context, filename string, options ...LoadFileOption) error {
	var opts LoadFileOptions
	for _, f := range options {
		f(&opts)
	}

	f, err := vm.FS.Open(filename)
	if err != nil {
		return err
	}
	defer func() {
		_ = f.Close()
	}()

	fi, err := f.Stat()
	if err != nil {
		return err
	}

	r, ok := vm.loaded[filename]
	loading := !ok // If it's not loaded yet
	loading = loading || opts.condition == LoadFileConditionTrue
	loading = loading || opts.condition == LoadFileConditionChanged && fi.ModTime().After(r.loadedAt)
	loading = loading || fi.ModTime().IsZero() // If the fs.FS doesn't report mod times
	if loading {
		b, err := io.ReadAll(f)
		if err != nil {
			return err
		}

		module, err := vm.Compile(ctx, string(b))
		if err != nil {
			return err
		}

		r.module = module
		r.loadedAt = time.Now()
	}

	if opts.importList == nil {
		vm.importPredicates(vm.typeIn, r.module, nil)
	} else {
		pis := make([]predicateIndicator, len(opts.importList))
		for i, spec := range opts.importList {
			pis[i] = predicateIndicator{
				name:  NewAtom(spec.Name),
				arity: Integer(spec.Arity),
			}
		}
		vm.importPredicates(vm.typeIn, r.module, pis)
	}

	return nil
}

func (vm *VM) importPredicates(to, from Atom, pis []predicateIndicator) {
	dst, src := vm.module(to), vm.module(from)
	if pis == nil {
		pis = make([]predicateIndicator, 0, len(src.procedures))
		for pi := range src.procedures {
			pis = append(pis, pi)
		}
	}
	if dst.procedures == nil {
		dst.procedures = map[predicateIndicator]procedureEntry{}
	}
	for _, pi := range pis {
		orig := dst.procedures[pi]
		e := src.procedures[pi]
		e.importedFrom = from
		e.exported = orig.exported
		dst.procedures[pi] = e
	}
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

type qualifiedPredicateIndicator struct {
	module Atom
	predicateIndicator
}

// predicateIndicator identifies a predicate e.g. (=)/2.
type predicateIndicator struct {
	name  Atom
	arity Integer
}

func (p predicateIndicator) WriteTerm(w io.Writer, opts *WriteOptions, env *Env) error {
	return WriteCompound(w, p, opts, env)
}

func (p predicateIndicator) Compare(t Term, env *Env) int {
	return CompareCompound(p, t, env)
}

func (p predicateIndicator) Functor() Atom {
	return atomSlash
}

func (p predicateIndicator) Arity() int {
	return 2
}

func (p predicateIndicator) Arg(n int) Term {
	if n == 0 {
		return p.name
	}
	return p.arity
}

func (p predicateIndicator) String() string {
	var sb strings.Builder
	_ = p.name.WriteTerm(&sb, &WriteOptions{
		quoted: true,
	}, nil)
	_, _ = fmt.Fprintf(&sb, "/%d", p.arity)
	return sb.String()
}

// Term returns p as term.
func (p predicateIndicator) Term() Term {
	return atomSlash.Apply(p.name, p.arity)
}

// Apply applies p to args.
func (p predicateIndicator) Apply(args ...Term) (Term, error) {
	if p.arity != Integer(len(args)) {
		return nil, &wrongNumberOfArgumentsError{expected: int(p.arity), actual: args}
	}
	return p.name.Apply(args...), nil
}

func piArg(t Term, env *Env) (predicateIndicator, func(int) Term, error) {
	switch f := env.Resolve(t).(type) {
	case Variable:
		return predicateIndicator{}, nil, InstantiationError(env)
	case Atom:
		return predicateIndicator{name: f, arity: 0}, nil, nil
	case Compound:
		return predicateIndicator{name: f.Functor(), arity: Integer(f.Arity())}, f.Arg, nil
	default:
		return predicateIndicator{}, nil, typeError(validTypeCallable, f, env)
	}
}

type wrongNumberOfArgumentsError struct {
	expected int
	actual   []Term
}

func (e *wrongNumberOfArgumentsError) Error() string {
	return fmt.Sprintf("wrong number of arguments: expected=%d, actual=%s", e.expected, e.actual)
}
