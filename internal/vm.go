package internal

import (
	"context"
	"io"
	"io/fs"
	"path/filepath"
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

type loadResult struct {
	module   Atom
	loadedAt time.Time
}

// VM is the core of a Prolog interpreter. The zero value for VM is a valid VM without any builtin predicates.
type VM struct {
	// Unknown is a callback that is triggered when the VM reaches to an unknown predicate while current_prolog_flag(unknown, warning).
	Unknown func(name Atom, args []Term)

	// FS is a file system that is referenced when the VM loads Prolog texts e.g. ensure_loaded/1.
	// It has no effect on open/4 nor open/3 which always access the actual file system.
	FS     fs.FS
	loaded map[string]loadResult

	modules map[Atom]*Module
	system  Atom
	typeIn  Atom

	Terms Heap
	Stack Stack

	// I/O
	streams    StreamPool
	userInput  StreamID
	userOutput StreamID
}

// Module returns the Module.
func (vm *VM) Module(name string) *Module {
	return vm.module(NewAtom(name))
}

// TypeInModule returns the type-in Module.
func (vm *VM) TypeInModule() *Module {
	return vm.module(vm.typeIn)
}

func (vm *VM) module(name Atom) *Module {
	if vm.modules == nil {
		vm.modules = map[Atom]*Module{}
	}

	if m, ok := vm.modules[name]; ok {
		return m
	}

	m := NewModule()
	if vm.modules == nil {
		vm.modules = map[Atom]*Module{}
	}
	vm.modules[name] = m

	if s := vm.system; s != 0 {
		vm.importPredicates(name, vm.system, nil)
	}

	return m
}

// SetModule sets the type-in Module.
func (vm *VM) SetModule(name Atom) {
	vm.typeIn = name
}

func (vm *VM) SetSystemModule(name Atom) {
	vm.system = name
}

func (vm *VM) AssertA(t Term) error {
	m := vm.typeIn
	m, t = vm.Terms.Unqualify(m, t)
	return vm.assertMerge(m, t, func(existing []clause, new []clause) clauses {
		return append(new, existing...)
	})
}

func (vm *VM) AssertZ(t Term) error {
	m := vm.typeIn
	m, t = vm.Terms.Unqualify(m, t)
	return vm.assertMerge(m, t, func(existing []clause, new []clause) clauses {
		return append(existing, new...)
	})
}

func (vm *VM) assertMerge(module Atom, t Term, merge func([]clause, []clause) clauses) error {
	m := vm.module(module)

	pi, _, err := vm.Terms.MustBeCallable(t)
	if err != nil {
		return err
	}

	if m.procedures == nil {
		m.procedures = map[Functor]procedureEntry{}
	}
	e, ok := m.procedures[pi]
	if !ok {
		e.dynamic = true
		e.procedure = clauses{}
		m.procedures[pi] = e
	}

	added, err := vm.compileTerm(context.Background(), t)
	if err != nil {
		return err
	}

	cs, ok := e.procedure.(clauses)
	if !ok || !e.dynamic {
		c, err := vm.Terms.PutFunctor(pi)
		if err != nil {
			return err
		}
		return &PermissionError{Operation: NewAtom("modify"), PermissionType: NewAtom("static_procedure"), Culprit: c}
	}

	e.procedure = merge(cs, added)
	m.procedures[pi] = e
	return nil
}

// Cont is a continuation.
type Cont func(context.Context) Promise

func Success(context.Context) Promise {
	return Bool(true)
}

func (vm *VM) Call(ctx context.Context, goal Term, k Cont) (promise Promise) {
	defer ensurePromise(&promise)

	fvs, err := freeVariables(vm.Terms, goal)
	if err != nil {
		return Error(err)
	}

	args := make([]Term, len(fvs))
	for i, fv := range fvs {
		t, err := vm.Terms.PutVariable(fv)
		if err != nil {
			return Error(err)
		}
		args[i] = t
	}

	head, err := vm.Terms.PutCompound(Atom('$'), args...)
	if err != nil {
		return Error(err)
	}

	clause, err := vm.Terms.PutCompound(NewAtom(":-"), head, goal)
	if err != nil {
		return Error(err)
	}

	return Delay(func(yield func(thunk func() Promise)) {
		yield(func() Promise {
			cs, err := vm.compileTerm(ctx, clause)
			if err != nil {
				return Error(err)
			}

			return cs.call(ctx, vm, args, k)
		})
	})
}

func (vm *VM) CallN(ctx context.Context, closure Term, additional []Term) Promise {
	module, closure := vm.Terms.Unqualify(vm.typeIn, closure)
	pi, arg, ok := vm.Terms.Callable(closure)
	if !ok {
		return Error(&TypeError{Type: NewAtom("callable"), Culprit: closure})
	}
	args := make([]Term, pi.Arity+len(additional))
	for i := 0; i < pi.Arity; i++ {
		var err error
		args[i], err = arg(i)
		if err != nil {
			return Error(err)
		}
	}

	g, err := vm.Terms.PutCompound(pi.Name, args...)
	if err != nil {
		return Error(err)
	}

	m, err := vm.Terms.PutAtom(module)
	if err != nil {
		return Error(err)
	}

	g, err = vm.Terms.PutCompound(Atom(':'), m, g)
	if err != nil {
		return Error(err)
	}

	k := ContextCont(ctx)
	return vm.Call(ctx, g, k)
}

// Arrive is the entry point of the VM.
func (vm *VM) Arrive(ctx context.Context, module, name Atom, args []Term, k Cont) (promise Promise) {
	defer ensurePromise(&promise)

	if vm.Unknown == nil {
		vm.Unknown = func(Atom, []Term) {}
	}

	m := vm.module(module)
	pi := Functor{Name: name, Arity: len(args)}
	e := m.procedures[pi]
	if e.procedure == nil {
		switch m.unknown {
		case unknownWarning:
			vm.Unknown(name, args)
			fallthrough
		case unknownFail:
			return Bool(false)
		default:
			c, err := vm.Terms.PutFunctor(pi)
			if err != nil {
				return Error(err)
			}
			return Error(&ExistenceError{
				ObjectType: NewAtom("procedure"),
				Culprit:    c,
			})
		}
	}

	return e.procedure.call(ctx, vm, args, k)
}

func (vm *VM) exec(ctx context.Context, pc bytecode, vars []Term, cont Cont, args []Term, aStack [][]Term) Promise {
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
			var err error
			ok, err = vm.Terms.Unify(arg, operand)
			if err != nil {
				return Error(err)
			}
		case opPutConst:
			args = append(args, operand)
		case opGetVar:
			v := vars[operand]
			arg, args = args[0], args[1:]
			var err error
			ok, err = vm.Terms.Unify(arg, v)
			if err != nil {
				return Error(err)
			}
		case opPutVar:
			v := vars[operand]
			args = append(args, v)
		case opGetFunctor:
			pi := vm.Terms.functors[operand]
			arg, aStack = vm.Terms.Resolve(args[0]), append(aStack, args[1:])
			c, err := vm.Terms.PutCompoundWithVarArgs(pi)
			if err != nil {
				return Error(err)
			}
			ok, err = vm.Terms.Unify(arg, c)
			if err != nil {
				return Error(err)
			}
			args = vm.Terms.terms[c+1 : c+Term(pi.Arity)]
		case opPutFunctor:
			pi := vm.Terms.functors[operand]
			vs := make([]Term, pi.Arity)
			c, err := vm.Terms.PutCompound(pi.Name, vs...)
			if err != nil {
				return Error(err)
			}
			args = append(args, c)
			aStack = append(aStack, args)
			args = vm.Terms.terms[c+1:]
		case opPop:
			args, aStack = aStack[len(aStack)-1], aStack[:len(aStack)-1]
		case opEnter:
			break
		case opCall:
			pi := vm.Terms.functors[operand]
			return delayAsCutParent(func(yield func(thunk func() Promise)) {
				yield(func() Promise {
					return vm.Arrive(ctx, pi.Module, pi.Name, args, func(ctx context.Context) Promise {
						return vm.exec(ctx, pc, vars, cont, nil, nil)
					})
				})
			})
		case opExit:
			return cont(ctx)
		case opCut:
			return delayWithCut(func(yield func(thunk func() Promise)) {
				yield(func() Promise {
					return vm.exec(ctx, pc, vars, cont, args, aStack)
				})
			})
		case opGetList:
			arg, aStack = args[0], append(aStack, args[1:])
			args = make([]Term, operand)
			for i := range args {
				var err error
				args[i], err = vm.Terms.PutVariable(NewVariable(&vm.Terms))
				if err != nil {
					return Error(err)
				}
			}
			l, err := vm.Terms.PutList(args...)
			if err != nil {
				return Error(err)
			}
			ok, err = vm.Terms.Unify(arg, l)
			if err != nil {
				return Error(err)
			}
		case opPutList:
			vs := make([]Term, operand)
			l, err := vm.Terms.PutList(vs...)
			if err != nil {
				return Error(err)
			}
			arg = l
			args = append(args, arg)
			aStack = append(aStack, args)
			args = vm.Terms.terms[l:]
		case opGetPartial:
			arg, aStack = args[0], append(aStack, args[1:])
			args = make([]Term, int(operand+1))
			for i := range args {
				var err error
				args[i], err = vm.Terms.PutVariable(NewVariable(&vm.Terms))
				if err != nil {
					return Error(err)
				}
			}
			pl, err := vm.Terms.PutPartialList(args[0], args[1:]...)
			if err != nil {
				return Error(err)
			}
			ok, err = vm.Terms.Unify(arg, pl)
			if err != nil {
				return Error(err)
			}
		case opPutPartial:
			vs := make([]Term, int(operand+1))
			pl, err := vm.Terms.PutPartialList(vs[0], vs[1:]...)
			if err != nil {
				return Error(err)
			}
			args = append(args, pl)
			aStack = append(aStack, args)
			args = vm.Terms.terms[pl:]
		}
	}

	return Bool(false)
}

// SetUserInput sets the given stream as user_input.
func (vm *VM) SetUserInput(r io.Reader) error {
	id, err := vm.streams.PutInputStream(r, WithStreamAlias(NewAtom("user_input")))
	if err != nil {
		return err
	}
	vm.userInput = id
	return nil
}

// SetUserOutput sets the given stream as user_output.
func (vm *VM) SetUserOutput(w io.Writer) error {
	id, err := vm.streams.PutOutputStream(w, WithStreamAlias(NewAtom("user_output")))
	if err != nil {
		return err
	}
	vm.userOutput = id
	return nil
}

type ImportSpec struct {
	Name  string
	Arity int
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

// LoadFileOption specifies how the VM loads the file.
type LoadFileOption func(*LoadFileOptions)

// LoadFileOptionIf specifies the condition of loading the file.
func LoadFileOptionIf(cond LoadFileCondition) func(*LoadFileOptions) {
	return func(opts *LoadFileOptions) {
		opts.condition = cond
	}
}

// LoadFileOptionImports specifies which predicates the type-in Module imports from the loaded Module.
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

	fs := extFS{
		fs:         vm.FS,
		extensions: []string{"", ".pl"},
	}
	f, err := fs.Open(filename)
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

	vm.importPredicates(vm.typeIn, r.module, opts.importList)

	return nil
}

type LoadOptions struct {
	importList []ImportSpec
}

// LoadOption specifies how the VM loads the prolog text.
type LoadOption func(*LoadOptions)

// LoadOptionImports specifies which predicates the type-in Module imports from the loaded Module.
func LoadOptionImports(importList []ImportSpec) func(options *LoadOptions) {
	return func(opts *LoadOptions) {
		opts.importList = importList
	}
}

// LoadText loads a Prolog text.
func (vm *VM) LoadText(ctx context.Context, text string, options ...LoadOption) error {
	var opts LoadOptions
	for _, f := range options {
		f(&opts)
	}

	module, err := vm.Compile(ctx, text)
	if err != nil {
		return err
	}

	vm.importPredicates(vm.typeIn, module, opts.importList)

	return nil
}

func (vm *VM) importPredicates(to, from Atom, importList []ImportSpec) {
	dst, src := vm.module(to), vm.module(from)

	var pis []Functor
	if importList == nil {
		pis = make([]Functor, 0, len(src.procedures))
		for pi := range src.procedures {
			pis = append(pis, pi)
		}
	} else {
		pis = make([]Functor, len(importList))
		for i, spec := range importList {
			pis[i] = Functor{
				Name:  NewAtom(spec.Name),
				Arity: spec.Arity,
			}
		}
	}

	if dst.procedures == nil {
		dst.procedures = map[Functor]procedureEntry{}
	}
	for _, pi := range pis {
		orig := dst.procedures[pi]
		e := src.procedures[pi]
		e.importedFrom = from
		e.exported = orig.exported
		dst.procedures[pi] = e
	}
}

// extFS is a file system that fills in file extensions if omitted.
type extFS struct {
	fs         fs.FS
	extensions []string
}

var (
	_ fs.FS     = extFS{}
	_ fs.StatFS = extFS{}
)

func (e extFS) Open(name string) (fs.File, error) {
	if ext := filepath.Ext(name); ext != "" {
		return e.fs.Open(name)
	}
	for _, ext := range e.extensions {
		f, err := e.fs.Open(name + ext)
		if err != nil {
			continue
		}
		return f, nil
	}
	return nil, fs.ErrNotExist
}

func (e extFS) Stat(name string) (fs.FileInfo, error) {
	for _, ext := range e.extensions {
		fi, err := fs.Stat(e.fs, name+ext)
		if err != nil {
			continue
		}
		return fi, nil
	}
	return nil, fs.ErrNotExist
}
