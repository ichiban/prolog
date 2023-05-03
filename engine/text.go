package engine

import (
	"context"
	"fmt"
	"io/fs"
	"strings"
)

// discontiguousError is an error that the user-defined predicate is defined by clauses which are not consecutive read-terms.
type discontiguousError struct {
	pi procedureIndicator
}

func (e *discontiguousError) Error() string {
	return fmt.Sprintf("%s is discontiguous", e.pi)
}

// Compile compiles the Prolog text and updates the DB accordingly.
func (vm *VM) Compile(ctx context.Context, s string, args ...interface{}) error {
	t := text{module: atomUser}
	if err := vm.compile(ctx, &t, s, args...); err != nil {
		return err
	}

	if err := t.flush(); err != nil {
		return err
	}

	if vm.procedures == nil {
		vm.procedures = map[procedureIndicator]procedureEntry{}
	}
	for pi, e := range t.procs {
		pi.module = t.module
		if existing, ok := vm.procedures[pi]; ok {
			if ecs, ok := existing.procedure.(clauses); ok && existing.multifile {
				if cs, ok := e.procedure.(clauses); ok && e.multifile {
					existing.procedure = append(ecs, cs...)
					vm.procedures[pi] = existing
					continue
				}
			}
		}

		vm.procedures[pi] = e
	}

	env := NewEnv().bind(varContext, procedureIndicator{module: t.module, name: atomInitialization, arity: 1})
	for _, g := range t.goals {
		ok, err := Call(vm, g, Success, env).Force(ctx)
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

	return nil
}

func (vm *VM) resetModule(module Atom) {
	for pi := range vm.procedures {
		if pi.module != module {
			continue
		}
		delete(vm.procedures, pi)
	}
	delete(vm.unknown, module)
	for opKey := range vm.operators {
		if opKey.module != module {
			continue
		}
		delete(vm.operators, opKey)
	}
	for charConvKey := range vm.charConversions {
		if charConvKey.module != module {
			continue
		}
		delete(vm.charConversions, charConvKey)
	}
	delete(vm.charConvEnabled, module)
	delete(vm.doubleQuotes, module)
	delete(vm.debug, module)
}

func (vm *VM) importModule(dst, src Atom, limit []procedureIndicator) {
	target := func(pi procedureIndicator) bool {
		if limit == nil {
			return true
		}
		for _, e := range limit {
			if e == (procedureIndicator{name: pi.name, arity: pi.arity}) {
				return true
			}
		}
		return false
	}
	for pi, e := range vm.procedures {
		if pi.module != src || !e.exported || !target(pi) {
			continue
		}
		pi.module = dst
		e.importedFrom = src
		vm.procedures[pi] = e
	}
	for opKey, ops := range vm.operators {
		if opKey.module != src {
			continue
		}
		opKey.module = dst
		vm.operators[opKey] = ops
	}
}

// Consult executes Prolog texts in files.
func Consult(vm *VM, files Term, k Cont, env *Env) *Promise {
	var filenames []Term
	iter := ListIterator{List: files, Env: env}
	for iter.Next() {
		filenames = append(filenames, iter.Current())
	}
	if err := iter.Err(); err != nil {
		filenames = []Term{files}
	}

	return Delay(func(ctx context.Context) *Promise {
		for _, filename := range filenames {
			if err := vm.ensureLoaded(ctx, filename, env); err != nil {
				return Error(err)
			}
		}

		return k(env)
	})
}

func (vm *VM) compile(ctx context.Context, text *text, s string, args ...interface{}) error {
	if text.procs == nil {
		text.procs = map[procedureIndicator]procedureEntry{}
	}

	s = ignoreShebangLine(s)
	p := newParserModule(vm, &text.module, strings.NewReader(s))
	if err := p.SetPlaceholder(NewAtom("?"), args...); err != nil {
		return err
	}

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
			if err := vm.directive(ctx, text, arg(0)); err != nil {
				return err
			}
			continue
		case procedureIndicator{name: atomIf, arity: 2}: // Rule
			pi, arg, err = piArg(arg(0), nil)
			if err != nil {
				return err
			}
			fallthrough
		default:
			if len(text.buf) > 0 && pi != text.buf[0].pi {
				if err := text.flush(); err != nil {
					return err
				}
			}

			cs, err := compile(text.module, et, nil)
			if err != nil {
				return err
			}

			text.buf = append(text.buf, cs...)
		}
	}
	return nil
}

func (vm *VM) directive(ctx context.Context, text *text, d Term) error {
	if err := text.flush(); err != nil {
		return err
	}

	switch pi, arg, _ := piArg(d, nil); pi {
	case procedureIndicator{name: atomModule, arity: 2}:
		switch m := arg(0).(type) {
		case Variable:
			return InstantiationError(nil)
		case Atom:
			text.module = m
		default:
			return typeError(validTypeAtom, m, nil)
		}

		vm.resetModule(text.module)
		vm.importModule(text.module, atomSystem, nil)

		iter := ListIterator{List: arg(1)}
		for iter.Next() {
			pi, _, err := piArg(iter.Current(), nil)
			if err != nil {
				return err
			}
			e := text.procs[pi]
			e.exported = true
			text.procs[pi] = e
		}
		return iter.Err()
	case procedureIndicator{name: atomUseModule, arity: 3}:
		var module Atom
		switch m := arg(0).(type) {
		case Variable:
			return InstantiationError(nil)
		case Atom:
			module = m
		default:
			return typeError(validTypeAtom, m, nil)
		}
		vm.importModule(text.module, module, nil)
		return nil
	case procedureIndicator{name: atomDynamic, arity: 1}:
		return text.forEachProcedureEntry(arg(0), func(pi procedureIndicator, e procedureEntry) {
			e.dynamic = true
			e.public = true
			text.procs[pi] = e
		})
	case procedureIndicator{name: atomMultifile, arity: 1}:
		return text.forEachProcedureEntry(arg(0), func(pi procedureIndicator, e procedureEntry) {
			e.multifile = true
			text.procs[pi] = e
		})
	case procedureIndicator{name: atomDiscontiguous, arity: 1}:
		return text.forEachProcedureEntry(arg(0), func(pi procedureIndicator, e procedureEntry) {
			e.discontiguous = true
			text.procs[pi] = e
		})
	case procedureIndicator{name: atomInitialization, arity: 1}:
		text.goals = append(text.goals, arg(0))
		return nil
	case procedureIndicator{name: atomInclude, arity: 1}:
		_, b, err := vm.open(arg(0), nil)
		if err != nil {
			return err
		}

		return vm.compile(ctx, text, string(b))
	case procedureIndicator{name: atomEnsureLoaded, arity: 1}:
		return vm.ensureLoaded(ctx, arg(0), nil)
	default:
		module := text.module
		if module == 0 {
			module = atomUser
		}
		env := NewEnv().bind(varContext, procedureIndicator{module: module, name: atomIf, arity: 1})
		ok, err := Call(vm, d, Success, env).Force(ctx)
		if err != nil {
			return err
		}
		if !ok {
			var sb strings.Builder
			s := NewOutputTextStream(&sb)
			_, _ = WriteTerm(vm, s, d, List(atomQuoted.Apply(atomTrue)), Success, nil).Force(ctx)
			return fmt.Errorf("failed directive: %s", sb.String())
		}
		return nil
	}
}

func (vm *VM) ensureLoaded(ctx context.Context, file Term, env *Env) error {
	f, b, err := vm.open(file, env)
	if err != nil {
		return err
	}

	if vm.loaded == nil {
		vm.loaded = map[string]struct{}{}
	}
	if _, ok := vm.loaded[f]; ok {
		return nil
	}
	defer func() {
		vm.loaded[f] = struct{}{}
	}()

	return vm.Compile(ctx, string(b))
}

func (vm *VM) open(file Term, env *Env) (string, []byte, error) {
	switch f := env.Resolve(file).(type) {
	case Variable:
		return "", nil, InstantiationError(env)
	case Atom:
		s := f.String()
		for _, f := range []string{s, s + ".pl"} {
			b, err := fs.ReadFile(vm.FS, f)
			if err != nil {
				continue
			}

			return f, b, nil
		}
		return "", nil, existenceError(objectTypeSourceSink, file, env)
	default:
		return "", nil, typeError(validTypeAtom, file, env)
	}
}

type text struct {
	module Atom
	buf    clauses
	procs  map[procedureIndicator]procedureEntry
	goals  []Term
}

func (t *text) forEachProcedureEntry(pi Term, f func(pi procedureIndicator, e procedureEntry)) error {
	iter := anyIterator{Any: pi}
	for iter.Next() {
		switch pi := iter.Current().(type) {
		case Variable:
			return InstantiationError(nil)
		case Compound:
			if pi.Functor() != atomSlash || pi.Arity() != 2 {
				return typeError(validTypePredicateIndicator, pi, nil)
			}
			switch n := pi.Arg(0).(type) {
			case Variable:
				return InstantiationError(nil)
			case Atom:
				switch a := pi.Arg(1).(type) {
				case Variable:
					return InstantiationError(nil)
				case Integer:
					pi := procedureIndicator{name: n, arity: a}
					f(pi, t.procs[pi])
				default:
					return typeError(validTypePredicateIndicator, pi, nil)
				}
			default:
				return typeError(validTypePredicateIndicator, pi, nil)
			}
		default:
			return typeError(validTypePredicateIndicator, pi, nil)
		}
	}
	return iter.Err()
}

func (t *text) flush() error {
	if len(t.buf) == 0 {
		return nil
	}

	pi := t.buf[0].pi
	e := t.procs[pi]
	if e.procedure == nil {
		e.procedure = clauses{}
	}
	cs := e.procedure.(clauses)
	if len(cs) > 0 && !e.discontiguous {
		return &discontiguousError{pi: pi}
	}
	e.procedure = append(cs, t.buf...)
	t.procs[pi] = e
	t.buf = t.buf[:0]
	return nil
}

func ignoreShebangLine(query string) string {
	if !strings.HasPrefix(query, "#!") {
		return query
	}
	i := strings.Index(query, "\n")
	if i < 0 {
		i = len(query)
	}
	return query[i:]
}
