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
	var t text
	if err := vm.compile(ctx, &t, s, args...); err != nil {
		return err
	}

	if err := t.flush(); err != nil {
		return err
	}

	m := vm.Module()
	if m.procedures == nil {
		m.procedures = map[procedureIndicator]procedure{}
	}
	for pi, u := range t.clauses {
		if existing, ok := m.procedures[pi].(*userDefined); ok && existing.multifile && u.multifile {
			existing.clauses = append(existing.clauses, u.clauses...)
			continue
		}

		m.procedures[pi] = u
	}

	for _, g := range t.goals {
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

	return nil
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
	if text.clauses == nil {
		text.clauses = map[procedureIndicator]*userDefined{}
	}

	s = ignoreShebangLine(s)
	p := NewParser(vm, strings.NewReader(s))
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

			cs, err := compile(et, nil)
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
	case procedureIndicator{name: atomDynamic, arity: 1}:
		return text.forEachUserDefined(arg(0), func(u *userDefined) {
			u.dynamic = true
			u.public = true
		})
	case procedureIndicator{name: atomMultifile, arity: 1}:
		return text.forEachUserDefined(arg(0), func(u *userDefined) {
			u.multifile = true
		})
	case procedureIndicator{name: atomDiscontiguous, arity: 1}:
		return text.forEachUserDefined(arg(0), func(u *userDefined) {
			u.discontiguous = true
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
		ok, err := Call(vm, d, Success, nil).Force(ctx)
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
	buf     clauses
	clauses map[procedureIndicator]*userDefined
	goals   []Term
}

func (t *text) forEachUserDefined(pi Term, f func(u *userDefined)) error {
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
					u, ok := t.clauses[pi]
					if !ok {
						u = &userDefined{}
						t.clauses[pi] = u
					}
					f(u)
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
	u, ok := t.clauses[pi]
	if !ok {
		u = &userDefined{}
		t.clauses[pi] = u
	}
	if len(u.clauses) > 0 && !u.discontiguous {
		return &discontiguousError{pi: pi}
	}
	u.clauses = append(u.clauses, t.buf...)
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
