package engine

import (
	"context"
	"fmt"
	"io/fs"
	"strings"
)

// DiscontiguousError is an error that the user-defined predicate is defined by clauses which are not consecutive read-terms.
type DiscontiguousError struct {
	PI ProcedureIndicator
}

func (e *DiscontiguousError) Error() string {
	return fmt.Sprintf("%s is discontiguous", e.PI)
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

	if vm.procedures == nil {
		vm.procedures = map[ProcedureIndicator]procedure{}
	}
	for pi, u := range t.clauses {
		if existing, ok := vm.procedures[pi].(*userDefined); ok && existing.multifile && u.multifile {
			existing.clauses = append(existing.clauses, u.clauses...)
			continue
		}

		vm.procedures[pi] = u
	}

	for _, g := range t.goals {
		ok, err := Call(vm, g, Success, nil).Force(ctx)
		if err != nil {
			return err
		}
		if !ok {
			var sb strings.Builder
			_ = vm.Write(&sb, g, &WriteOptions{Quoted: true}, nil)
			return fmt.Errorf("failed initialization goal: %s", sb.String())
		}
	}

	return nil
}

func (vm *VM) compile(ctx context.Context, text *text, s string, args ...interface{}) error {
	if text.clauses == nil {
		text.clauses = map[ProcedureIndicator]*userDefined{}
	}

	s = ignoreShebangLine(s)
	p := vm.Parser(strings.NewReader(s), nil)
	if err := p.Replace(NewAtom("?"), args...); err != nil {
		return err
	}

	for p.More() {
		t, err := p.Term()
		if err != nil {
			return err
		}

		et, err := expand(vm, t, nil)
		if err != nil {
			return err
		}

		pi, arg, err := PI(et, nil)
		if err != nil {
			return err
		}
		switch pi {
		case ProcedureIndicator{Name: atomIf, Arity: 1}: // Directive
			if err := vm.directive(ctx, text, arg(0)); err != nil {
				return err
			}
			continue
		case ProcedureIndicator{Name: atomIf, Arity: 2}: // Rule
			pi, arg, err = PI(arg(0), nil)
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

	switch pi, arg, _ := PI(d, nil); pi {
	case ProcedureIndicator{Name: atomDynamic, Arity: 1}:
		return text.forEachUserDefined(arg(0), func(u *userDefined) {
			u.dynamic = true
			u.public = true
		})
	case ProcedureIndicator{Name: atomMultifile, Arity: 1}:
		return text.forEachUserDefined(arg(0), func(u *userDefined) {
			u.multifile = true
		})
	case ProcedureIndicator{Name: atomDiscontiguous, Arity: 1}:
		return text.forEachUserDefined(arg(0), func(u *userDefined) {
			u.discontiguous = true
		})
	case ProcedureIndicator{Name: atomInitialization, Arity: 1}:
		text.goals = append(text.goals, arg(0))
		return nil
	case ProcedureIndicator{Name: atomInclude, Arity: 1}:
		_, b, err := vm.open(arg(0), nil)
		if err != nil {
			return err
		}

		return vm.compile(ctx, text, string(b))
	case ProcedureIndicator{Name: atomEnsureLoaded, Arity: 1}:
		return vm.ensureLoaded(ctx, arg(0), nil)
	default:
		ok, err := Call(vm, d, Success, nil).Force(ctx)
		if err != nil {
			return err
		}
		if !ok {
			var sb strings.Builder
			_ = vm.Write(&sb, d, &WriteOptions{Quoted: true}, nil)
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
		return "", nil, ExistenceError(ObjectTypeSourceSink, file, env)
	default:
		return "", nil, TypeError(ValidTypeAtom, file, env)
	}
}

type text struct {
	buf     clauses
	clauses map[ProcedureIndicator]*userDefined
	goals   []Term
}

func (t *text) forEachUserDefined(pi Term, f func(u *userDefined)) error {
	iter := AnyIterator{Any: pi}
	for iter.Next() {
		elem := iter.Current()
		key, err := NewProcedureIndicator(elem, nil)
		if err != nil {
			return err
		}
		u, ok := t.clauses[key]
		if !ok {
			u = &userDefined{}
			t.clauses[key] = u
		}
		f(u)
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
		return &DiscontiguousError{PI: pi}
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
