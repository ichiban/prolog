package term

import (
	"slices"
)

func (a *Arena) VariableSet(t Handle) []Handle {
	// 7.1.1.1 Variable set of a term.
	t = a.Deref(t)
	if _, ok := a.Variable(t); ok {
		return []Handle{t}
	}

	if _, ok := a.Functor(t); !ok {
		return nil
	}

	var vs []Handle
	for arg := range a.Args(t) {
		vs = append(vs, a.VariableSet(arg)...)
	}
	slices.SortFunc(vs, a.Compare)
	return slices.Compact(vs)
}

func (a *Arena) ExistentialVariableSet(t Handle) []Handle {
	// 7.1.1.3 Existential variables set of a term
	t = a.Deref(t)
	if f, ok := a.Functor(t); !ok || f != NewFunctor(NewAtomRune('^'), 2) {
		return nil
	}
	v, g := a.Arg(t, 0), a.Arg(t, 1)
	evs := a.VariableSet(v)
	evs = append(evs, a.ExistentialVariableSet(g)...)
	slices.SortFunc(evs, a.Compare)
	return slices.Compact(evs)
}

func (a *Arena) FreeVariableSet(t, v Handle) []Handle {
	// 7.1.1.4 Free variables set of a term
	vs := a.VariableSet(t)
	bv := a.VariableSet(v)
	bv = append(bv, a.ExistentialVariableSet(t)...)
	slices.SortFunc(bv, a.Compare)
	bv = slices.Compact(bv)
	return slices.DeleteFunc(vs, func(v Handle) bool {
		return slices.Contains(bv, v)
	})
}
