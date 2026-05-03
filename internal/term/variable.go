package term

import (
	"slices"
)

func VariableSet(t Handle) []Handle {
	// 7.1.1.1 Variable set of a term.
	t = t.Deref()
	if _, ok := t.Variable(); ok {
		return []Handle{t}
	}

	if _, ok := t.Functor(); !ok {
		return nil
	}

	var vs []Handle
	for arg := range t.Args() {
		vs = append(vs, VariableSet(arg)...)
	}
	slices.SortFunc(vs, Compare)
	return slices.Compact(vs)
}

func ExistentialVariableSet(t Handle) []Handle {
	// 7.1.1.3 Existential variables set of a term
	t = t.Deref()
	if f, ok := t.Functor(); !ok || f != NewFunctor(NewAtomRune('^'), 2) {
		return nil
	}
	v, g := t.Arg(0), t.Arg(1)
	evs := VariableSet(v)
	evs = append(evs, ExistentialVariableSet(g)...)
	slices.SortFunc(evs, Compare)
	return slices.Compact(evs)
}

func FreeVariableSet(t, v Handle) []Handle {
	// 7.1.1.4 Free variables set of a term
	vs := VariableSet(t)
	bv := VariableSet(v)
	bv = append(bv, ExistentialVariableSet(t)...)
	slices.SortFunc(bv, Compare)
	bv = slices.Compact(bv)
	return slices.DeleteFunc(vs, func(v Handle) bool {
		return slices.Contains(bv, v)
	})
}
