package term

import (
	"iter"
	"slices"
)

// VariableName is a mapping from a variable term to its name or vice versa.
// After reading, a variable term looses its textual representation.
// You can use VariableName to keep track of and restore it on writing.
type VariableName struct {
	Variable Handle
	Name     string
	Count    int
}

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

func (a *Arena) WitnessVariables(t Handle) iter.Seq[Handle] {
	// 7.1.1.5 Witness variable list of a term
	return a.witnessVariables(t, map[Handle]struct{}{})
}

func (a *Arena) witnessVariables(t Handle, witness map[Handle]struct{}) iter.Seq[Handle] {
	return func(yield func(Handle) bool) {
		t = a.Deref(t)
		if _, ok := a.Variable(t); ok {
			if _, ok := witness[t]; !ok {
				witness[t] = struct{}{}
				_ = yield(t)
			}
			return
		}

		for arg := range a.Args(t) {
			for w := range a.witnessVariables(arg, witness) {
				if !yield(w) {
					return
				}
			}
		}
	}
}
