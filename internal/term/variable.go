package term

import (
	"iter"
	"slices"
)

// VariableName is a mapping from a variable term to its name or vice versa.
// After reading, a variable term looses its textual representation.
// You can use VariableName to keep track of and restore it on writing.
type VariableName struct {
	Variable Cell
	Name     string
	Count    int
}

func (a *Arena) VariableSet(t Cell) []Cell {
	// 7.1.1.1 Variable set of a term.
	t = a.Deref(t)
	if _, ok := a.Variable(t); ok {
		return []Cell{t}
	}

	if _, ok := a.Functor(t); !ok {
		return nil
	}

	var vs []Cell
	for arg := range a.Args(t) {
		vs = append(vs, a.VariableSet(arg)...)
	}
	slices.SortFunc(vs, a.Compare)
	return slices.Compact(vs)
}

func (a *Arena) ExistentialVariableSet(t Cell) []Cell {
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

func (a *Arena) FreeVariableSet(t, v Cell) []Cell {
	// 7.1.1.4 Free variables set of a term
	vs := a.VariableSet(t)
	bv := a.VariableSet(v)
	bv = append(bv, a.ExistentialVariableSet(t)...)
	slices.SortFunc(bv, a.Compare)
	bv = slices.Compact(bv)
	return slices.DeleteFunc(vs, func(v Cell) bool {
		return slices.Contains(bv, v)
	})
}

func (a *Arena) WitnessVariables(t Cell) iter.Seq[Cell] {
	// 7.1.1.5 Witness variable list of a term
	return a.witnessVariables(t, map[Cell]struct{}{})
}

func (a *Arena) witnessVariables(t Cell, witness map[Cell]struct{}) iter.Seq[Cell] {
	return func(yield func(Cell) bool) {
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

func (a *Arena) Variant(t1, t2 Cell) bool {
	s := map[Cell]Cell{}
	rest := [][2]Cell{
		{t1, t2},
	}
	var xy [2]Cell
	for len(rest) > 0 {
		rest, xy = rest[:len(rest)-1], rest[len(rest)-1]
		x, y := a.Deref(xy[0]), a.Deref(xy[1])
		if _, ok := a.Variable(x); ok {
			if _, ok := a.Variable(y); !ok {
				return false
			}
			if z, ok := s[x]; ok {
				if z != y {
					return false
				}
			} else {
				s[x] = y
			}
			continue
		}
		if fx, ok := a.Functor(x); ok {
			fy, ok := a.Functor(y)
			if !ok {
				return false
			}
			if fx != fy {
				return false
			}
			for i := 0; i < fx.Arity(); i++ {
				rest = append(rest, [2]Cell{a.Arg(x, i), a.Arg(y, i)})
			}
			continue
		}
		if x != y {
			return false
		}
	}
	return true
}
