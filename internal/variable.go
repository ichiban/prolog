package internal

import (
	"fmt"
	"github.com/ichiban/prolog/internal/rbtree"
)

import (
	"errors"
)

var (
	ErrTooManyBindings = errors.New("too many bindings")
)

type Variable int32

func NewVariable(t *Heap) Variable {
	t.env.lastVariable++
	return t.env.lastVariable
}

func (v Variable) String() string {
	return fmt.Sprintf("_%d", v)
}

type Env struct {
	bindings     rbtree.Map[Variable, Term]
	lastVariable Variable
}

// Lookup returns a term that the given Variable is bound to.
func (e *Env) Lookup(v Variable) (Term, bool) {
	k := v
	if k%2 == 0 {
		k = -1 * k
	}

	id, ok := e.bindings.Get(k)
	if !ok {
		return 0, false
	}
	return id, true
}

// Bind adds a new entry to the environment.
func (e *Env) Bind(v Variable, t Term) error {
	k := v
	if k%2 == 0 {
		k = -1 * k
	}

	if !e.bindings.SafeSet(v, t) {
		return ErrTooManyBindings
	}

	return nil
}

// VariableSet is a set of variables. The key is the variable and the value is the number of occurrences.
// So if you look at the value it's a multi set of variable occurrences and if you ignore the value it's a set of occurrences (witness).
type VariableSet map[Variable]int

func newVariableSet(t *Heap, id Term) (VariableSet, error) {
	s := VariableSet{}
	for ids := []Term{id}; len(ids) > 0; ids, id = ids[:len(ids)-1], ids[len(ids)-1] {
		if v, ok := t.Variable(id); ok {
			s[v] += 1
			continue
		}
		if f, arg, ok := t.Compound(id); ok {
			for i := 0; i < f.Arity; i++ {
				a, err := arg(i)
				if err != nil {
					return nil, err
				}
				ids = append(ids, a)
			}
		}
	}
	return s, nil
}

func newExistentialVariablesSet(t *Heap, id Term) (VariableSet, error) {
	ev := VariableSet{}
	for ids := []Term{id}; len(ids) > 0; ids, id = ids[:len(ids)-1], ids[len(ids)-1] {
		if f, arg, ok := t.Compound(id); ok && f == (Functor{Name: Atom('^'), Arity: 2}) {
			l, err := arg(0)
			if err != nil {
				return nil, err
			}

			r, err := arg(1)
			if err != nil {
				return nil, err
			}

			s, err := newVariableSet(t, l)
			if err != nil {
				return nil, err
			}
			for v, o := range s {
				ev[v] = o
			}

			ids = append(ids, r)
		}
	}
	return ev, nil
}

func NewFreeVariablesSet(p *Heap, t, v Term) (VariableSet, error) {
	s, err := newVariableSet(p, t)
	if err != nil {
		return nil, err
	}

	bv, err := newVariableSet(p, v)
	if err != nil {
		return nil, err
	}

	ev, err := newExistentialVariablesSet(p, t)
	if err != nil {
		return nil, err
	}

	for v := range ev {
		bv[v] += 1
	}

	fv := make(VariableSet, len(s))
	for v, n := range s {
		if m, ok := bv[v]; !ok {
			fv[v] = n + m
		}
	}
	return fv, nil
}

func freeVariables(p Heap, t Term) ([]Variable, error) {
	return appendFreeVariables(p, nil, t)
}

func appendFreeVariables(p Heap, fvs []Variable, t Term) ([]Variable, error) {
	if t, ok := p.Variable(t); ok {
		for _, v := range fvs {
			if v == t {
				return fvs, nil
			}
		}
		return append(fvs, t), nil
	}

	if f, arg, ok := p.Compound(t); ok {
		for i := 0; i < f.Arity; i++ {
			a, err := arg(i)
			if err != nil {
				return nil, err
			}
			fvs, err = appendFreeVariables(p, fvs, a)
			if err != nil {
				return nil, err
			}
		}
	}

	return fvs, nil
}
