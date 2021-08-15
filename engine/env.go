package engine

import "errors"

// Env is an environment stack.
type Env []Binding

type Binding struct {
	Variable Variable
	Value    Term
	// attributes?
}

func (e Env) Lookup(v Variable) (Term, bool) {
	for i := len(e) - 1; i >= 0; i-- {
		b := e[i]
		if b.Variable == v {
			return b.Value, true
		}
	}
	return nil, false
}

// Resolve follows the variable chain and returns the first non-variable term or the last free variable.
func (e Env) Resolve(t Term) Term {
	var stop []Variable
	for t != nil {
		switch v := t.(type) {
		case Variable:
			for _, s := range stop {
				if v == s {
					return v
				}
			}
			ref, ok := e.Lookup(v)
			if !ok {
				return v
			}
			stop = append(stop, v)
			t = ref
		default:
			return v
		}
	}
	return nil
}

// Ground removes variables in term t.
func (e Env) Ground(t Term) (Term, error) {
	switch t := e.Resolve(t).(type) {
	case Variable:
		return nil, errors.New("can't ground")
	case *Compound:
		c := Compound{
			Functor: t.Functor,
			Args:    make([]Term, len(t.Args)),
		}
		for i := 0; i < len(c.Args); i++ {
			g, err := e.Ground(t.Args[i])
			if err != nil {
				return nil, err
			}
			c.Args[i] = g
		}
		return &c, nil
	default:
		return t, nil
	}
}

// FreeVariables extracts variables in the given terms.
func (e Env) FreeVariables(ts ...Term) []Variable {
	var fvs []Variable
	for _, t := range ts {
		fvs = e.appendFreeVariables(fvs, t)
	}
	return fvs
}

func (e Env) appendFreeVariables(fvs []Variable, t Term) []Variable {
	switch t := t.(type) {
	case Variable:
		if ref, ok := e.Lookup(t); ok {
			return e.appendFreeVariables(fvs, ref)
		}
		for _, v := range fvs {
			if v == t {
				return fvs
			}
		}
		return append(fvs, t)
	case *Compound:
		for _, arg := range t.Args {
			fvs = e.appendFreeVariables(fvs, arg)
		}
	}
	return fvs
}
