package engine

// Env is an environment frame.
type Env struct {
	Up       *Env
	bindings []binding
}

func NewEnv(up *Env) *Env {
	return &Env{Up: up}
}

func (e *Env) Bind(v Variable, t Term) {
	for i, b := range e.bindings {
		if b.variable == v {
			e.bindings[i].value = t
			return
		}
	}
	e.bindings = append(e.bindings, binding{
		variable: v,
		value:    t,
	})
}

func (e *Env) Lookup(v Variable) (Term, bool) {
	for env := e; env != nil; env = env.Up {
		for _, b := range env.bindings {
			if b.variable == v {
				return b.value, true
			}
		}
	}
	return nil, false
}

// Resolve follows the variable chain and returns the first non-variable term or the last free variable.
func (e *Env) Resolve(t Term) Term {
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

// FreeVariables extracts variables in the given terms.
// TODO: do we really need this?
func (e *Env) FreeVariables(ts ...Term) []Variable {
	var fvs []Variable
	for _, t := range ts {
		fvs = e.appendFreeVariables(fvs, t)
	}
	return fvs
}

func (e *Env) appendFreeVariables(fvs []Variable, t Term) []Variable {
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

type binding struct {
	variable Variable
	value    Term
}
