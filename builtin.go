package prolog

import (
	"errors"
	"fmt"
	"sort"
)

func Unify(t1, t2 Term) (bool, error) {
	return t1.Unify(t2), nil
}

func Functor(term, name, arity Term) (bool, error) {
	var v *Variable
	for v == nil {
		switch t := term.(type) {
		case Atom:
			return t.Unify(name) && Integer(0).Unify(arity), nil
		case *Variable:
			if t.Ref == nil {
				v = t
				break
			}
			term = t.Ref
		case *Compound:
			return t.Functor.Unify(name) && Integer(len(t.Args)).Unify(arity), nil
		default:
			return false, nil
		}
	}

	var n *Atom
	for n == nil {
		switch t := name.(type) {
		case Atom:
			n = &t
		case *Variable:
			if t.Ref == nil {
				return false, errors.New("invalid arguments: atom is not instantiated")
			}
			name = t.Ref
		default:
			return false, fmt.Errorf("invalid arguments: name is %T", name)
		}
	}

	var a *Integer
	for a == nil {
		switch t := arity.(type) {
		case Integer:
			a = &t
		case *Variable:
			if t.Ref == nil {
				return false, errors.New("invalid arguments")
			}
			arity = t.Ref
		default:
			return false, errors.New("invalid arguments")
		}
	}

	if *a == 0 {
		return v.Unify(*a), nil
	}

	vars := make([]Term, *a)
	for i := range vars {
		var v Variable
		vars[i] = &v
	}

	return v.Unify(&Compound{
		Functor: *n,
		Args:    vars,
	}), nil
}

func Univ(term, list Term) (bool, error) {
	var c *Compound
	for c == nil {
		switch t := term.(type) {
		case *Variable:
			if t.Ref == nil {
				var car, cdr Variable
				if !list.Unify(Cons(&car, &cdr)) {
					return false, errors.New("invalid argument")
				}
				var f *Atom
				for f == nil {
					switch t := car.Ref.(type) {
					case Atom:
						f = &t
					case *Variable:
						if t.Ref == nil {
							return false, errors.New("invalid argument")
						}
						car.Ref = t.Ref
					default:
						return false, errors.New("invalid argument")
					}
				}

				list = cdr.Ref

				var args []Term
				for list != Atom("[]") {
					var car, cdr Variable
					if !list.Unify(Cons(&car, &cdr)) {
						return false, errors.New("invalid argument")
					}
					args = append(args, car.Ref)
					list = cdr.Ref
				}

				return term.Unify(&Compound{
					Functor: *f,
					Args:    args,
				}), nil
			}
			term = t.Ref
		case *Compound:
			c = t
		default:
			return false, errors.New("invalid argument")
		}
	}

	l := Term(Atom("[]"))
	for i := len(c.Args) - 1; i >= 0; i-- {
		l = Cons(c.Args[i], l)
	}
	return list.Unify(Cons(c.Functor, l)), nil
}

func (e *Engine) Op(precedence, typ, name Term) (bool, error) {
	p, ok := Resolve(precedence).(Integer)
	if !ok {
		return false, fmt.Errorf("invalid precedence: %s", precedence)
	}

	t, ok := Resolve(typ).(Atom)
	if !ok {
		return false, fmt.Errorf("invalid type: %s", typ)
	}

	n, ok := Resolve(name).(Atom)
	if !ok {
		return false, fmt.Errorf("invalid name: %s", name)
	}

	// already defined?
	for i, o := range e.operators {
		if o.Type != t || o.Name != n {
			continue
		}

		// remove it first so that we can insert it again in the right position
		copy(e.operators[i:], e.operators[i+1:])
		e.operators[len(e.operators)-1] = operator{}
		e.operators = e.operators[:len(e.operators)-1]

		// or keep it removed.
		if p == 0 {
			return true, nil
		}
	}

	// insert
	i := sort.Search(len(e.operators), func(i int) bool {
		return e.operators[i].Precedence >= p
	})
	e.operators = append(e.operators, operator{})
	copy(e.operators[i+1:], e.operators[i:])
	e.operators[i] = operator{
		Precedence: p,
		Type:       t,
		Name:       n,
	}

	return true, nil
}

func (e *Engine) CurrentOp(precedence, typ, name Term) (bool, error) {
	for _, op := range e.operators {
		if op.Precedence.Unify(precedence) && op.Type.Unify(typ) && op.Name.Unify(name) {
			return true, nil
		}
	}
	return false, nil
}

func (e *Engine) Assertz(t Term) (bool, error) {
	t = Resolve(t)
	var name string
	switch t := t.(type) {
	case Atom:
		name = fmt.Sprintf("%s/0", t)
	case *Compound:
		type pf struct {
			functor Atom
			arity   int
		}
		switch (pf{functor: t.Functor, arity: len(t.Args)}) {
		case pf{functor: ":-", arity: 2}:
			switch h := t.Args[0].(type) {
			case Atom:
				name = fmt.Sprintf("%s/0", h)
			case *Compound:
				name = fmt.Sprintf("%s/%d", h.Functor, len(h.Args))
			default:
				return false, fmt.Errorf("not a clause: %s", t.Args[0])
			}
		case pf{functor: ":-", arity: 1}: // directive
			return e.call(t.Args[0])
		default:
			name = fmt.Sprintf("%s/%d", t.Functor, len(t.Args))
		}
	default:
		return false, fmt.Errorf("not a clause: %s", t)
	}

	p, ok := e.procedures[name]
	if !ok {
		p = clauses{}
	}

	cs, ok := p.(clauses)
	if !ok {
		return false, errors.New("builtin")
	}
	var c clause
	if err := c.compile(t); err != nil {
		return false, err
	}

	e.procedures[name] = append(cs, c)
	return true, nil
}
