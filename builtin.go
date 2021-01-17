package prolog

import (
	"errors"
	"fmt"
	"sort"
	"strings"
)

func (e *Engine) Call(t Term, k func() (bool, error)) (bool, error) {
	name, args, err := nameArgs(t)
	if err != nil {
		return false, err
	}
	return e.arrive(name, args, k)
}

var errCut = errors.New("cut")

func Cut(k func() (bool, error)) (bool, error) {
	ok, err := k()
	if err != nil {
		if errors.Is(err, errCut) {
			return ok, err
		}
		return false, err
	}
	return ok, errCut
}

func Unify(t1, t2 Term, k func() (bool, error)) (bool, error) {
	a := newAssignment(t1, t2)
	if !t1.Unify(t2, false) {
		a.reset()
		return false, nil
	}
	return k()
}

func UnifyWithOccursCheck(t1, t2 Term, k func() (bool, error)) (bool, error) {
	a := newAssignment(t1, t2)
	if !t1.Unify(t2, true) {
		a.reset()
		return false, nil
	}
	return k()
}

func TypeVar(t Term, k func() (bool, error)) (bool, error) {
	if _, ok := Resolve(t).(*Variable); !ok {
		return false, nil
	}
	return k()
}

func TypeFloat(t Term, k func() (bool, error)) (bool, error) {
	if _, ok := Resolve(t).(Float); !ok {
		return false, nil
	}
	return k()
}

func TypeInteger(t Term, k func() (bool, error)) (bool, error) {
	if _, ok := Resolve(t).(Integer); !ok {
		return false, nil
	}
	return k()
}

func TypeAtom(t Term, k func() (bool, error)) (bool, error) {
	if _, ok := Resolve(t).(Atom); !ok {
		return false, nil
	}
	return k()
}

func TypeCompound(t Term, k func() (bool, error)) (bool, error) {
	if _, ok := Resolve(t).(*Compound); !ok {
		return false, nil
	}
	return k()
}

func Functor(term, name, arity Term, k func() (bool, error)) (bool, error) {
	var v *Variable
	for v == nil {
		switch t := term.(type) {
		case Atom:
			if !t.Unify(name, false) || !Integer(0).Unify(arity, false) {
				return false, nil
			}
			return k()
		case *Variable:
			if t.Ref == nil {
				v = t
				break
			}
			term = t.Ref
		case *Compound:
			if !t.Functor.Unify(name, false) || !Integer(len(t.Args)).Unify(arity, false) {
				return false, nil
			}
			return k()
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
		if !v.Unify(*a, false) {
			return false, nil
		}
		return k()
	}

	vars := make([]Term, *a)
	for i := range vars {
		var v Variable
		vars[i] = &v
	}

	if !v.Unify(&Compound{
		Functor: *n,
		Args:    vars,
	}, false) {
		return false, nil
	}
	return k()
}

func Arg(arg, term, value Term, k func() (bool, error)) (bool, error) {
	c, ok := Resolve(term).(*Compound)
	if !ok {
		return false, errors.New("term should be compound")
	}

	n, ok := Resolve(arg).(Integer)
	if !ok {
		a := newAssignment(arg, term, value)
		for i, t := range c.Args {
			if arg.Unify(Integer(i+1), false) && value.Unify(t, false) {
				ok, err := k()
				if err != nil {
					if errors.Is(err, errCut) {
						return ok, err
					}
					return false, err
				}
				if ok {
					return true, nil
				}
			}
			a.reset()
		}
		return false, nil
	}
	if n == 0 || int(n) >= len(c.Args) {
		return false, nil
	}
	if n < 0 {
		return false, errors.New("arg shouldn't be negative")
	}

	if !c.Args[int(n)-1].Unify(value, false) {
		return false, nil
	}

	return k()
}

func Univ(term, list Term, k func() (bool, error)) (bool, error) {
	var c *Compound
	for c == nil {
		switch t := term.(type) {
		case *Variable:
			if t.Ref == nil {
				var car, cdr Variable
				if !list.Unify(Cons(&car, &cdr), false) {
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
					if !list.Unify(Cons(&car, &cdr), false) {
						return false, errors.New("invalid argument")
					}
					args = append(args, car.Ref)
					list = cdr.Ref
				}

				if !term.Unify(&Compound{
					Functor: *f,
					Args:    args,
				}, false) {
					return false, nil
				}
				return k()
			}
			term = t.Ref
		case *Compound:
			c = t
		default:
			return false, errors.New("invalid argument")
		}
	}

	l := List()
	for i := len(c.Args) - 1; i >= 0; i-- {
		l = Cons(c.Args[i], l)
	}
	if !list.Unify(Cons(c.Functor, l), false) {
		return false, nil
	}
	return k()
}

func CopyTerm(in, out Term, k func() (bool, error)) (bool, error) {
	return Unify(in.Copy(), out, k)
}

func (e *Engine) Op(precedence, typ, name Term, k func() (bool, error)) (bool, error) {
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
			return k()
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

	return k()
}

func (e *Engine) CurrentOp(precedence, typ, name Term, k func() (bool, error)) (bool, error) {
	a := newAssignment(precedence, typ, name)

	for _, op := range e.operators {
		if op.Precedence.Unify(precedence, false) && op.Type.Unify(typ, false) && op.Name.Unify(name, false) {
			ok, err := k()
			if err != nil {
				if errors.Is(err, errCut) {
					return ok, err
				}
				return false, err
			}
			if ok {
				return true, nil
			}
		}
		a.reset()
	}

	return false, nil
}

func (e *Engine) Assertz(t Term, k func() (bool, error)) (bool, error) {
	return e.assert(t, k, func(cs clauses, c clause) clauses {
		return append(cs, c)
	})
}

func (e *Engine) Asserta(t Term, k func() (bool, error)) (bool, error) {
	return e.assert(t, k, func(cs clauses, c clause) clauses {
		return append(clauses{c}, cs...)
	})
}

func (e *Engine) assert(t Term, k func() (bool, error), merge func(clauses, clause) clauses) (bool, error) {
	name, args, err := nameArgs(t)
	if err != nil {
		return false, err
	}

	switch name {
	case "(:-)/1": // directive
		var d Variable
		args.Unify(Cons(&d, &Variable{}), false)
		name, args, err := nameArgs(&d)
		if err != nil {
			return false, err
		}
		ok, err := e.arrive(name, args, k)
		if err != nil {
			return false, err
		}
		return ok, nil
	case "(:-)/2":
		var h Variable
		args.Unify(Cons(&h, &Variable{}), false)
		name, _, err = nameArgs(&h)
		if err != nil {
			return false, err
		}
	}

	if e.procedures == nil {
		e.procedures = map[string]procedure{}
	}
	p, ok := e.procedures[name]
	if !ok {
		p = clauses{}
	}

	cs, ok := p.(clauses)
	if !ok {
		return false, errors.New("builtin")
	}
	c := clause{
		name: name,
	}
	if err := c.compile(t); err != nil {
		return false, err
	}

	e.procedures[name] = merge(cs, c)
	return k()
}

func nameArgs(t Term) (string, Term, error) {
	switch f := Resolve(t).(type) {
	case Atom:
		return PrincipalFunctor(f, 0).String(), List(), nil
	case *Compound:
		return PrincipalFunctor(f.Functor, Integer(len(f.Args))).String(), List(f.Args...), nil
	default:
		return "", nil, errors.New("not callable")
	}
}

func Repeat(k func() (bool, error)) (bool, error) {
	for {
		ok, err := k()
		if err != nil {
			if errors.Is(err, errCut) {
				return ok, err
			}
			return false, err
		}
		if ok {
			return true, nil
		}
	}
}

func (e *Engine) BagOf(template, goal, bag Term, k func() (bool, error)) (bool, error) {
	var qualifier, body Variable
	if goal.Unify(&Compound{
		Functor: "^",
		Args:    []Term{&qualifier, &body},
	}, false) {
		goal = body.Ref
	}

	a := newAssignment(goal)

	freeVariables := newAssignment(template, &qualifier)
	groupingVariables := make(assignment, 0, len(a))
	for _, v := range a {
		if freeVariables.contains(v) {
			continue
		}
		groupingVariables = append(groupingVariables, v)
	}

	type solution struct {
		snapshots []Term // snapshot of grouping variable values
		bag       []Term
	}

	var solutions []solution
	_, err := e.Call(goal, func() (bool, error) {
		snapshots := make([]Term, len(groupingVariables))
		for i, v := range groupingVariables {
			snapshots[i] = v.Ref
		}

	solutions:
		for i, s := range solutions {
			for i := range groupingVariables {
				ok, err := Compare(Atom("="), s.snapshots[i], snapshots[i], done)
				if err != nil {
					return false, err
				}
				if !ok {
					continue solutions
				}
			}
			solutions[i].bag = append(s.bag, template.Copy())
			return false, nil // ask for more solutions
		}

		solutions = append(solutions, solution{
			snapshots: snapshots,
			bag:       []Term{template.Copy()},
		})
		return false, nil // ask for more solutions
	})
	if err != nil {
		return false, err
	}

	if len(solutions) == 0 {
		return false, nil
	}

	b := newAssignment(bag)
	for _, s := range solutions {
		// revert to snapshot
		for i, v := range groupingVariables {
			v.Ref = s.snapshots[i]
		}

		if bag.Unify(List(s.bag...), false) {
			ok, err := k()
			if err != nil {
				return false, err
			}
			if ok {
				return ok, nil
			}
		}

		b.reset()
	}

	return false, nil
}

func Compare(order, term1, term2 Term, k func() (bool, error)) (bool, error) {
	d := compare(term1, term2)
	switch {
	case d < 0:
		return Unify(Atom("<"), order, k)
	case d == 0:
		return Unify(Atom("="), order, k)
	case d > 0:
		return Unify(Atom(">"), order, k)
	default:
		return false, errors.New("unreachable")
	}
}

func compare(a, b Term) int64 {
	a, b = Resolve(a), Resolve(b)
	switch a := a.(type) {
	case *Variable:
		switch b := b.(type) {
		case *Variable:
			return int64(strings.Compare(fmt.Sprintf("%p", a), fmt.Sprintf("%p", b)))
		default:
			return -1
		}
	case Float:
		switch b := b.(type) {
		case *Variable:
			return 1
		case Float:
			return int64(a - b)
		case Integer:
			d := int64(a - Float(b))
			if d == 0 {
				return -1
			}
			return d
		default:
			return -1
		}
	case Integer:
		switch b := b.(type) {
		case *Variable:
			return 1
		case Float:
			d := int64(Float(a) - b)
			if d == 0 {
				return 1
			}
			return d
		case Integer:
			return int64(a - b)
		default:
			return -1
		}
	case Atom:
		switch b := b.(type) {
		case *Variable, Float, Integer:
			return 1
		case Atom:
			return int64(strings.Compare(string(a), string(b)))
		default:
			return -1
		}
	case *Compound:
		switch b := b.(type) {
		case *Compound:
			d := len(a.Args) - len(b.Args)
			if d != 0 {
				return int64(d)
			}

			d = strings.Compare(string(a.Functor), string(b.Functor))
			if d != 0 {
				return int64(d)
			}

			for i := range a.Args {
				d := compare(a.Args[i], b.Args[i])
				if d != 0 {
					return d
				}
			}

			return 0
		default:
			return 1
		}
	default:
		return 1
	}
}

type Exception struct {
	Term
}

func (e *Exception) Error() string {
	return e.String()
}

func Throw(t Term, _ func() (bool, error)) (bool, error) {
	return false, &Exception{Term: Resolve(t).Copy()}
}

func (e *Engine) Catch(goal, catcher, recover Term, k func() (bool, error)) (bool, error) {
	ok, err := e.Call(goal, done)
	if err != nil {
		if ex, ok := err.(*Exception); ok && catcher.Unify(ex.Term, false) {
			return e.Call(recover, k)
		}
		return false, err
	}
	if !ok {
		return false, nil
	}
	return k()
}

func (e *Engine) CurrentPredicate(pf Term, k func() (bool, error)) (bool, error) {
	a := newAssignment(pf)
	for key := range e.procedures {
		p := NewParser(key, &operators{
			{Precedence: 400, Type: "yfx", Name: "/"},
		})
		t, err := p.Term()
		if err != nil {
			return false, err
		}

		if pf.Unify(t, false) {
			ok, err := k()
			if err != nil {
				return false, err
			}
			if ok {
				return true, nil
			}
		}

		a.reset()
	}
	return false, nil
}
