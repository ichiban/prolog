package prolog

import (
	"errors"
	"fmt"
)

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
