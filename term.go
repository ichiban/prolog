package prolog

import (
	"fmt"
	"strconv"
	"strings"
)

type Term interface {
	fmt.Stringer
	Unify(Term) bool
	Simplify() Term
}

type Atom string

func (a Atom) String() string {
	return string(a)
}

func (a Atom) PrincipalFunctor() *pFunctor {
	return &pFunctor{functor: a, arity: 0}
}

func (a Atom) Equal(t xrRecord) bool {
	v, ok := t.(Atom)
	return ok && a == v
}

func (a Atom) Unify(t Term) bool {
	switch t := t.(type) {
	case Atom:
		return a == t
	case *Variable:
		return t.Unify(a)
	default:
		return false
	}
}

func (a Atom) Simplify() Term {
	return a
}

type Integer int64

func (i Integer) String() string {
	return strconv.FormatInt(int64(i), 10)
}

func (i Integer) Equal(t xrRecord) bool {
	v, ok := t.(Integer)
	return ok && i == v
}

func (i Integer) Unify(t Term) bool {
	switch t := t.(type) {
	case Integer:
		return i == t
	case *Variable:
		return t.Unify(i)
	default:
		return false
	}
}

func (i Integer) Simplify() Term {
	return i
}

type Variable struct {
	Name string
	Ref  Term
}

func (v *Variable) String() string {
	name := v.Name
	if name == "" {
		name = fmt.Sprintf("_%p", v)
	}
	if v.Ref == nil {
		return name
	}
	return fmt.Sprintf("%s = %s", name, v.Ref)
}

func (v *Variable) Unify(t Term) bool {
	if v.Ref != nil {
		return v.Ref.Unify(t)
	}
	v.Ref = t
	return true
}

func (v *Variable) Simplify() Term {
	if v.Ref == nil {
		return v
	}
	return v.Ref.Simplify()
}

type Compound struct {
	Functor Atom
	Args    []Term
}

func (c *Compound) String() string {
	if c.Functor == "." && len(c.Args) == 2 { // list
		t := Term(c)
		var (
			elems []string
			rest  string
		)
		for {
			if l, ok := t.(*Compound); ok && l.Functor == "." && len(l.Args) == 2 {
				elems = append(elems, l.Args[0].String())
				t = l.Args[1]
				continue
			}
			if a, ok := t.(Atom); ok && a == "[]" {
				break
			}
			rest = "|" + t.String()
			break
		}
		return fmt.Sprintf("[%s%s]", strings.Join(elems, ", "), rest)
	}

	args := make([]string, len(c.Args))
	for i, arg := range c.Args {
		args[i] = arg.String()
	}
	return fmt.Sprintf("%s(%s)", c.Functor, strings.Join(args, ", "))
}

func (c *Compound) PrincipalFunctor() *pFunctor {
	return &pFunctor{functor: c.Functor, arity: len(c.Args)}
}

func (c *Compound) Unify(t Term) bool {
	switch t := t.(type) {
	case *Compound:
		if c.Functor != t.Functor {
			return false
		}
		if len(c.Args) != len(t.Args) {
			return false
		}
		for i := range c.Args {
			if !c.Args[i].Unify(t.Args[i]) {
				return false
			}
		}
		return true
	case *Variable:
		return t.Unify(c)
	default:
		return false
	}
}

func (c *Compound) Simplify() Term {
	args := make([]Term, len(c.Args))
	for i := range args {
		args[i] = c.Args[i].Simplify()
	}
	return &Compound{Functor: c.Functor, Args: args}
}

func Cons(car, cdr Term) Term {
	return &Compound{
		Functor: ".",
		Args:    []Term{car, cdr},
	}
}

func List(ts ...Term) Term {
	l := Term(Atom("[]"))
	for i := len(ts) - 1; i >= 0; i-- {
		l = Cons(ts[i], l)
	}
	return l
}
