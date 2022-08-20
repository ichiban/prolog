package engine

import (
	"sort"
)

// Compound is a Prolog compound.
type Compound interface {
	Functor() Atom
	Arity() int
	Arg(n int) Term
}

type compound struct {
	functor Atom
	args    []Term
}

func (c *compound) Functor() Atom {
	return c.functor
}

func (c *compound) Arity() int {
	return len(c.args)
}

func (c *compound) Arg(n int) Term {
	return c.args[n]
}

// Cons returns a list consists of a first element car and the rest cdr.
func Cons(car, cdr Term) Term {
	return &compound{
		functor: ".",
		args:    []Term{car, cdr},
	}
}

// List returns a list of ts.
func List(ts ...Term) Term {
	return ListRest(Atom("[]"), ts...)
}

// ListRest returns a list of ts followed by rest.
func ListRest(rest Term, ts ...Term) Term {
	l := rest
	for i := len(ts) - 1; i >= 0; i-- {
		l = Cons(ts[i], l)
	}
	return l
}

// Set returns a list of ts which elements are unique.
func (e *Env) Set(ts ...Term) Term {
	sort.Slice(ts, func(i, j int) bool {
		return e.Compare(ts[i], ts[j]) == OrderLess
	})
	us := make([]Term, 0, len(ts))
	for _, t := range ts {
		if len(us) > 0 && e.Compare(us[len(us)-1], t) == OrderEqual {
			continue
		}
		us = append(us, t)
	}
	return List(us...)
}

// Slice returns a Term slice containing the elements of list.
// It errors if the given Term is not a list.
func Slice(list Term, env *Env) ([]Term, error) {
	var ret []Term
	iter := ListIterator{List: list, Env: env}
	for iter.Next() {
		ret = append(ret, env.Resolve(iter.Current()))
	}
	return ret, iter.Err()
}

// Seq returns a sequence of ts separated by sep.
func Seq(sep Atom, ts ...Term) Term {
	s, ts := ts[len(ts)-1], ts[:len(ts)-1]
	for i := len(ts) - 1; i >= 0; i-- {
		s = &compound{
			functor: sep,
			args:    []Term{ts[i], s},
		}
	}
	return s
}

// Pair returns a pair of k and v.
func Pair(k, v Term) Term {
	return &compound{
		functor: "-",
		args:    []Term{k, v},
	}
}
