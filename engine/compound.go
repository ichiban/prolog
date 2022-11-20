package engine

import (
	"fmt"
	"sort"
	"strings"
	"unicode/utf8"
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

func (c *compound) GoString() string {
	return fmt.Sprintf(`&engine.compound{functor:%#v, args:%#v}`, c.functor, c.args)
}

// Cons returns a list consists of a first element car and the rest cdr.
func Cons(car, cdr Term) Term {
	return atomDot.Apply(car, cdr)
}

type list []Term

func (l list) TermID() TermID { // Slices are not comparable.
	type listID struct {
		len  int
		head *Term
	}
	id := listID{
		len: len(l),
	}
	if len(l) > 0 {
		id.head = &l[0]
	}
	return id
}

func (l list) Functor() Atom {
	return atomDot
}

func (l list) Arity() int {
	return 2
}

func (l list) Arg(n int) Term {
	var t Term
	switch n {
	case 0:
		t = l[0]
	case 1:
		if len(l) == 1 {
			t = atomEmptyList
			break
		}
		t = l[1:]
	}
	return t
}

func (l list) GoString() string {
	var sb strings.Builder
	_, _ = sb.WriteString(`engine.list{`)
	for i, e := range l {
		if i != 0 {
			_, _ = sb.WriteString(`, `)
		}
		_, _ = fmt.Fprintf(&sb, "%#v", e)
	}
	_, _ = sb.WriteString(`}`)
	return sb.String()
}

// List returns a list of ts.
func List(ts ...Term) Term {
	if len(ts) == 0 {
		return atomEmptyList
	}
	return list(ts)
}

type partial struct {
	Compound
	tail Term
}

func (p partial) TermID() TermID { // The underlying compound might not be comparable.
	type partialID struct {
		prefixID, tailID TermID
	}
	return partialID{
		prefixID: ID(p.Compound),
		tailID:   ID(p.tail),
	}
}

func (p partial) Arg(n int) Term {
	t := p.Compound.Arg(n)
	if c := p.Compound; c.Functor() == atomDot && c.Arity() == 2 && n == 1 {
		if t == atomEmptyList {
			t = p.tail
		} else {
			t = partial{Compound: t.(Compound), tail: p.tail}
		}
	}
	return t
}

func (p partial) GoString() string {
	return fmt.Sprintf(`engine.partial{Compound:%#v, tail:%#v}`, p.Compound, p.tail)
}

// ListRest returns a list of ts followed by rest.
func ListRest(rest Term, ts ...Term) Term {
	if len(ts) == 0 {
		return rest
	}
	return partial{
		Compound: list(ts),
		tail:     rest,
	}
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
		s = sep.Apply(ts[i], s)
	}
	return s
}

// Pair returns a pair of k and v.
func Pair(k, v Term) Term {
	return atomMinus.Apply(k, v)
}

func tuple(args ...Term) Term {
	return Atom(0).Apply(args...)
}

type charList string

func (c charList) Functor() Atom {
	return atomDot
}

func (c charList) Arity() int {
	return 2
}

func (c charList) Arg(n int) Term {
	r, i := utf8.DecodeRuneInString(string(c))
	var t Term
	switch n {
	case 0:
		t = Atom(r)
	case 1:
		if i == len(c) {
			t = atomEmptyList
		} else {
			t = c[i:]
		}
	}
	return t
}

// CharList returns a character list.
func CharList(s string) Term {
	if s == "" {
		return atomEmptyList
	}
	return charList(s)
}

type codeList string

func (c codeList) Functor() Atom {
	return atomDot
}

func (c codeList) Arity() int {
	return 2
}

func (c codeList) Arg(n int) Term {
	r, i := utf8.DecodeRuneInString(string(c))
	var t Term
	switch n {
	case 0:
		t = Integer(r)
	case 1:
		if i == len(c) {
			t = atomEmptyList
		} else {
			t = c[i:]
		}
	}
	return t
}

// CodeList returns a character code list.
func CodeList(s string) Term {
	if s == "" {
		return atomEmptyList
	}
	return codeList(s)
}
