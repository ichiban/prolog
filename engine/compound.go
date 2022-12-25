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

func (l list) termID() termID { // Slices are not comparable.
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

func (l list) Len() int {
	return len(l)
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
	vector vector
	tail   *Term
}

func (p *partial) termID() termID { // The underlying compound might not be comparable.
	type partialID struct {
		prefixID, tailID termID
	}
	return partialID{
		prefixID: id(p.vector),
		tailID:   p.tail,
	}
}

func (p *partial) Functor() Atom {
	return atomDot
}

func (p *partial) Arity() int {
	return 2
}

func (p *partial) Arg(n int) Term {
	t := p.vector.Arg(n)
	if c := p.vector; c.Functor() == atomDot && c.Arity() == 2 && n == 1 {
		if t == atomEmptyList {
			t = *p.tail
		} else {
			t = &partial{vector: t.(vector), tail: p.tail}
		}
	}
	return t
}

func (p *partial) GoString() string {
	return fmt.Sprintf(`engine.partial{vector:%#v, tail:%#v}`, p.vector, *p.tail)
}

// PartialList returns a list of ts followed by tail.
func PartialList(tail Term, ts ...Term) Term {
	if len(ts) == 0 {
		return tail
	}
	return &partial{
		vector: list(ts),
		tail:   &tail,
	}
}

// set returns a list of ts which elements are unique.
func (e *Env) set(ts ...Term) Term {
	sort.Slice(ts, func(i, j int) bool {
		return e.compare(ts[i], ts[j]) == -1
	})
	us := make([]Term, 0, len(ts))
	for _, t := range ts {
		if len(us) > 0 && e.compare(us[len(us)-1], t) == 0 {
			continue
		}
		us = append(us, t)
	}
	return List(us...)
}

// slice returns a Term slice containing the elements of list.
// It errors if the given Term is not a list.
func slice(list Term, env *Env) ([]Term, error) {
	var ret []Term
	iter := ListIterator{List: list, Env: env}
	for iter.Next() {
		ret = append(ret, env.Resolve(iter.Current()))
	}
	return ret, iter.Err()
}

// seq returns a sequence of ts separated by sep.
func seq(sep Atom, ts ...Term) Term {
	s, ts := ts[len(ts)-1], ts[:len(ts)-1]
	for i := len(ts) - 1; i >= 0; i-- {
		s = sep.Apply(ts[i], s)
	}
	return s
}

func pair(k, v Term) Term {
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

func (c charList) Len() int {
	return len([]rune(c))
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

func (c codeList) Len() int {
	return len([]rune(c))
}

// CodeList returns a character code list.
func CodeList(s string) Term {
	if s == "" {
		return atomEmptyList
	}
	return codeList(s)
}

type sparseCompoundPair struct {
	index int
	arg   Term
}

type sparseCompound struct {
	functor Atom
	arity   int
	pairs   []sparseCompoundPair
}

func (s *sparseCompound) Functor() Atom {
	return s.functor
}

func (s *sparseCompound) Arity() int {
	return s.arity
}

func (s *sparseCompound) Arg(n int) Term {
	i, found := sort.Find(len(s.pairs), func(i int) int {
		return n - s.pairs[i].index
	})
	if found {
		return s.pairs[i].arg
	}
	v := NewVariable()
	s.pairs = append(s.pairs, sparseCompoundPair{})
	copy(s.pairs[i+1:], s.pairs[i:])
	s.pairs[i] = sparseCompoundPair{index: n, arg: v}
	return v
}

func freshVarList(n int) Term {
	if n == 0 {
		return atomEmptyList
	}
	return &sparseList{len: n}
}

type sparseListPair struct {
	index int
	elem  Term
}

type sparseList struct {
	len    int
	offset int
	pairs  *[]sparseListPair
}

func (s *sparseList) Functor() Atom {
	return atomDot
}

func (s *sparseList) Arity() int {
	return 2
}

func (s *sparseList) Arg(n int) Term {
	if s.pairs == nil {
		s.pairs = &[]sparseListPair{}
	}

	if n == 1 {
		offset := s.offset + 1
		if offset == s.len {
			return atomEmptyList
		}
		return &sparseList{
			len:    s.len,
			offset: offset,
			pairs:  s.pairs,
		}
	}

	i, found := sort.Find(len(*s.pairs), func(i int) int {
		return s.offset - (*s.pairs)[i].index
	})
	if found {
		return (*s.pairs)[i].elem
	}
	v := NewVariable()
	*s.pairs = append(*s.pairs, sparseListPair{})
	copy((*s.pairs)[i+1:], (*s.pairs)[i:])
	(*s.pairs)[i] = sparseListPair{index: s.offset, elem: v}
	return v
}

func (s *sparseList) Len() int {
	return s.len
}

// vector is a list which element can be accessed by index.
type vector interface {
	Compound
	Len() int
}
