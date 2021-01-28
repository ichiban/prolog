package prolog

import (
	"fmt"
	"io"
	"sort"
	"strconv"
	"strings"
)

var defaultOperators = []operator{
	{Precedence: 400, Type: "yfx", Name: "/"}, // for principal functors
}

type Term interface {
	fmt.Stringer
	TermString(operators, *[]*Variable) string
	Unify(Term, bool) bool
	Copy() Term
}

type Variable struct {
	Name string
	Ref  Term
}

func (v *Variable) String() string {
	var stop []*Variable
	return v.TermString(defaultOperators, &stop)
}

func (v *Variable) TermString(os operators, stop *[]*Variable) string {
	name := v.Name
	if name == "" {
		if v.Ref != nil {
			return v.Ref.TermString(os, stop)
		}
		name = fmt.Sprintf("_%p", v)
	}
	if v.Ref == nil {
		return name
	}
	if stop != nil {
		for _, s := range *stop {
			if v == s {
				return name
			}
		}
	}
	*stop = append(*stop, v)
	return fmt.Sprintf("%s = %s", name, v.Ref.TermString(os, stop))
}

func (v *Variable) Unify(t Term, occursCheck bool) bool {
	if occursCheck && Contains(t, v) {
		return false
	}
	if v.Ref != nil {
		return v.Ref.Unify(t, occursCheck)
	}
	if w, ok := t.(*Variable); ok && w.Ref == nil {
		t = &Variable{}
		w.Ref = t
	}
	v.Ref = t
	return true
}

func (v *Variable) Copy() Term {
	if v.Ref == nil {
		return &Variable{}
	}
	return &Variable{Ref: v.Ref.Copy()}
}

type Float float64

func (f Float) String() string {
	return f.TermString(nil, nil)
}

func (f Float) TermString(operators, *[]*Variable) string {
	return strconv.FormatFloat(float64(f), 'f', -1, 64)
}

func (f Float) Unify(t Term, occursCheck bool) bool {
	switch t := t.(type) {
	case Float:
		return f == t
	case *Variable:
		return t.Unify(f, occursCheck)
	default:
		return false
	}
}

func (f Float) Copy() Term {
	return f
}

type Integer int64

func (i Integer) String() string {
	return i.TermString(nil, nil)
}

func (i Integer) TermString(operators, *[]*Variable) string {
	return strconv.FormatInt(int64(i), 10)
}

func (i Integer) Unify(t Term, occursCheck bool) bool {
	switch t := t.(type) {
	case Integer:
		return i == t
	case *Variable:
		return t.Unify(i, occursCheck)
	default:
		return false
	}
}

func (i Integer) Copy() Term {
	return i
}

type Atom string

func (a Atom) String() string {
	return a.TermString(nil, nil)
}

func (a Atom) TermString(operators, *[]*Variable) string {
	return string(a)
}

func (a Atom) Unify(t Term, occursCheck bool) bool {
	switch t := t.(type) {
	case Atom:
		return a == t
	case *Variable:
		return t.Unify(a, occursCheck)
	default:
		return false
	}
}

func (a Atom) Copy() Term {
	return a
}

type Compound struct {
	Functor Atom
	Args    []Term
}

func (c *Compound) String() string {
	var stop []*Variable
	return c.TermString(defaultOperators, &stop)
}

func (c *Compound) TermString(os operators, stop *[]*Variable) string {
	if c.Functor == "." && len(c.Args) == 2 { // list
		t := Term(c)
		var (
			elems []string
			rest  string
		)
		for {
			if l, ok := t.(*Compound); ok && l.Functor == "." && len(l.Args) == 2 {
				elems = append(elems, l.Args[0].TermString(os, stop))
				t = l.Args[1]
				continue
			}
			if a, ok := t.(Atom); ok && a == "[]" {
				break
			}
			rest = "|" + t.TermString(os, stop)
			break
		}
		return fmt.Sprintf("[%s%s]", strings.Join(elems, ", "), rest)
	}

	switch len(c.Args) {
	case 1:
		for _, o := range os {
			if o.Name != c.Functor {
				continue
			}
			switch o.Type {
			case `xf`, `yf`:
				l := []rune(c.Args[0].TermString(os, stop))
				f := []rune(c.Functor.TermString(os, stop))
				if isExtendedGraphic(l[len(l)-1]) && isExtendedGraphic(f[0]) {
					return fmt.Sprintf("(%s)%s", string(l), string(f))
				}
				return fmt.Sprintf("%s%s", string(l), string(f))
			case `fx`, `fy`:
				f := []rune(c.Functor.TermString(os, stop))
				r := []rune(c.Args[0].TermString(os, stop))
				if isExtendedGraphic(f[len(f)-1]) && isExtendedGraphic(r[0]) {
					return fmt.Sprintf("%s(%s)", string(f), string(r))
				}
				return fmt.Sprintf("%s%s", string(f), string(r))
			}
		}
	case 2:
		for _, o := range os {
			if o.Name != c.Functor {
				continue
			}
			switch o.Type {
			case `xfx`, `xfy`, `yfx`:
				l := []rune(c.Args[0].TermString(os, stop))
				f := []rune(c.Functor.TermString(os, stop))
				r := []rune(c.Args[1].TermString(os, stop))
				switch {
				case isExtendedGraphic(l[len(l)-1]) && isExtendedGraphic(f[0]) && isExtendedGraphic(f[len(f)-1]) && isExtendedGraphic(r[0]):
					return fmt.Sprintf("(%s)%s(%s)", string(l), string(f), string(r))
				case isExtendedGraphic(l[len(l)-1]) && isExtendedGraphic(f[0]):
					return fmt.Sprintf("(%s)%s%s", string(l), string(f), string(r))
				case isExtendedGraphic(f[len(f)-1]) && isExtendedGraphic(r[0]):
					return fmt.Sprintf("%s%s(%s)", string(l), string(f), string(r))
				default:
					return fmt.Sprintf("%s%s%s", string(l), string(f), string(r))
				}
			}
		}
	}

	args := make([]string, len(c.Args))
	for i, arg := range c.Args {
		args[i] = arg.TermString(os, stop)
	}
	return fmt.Sprintf("%s(%s)", c.Functor.TermString(os, stop), strings.Join(args, ", "))
}

func (c *Compound) Unify(t Term, occursCheck bool) bool {
	switch t := t.(type) {
	case *Compound:
		if c.Functor != t.Functor {
			return false
		}
		if len(c.Args) != len(t.Args) {
			return false
		}
		for i := range c.Args {
			if !c.Args[i].Unify(t.Args[i], occursCheck) {
				return false
			}
		}
		return true
	case *Variable:
		return t.Unify(c, occursCheck)
	default:
		return false
	}
}

func (c *Compound) Copy() Term {
	args := make([]Term, len(c.Args))
	for i, a := range c.Args {
		args[i] = a.Copy()
	}
	return &Compound{
		Functor: c.Functor,
		Args:    args,
	}
}

func PrincipalFunctor(name Atom, arity Integer) Term {
	return &Compound{
		Functor: "/",
		Args:    []Term{name, arity},
	}
}

func Cons(car, cdr Term) Term {
	return &Compound{
		Functor: ".",
		Args:    []Term{car, cdr},
	}
}

func List(ts ...Term) Term {
	return ListRest(Atom("[]"), ts...)
}

func ListRest(rest Term, ts ...Term) Term {
	l := rest
	for i := len(ts) - 1; i >= 0; i-- {
		l = Cons(ts[i], l)
	}
	return l
}

func Set(ts ...Term) Term {
	if len(ts) < 2 {
		return List(ts...)
	}
	us := make([]Term, len(ts))
	copy(us, ts)
	sort.Slice(us, func(i, j int) bool {
		return compare(us[i], us[j]) < 0
	})
	n := 1
	for _, u := range us[1:] {
		if compare(us[n-1], u) == 0 {
			continue
		}
		us[n] = u
		n++
	}
	for i := range us[n:] {
		us[n+i] = nil
	}
	return List(us[:n]...)
}

func Resolve(t Term) Term {
	var stop []*Variable
	for t != nil {
		switch v := t.(type) {
		case Float, Integer, Atom, *Compound, Stream:
			return v
		case *Variable:
			if v.Ref == nil {
				return v
			}
			for _, s := range stop {
				if v == s {
					return v
				}
			}
			stop = append(stop, v)
			t = v.Ref
		}
	}
	return nil
}

func Contains(t, s Term) bool {
	switch t := t.(type) {
	case *Variable:
		if t == s {
			return true
		}
		if t.Ref == nil {
			return false
		}
		return Contains(t.Ref, s)
	case *Compound:
		if s, ok := s.(Atom); ok && t.Functor == s {
			return true
		}
		for _, a := range t.Args {
			if Contains(a, s) {
				return true
			}
		}
		return false
	default:
		return t == s
	}
}

func Rulify(t Term) Term {
	t = Resolve(t)
	if c, ok := t.(*Compound); ok && c.Functor == ":-" && len(c.Args) == 2 {
		return t
	}
	return &Compound{Functor: ":-", Args: []Term{t, Atom("true")}}
}

type Stream struct {
	io.ReadWriteCloser
}

func (s Stream) String() string {
	return s.TermString(nil, nil)
}

func (s Stream) TermString(operators, *[]*Variable) string {
	return fmt.Sprintf("<stream>(%p)", s.ReadWriteCloser)
}

func (s Stream) Unify(t Term, occursCheck bool) bool {
	switch t := t.(type) {
	case Stream:
		return s.ReadWriteCloser == t.ReadWriteCloser
	case *Variable:
		return t.Unify(s, occursCheck)
	default:
		return false
	}
}

func (s Stream) Copy() Term {
	return s
}
