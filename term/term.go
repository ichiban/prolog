package term

import (
	"fmt"
	"io"
	"strings"
)

// Interface is a prolog term.
type Interface interface {
	fmt.Stringer
	WriteTerm(io.Writer, WriteTermOptions, Env) error
	Unify(Interface, bool, *Env) bool
}

// Contains checks if t contains s.
func Contains(t, s Interface, env *Env) bool {
	switch t := t.(type) {
	case Variable:
		if t == s {
			return true
		}
		ref, ok := env.Lookup(t)
		if !ok {
			return false
		}
		return Contains(ref, s, env)
	case *Compound:
		if s, ok := s.(Atom); ok && t.Functor == s {
			return true
		}
		for _, a := range t.Args {
			if Contains(a, s, env) {
				return true
			}
		}
		return false
	default:
		return t == s
	}
}

// Rulify returns t if t is in a form of P:-Q, t:-true otherwise.
func Rulify(t Interface, env Env) Interface {
	t = env.Resolve(t)
	if c, ok := t.(*Compound); ok && c.Functor == ":-" && len(c.Args) == 2 {
		return t
	}
	return &Compound{Functor: ":-", Args: []Interface{t, Atom("true")}}
}

// WriteTermOptions describes options to write terms.
type WriteTermOptions struct {
	Quoted      bool
	Ops         Operators
	NumberVars  bool
	Descriptive bool
}

var DefaultWriteTermOptions = WriteTermOptions{
	Quoted: true,
	Ops: Operators{
		{Priority: 500, Specifier: "yfx", Name: "+"}, // for flag+value
		{Priority: 400, Specifier: "yfx", Name: "/"}, // for principal functors
	},
	NumberVars: false,
}

func Compare(a, b Interface) int64 {
	switch a := a.(type) {
	case Variable:
		switch b := b.(type) {
		case Variable:
			return int64(strings.Compare(string(a), string(b)))
		default:
			return -1
		}
	case Float:
		switch b := b.(type) {
		case Variable:
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
		case Variable:
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
		case Variable, Float, Integer:
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
				d := Compare(a.Args[i], b.Args[i])
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
