package engine

import (
	"fmt"
	"io"
	"strings"
)

// Term is a prolog term.
type Term interface {
	fmt.Stringer
	Unify(Term, bool, *Env) (*Env, bool)
	Unparse(func(Token), WriteTermOptions, *Env)
	Compare(Term, *Env) int64
}

// Contains checks if t contains s.
func Contains(t, s Term, env *Env) bool {
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
func Rulify(t Term, env *Env) Term {
	t = env.Resolve(t)
	if c, ok := t.(*Compound); ok && c.Functor == ":-" && len(c.Args) == 2 {
		return t
	}
	return &Compound{Functor: ":-", Args: []Term{t, Atom("true")}}
}

// WriteTermOptions describes options to write terms.
type WriteTermOptions struct {
	Quoted     bool
	Ops        Operators
	NumberVars bool

	Priority int
}

func (o WriteTermOptions) withPriority(p int) WriteTermOptions {
	ret := o
	ret.Priority = p
	return ret
}

var defaultWriteTermOptions = WriteTermOptions{
	Quoted: true,
	Ops: Operators{
		{Priority: 500, Specifier: OperatorSpecifierYFX, Name: "+"}, // for flag+value
		{Priority: 400, Specifier: OperatorSpecifierYFX, Name: "/"}, // for principal functors
	},
	Priority: 1200,
}

// Write outputs one of the external representations of the term.
func Write(w io.Writer, t Term, opts WriteTermOptions, env *Env) error {
	var (
		last TokenKind
		err  error
	)
	env.Resolve(t).Unparse(func(token Token) {
		if err != nil {
			return
		}
		var sb strings.Builder
		if spacing[last][token.Kind] {
			_, _ = sb.WriteString(" ")
		}
		_, _ = sb.WriteString(token.Val)
		last = token.Kind
		_, err = fmt.Fprint(w, sb.String())
	}, opts, env)
	return err
}
