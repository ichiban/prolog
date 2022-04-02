package engine

import (
	"fmt"
	"io"
	"strings"
)

// Term is a prolog term.
type Term interface {
	Unify(Term, bool, *Env) (*Env, bool)
	Unparse(func(Token), *Env, ...WriteOption)
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

type writeOptions struct {
	quoted        bool
	ops           operators
	variableNames map[Variable]Atom
	numberVars    bool
	priority      int
}

var defaultWriteOptions = writeOptions{
	ops: operators{
		{priority: 500, specifier: operatorSpecifierYFX, name: "+"}, // for flag+value
		{priority: 400, specifier: operatorSpecifierYFX, name: "/"}, // for principal functors
	},
	variableNames: map[Variable]Atom{},
	priority:      1200,
}

// WriteOption is an option for Write.
type WriteOption func(*writeOptions)

// WithQuoted sets if atoms are quoted as needed.
func WithQuoted(b bool) WriteOption {
	return func(options *writeOptions) {
		options.quoted = b
	}
}

// WithIgnoreOps sets if the operator notation is used.
func (state *State) WithIgnoreOps(b bool) WriteOption {
	if b {
		return withOps(nil)
	}

	return withOps(state.operators)
}

func withOps(ops operators) WriteOption {
	return func(options *writeOptions) {
		options.ops = ops
	}
}

// WithVariableNames sets the mappings from a variable to an atom.
func WithVariableNames(variableNames map[Variable]Atom) WriteOption {
	return func(options *writeOptions) {
		options.variableNames = variableNames
	}
}

// WithNumberVars sets if a compound `'$VAR'(N)` where N is an integer is written as a variable.
func WithNumberVars(b bool) WriteOption {
	return func(options *writeOptions) {
		options.numberVars = b
	}
}

// WithPriority sets priority which determines if an expression is enclosed by a pair of parentheses.
func WithPriority(p int) WriteOption {
	return func(options *writeOptions) {
		options.priority = p
	}
}

// Write outputs one of the external representations of the term.
func Write(w io.Writer, t Term, env *Env, opts ...WriteOption) error {
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
	}, env, opts...)
	return err
}
