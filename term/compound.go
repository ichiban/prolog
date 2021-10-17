package term

import (
	"fmt"
	"sort"
	"strings"

	"github.com/ichiban/prolog/syntax"
)

// Compound is a prolog compound.
type Compound struct {
	Functor Atom
	Args    []Interface
}

func (c *Compound) String() string {
	var sb strings.Builder
	_ = Write(&sb, c, defaultWriteTermOptions, nil)
	return sb.String()
}

// Unify unifies the compound with t.
func (c *Compound) Unify(t Interface, occursCheck bool, env *Env) (*Env, bool) {
	switch t := env.Resolve(t).(type) {
	case *Compound:
		if c.Functor != t.Functor {
			return env, false
		}
		if len(c.Args) != len(t.Args) {
			return env, false
		}
		var ok bool
		for i := range c.Args {
			env, ok = c.Args[i].Unify(t.Args[i], occursCheck, env)
			if !ok {
				return env, false
			}
		}
		return env, true
	case Variable:
		return t.Unify(c, occursCheck, env)
	default:
		return env, false
	}
}

// Unparse emits tokens that represent the compound.
func (c *Compound) Unparse(emit func(syntax.Token), opts WriteTermOptions, env *Env) {
	if c.Functor == "." && len(c.Args) == 2 { // list
		emit(syntax.Token{Kind: syntax.TokenBracketL, Val: "["})
		env.Resolve(c.Args[0]).Unparse(emit, opts, env)
		t := env.Resolve(c.Args[1])
		for {
			if l, ok := t.(*Compound); ok && l.Functor == "." && len(l.Args) == 2 {
				emit(syntax.Token{Kind: syntax.TokenComma, Val: ","})
				env.Resolve(l.Args[0]).Unparse(emit, opts, env)
				t = env.Resolve(l.Args[1])
				continue
			}
			if a, ok := t.(Atom); ok && a == "[]" {
				break
			}
			emit(syntax.Token{Kind: syntax.TokenBar, Val: "|"})
			t.Unparse(emit, opts, env)
			break
		}
		emit(syntax.Token{Kind: syntax.TokenBracketR, Val: "]"})
		return
	}

	switch len(c.Args) {
	case 1:
		for _, op := range opts.Ops {
			if op.Name != c.Functor {
				continue
			}
			switch op.Specifier {
			case OperatorSpecifierFX, OperatorSpecifierFY:
				if int(op.Priority) > opts.Priority {
					emit(syntax.Token{Kind: syntax.TokenParenL, Val: "("})
					defer emit(syntax.Token{Kind: syntax.TokenParenR, Val: ")"})
				}
				c.Functor.Unparse(emit, opts, env)
				{
					opts := opts
					opts.Priority = int(op.Priority)
					env.Resolve(c.Args[0]).Unparse(emit, opts, env)
				}
				return
			case OperatorSpecifierXF, OperatorSpecifierYF:
				if int(op.Priority) > opts.Priority {
					emit(syntax.Token{Kind: syntax.TokenParenL, Val: "("})
					defer emit(syntax.Token{Kind: syntax.TokenParenR, Val: ")"})
				}
				{
					opts := opts
					opts.Priority = int(op.Priority)
					env.Resolve(c.Args[0]).Unparse(emit, opts, env)
				}
				c.Functor.Unparse(emit, opts, env)
				return
			}
		}
	case 2:
		for _, op := range opts.Ops {
			if op.Name != c.Functor {
				continue
			}
			switch op.Specifier {
			case OperatorSpecifierXFX, OperatorSpecifierXFY, OperatorSpecifierYFX:
				if int(op.Priority) > opts.Priority {
					emit(syntax.Token{Kind: syntax.TokenParenL, Val: "("})
					defer emit(syntax.Token{Kind: syntax.TokenParenR, Val: ")"})
				}
				{
					opts := opts
					opts.Priority = int(op.Priority)
					env.Resolve(c.Args[0]).Unparse(emit, opts, env)
				}
				c.Functor.Unparse(emit, opts, env)
				{
					opts := opts
					opts.Priority = int(op.Priority)
					env.Resolve(c.Args[1]).Unparse(emit, opts, env)
				}
				return
			}
		}
	}

	if opts.NumberVars && c.Functor == "$VAR" && len(c.Args) == 1 {
		switch n := env.Resolve(c.Args[0]).(type) {
		case Integer:
			const letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
			i, j := int(n)%len(letters), int(n)/len(letters)
			if j == 0 {
				s := string(letters[i])
				emit(syntax.Token{Kind: syntax.TokenVariable, Val: s})
				return
			}
			s := fmt.Sprintf("%s%d", string(letters[i]), j)
			emit(syntax.Token{Kind: syntax.TokenVariable, Val: s})
			return
		}
	}

	c.Functor.Unparse(emit, opts, env)
	emit(syntax.Token{Kind: syntax.TokenParenL, Val: "("})
	env.Resolve(c.Args[0]).Unparse(emit, opts, env)
	for _, arg := range c.Args[1:] {
		emit(syntax.Token{Kind: syntax.TokenComma, Val: ","})
		env.Resolve(arg).Unparse(emit, opts, env)
	}
	emit(syntax.Token{Kind: syntax.TokenParenR, Val: ")"})
}

// Cons returns a list consists of a first element car and the rest cdr.
func Cons(car, cdr Interface) Interface {
	return &Compound{
		Functor: ".",
		Args:    []Interface{car, cdr},
	}
}

// List returns a list of ts.
func List(ts ...Interface) Interface {
	return ListRest(Atom("[]"), ts...)
}

// ListRest returns a list of ts followed by rest.
func ListRest(rest Interface, ts ...Interface) Interface {
	l := rest
	for i := len(ts) - 1; i >= 0; i-- {
		l = Cons(ts[i], l)
	}
	return l
}

// Set returns a list of ts which elements are unique.
func Set(ts ...Interface) Interface {
	if len(ts) < 2 {
		return List(ts...)
	}
	us := make([]Interface, len(ts))
	copy(us, ts)
	sort.Slice(us, func(i, j int) bool {
		return Compare(us[i], us[j], nil) < 0
	})
	n := 1
	for _, u := range us[1:] {
		if Compare(us[n-1], u, nil) == 0 {
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
