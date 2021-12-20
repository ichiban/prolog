package engine

import (
	"fmt"
	"sort"
	"strings"
)

// Compound is a prolog compound.
type Compound struct {
	Functor Atom
	Args    []Term
}

func (c *Compound) String() string {
	var sb strings.Builder
	_ = Write(&sb, c, defaultWriteTermOptions, nil)
	return sb.String()
}

// Unify unifies the compound with t.
func (c *Compound) Unify(t Term, occursCheck bool, env *Env) (*Env, bool) {
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
func (c *Compound) Unparse(emit func(Token), opts WriteTermOptions, env *Env) {
	if c.Functor == "." && len(c.Args) == 2 {
		c.unparseList(emit, opts, env)
		return
	}

	if c.Functor == "{}" && len(c.Args) == 1 {
		c.unparseBlock(emit, opts, env)
		return
	}

	if op := opts.Ops.find(c.Functor, len(c.Args)); op != nil {
		[operatorSpecifierLen]func(Operator, func(Token), WriteTermOptions, *Env){
			OperatorSpecifierFX:  c.unparseFX,
			OperatorSpecifierFY:  c.unparseFY,
			OperatorSpecifierXF:  c.unparseXF,
			OperatorSpecifierYF:  c.unparseYF,
			OperatorSpecifierXFX: c.unparseXFX,
			OperatorSpecifierXFY: c.unparseXFY,
			OperatorSpecifierYFX: c.unparseYFX,
		}[op.Specifier](*op, emit, opts, env)
		return
	}

	if n, ok := env.Resolve(c.Args[0]).(Integer); ok && opts.NumberVars && c.Functor == "$VAR" && len(c.Args) == 1 {
		c.unparseNumberVar(n, emit)
		return
	}

	c.unparse(emit, opts, env)
}

func (c *Compound) unparseFX(op Operator, emit func(Token), opts WriteTermOptions, env *Env) {
	if int(op.Priority) > opts.Priority {
		emit(Token{Kind: TokenParenL, Val: "("})
		defer emit(Token{Kind: TokenParenR, Val: ")"})
	}
	c.Functor.Unparse(emit, opts, env)
	env.Resolve(c.Args[0]).Unparse(emit, opts.withPriority(int(op.Priority)-1), env)
}

func (c *Compound) unparseFY(op Operator, emit func(Token), opts WriteTermOptions, env *Env) {
	if int(op.Priority) > opts.Priority {
		emit(Token{Kind: TokenParenL, Val: "("})
		defer emit(Token{Kind: TokenParenR, Val: ")"})
	}
	c.Functor.Unparse(emit, opts, env)
	env.Resolve(c.Args[0]).Unparse(emit, opts.withPriority(int(op.Priority)), env)
}

func (c *Compound) unparseXF(op Operator, emit func(Token), opts WriteTermOptions, env *Env) {
	if int(op.Priority) > opts.Priority {
		emit(Token{Kind: TokenParenL, Val: "("})
		defer emit(Token{Kind: TokenParenR, Val: ")"})
	}
	env.Resolve(c.Args[0]).Unparse(emit, opts.withPriority(int(op.Priority)-1), env)
	c.Functor.Unparse(emit, opts, env)
}

func (c *Compound) unparseYF(op Operator, emit func(Token), opts WriteTermOptions, env *Env) {
	if int(op.Priority) > opts.Priority {
		emit(Token{Kind: TokenParenL, Val: "("})
		defer emit(Token{Kind: TokenParenR, Val: ")"})
	}
	env.Resolve(c.Args[0]).Unparse(emit, opts.withPriority(int(op.Priority)), env)
	c.Functor.Unparse(emit, opts, env)
}

func (c *Compound) unparseXFX(op Operator, emit func(Token), opts WriteTermOptions, env *Env) {
	if int(op.Priority) > opts.Priority {
		emit(Token{Kind: TokenParenL, Val: "("})
		defer emit(Token{Kind: TokenParenR, Val: ")"})
	}
	env.Resolve(c.Args[0]).Unparse(emit, opts.withPriority(int(op.Priority)-1), env)
	c.Functor.Unparse(emit, opts, env)
	env.Resolve(c.Args[1]).Unparse(emit, opts.withPriority(int(op.Priority)-1), env)
}

func (c *Compound) unparseXFY(op Operator, emit func(Token), opts WriteTermOptions, env *Env) {
	if int(op.Priority) > opts.Priority {
		emit(Token{Kind: TokenParenL, Val: "("})
		defer emit(Token{Kind: TokenParenR, Val: ")"})
	}
	env.Resolve(c.Args[0]).Unparse(emit, opts.withPriority(int(op.Priority)-1), env)
	c.Functor.Unparse(emit, opts, env)
	env.Resolve(c.Args[1]).Unparse(emit, opts.withPriority(int(op.Priority)), env)
}

func (c *Compound) unparseYFX(op Operator, emit func(Token), opts WriteTermOptions, env *Env) {
	if int(op.Priority) > opts.Priority {
		emit(Token{Kind: TokenParenL, Val: "("})
		defer emit(Token{Kind: TokenParenR, Val: ")"})
	}
	env.Resolve(c.Args[0]).Unparse(emit, opts.withPriority(int(op.Priority)), env)
	c.Functor.Unparse(emit, opts, env)
	env.Resolve(c.Args[1]).Unparse(emit, opts.withPriority(int(op.Priority)-1), env)
}

func (c *Compound) unparseList(emit func(Token), opts WriteTermOptions, env *Env) {
	emit(Token{Kind: TokenBracketL, Val: "["})
	env.Resolve(c.Args[0]).Unparse(emit, opts, env)
	t := env.Resolve(c.Args[1])
	for {
		if l, ok := t.(*Compound); ok && l.Functor == "." && len(l.Args) == 2 {
			emit(Token{Kind: TokenComma, Val: ","})
			env.Resolve(l.Args[0]).Unparse(emit, opts, env)
			t = env.Resolve(l.Args[1])
			continue
		}
		if a, ok := t.(Atom); ok && a == "[]" {
			break
		}
		emit(Token{Kind: TokenBar, Val: "|"})
		t.Unparse(emit, opts, env)
		break
	}
	emit(Token{Kind: TokenBracketR, Val: "]"})
}

func (c *Compound) unparseBlock(emit func(Token), opts WriteTermOptions, env *Env) {
	emit(Token{Kind: TokenBraceL, Val: "{"})
	env.Resolve(c.Args[0]).Unparse(emit, opts, env)
	emit(Token{Kind: TokenBraceR, Val: "}"})
}

func (c *Compound) unparseNumberVar(n Integer, emit func(Token)) {
	const letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	i, j := int(n)%len(letters), int(n)/len(letters)
	if j == 0 {
		s := string(letters[i])
		emit(Token{Kind: TokenVariable, Val: s})
		return
	}
	s := fmt.Sprintf("%s%d", string(letters[i]), j)
	emit(Token{Kind: TokenVariable, Val: s})
}

func (c *Compound) unparse(emit func(Token), opts WriteTermOptions, env *Env) {
	c.Functor.Unparse(emit, opts, env)
	emit(Token{Kind: TokenParenL, Val: "("})
	env.Resolve(c.Args[0]).Unparse(emit, opts, env)
	for _, arg := range c.Args[1:] {
		emit(Token{Kind: TokenComma, Val: ","})
		env.Resolve(arg).Unparse(emit, opts, env)
	}
	emit(Token{Kind: TokenParenR, Val: ")"})
}

// Cons returns a list consists of a first element car and the rest cdr.
func Cons(car, cdr Term) Term {
	return &Compound{
		Functor: ".",
		Args:    []Term{car, cdr},
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
func Set(ts ...Term) Term {
	if len(ts) < 2 {
		return List(ts...)
	}
	us := make([]Term, len(ts))
	copy(us, ts)
	sort.Slice(us, func(i, j int) bool {
		return compare(us[i], us[j], nil) < 0
	})
	n := 1
	for _, u := range us[1:] {
		if compare(us[n-1], u, nil) == 0 {
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

// EachList iterates over list.
func EachList(list Term, f func(elem Term) error, env *Env) error {
	whole := list
	for {
		switch l := env.Resolve(list).(type) {
		case Variable:
			return InstantiationError(whole)
		case Atom:
			if l != "[]" {
				return typeErrorList(l)
			}
			return nil
		case *Compound:
			if l.Functor != "." || len(l.Args) != 2 {
				return typeErrorList(l)
			}
			if err := f(l.Args[0]); err != nil {
				return err
			}
			list = l.Args[1]
		default:
			return typeErrorList(l)
		}
	}
}

// Slice returns a Term slice containing the elements of list.
// It errors if the given Term is not a list.
func Slice(list Term, env *Env) (ret []Term, err error) {
	err = EachList(list, func(elem Term) error {
		ret = append(ret, env.Resolve(elem))
		return nil
	}, env)
	return
}

// Seq returns a sequence of ts separated by sep.
func Seq(sep Atom, ts ...Term) Term {
	s, ts := ts[len(ts)-1], ts[:len(ts)-1]
	for i := len(ts) - 1; i >= 0; i-- {
		s = &Compound{
			Functor: sep,
			Args:    []Term{ts[i], s},
		}
	}
	return s
}

// EachSeq iterates over a sequence seq separated by sep.
func EachSeq(seq Term, sep Atom, f func(elem Term) error, env *Env) error {
	for {
		p, ok := env.Resolve(seq).(*Compound)
		if !ok || p.Functor != sep || len(p.Args) != 2 {
			break
		}
		if err := f(p.Args[0]); err != nil {
			return err
		}
		seq = p.Args[1]
	}
	return f(seq)
}

// Each iterates over either a list or comma-delimited sequence.
func Each(any Term, f func(elem Term) error, env *Env) error {
	if c, ok := env.Resolve(any).(*Compound); ok && c.Functor == "." && len(c.Args) == 2 {
		return EachList(any, f, env)
	}
	return EachSeq(any, ",", f, env)
}
