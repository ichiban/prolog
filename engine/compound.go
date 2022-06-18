package engine

import (
	"fmt"
	"sort"
)

// Compound is a prolog compound.
type Compound struct {
	Functor Atom
	Args    []Term
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
func (c *Compound) Unparse(emit func(Token), env *Env, opts ...WriteOption) {
	wo := defaultWriteOptions
	for _, o := range opts {
		o(&wo)
	}

	if c.Functor == "." && len(c.Args) == 2 {
		c.unparseList(emit, env, opts...)
		return
	}

	if c.Functor == "{}" && len(c.Args) == 1 {
		c.unparseBlock(emit, env, opts...)
		return
	}

	switch len(c.Args) {
	case 1:
		for _, op := range wo.ops {
			if op.name != c.Functor {
				continue
			}
			switch op.specifier {
			case operatorSpecifierFX:
				c.unparseFX(op, emit, env, opts...)
				return
			case operatorSpecifierFY:
				c.unparseFY(op, emit, env, opts...)
				return
			case operatorSpecifierXF:
				c.unparseXF(op, emit, env, opts...)
				return
			case operatorSpecifierYF:
				c.unparseYF(op, emit, env, opts...)
				return
			}
		}
	case 2:
		for _, op := range wo.ops {
			if op.name != c.Functor {
				continue
			}
			switch op.specifier {
			case operatorSpecifierXFX:
				c.unparseXFX(op, emit, env, opts...)
				return
			case operatorSpecifierXFY:
				c.unparseXFY(op, emit, env, opts...)
				return
			case operatorSpecifierYFX:
				c.unparseYFX(op, emit, env, opts...)
				return
			}
		}
	}

	if n, ok := env.Resolve(c.Args[0]).(Integer); ok && wo.numberVars && c.Functor == "$VAR" && len(c.Args) == 1 {
		c.unparseNumberVar(n, emit)
		return
	}

	c.unparse(emit, env, opts...)
}

func (c *Compound) unparseFX(op operator, emit func(Token), env *Env, opts ...WriteOption) {
	wto := defaultWriteOptions
	for _, o := range opts {
		o(&wto)
	}

	if int(op.priority) > wto.priority {
		emit(Token{Kind: TokenOpen, Val: "("})
		defer emit(Token{Kind: TokenClose, Val: ")"})
	}
	c.Functor.Unparse(emit, env, opts...)
	env.Resolve(c.Args[0]).Unparse(emit, env, append(opts, WithPriority(int(op.priority-1)))...)
}

func (c *Compound) unparseFY(op operator, emit func(Token), env *Env, opts ...WriteOption) {
	wto := defaultWriteOptions
	for _, o := range opts {
		o(&wto)
	}

	if int(op.priority) > wto.priority {
		emit(Token{Kind: TokenOpen, Val: "("})
		defer emit(Token{Kind: TokenClose, Val: ")"})
	}
	c.Functor.Unparse(emit, env, opts...)
	env.Resolve(c.Args[0]).Unparse(emit, env, append(opts, WithPriority(int(op.priority)))...)
}

func (c *Compound) unparseXF(op operator, emit func(Token), env *Env, opts ...WriteOption) {
	wto := defaultWriteOptions
	for _, o := range opts {
		o(&wto)
	}

	if int(op.priority) > wto.priority {
		emit(Token{Kind: TokenOpen, Val: "("})
		defer emit(Token{Kind: TokenClose, Val: ")"})
	}
	env.Resolve(c.Args[0]).Unparse(emit, env, append(opts, WithPriority(int(op.priority-1)))...)
	c.Functor.Unparse(emit, env, opts...)
}

func (c *Compound) unparseYF(op operator, emit func(Token), env *Env, opts ...WriteOption) {
	wto := defaultWriteOptions
	for _, o := range opts {
		o(&wto)
	}

	if int(op.priority) > wto.priority {
		emit(Token{Kind: TokenOpen, Val: "("})
		defer emit(Token{Kind: TokenClose, Val: ")"})
	}
	env.Resolve(c.Args[0]).Unparse(emit, env, append(opts, WithPriority(int(op.priority)))...)
	c.Functor.Unparse(emit, env, opts...)
}

func (c *Compound) unparseXFX(op operator, emit func(Token), env *Env, opts ...WriteOption) {
	wto := defaultWriteOptions
	for _, o := range opts {
		o(&wto)
	}

	if int(op.priority) > wto.priority {
		emit(Token{Kind: TokenOpen, Val: "("})
		defer emit(Token{Kind: TokenClose, Val: ")"})
	}
	env.Resolve(c.Args[0]).Unparse(emit, env, append(opts, WithPriority(int(op.priority)-1))...)
	c.Functor.Unparse(emit, env, opts...)
	env.Resolve(c.Args[1]).Unparse(emit, env, append(opts, WithPriority(int(op.priority)-1))...)
}

func (c *Compound) unparseXFY(op operator, emit func(Token), env *Env, opts ...WriteOption) {
	wto := defaultWriteOptions
	for _, o := range opts {
		o(&wto)
	}

	if int(op.priority) > wto.priority {
		emit(Token{Kind: TokenOpen, Val: "("})
		defer emit(Token{Kind: TokenClose, Val: ")"})
	}
	env.Resolve(c.Args[0]).Unparse(emit, env, append(opts, WithPriority(int(op.priority)-1))...)
	c.Functor.Unparse(emit, env, opts...)
	env.Resolve(c.Args[1]).Unparse(emit, env, append(opts, WithPriority(int(op.priority)))...)
}

func (c *Compound) unparseYFX(op operator, emit func(Token), env *Env, opts ...WriteOption) {
	wto := defaultWriteOptions
	for _, o := range opts {
		o(&wto)
	}

	if int(op.priority) > wto.priority {
		emit(Token{Kind: TokenOpen, Val: "("})
		defer emit(Token{Kind: TokenClose, Val: ")"})
	}
	env.Resolve(c.Args[0]).Unparse(emit, env, append(opts, WithPriority(int(op.priority)))...)
	c.Functor.Unparse(emit, env, opts...)
	env.Resolve(c.Args[1]).Unparse(emit, env, append(opts, WithPriority(int(op.priority)-1))...)
}

func (c *Compound) unparseList(emit func(Token), env *Env, opts ...WriteOption) {
	wto := defaultWriteOptions
	for _, o := range opts {
		o(&wto)
	}

	var comma bool
	emit(Token{Kind: TokenOpenList, Val: "["})
	iter := ListIterator{List: c, Env: env}
	for iter.Next() {
		if comma {
			emit(Token{Kind: TokenComma, Val: ","})
		}
		env.Resolve(iter.Current()).Unparse(emit, env, opts...)
		comma = true
	}
	if err := iter.Err(); err != nil {
		emit(Token{Kind: TokenBar, Val: "|"})

		suffix := iter.Suffix()
		if c, ok := suffix.(*Compound); ok && c.Functor == "." && len(c.Args) == 2 {
			emit(Token{Kind: TokenGraphic, Val: "..."})
		} else {
			suffix.Unparse(emit, env, opts...)
		}
	}
	emit(Token{Kind: TokenCloseList, Val: "]"})
}

func (c *Compound) unparseBlock(emit func(Token), env *Env, opts ...WriteOption) {
	emit(Token{Kind: TokenOpenCurly, Val: "{"})
	env.Resolve(c.Args[0]).Unparse(emit, env, opts...)
	emit(Token{Kind: TokenCloseCurly, Val: "}"})
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

func (c *Compound) unparse(emit func(Token), env *Env, opts ...WriteOption) {
	c.Functor.Unparse(emit, env, opts...)
	emit(Token{Kind: TokenOpen, Val: "("})
	env.Resolve(c.Args[0]).Unparse(emit, env, opts...)
	for _, arg := range c.Args[1:] {
		emit(Token{Kind: TokenComma, Val: ","})
		env.Resolve(arg).Unparse(emit, env, opts...)
	}
	emit(Token{Kind: TokenClose, Val: ")"})
}

// Compare compares the compound to another term.
func (c *Compound) Compare(t Term, env *Env) int64 {
	switch t := env.Resolve(t).(type) {
	case *Compound:
		if d := len(c.Args) - len(t.Args); d != 0 {
			return int64(d)
		}

		if d := c.Functor.Compare(t.Functor, env); d != 0 {
			return d
		}

		for i, a := range c.Args {
			if d := a.Compare(t.Args[i], env); d != 0 {
				return d
			}
		}

		return 0
	default:
		return 1
	}
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
func (e *Env) Set(ts ...Term) Term {
	sort.Slice(ts, func(i, j int) bool {
		return ts[i].Compare(ts[j], e) < 0
	})
	us := make([]Term, 0, len(ts))
	for _, t := range ts {
		if len(us) > 0 && us[len(us)-1].Compare(t, e) == 0 {
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
		s = &Compound{
			Functor: sep,
			Args:    []Term{ts[i], s},
		}
	}
	return s
}

// Pair returns a pair of k and v.
func Pair(k, v Term) Term {
	return &Compound{
		Functor: "-",
		Args:    []Term{k, v},
	}
}
