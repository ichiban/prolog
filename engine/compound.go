package engine

import (
	"fmt"
	"io"
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

// WriteTerm writes the Compound to the io.Writer.
func (c *Compound) WriteTerm(w io.Writer, opts *WriteOptions, env *Env) error {
	ok, err := c.visit(w, opts)
	if err != nil || ok {
		return err
	}

	if n, ok := env.Resolve(c.Args[0]).(Integer); ok && opts.NumberVars && c.Functor == "$VAR" && len(c.Args) == 1 && n >= 0 {
		return c.writeTermNumberVars(w, n)
	}

	if !opts.IgnoreOps {
		if c.Functor == "." && len(c.Args) == 2 {
			return c.writeTermList(w, opts, env)
		}

		if c.Functor == "{}" && len(c.Args) == 1 {
			return c.writeTermCurlyBracketed(w, opts, env)
		}
	}

	if opts.IgnoreOps {
		return c.writeTermFunctionalNotation(w, opts, env)
	}

	for _, o := range opts.ops {
		if o.name == c.Functor && o.specifier.arity() == len(c.Args) {
			return c.writeTermOp(w, opts, env, &o)
		}
	}

	return c.writeTermFunctionalNotation(w, opts, env)
}

func (c *Compound) visit(w io.Writer, opts *WriteOptions) (bool, error) {
	if opts.visited == nil {
		opts.visited = map[Term]struct{}{}
	}

	if _, ok := opts.visited[c]; ok {
		_, err := fmt.Fprint(w, "...")
		return true, err
	}
	opts.visited[c] = struct{}{}
	return false, nil
}

func (c *Compound) writeTermNumberVars(w io.Writer, n Integer) error {
	const letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	ew := errWriter{w: w}
	i, j := int(n)%len(letters), int(n)/len(letters)
	_, _ = fmt.Fprint(&ew, string(letters[i]))
	if j != 0 {
		_, _ = fmt.Fprint(&ew, j)
	}
	return ew.err
}

func (c *Compound) writeTermList(w io.Writer, opts *WriteOptions, env *Env) error {
	ew := errWriter{w: w}
	opts = opts.withPriority(999).withLeft(operator{}).withRight(operator{})
	_, _ = fmt.Fprint(&ew, "[")
	_ = c.Args[0].WriteTerm(&ew, opts, env)
	iter := ListIterator{List: c.Args[1], Env: env}
	for iter.Next() {
		_, _ = fmt.Fprint(&ew, ",")
		_ = iter.Current().WriteTerm(&ew, opts, env)
	}
	if err := iter.Err(); err != nil {
		_, _ = fmt.Fprint(&ew, "|")
		s := iter.Suffix()
		if l, ok := iter.Suffix().(*Compound); ok && l.Functor == "." && len(l.Args) == 2 {
			_, _ = fmt.Fprint(&ew, "...")
		} else {
			_ = s.WriteTerm(&ew, opts, env)
		}
	}
	_, _ = fmt.Fprint(&ew, "]")
	return ew.err
}

func (c *Compound) writeTermCurlyBracketed(w io.Writer, opts *WriteOptions, env *Env) error {
	ew := errWriter{w: w}
	_, _ = fmt.Fprint(&ew, "{")
	_ = c.Args[0].WriteTerm(&ew, opts.withLeft(operator{}), env)
	_, _ = fmt.Fprint(&ew, "}")
	return ew.err
}

func (c *Compound) writeTermFunctionalNotation(w io.Writer, opts *WriteOptions, env *Env) error {
	ew := errWriter{w: w}
	opts = opts.withRight(operator{})
	_ = c.Functor.WriteTerm(&ew, opts, env)
	_, _ = fmt.Fprint(&ew, "(")
	opts = opts.withLeft(operator{}).withPriority(999)
	for i, a := range c.Args {
		if i != 0 {
			_, _ = fmt.Fprint(&ew, ",")
		}
		_ = a.WriteTerm(&ew, opts, env)
	}
	_, _ = fmt.Fprint(&ew, ")")
	return ew.err
}

var writeTermOps = [...]func(c *Compound, w io.Writer, opts *WriteOptions, env *Env, op *operator) error{
	operatorSpecifierFX:  (*Compound).writeTermOpPrefix,
	operatorSpecifierFY:  (*Compound).writeTermOpPrefix,
	operatorSpecifierXF:  (*Compound).writeTermOpPostfix,
	operatorSpecifierYF:  (*Compound).writeTermOpPostfix,
	operatorSpecifierXFX: (*Compound).writeTermOpInfix,
	operatorSpecifierXFY: (*Compound).writeTermOpInfix,
	operatorSpecifierYFX: (*Compound).writeTermOpInfix,
}

func (c *Compound) writeTermOp(w io.Writer, opts *WriteOptions, env *Env, op *operator) error {
	return writeTermOps[op.specifier](c, w, opts, env, op)
}

func (c *Compound) writeTermOpPrefix(w io.Writer, opts *WriteOptions, env *Env, op *operator) error {
	ew := errWriter{w: w}
	opts = opts.withFreshVisited()
	_, r := op.bindingPriorities()
	openClose := opts.priority < op.priority || (opts.right != operator{} && r >= opts.right.priority)

	if opts.left != (operator{}) {
		_, _ = fmt.Fprint(&ew, " ")
	}
	if openClose {
		_, _ = fmt.Fprint(&ew, "(")
		opts = opts.withLeft(operator{}).withRight(operator{})
	}
	_ = c.Functor.WriteTerm(&ew, opts.withLeft(operator{}).withRight(operator{}), env)
	_ = c.Args[0].WriteTerm(&ew, opts.withPriority(r).withLeft(*op), env)
	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}
	return ew.err
}

func (c *Compound) writeTermOpPostfix(w io.Writer, opts *WriteOptions, env *Env, op *operator) error {
	ew := errWriter{w: w}
	opts = opts.withFreshVisited()
	l, _ := op.bindingPriorities()
	openClose := opts.priority < op.priority || (opts.left.name == "-" && opts.left.specifier&operatorSpecifierClass == operatorSpecifierPrefix)

	if openClose {
		if opts.left != (operator{}) {
			_, _ = fmt.Fprint(&ew, " ")
		}
		_, _ = fmt.Fprint(&ew, "(")
		opts = opts.withLeft(operator{}).withRight(operator{})
	}
	_ = c.Args[0].WriteTerm(&ew, opts.withPriority(l).withRight(*op), env)
	_ = c.Functor.WriteTerm(&ew, opts.withLeft(operator{}).withRight(operator{}), env)
	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	} else if opts.right != (operator{}) {
		_, _ = fmt.Fprint(&ew, " ")
	}
	return ew.err
}

func (c *Compound) writeTermOpInfix(w io.Writer, opts *WriteOptions, env *Env, op *operator) error {
	ew := errWriter{w: w}
	opts = opts.withFreshVisited()
	l, r := op.bindingPriorities()
	openClose := opts.priority < op.priority ||
		(opts.left.name == "-" && opts.left.specifier&operatorSpecifierClass == operatorSpecifierPrefix) ||
		(opts.right != operator{} && r >= opts.right.priority)

	if openClose {
		if opts.left.name != "" && opts.left.specifier&operatorSpecifierClass == operatorSpecifierPrefix {
			_, _ = fmt.Fprint(&ew, " ")
		}
		_, _ = fmt.Fprint(&ew, "(")
		opts = opts.withLeft(operator{}).withRight(operator{})
	}
	_ = c.Args[0].WriteTerm(&ew, opts.withPriority(l).withRight(*op), env)
	switch c.Functor {
	case ",", "|":
		_, _ = fmt.Fprint(&ew, c.Functor)
	default:
		_ = c.Functor.WriteTerm(&ew, opts.withLeft(operator{}).withRight(operator{}), env)
	}
	_ = c.Args[1].WriteTerm(&ew, opts.withPriority(r).withLeft(*op), env)
	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}
	return ew.err
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
