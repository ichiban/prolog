package engine

import (
	"fmt"
	"io"
	"sort"
	"strconv"
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

func (c *Compound) WriteTerm(w io.Writer, opts *WriteOptions, env *Env) error {
	if opts.visited == nil {
		opts.visited = map[Term]struct{}{}
	}
	if _, ok := opts.visited[c]; ok {
		_, err := fmt.Fprint(w, "...")
		return err
	}
	opts.visited[c] = struct{}{}

	if n, ok := env.Resolve(c.Args[0]).(Integer); ok && opts.NumberVars && c.Functor == "$VAR" && len(c.Args) == 1 {
		return c.writeTermNumberVars(w, n)
	}

	if c.Functor == "." && len(c.Args) == 2 && !opts.IgnoreOps {
		return c.writeTermList(w, opts, env)
	}

	if c.Functor == "{}" && len(c.Args) == 1 && !opts.IgnoreOps {
		return c.writeTermCurlyBracketed(w, opts, env)
	}

	var op *operator
	for _, o := range opts.ops {
		if o.name == c.Functor && o.specifier.arity() == len(c.Args) {
			op = &o
			break
		}
	}

	if op == nil || opts.IgnoreOps {
		return c.writeTermFunctionalNotation(w, opts, env)
	}

	return c.writeTermOp(w, opts, env, op)
}

func (c *Compound) writeTermNumberVars(w io.Writer, n Integer) error {
	const letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	i, j := int(n)%len(letters), int(n)/len(letters)
	s := string(letters[i])
	if j != 0 {
		s += strconv.Itoa(j)
	}
	_, err := fmt.Fprint(w, s)
	return err
}

func (c *Compound) writeTermList(w io.Writer, opts *WriteOptions, env *Env) error {
	if _, err := fmt.Fprint(w, "["); err != nil {
		return err
	}
	if err := c.Args[0].WriteTerm(w, opts.withBefore(operator{}), env); err != nil {
		return err
	}
	iter := ListIterator{List: c.Args[1], Env: env}
	for iter.Next() {
		if _, err := fmt.Fprint(w, ","); err != nil {
			return err
		}
		if err := iter.Current().WriteTerm(w, opts.withBefore(operator{}), env); err != nil {
			return err
		}
	}
	switch s := iter.Suffix().(type) {
	case Atom:
		if s == "[]" {
			break
		}
		if _, err := fmt.Fprint(w, "|"); err != nil {
			return err
		}
		if err := s.WriteTerm(w, opts.withBefore(operator{}), env); err != nil {
			return err
		}
	case *Compound:
		if _, err := fmt.Fprint(w, "|"); err != nil {
			return err
		}
		if c.Functor == "." && len(c.Args) == 2 {
			if _, err := fmt.Fprint(w, "..."); err != nil {
				return err
			}
			break
		}
		if err := s.WriteTerm(w, opts.withBefore(operator{}), env); err != nil {
			return err
		}
	default:
		if _, err := fmt.Fprint(w, "|"); err != nil {
			return err
		}
		if err := s.WriteTerm(w, opts.withBefore(operator{}), env); err != nil {
			return err
		}
	}
	_, err := fmt.Fprint(w, "]")
	return err
}

func (c *Compound) writeTermCurlyBracketed(w io.Writer, opts *WriteOptions, env *Env) error {
	if _, err := fmt.Fprint(w, "{"); err != nil {
		return err
	}
	if err := c.Args[0].WriteTerm(w, opts.withBefore(operator{}), env); err != nil {
		return err
	}
	_, err := fmt.Fprint(w, "}")
	return err
}

func (c *Compound) writeTermFunctionalNotation(w io.Writer, opts *WriteOptions, env *Env) error {
	if err := c.Functor.WriteTerm(w, opts, env); err != nil {
		return err
	}
	if _, err := fmt.Fprint(w, "("); err != nil {
		return err
	}
	for i, a := range c.Args {
		if i != 0 {
			if _, err := fmt.Fprint(w, ","); err != nil {
				return err
			}
		}
		if err := a.WriteTerm(w, opts.withBefore(operator{}), env); err != nil {
			return err
		}
	}
	_, err := fmt.Fprint(w, ")")
	return err
}

func (c *Compound) writeTermOp(w io.Writer, opts *WriteOptions, env *Env, op *operator) error {
	opts = opts.withFreshVisited()
	l, r := op.bindingPriorities()
	openClose := opts.priority < op.priority

	switch op.specifier {
	case operatorSpecifierFX, operatorSpecifierFY:
		if opts.before != (operator{}) {
			if _, err := fmt.Fprint(w, " "); err != nil {
				return err
			}
		}
		if openClose {
			if _, err := fmt.Fprint(w, "("); err != nil {
				return err
			}
		}
		if _, err := fmt.Fprint(w, c.Functor); err != nil {
			return err
		}
		if err := c.Args[0].WriteTerm(w, opts.withPriority(r).withBefore(*op).withAfter(operator{}), env); err != nil {
			return err
		}
		if openClose {
			if _, err := fmt.Fprint(w, ")"); err != nil {
				return err
			}
		}
	case operatorSpecifierXF, operatorSpecifierYF:
		openClose = openClose || opts.before.name == "-" && opts.before.specifier == operatorSpecifierFX || opts.before.specifier == operatorSpecifierFY
		if openClose {
			if opts.before != (operator{}) {
				if _, err := fmt.Fprint(w, " "); err != nil {
					return err
				}
			}
			if _, err := fmt.Fprint(w, "("); err != nil {
				return err
			}
		}
		if err := c.Args[0].WriteTerm(w, opts.withPriority(l).withBefore(operator{}).withAfter(*op), env); err != nil {
			return err
		}
		if _, err := fmt.Fprint(w, c.Functor); err != nil {
			return err
		}
		if openClose {
			if _, err := fmt.Fprint(w, ")"); err != nil {
				return err
			}
		} else if opts.after != (operator{}) {
			if _, err := fmt.Fprint(w, " "); err != nil {
				return err
			}
		}
	case operatorSpecifierXFX, operatorSpecifierXFY, operatorSpecifierYFX:
		openClose = openClose || opts.before.name == "-" && opts.before.specifier == operatorSpecifierFX || opts.before.specifier == operatorSpecifierFY
		if openClose {
			if opts.before.name != "" && (opts.before.specifier == operatorSpecifierFX || opts.before.specifier == operatorSpecifierFY) {
				if _, err := fmt.Fprint(w, " "); err != nil {
					return err
				}
			}
			if _, err := fmt.Fprint(w, "("); err != nil {
				return err
			}
		}
		if err := c.Args[0].WriteTerm(w, opts.withPriority(l).withBefore(operator{}).withAfter(*op), env); err != nil {
			return err
		}
		if _, err := fmt.Fprint(w, c.Functor); err != nil {
			return err
		}
		if err := c.Args[1].WriteTerm(w, opts.withPriority(r).withBefore(*op).withAfter(operator{}), env); err != nil {
			return err
		}
		if openClose {
			if _, err := fmt.Fprint(w, ")"); err != nil {
				return err
			}
		}
	}

	return nil
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
