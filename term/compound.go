package term

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"sort"

	"github.com/ichiban/prolog/syntax"
)

// Compound is a prolog compound.
type Compound struct {
	Functor Atom
	Args    []Interface
}

func (c *Compound) String() string {
	var buf bytes.Buffer
	_ = c.WriteTerm(&buf, DefaultWriteTermOptions, nil)
	return buf.String()
}

// WriteTerm writes the compound into w.
func (c *Compound) WriteTerm(w io.Writer, opts WriteTermOptions, env *Env) error {
	if c.Functor == "." && len(c.Args) == 2 { // list
		if _, err := fmt.Fprint(w, "["); err != nil {
			return err
		}
		if err := env.Resolve(c.Args[0]).WriteTerm(w, opts, env); err != nil {
			return err
		}
		t := env.Resolve(c.Args[1])
		for {
			if l, ok := t.(*Compound); ok && l.Functor == "." && len(l.Args) == 2 {
				if _, err := fmt.Fprint(w, ", "); err != nil {
					return err
				}
				if err := env.Resolve(l.Args[0]).WriteTerm(w, opts, env); err != nil {
					return err
				}
				t = env.Resolve(l.Args[1])
				continue
			}
			if a, ok := t.(Atom); ok && a == "[]" {
				break
			}
			if _, err := fmt.Fprint(w, "|"); err != nil {
				return err
			}
			if err := t.WriteTerm(w, opts, env); err != nil {
				return err
			}
			break
		}
		_, err := fmt.Fprint(w, "]")
		return err
	}

	switch len(c.Args) {
	case 1:
		for _, o := range opts.Ops {
			if o.Name != c.Functor {
				continue
			}
			switch o.Specifier {
			case `xf`, `yf`:
				var lb, fb bytes.Buffer
				if err := env.Resolve(c.Args[0]).WriteTerm(&lb, opts, env); err != nil {
					return err
				}
				if err := c.Functor.WriteTerm(&fb, opts, env); err != nil {
					return err
				}

				l := []rune(lb.String())
				f := []rune(fb.String())
				if syntax.IsExtendedGraphic(l[len(l)-1]) && syntax.IsExtendedGraphic(f[0]) {
					_, err := fmt.Fprintf(w, "(%s)%s", string(l), string(f))
					return err
				}
				_, err := fmt.Fprintf(w, "%s%s", string(l), string(f))
				return err
			case `fx`, `fy`:
				var fb, rb bytes.Buffer
				if err := c.Functor.WriteTerm(&fb, opts, env); err != nil {
					return err
				}
				if err := env.Resolve(c.Args[0]).WriteTerm(&rb, opts, env); err != nil {
					return err
				}

				f := []rune(fb.String())
				r := []rune(rb.String())
				if syntax.IsExtendedGraphic(f[len(f)-1]) && syntax.IsExtendedGraphic(r[0]) {
					_, err := fmt.Fprintf(w, "%s(%s)", string(f), string(r))
					return err
				}
				_, err := fmt.Fprintf(w, "%s%s", string(f), string(r))
				return err
			}
		}
	case 2:
		for _, o := range opts.Ops {
			if o.Name != c.Functor {
				continue
			}
			switch o.Specifier {
			case `xfx`, `xfy`, `yfx`:
				var lb, fb, rb bytes.Buffer
				lt := env.Resolve(c.Args[0])
				if err := lt.WriteTerm(&lb, opts, env); err != nil {
					return err
				}
				if err := c.Functor.WriteTerm(&fb, opts, env); err != nil {
					return err
				}
				if err := env.Resolve(c.Args[1]).WriteTerm(&rb, opts, env); err != nil {
					return err
				}

				l := []rune(lb.String())
				f := []rune(fb.String())
				r := []rune(rb.String())
				switch {
				case len(l) > 0 && syntax.IsExtendedGraphic(l[len(l)-1]) && len(f) > 0 && syntax.IsExtendedGraphic(f[0]) && syntax.IsExtendedGraphic(f[len(f)-1]) && len(r) > 0 && syntax.IsExtendedGraphic(r[0]):
					_, err := fmt.Fprintf(w, "(%s)%s(%s)", string(l), string(f), string(r))
					return err
				case len(l) > 0 && syntax.IsExtendedGraphic(l[len(l)-1]) && len(f) > 0 && syntax.IsExtendedGraphic(f[0]):
					_, err := fmt.Fprintf(w, "(%s)%s%s", string(l), string(f), string(r))
					return err
				case len(f) > 0 && syntax.IsExtendedGraphic(f[len(f)-1]) && len(r) > 0 && syntax.IsExtendedGraphic(r[0]):
					_, err := fmt.Fprintf(w, "%s%s(%s)", string(l), string(f), string(r))
					return err
				default:
					_, err := fmt.Fprintf(w, "%s%s%s", string(l), string(f), string(r))
					return err
				}
			}
		}
	}

	if opts.NumberVars && c.Functor == "$VAR" && len(c.Args) == 1 {
		switch n := env.Resolve(c.Args[0]).(type) {
		case Integer:
			const letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
			i, j := int(n)%len(letters), int(n)/len(letters)
			if j == 0 {
				_, err := fmt.Fprintf(w, "%s", string(letters[i]))
				return err
			}
			_, err := fmt.Fprintf(w, "%s%d", string(letters[i]), j)
			return err
		default:
			return errors.New("not an integer")
		}
	}

	if err := c.Functor.WriteTerm(w, opts, env); err != nil {
		return err
	}
	if _, err := fmt.Fprint(w, "("); err != nil {
		return err
	}
	if err := env.Resolve(c.Args[0]).WriteTerm(w, opts, env); err != nil {
		return err
	}
	for _, arg := range c.Args[1:] {
		if _, err := fmt.Fprint(w, ", "); err != nil {
			return err
		}
		if err := env.Resolve(arg).WriteTerm(w, opts, env); err != nil {
			return err
		}
	}
	_, err := fmt.Fprint(w, ")")
	return err
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
