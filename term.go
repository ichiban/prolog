package prolog

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"regexp"
	"sort"
	"strconv"
	"strings"

	"github.com/ichiban/prolog/internal"
)

// Term is a prolog term.
type Term interface {
	fmt.Stringer
	WriteTerm(io.Writer, WriteTermOptions) error
	Unify(Term, bool) bool
	Copy() Term
}

// Variable is a prolog variable.
type Variable struct {
	Name string
	Ref  Term
}

func (v *Variable) String() string {
	var buf bytes.Buffer
	_ = v.WriteTerm(&buf, defaultWriteTermOptions)
	return buf.String()
}

// WriteTerm writes the variable into w.
func (v *Variable) WriteTerm(w io.Writer, opts WriteTermOptions) error {
	if opts.Descriptive && v.Ref != nil {
		if v.Name != "" {
			if _, err := fmt.Fprintf(w, "%s = ", v.Name); err != nil {
				return err
			}
		}
		return v.Ref.WriteTerm(w, opts)
	}
	if v.Name == "" {
		_, err := fmt.Fprintf(w, "_%p", v)
		return err
	}
	_, err := fmt.Fprint(w, v.Name)
	return err
}

// Unify unifies the variable with t.
func (v *Variable) Unify(t Term, occursCheck bool) bool {
	t = Resolve(t)
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

// Copy returns a fresh variable with a copy of Ref.
func (v *Variable) Copy() Term {
	if v.Ref == nil {
		return &Variable{}
	}
	return &Variable{Ref: v.Ref.Copy()}
}

// Float is a prolog floating-point number.
type Float float64

func (f Float) String() string {
	var buf bytes.Buffer
	_ = f.WriteTerm(&buf, defaultWriteTermOptions)
	return buf.String()
}

// WriteTerm writes the float into w.
func (f Float) WriteTerm(w io.Writer, _ WriteTermOptions) error {
	_, err := fmt.Fprint(w, strconv.FormatFloat(float64(f), 'f', -1, 64))
	return err
}

// Unify unifies the float with t.
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

// Copy simply returns the float.
func (f Float) Copy() Term {
	return f
}

// Integer is a prolog integer.
type Integer int64

func (i Integer) String() string {
	var buf bytes.Buffer
	_ = i.WriteTerm(&buf, defaultWriteTermOptions)
	return buf.String()
}

// WriteTerm writes the integer into w.
func (i Integer) WriteTerm(w io.Writer, _ WriteTermOptions) error {
	_, err := fmt.Fprint(w, strconv.FormatInt(int64(i), 10))
	return err
}

// Unify unifies the integer with t.
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

// Copy simply returns the integer.
func (i Integer) Copy() Term {
	return i
}

// Atom is a prolog atom.
type Atom string

func (a Atom) String() string {
	var buf bytes.Buffer
	_ = a.WriteTerm(&buf, defaultWriteTermOptions)
	return buf.String()
}

var unquotedAtomPattern = regexp.MustCompile(`\A(?:[a-z]\w*|[#$&*+\-./:<=>?@^~\\]+|\[])\z`)
var quotedAtomEscapePattern = regexp.MustCompile("[[:cntrl:]]|\\\\|'|\"|`")

// WriteTerm writes the atom into w.
func (a Atom) WriteTerm(w io.Writer, opts WriteTermOptions) error {
	if !opts.Quoted || unquotedAtomPattern.MatchString(string(a)) {
		_, err := fmt.Fprint(w, string(a))
		return err
	}

	s := quotedAtomEscapePattern.ReplaceAllStringFunc(string(a), func(s string) string {
		switch s {
		case "\a":
			return `\a`
		case "\b":
			return `\b`
		case "\f":
			return `\f`
		case "\n":
			return `\n`
		case "\r":
			return `\r`
		case "\t":
			return `\t`
		case "\v":
			return `\v`
		case `\`:
			return `\\`
		case `'`:
			return `\'`
		case `"`:
			return `\"`
		case "`":
			return "\\`"
		default:
			var ret []string
			for _, r := range s {
				ret = append(ret, fmt.Sprintf(`\x%x\`, r))
			}
			return strings.Join(ret, "")
		}
	})

	_, err := fmt.Fprintf(w, "'%s'", s)
	return err
}

// Unify unifies the atom with t.
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

// Copy simply returns the atom.
func (a Atom) Copy() Term {
	return a
}

// Compound is a prolog compound.
type Compound struct {
	Functor Atom
	Args    []Term
}

func (c *Compound) String() string {
	var buf bytes.Buffer
	_ = c.WriteTerm(&buf, defaultWriteTermOptions)
	return buf.String()
}

// WriteTerm writes the compound into w.
func (c *Compound) WriteTerm(w io.Writer, opts WriteTermOptions) error {
	if c.Functor == "." && len(c.Args) == 2 { // list
		if _, err := fmt.Fprint(w, "["); err != nil {
			return err
		}
		if err := Resolve(c.Args[0]).WriteTerm(w, opts); err != nil {
			return err
		}
		t := Resolve(c.Args[1])
		for {
			if l, ok := t.(*Compound); ok && l.Functor == "." && len(l.Args) == 2 {
				if _, err := fmt.Fprint(w, ", "); err != nil {
					return err
				}
				if err := Resolve(l.Args[0]).WriteTerm(w, opts); err != nil {
					return err
				}
				t = Resolve(l.Args[1])
				continue
			}
			if a, ok := t.(Atom); ok && a == "[]" {
				break
			}
			if _, err := fmt.Fprint(w, "|"); err != nil {
				return err
			}
			if err := t.WriteTerm(w, opts); err != nil {
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
				if err := Resolve(c.Args[0]).WriteTerm(&lb, opts); err != nil {
					return err
				}
				if err := c.Functor.WriteTerm(&fb, opts); err != nil {
					return err
				}

				l := []rune(lb.String())
				f := []rune(fb.String())
				if internal.IsExtendedGraphic(l[len(l)-1]) && internal.IsExtendedGraphic(f[0]) {
					_, err := fmt.Fprintf(w, "(%s)%s", string(l), string(f))
					return err
				}
				_, err := fmt.Fprintf(w, "%s%s", string(l), string(f))
				return err
			case `fx`, `fy`:
				var fb, rb bytes.Buffer
				if err := c.Functor.WriteTerm(&fb, opts); err != nil {
					return err
				}
				if err := Resolve(c.Args[0]).WriteTerm(&rb, opts); err != nil {
					return err
				}

				f := []rune(fb.String())
				r := []rune(rb.String())
				if internal.IsExtendedGraphic(f[len(f)-1]) && internal.IsExtendedGraphic(r[0]) {
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
				if err := Resolve(c.Args[0]).WriteTerm(&lb, opts); err != nil {
					return err
				}
				if err := c.Functor.WriteTerm(&fb, opts); err != nil {
					return err
				}
				if err := Resolve(c.Args[1]).WriteTerm(&rb, opts); err != nil {
					return err
				}

				l := []rune(lb.String())
				f := []rune(fb.String())
				r := []rune(rb.String())
				switch {
				case internal.IsExtendedGraphic(l[len(l)-1]) && internal.IsExtendedGraphic(f[0]) && internal.IsExtendedGraphic(f[len(f)-1]) && internal.IsExtendedGraphic(r[0]):
					_, err := fmt.Fprintf(w, "(%s)%s(%s)", string(l), string(f), string(r))
					return err
				case internal.IsExtendedGraphic(l[len(l)-1]) && internal.IsExtendedGraphic(f[0]):
					_, err := fmt.Fprintf(w, "(%s)%s%s", string(l), string(f), string(r))
					return err
				case internal.IsExtendedGraphic(f[len(f)-1]) && internal.IsExtendedGraphic(r[0]):
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
		switch arg := Resolve(c.Args[0]).(type) {
		case Integer:
			const letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
			ls := []rune(letters)
			var buf strings.Builder
			for n := int(arg); n > 0; n = n / len(ls) {
				m := n % len(ls)
				if _, err := buf.WriteRune(ls[m-1]); err != nil {
					return err
				}
			}
			_, err := fmt.Fprint(w, buf.String())
			return err
		default:
			return errors.New("not an integer")
		}
	}

	if err := c.Functor.WriteTerm(w, opts); err != nil {
		return err
	}
	if _, err := fmt.Fprint(w, "("); err != nil {
		return err
	}
	if err := Resolve(c.Args[0]).WriteTerm(w, opts); err != nil {
		return err
	}
	for _, arg := range c.Args[1:] {
		if _, err := fmt.Fprint(w, ", "); err != nil {
			return err
		}
		if err := Resolve(arg).WriteTerm(w, opts); err != nil {
			return err
		}
	}
	_, err := fmt.Fprint(w, ")")
	return err
}

// Unify unifies the compound with t.
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

// Copy returns a fresh compound with copies of arguments.
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

// Each iterates over list.
func Each(list Term, f func(elem Term) error) error {
	whole := list
	for {
		switch l := Resolve(list).(type) {
		case *Variable:
			return InstantiationError(whole)
		case Atom:
			if l != "[]" {
				return TypeErrorList(l)
			}
			return nil
		case *Compound:
			if l.Functor != "." || len(l.Args) != 2 {
				return TypeErrorList(l)
			}
			if err := f(l.Args[0]); err != nil {
				return err
			}
			list = l.Args[1]
		default:
			return TypeErrorList(l)
		}
	}
}

// Resolve follows the variable chain and returns the first non-variable term or the last free variable.
func Resolve(t Term) Term {
	var stop []*Variable
	for t != nil {
		switch v := t.(type) {
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
		default:
			return v
		}
	}
	return nil
}

// Contains checks if t contains s.
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

// Rulify returns t if t is in a form of P:-Q, t:-true otherwise.
func Rulify(t Term) Term {
	t = Resolve(t)
	if c, ok := t.(*Compound); ok && c.Functor == ":-" && len(c.Args) == 2 {
		return t
	}
	return &Compound{Functor: ":-", Args: []Term{t, Atom("true")}}
}

// Stream is a prolog stream.
type Stream struct {
	io.Reader
	io.Writer
	io.Closer

	mode      Atom
	alias     Atom
	eofAction Atom
	typ       Atom
}

func (s *Stream) String() string {
	var buf bytes.Buffer
	_ = s.WriteTerm(&buf, defaultWriteTermOptions)
	return buf.String()
}

// WriteTerm writes the stream into w.
func (s *Stream) WriteTerm(w io.Writer, _ WriteTermOptions) error {
	_, err := fmt.Fprintf(w, "<stream>(%p)", s)
	return err
}

// Unify unifies the stream with t.
func (s *Stream) Unify(t Term, occursCheck bool) bool {
	switch t := t.(type) {
	case *Stream:
		return s == t
	case *Variable:
		return t.Unify(s, occursCheck)
	default:
		return false
	}
}

// Copy simply returns the stream.
func (s *Stream) Copy() Term {
	return s
}

// WriteTermOptions describes options to write terms.
type WriteTermOptions struct {
	Quoted      bool
	Ops         Operators
	NumberVars  bool
	Descriptive bool
}

var defaultWriteTermOptions = WriteTermOptions{
	Quoted: true,
	Ops: []Operator{
		{Priority: 400, Specifier: "yfx", Name: "/"}, // for principal functors
	},
	NumberVars: false,
}

type readTermOptions struct {
	singletons    *Variable
	variables     *Variable
	variableNames *Variable
}

// procedureIndicator is a specialized variant of Compound.
type procedureIndicator struct {
	name  Atom
	arity Integer
}

func (p procedureIndicator) String() string {
	var buf bytes.Buffer
	_ = p.WriteTerm(&buf, defaultWriteTermOptions)
	return buf.String()
}

func (p procedureIndicator) WriteTerm(w io.Writer, _ WriteTermOptions) error {
	_, err := fmt.Fprintf(w, "%s/%d", p.name, p.arity)
	return err
}

func (p procedureIndicator) Unify(t Term, _ bool) bool {
	pf, ok := t.(procedureIndicator)
	return ok && p.name == pf.name && p.arity == pf.arity
}

func (p procedureIndicator) Copy() Term {
	return p
}

func piArgs(t Term) (procedureIndicator, Term, error) {
	switch f := Resolve(t).(type) {
	case *Variable:
		return procedureIndicator{}, nil, InstantiationError(t)
	case Atom:
		return procedureIndicator{name: f, arity: 0}, List(), nil
	case *Compound:
		return procedureIndicator{name: f.Functor, arity: Integer(len(f.Args))}, List(f.Args...), nil
	default:
		return procedureIndicator{}, nil, TypeErrorCallable(t)
	}
}
