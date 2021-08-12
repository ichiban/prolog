package engine

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"sync"

	"github.com/ichiban/prolog/internal"
)

// Term is a prolog term.
type Term interface {
	fmt.Stringer
	WriteTerm(io.Writer, WriteTermOptions, *Env) error
	Unify(Term, bool, *Env) bool
}

// Variable is a prolog variable.
type Variable string

var (
	varCounter = 0
	varMutex   sync.Mutex
)

const anonVarPrefix = "_\u200b"

func NewVariable() Variable {
	varMutex.Lock()
	defer varMutex.Unlock()

	varCounter++
	return Variable(fmt.Sprintf("%s%d", anonVarPrefix, varCounter))
}

func (v Variable) Anonymous() bool {
	return strings.HasPrefix(string(v), anonVarPrefix)
}

func (v Variable) String() string {
	var buf bytes.Buffer
	_ = v.WriteTerm(&buf, defaultWriteTermOptions, nil)
	return buf.String()
}

// WriteTerm writes the variable into w.
func (v Variable) WriteTerm(w io.Writer, opts WriteTermOptions, env *Env) error {
	ref, ok := env.Lookup(v)
	if !ok && opts.Descriptive {
		if v != "" {
			if _, err := fmt.Fprintf(w, "%s = ", v); err != nil {
				return err
			}
		}
		return ref.WriteTerm(w, opts, env)
	}
	_, err := fmt.Fprint(w, string(v))
	return err
}

// Unify unifies the variable with t.
func (v Variable) Unify(t Term, occursCheck bool, env *Env) bool {
	t = env.Resolve(t)
	if occursCheck && Contains(t, v, env) {
		return false
	}
	if ref, ok := env.Lookup(v); ok {
		return ref.Unify(t, occursCheck, env)
	}
	if w, ok := t.(Variable); ok {
		if _, ok := env.Lookup(w); !ok {
			t = NewVariable()
			env.Bind(w, t)
		}
	}
	env.Bind(v, t)
	return true
}

// Float is a prolog floating-point number.
type Float float64

func (f Float) String() string {
	var buf bytes.Buffer
	_ = f.WriteTerm(&buf, defaultWriteTermOptions, nil)
	return buf.String()
}

// WriteTerm writes the float into w.
func (f Float) WriteTerm(w io.Writer, _ WriteTermOptions, _ *Env) error {
	_, err := fmt.Fprint(w, strconv.FormatFloat(float64(f), 'f', -1, 64))
	return err
}

// Unify unifies the float with t.
func (f Float) Unify(t Term, occursCheck bool, env *Env) bool {
	switch t := t.(type) {
	case Float:
		return f == t
	case Variable:
		return t.Unify(f, occursCheck, env)
	default:
		return false
	}
}

// Integer is a prolog integer.
type Integer int64

func (i Integer) String() string {
	var buf bytes.Buffer
	_ = i.WriteTerm(&buf, defaultWriteTermOptions, nil)
	return buf.String()
}

// WriteTerm writes the integer into w.
func (i Integer) WriteTerm(w io.Writer, _ WriteTermOptions, _ *Env) error {
	_, err := fmt.Fprint(w, strconv.FormatInt(int64(i), 10))
	return err
}

// Unify unifies the integer with t.
func (i Integer) Unify(t Term, occursCheck bool, env *Env) bool {
	switch t := t.(type) {
	case Integer:
		return i == t
	case Variable:
		return t.Unify(i, occursCheck, env)
	default:
		return false
	}
}

// Atom is a prolog atom.
type Atom string

func (a Atom) String() string {
	var buf bytes.Buffer
	_ = a.WriteTerm(&buf, defaultWriteTermOptions, nil)
	return buf.String()
}

var unquotedAtomPattern = regexp.MustCompile(`\A(?:[a-z]\w*|[#$&*+\-./:<=>?@^~\\]+|\[])\z`)
var quotedAtomEscapePattern = regexp.MustCompile("[[:cntrl:]]|\\\\|'|\"|`")

// WriteTerm writes the atom into w.
func (a Atom) WriteTerm(w io.Writer, opts WriteTermOptions, _ *Env) error {
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
func (a Atom) Unify(t Term, occursCheck bool, env *Env) bool {
	switch t := t.(type) {
	case Atom:
		return a == t
	case Variable:
		return t.Unify(a, occursCheck, env)
	default:
		return false
	}
}

// Compound is a prolog compound.
type Compound struct {
	Functor Atom
	Args    []Term
}

func (c *Compound) String() string {
	var buf bytes.Buffer
	_ = c.WriteTerm(&buf, defaultWriteTermOptions, nil)
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
				if internal.IsExtendedGraphic(l[len(l)-1]) && internal.IsExtendedGraphic(f[0]) {
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
				if err := env.Resolve(c.Args[0]).WriteTerm(&lb, opts, env); err != nil {
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
		switch arg := env.Resolve(c.Args[0]).(type) {
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
func (c *Compound) Unify(t Term, occursCheck bool, env *Env) bool {
	switch t := t.(type) {
	case *Compound:
		if c.Functor != t.Functor {
			return false
		}
		if len(c.Args) != len(t.Args) {
			return false
		}
		for i := range c.Args {
			if !c.Args[i].Unify(t.Args[i], occursCheck, env) {
				return false
			}
		}
		return true
	case Variable:
		return t.Unify(c, occursCheck, env)
	default:
		return false
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
func Each(list Term, f func(elem Term) error, env *Env) error {
	whole := list
	for {
		switch l := env.Resolve(list).(type) {
		case Variable:
			return instantiationError(whole)
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

type streamMode int

const (
	streamModeRead streamMode = iota
	streamModeWrite
	streamModeAppend
)

type eofAction int

const (
	eofActionEOFCode eofAction = iota
	eofActionError
	eofActionReset
)

type streamType int

const (
	streamTypeText streamType = iota
	streamTypeBinary
)

// Stream is a prolog stream.
type Stream struct {
	source io.Reader
	sink   io.Writer
	closer io.Closer

	mode       streamMode
	alias      Atom
	eofAction  eofAction
	reposition bool
	streamType streamType
}

func (s *Stream) String() string {
	var buf bytes.Buffer
	_ = s.WriteTerm(&buf, defaultWriteTermOptions, nil)
	return buf.String()
}

// WriteTerm writes the stream into w.
func (s *Stream) WriteTerm(w io.Writer, _ WriteTermOptions, _ *Env) error {
	if s.alias != "" {
		_, err := fmt.Fprintf(w, "<stream>(%s)", s.alias)
		return err
	}
	_, err := fmt.Fprintf(w, "<stream>(%p)", s)
	return err
}

// Unify unifies the stream with t.
func (s *Stream) Unify(t Term, occursCheck bool, env *Env) bool {
	switch t := t.(type) {
	case *Stream:
		return s == t
	case Variable:
		return t.Unify(s, occursCheck, env)
	default:
		return false
	}
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
	Ops: Operators{
		{Priority: 400, Specifier: "yfx", Name: "/"}, // for principal functors
	},
	NumberVars: false,
}

type readTermOptions struct {
	singletons    Variable
	variables     Variable
	variableNames Variable
}

// procedureIndicator is a specialized variant of Compound.
type procedureIndicator struct {
	name  Atom
	arity Integer
}

func (p procedureIndicator) String() string {
	var buf bytes.Buffer
	_ = p.WriteTerm(&buf, defaultWriteTermOptions, nil)
	return buf.String()
}

func (p procedureIndicator) WriteTerm(w io.Writer, _ WriteTermOptions, _ *Env) error {
	_, err := fmt.Fprintf(w, "%s/%d", p.name, p.arity)
	return err
}

func (p procedureIndicator) Unify(t Term, _ bool, _ *Env) bool {
	pf, ok := t.(procedureIndicator)
	return ok && p.name == pf.name && p.arity == pf.arity
}

func piArgs(t Term, env *Env) (procedureIndicator, Term, error) {
	switch f := env.Resolve(t).(type) {
	case Variable:
		return procedureIndicator{}, nil, instantiationError(t)
	case Atom:
		return procedureIndicator{name: f, arity: 0}, List(), nil
	case *Compound:
		return procedureIndicator{name: f.Functor, arity: Integer(len(f.Args))}, List(f.Args...), nil
	default:
		return procedureIndicator{}, nil, typeErrorCallable(t)
	}
}
