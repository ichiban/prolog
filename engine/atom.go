package engine

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"regexp"
	"strings"
)

var (
	graphicalAtomPattern    = regexp.MustCompile(`\A[#$&*+\-./:<=>?@^~\\]+\z`)
	quotedAtomEscapePattern = regexp.MustCompile(`[[:cntrl:]]|\\|'`)
)

// Atom is a prolog atom.
type Atom string

// Unify unifies the atom with t.
func (a Atom) Unify(t Term, occursCheck bool, env *Env) (*Env, bool) {
	switch t := env.Resolve(t).(type) {
	case Atom:
		return env, a == t
	case Variable:
		return t.Unify(a, occursCheck, env)
	default:
		return env, false
	}
}

// Apply returns a Compound which Functor is the Atom and Args are the arguments. If the arguments are empty,
// then returns itself.
func (a Atom) Apply(args ...Term) Term {
	if len(args) == 0 {
		return a
	}
	return &Compound{
		Functor: a,
		Args:    args,
	}
}

// WriteTerm writes the Atom to the io.Writer.
func (a Atom) WriteTerm(w io.Writer, opts *WriteOptions, _ *Env) error {
	ew := errWriter{w: w}
	var openClose bool
	if opts.before != (operator{}) || opts.after != (operator{}) {
		for _, op := range opts.ops {
			if op.name == a {
				openClose = true
				break
			}
		}
	}

	if openClose {
		if opts.before.name != "" && (opts.before.specifier == operatorSpecifierFX || opts.before.specifier == operatorSpecifierFY) {
			_, _ = fmt.Fprint(&ew, " ")
		}
		_, _ = fmt.Fprint(&ew, "(")
	}

	s := string(a)
	if opts.Quoted {
		p := newParser(bufio.NewReader(bytes.NewBufferString(string(a))))
		if a, err := p.atom(); err != nil || string(a) != s {
			s = quote(s)
		}
	}
	_, _ = fmt.Fprint(&ew, s)

	if openClose {
		_, _ = fmt.Fprint(&ew, ")")
	}

	return ew.err
}

// Compare compares the atom to another term.
func (a Atom) Compare(t Term, env *Env) int64 {
	switch t := env.Resolve(t).(type) {
	case Variable, Float, Integer:
		return 1
	case Atom:
		return int64(strings.Compare(string(a), string(t)))
	default:
		return -1
	}
}

func quote(s string) string {
	return fmt.Sprintf("'%s'", quotedAtomEscapePattern.ReplaceAllStringFunc(s, quotedIdentEscape))
}

func quotedIdentEscape(s string) string {
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
	default:
		var ret []string
		for _, r := range s {
			ret = append(ret, fmt.Sprintf(`\x%x\`, r))
		}
		return strings.Join(ret, "")
	}
}
