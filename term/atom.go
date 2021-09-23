package term

import (
	"bytes"
	"fmt"
	"io"
	"regexp"
	"strings"
)

// Atom is a prolog atom.
type Atom string

func (a Atom) String() string {
	var buf bytes.Buffer
	_ = a.WriteTerm(&buf, DefaultWriteTermOptions, nil)
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
func (a Atom) Unify(t Interface, occursCheck bool, env *Env) (*Env, bool) {
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
func (a Atom) Apply(args ...Interface) Interface {
	if len(args) == 0 {
		return a
	}
	return &Compound{
		Functor: a,
		Args:    args,
	}
}
