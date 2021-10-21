package term

import (
	"fmt"
	"regexp"
	"strings"

	"github.com/ichiban/prolog/syntax"
)

// Atom is a prolog atom.
type Atom string

func (a Atom) String() string {
	var sb strings.Builder
	_ = Write(&sb, a, defaultWriteTermOptions, nil)
	return sb.String()
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

var unquotedAtomPattern = regexp.MustCompile(`\A[a-z]\w*\z`)
var graphicalAtomPattern = regexp.MustCompile(`\A[#$&*+\-./:<=>?@^~\\]+\z`)
var quotedAtomEscapePattern = regexp.MustCompile("[[:cntrl:]]|\\\\|'|\"|`")

// Unparse emits tokens that represent the atom.
func (a Atom) Unparse(emit func(syntax.Token), opts WriteTermOptions, _ *Env) {
	switch {
	case graphicalAtomPattern.MatchString(string(a)):
		emit(syntax.Token{Kind: syntax.TokenGraphic, Val: string(a)})
	case opts.Quoted && !unquotedAtomPattern.MatchString(string(a)):
		s := quotedAtomEscapePattern.ReplaceAllStringFunc(string(a), quotedIdentEscape)
		emit(syntax.Token{Kind: syntax.TokenQuotedIdent, Val: fmt.Sprintf("'%s'", s)})
	default:
		emit(syntax.Token{Kind: syntax.TokenIdent, Val: string(a)})
	}
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
}
