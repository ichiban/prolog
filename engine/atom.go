package engine

import (
	"fmt"
	"regexp"
	"strings"
)

var (
	unquotedAtomPattern     = regexp.MustCompile(`\A[a-z]\w*\z`)
	graphicalAtomPattern    = regexp.MustCompile(`\A[#$&*+\-./:<=>?@^~\\]+\z`)
	quotedAtomEscapePattern = regexp.MustCompile("[[:cntrl:]]|\\\\|'|\"|`")
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

// Unparse emits tokens that represent the atom.
func (a Atom) Unparse(emit func(Token), _ *Env, opts ...WriteOption) {
	wto := defaultWriteOptions
	for _, o := range opts {
		o(&wto)
	}

	switch {
	case a == ",":
		emit(Token{Kind: TokenComma, Val: ","})
	case a == "[]":
		emit(Token{Kind: TokenOpenList, Val: "["})
		emit(Token{Kind: TokenCloseList, Val: "]"})
	case a == "{}":
		emit(Token{Kind: TokenOpenCurly, Val: "{"})
		emit(Token{Kind: TokenCloseCurly, Val: "}"})
	case a == ";":
		emit(Token{Kind: TokenSemicolon, Val: string(a)})
	case a == "!":
		emit(Token{Kind: TokenCut, Val: string(a)})
	case graphicalAtomPattern.MatchString(string(a)):
		emit(Token{Kind: TokenGraphic, Val: string(a)})
	case wto.quoted && !unquotedAtomPattern.MatchString(string(a)):
		emit(Token{Kind: TokenQuoted, Val: quote(string(a))})
	default:
		emit(Token{Kind: TokenLetterDigit, Val: string(a)})
	}
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
