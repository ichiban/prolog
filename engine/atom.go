package engine

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

var (
	unquotedAtomPattern      = regexp.MustCompile(`\A[a-z]\w*\z`)
	graphicalAtomPattern     = regexp.MustCompile(`\A[#$&*+\-./:<=>?@^~\\]+\z`)
	quotedAtomEscapePattern  = regexp.MustCompile("[[:cntrl:]]|\\\\|'|\"|`")
	quotedIdentEscapePattern = regexp.MustCompile("''|\\\\(?:[\\nabfnrtv\\\\'\"`]|(?:x[\\da-fA-F]+|[0-8]+)\\\\)")
)

// Atom is a prolog atom.
type Atom string

func (a Atom) String() string {
	var sb strings.Builder
	_ = Write(&sb, a, defaultWriteTermOptions, nil)
	return sb.String()
}

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
func (a Atom) Unparse(emit func(Token), opts WriteTermOptions, _ *Env) {
	switch {
	case a == ",":
		emit(Token{Kind: TokenComma, Val: ","})
	case a == "[]", a == "{}":
		emit(Token{Kind: TokenIdent, Val: string(a)})
	case graphicalAtomPattern.MatchString(string(a)):
		emit(Token{Kind: TokenGraphic, Val: string(a)})
	case opts.Quoted && !unquotedAtomPattern.MatchString(string(a)):
		emit(Token{Kind: TokenQuotedIdent, Val: quote(string(a))})
	default:
		emit(Token{Kind: TokenIdent, Val: string(a)})
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

func quoteSlice(ss []string) []string {
	ret := make([]string, len(ss))
	for i, s := range ss {
		ret[i] = quote(s)
	}
	return ret
}

func unquote(s string) string {
	return quotedIdentEscapePattern.ReplaceAllStringFunc(s[1:len(s)-1], quotedIdentUnescape)
}

func quotedIdentUnescape(s string) string {
	switch s {
	case "''":
		return "'"
	case "\\\n":
		return ""
	case `\a`:
		return "\a"
	case `\b`:
		return "\b"
	case `\f`:
		return "\f"
	case `\n`:
		return "\n"
	case `\r`:
		return "\r"
	case `\t`:
		return "\t"
	case `\v`:
		return "\v"
	case `\\`:
		return `\`
	case `\'`:
		return `'`
	case `\"`:
		return `"`
	case "\\`":
		return "`"
	default: // `\x23\` or `\23\`
		s = s[1 : len(s)-1] // `x23` or `23`
		base := 8

		if s[0] == 'x' {
			s = s[1:]
			base = 16
		}

		r, _ := strconv.ParseInt(s, base, 4*8) // rune is up to 4 bytes
		return string(rune(r))
	}
}
