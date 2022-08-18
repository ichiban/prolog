package engine

import (
	"bufio"
	"bytes"
	"fmt"
	"regexp"
	"strings"
)

var (
	graphicalAtomPattern    = regexp.MustCompile(`\A[#$&*+\-./:<=>?@^~\\]+\z`)
	quotedAtomEscapePattern = regexp.MustCompile(`[[:cntrl:]]|\\|'`)
)

// Atom is a prolog atom.
type Atom string

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

func needQuoted(a Atom) bool {
	p := newParser(bufio.NewReader(bytes.NewBufferString(string(a))))
	parsed, err := p.atom()
	return err != nil || parsed != a
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

func letterDigit(a Atom) bool {
	return len(a) > 0 && isSmallLetterChar([]rune(a)[0])
}

func graphic(a Atom) bool {
	return len(a) > 0 && (isGraphicChar([]rune(a)[0]) || []rune(a)[0] == '\\')
}
