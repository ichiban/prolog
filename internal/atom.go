package internal

import (
	"fmt"
	"github.com/ichiban/prolog/internal/rbtree"
	"github.com/ichiban/prolog/internal/ring"
	"regexp"
	"strings"
)

var (
	quotedAtomEscapePattern = regexp.MustCompile(`[[:cntrl:]]|\\|'`)
)

// Well-known atoms.
var (
	atomEmptyList  = "[]"
	atomEmptyBlock = "{}"
	atomEllipsis   = `...`
)

func needQuoted(a string) bool {
	p := Parser{
		Lexer: Lexer{
			input: newRuneRingBuffer(strings.NewReader(a)),
		},
		buf: ring.NewBuffer[Token](4),
	}
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

func letterDigit(s string) bool {
	return len(s) > 0 && isSmallLetterChar([]rune(s)[0])
}

func graphic(s string) bool {
	return len(s) > 0 && (isGraphicChar([]rune(s)[0]) || []rune(s)[0] == '\\')
}

type atomID int32

type AtomTable struct {
	lastID atomID
	ids    rbtree.Map[string, atomID]
	names  rbtree.Map[atomID, string]
}

func (s *AtomTable) Put(name string) (atomID, bool) {
	if id, ok := s.ids.Get(name); ok {
		return id, true
	}

	s.lastID++
	id := s.lastID

	if !s.ids.SafeSet(name, id) || !s.names.SafeSet(id, name) {
		return 0, false
	}

	return id, true
}

func (s *AtomTable) Grow(size int) {
	s.ids.Grow(size)
	s.names.Grow(size)
}
