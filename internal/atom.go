package internal

import (
	"fmt"
	"github.com/ichiban/prolog/internal/rbtree"
	"github.com/ichiban/prolog/internal/ring"
	"regexp"
	"strings"
	"sync"
	"unicode/utf8"
)

var (
	quotedAtomEscapePattern = regexp.MustCompile(`[[:cntrl:]]|\\|'`)
)

var (
	atomTable = struct {
		sync.RWMutex
		names []string
		atoms map[string]Atom
	}{
		atoms: map[string]Atom{},
	}
)

// Well-known atoms.
var (
	atomEmptyList  = NewAtom("[]")
	atomEmptyBlock = NewAtom("{}")
	atomEllipsis   = NewAtom(`...`)
)

// Atom is either a rune or an ID for interned string.
type Atom int64

func NewAtom(name string) Atom {
	// A one-char atom is just a rune.
	if r, n := utf8.DecodeLastRuneInString(name); r != utf8.RuneError && n == len(name) {
		return Atom(r)
	}

	atomTable.Lock()
	defer atomTable.Unlock()

	a, ok := atomTable.atoms[name]
	if ok {
		return a
	}

	a = Atom(len(atomTable.names) + (utf8.MaxRune + 1))
	atomTable.atoms[name] = a
	atomTable.names = append(atomTable.names, name)
	return a
}

func (a Atom) String() string {
	if a <= utf8.MaxRune {
		return string(rune(a))
	}
	atomTable.RLock()
	defer atomTable.RUnlock()
	return atomTable.names[a-(utf8.MaxRune+1)]
}

func needQuoted(a Atom) bool {
	p := Parser{
		Lexer: Lexer{
			input: newRuneRingBuffer(strings.NewReader(a.String())),
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

func letterDigit(a Atom) bool {
	s := a.String()
	return len(s) > 0 && isSmallLetterChar([]rune(s)[0])
}

func graphic(a Atom) bool {
	s := a.String()
	return len(s) > 0 && (isGraphicChar([]rune(s)[0]) || []rune(s)[0] == '\\')
}

type atomID int32

type SymbolTable struct {
	lastID atomID
	ids    rbtree.Map[string, atomID]
	names  rbtree.Map[atomID, string]
}

func (s *SymbolTable) Put(name string) (atomID, bool) {
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

func (s *SymbolTable) Grow(size int) {
	s.ids.Grow(size)
	s.names.Grow(size)
}
