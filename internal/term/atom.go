package term

import (
	"unicode/utf8"

	"github.com/ichiban/prolog/v2/internal/side"
)

var (
	atomDot       = NewAtomRune('.')
	atomEmptyList = NewAtom("[]")
	atomMinus     = NewAtomRune('-')
)

// Atom is an interned string.
type Atom struct {
	kind  atomKind
	value int32
}

// NewAtom returns an atom.
func NewAtom(ident string) Atom {
	// A one-char atom has an economic representation.
	if r, n := utf8.DecodeLastRuneInString(ident); r != utf8.RuneError && n == len(ident) {
		return NewAtomRune(r)
	}

	return Atom{kind: atomKindID, value: int32(atomTable.Add(ident))}
}

// NewAtomRune returns an atom.
func NewAtomRune(r rune) Atom {
	return Atom{kind: atomKindRune, value: r}
}

func (a Atom) String() string {
	switch a.kind {
	case atomKindRune:
		return string(a.value)
	case atomKindID:
		return atomTable.Get(int(a.value))
	default:
		return ""
	}
}

func (a Atom) Rune() rune {
	switch a.kind {
	case atomKindRune:
		return a.value
	case atomKindID:
		// No one-char atoms are in atomTable.
		fallthrough
	default:
		return utf8.RuneError
	}
}

type atomKind int8

const (
	atomKindInvalid atomKind = iota
	atomKindRune
	atomKindID
)

var atomTable side.Pool[string]
