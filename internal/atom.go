package internal

import (
	"sync"
	"unicode/utf8"
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

// Atom is either a rune or an ID for an interned string.
type Atom int

// NewAtom interns the given string and returns an Atom.
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
