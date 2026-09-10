package ir

import (
	"fmt"
	"iter"
	"strings"

	"github.com/ichiban/prolog/v2/internal/term"
)

type Module struct {
	Name           term.Atom
	Clauses        []Clause
	Initialization []term.Cell
}

// Cells yields every cell the module under construction holds. Compiling a text
// runs its directives, which can collect, so what has been compiled so far has
// to be a GC root until the module is loaded and its cells are embedded in the
// image.
func (m *Module) Cells() iter.Seq[*term.Cell] {
	return func(yield func(*term.Cell) bool) {
		for i := range m.Initialization {
			if !yield(&m.Initialization[i]) {
				return
			}
		}
		for i := range m.Clauses {
			c := &m.Clauses[i]
			if !yield(&c.FirstArg.Term) {
				return
			}
			for j := range c.Code {
				inst := &c.Code[j]
				if !yield(&inst.A.Term) || !yield(&inst.B.Term) {
					return
				}
			}
		}
	}
}

type ModuleStringer struct {
	Arena *term.Arena
	*Module
}

func (m ModuleStringer) String() string {
	if m.Module == nil {
		return ""
	}
	var sb strings.Builder
	_, _ = fmt.Fprintf(&sb, "module: %s\n", m.Name)
	for _, clause := range m.Clauses {
		clause := ClauseStringer{Arena: m.Arena, Clause: &clause}
		_, _ = fmt.Fprintf(&sb, "%s\n", &clause)
	}
	return sb.String()
}
