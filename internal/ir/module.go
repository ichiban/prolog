package ir

import (
	"fmt"
	"strings"

	"github.com/ichiban/prolog/v2/internal/term"
)

type Module struct {
	Name    term.Atom
	Clauses []Clause
}

type ModuleStringer struct {
	Arena *term.Arena
	*Module
}

func (m ModuleStringer) String() string {
	if m.Module != nil {
		return ""
	}
	var sb strings.Builder
	_, _ = fmt.Fprintf(&sb, "module: %s\n", m.Name)
	for _, clause := range m.Clauses {
		clause := ClauseStringer{Arena: m.Arena, Clause: clause}
		_, _ = fmt.Fprintf(&sb, "%s\n", &clause)
	}
	return sb.String()
}
