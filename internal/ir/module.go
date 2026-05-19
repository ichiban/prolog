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

func (m *Module) String() string {
	if m == nil {
		return ""
	}
	var sb strings.Builder
	_, _ = fmt.Fprintf(&sb, "module: %s\n", m.Name)
	for _, clause := range m.Clauses {
		_, _ = fmt.Fprintf(&sb, "%s\n", &clause)
	}
	return sb.String()
}
