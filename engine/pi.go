package engine

import (
	"bytes"
	"fmt"
	"io"

	"github.com/ichiban/prolog/term"
)

// procedureIndicator is a specialized variant of Compound.
type procedureIndicator struct {
	name  term.Atom
	arity term.Integer
}

func (p procedureIndicator) String() string {
	var buf bytes.Buffer
	_ = p.WriteTerm(&buf, term.DefaultWriteTermOptions, nil)
	return buf.String()
}

func (p procedureIndicator) WriteTerm(w io.Writer, _ term.WriteTermOptions, _ term.Env) error {
	_, err := fmt.Fprintf(w, "%s/%d", p.name, p.arity)
	return err
}

func (p procedureIndicator) Unify(t term.Interface, _ bool, _ *term.Env) bool {
	pf, ok := t.(procedureIndicator)
	return ok && p.name == pf.name && p.arity == pf.arity
}
