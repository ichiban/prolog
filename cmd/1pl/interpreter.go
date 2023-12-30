package main

import (
	"fmt"
	"github.com/ichiban/prolog"
	"github.com/ichiban/prolog/engine"
	"io"
)

// New creates a prolog.Interpreter with some helper predicates.
func New(r io.Reader, w io.Writer) *prolog.Interpreter {
	i := prolog.New(r, w)
	m := i.Module()
	m.Register4("skip_max_list", engine.SkipMaxList)
	m.Register2("go_string", func(vm *engine.VM, term, s engine.Term, k engine.Cont, env *engine.Env) *engine.Promise {
		return engine.Unify(vm, s, engine.NewAtom(fmt.Sprintf("%#v", term)), k, env)
	})
	return i
}
