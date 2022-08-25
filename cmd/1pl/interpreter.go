package main

import (
	"fmt"
	"io"

	"github.com/ichiban/prolog"
	"github.com/ichiban/prolog/engine"
)

// New creates a prolog.Interpreter with some helper predicates.
func New(r io.Reader, w io.Writer) *prolog.Interpreter {
	i := prolog.New(r, w)
	i.Register2("call_nth", i.CallNth)
	i.Register4("skip_max_list", engine.SkipMaxList)
	i.Register2("go_type", func(term, typ engine.Term, k func(*engine.Env) *engine.Promise, env *engine.Env) *engine.Promise {
		return engine.Unify(typ, engine.Atom(fmt.Sprintf("%T", env.Resolve(term))), k, env)
	})
	i.Register3("partial", engine.Partial)
	return i
}
