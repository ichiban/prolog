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
	i.Register2(engine.NewAtom("call_nth"), engine.CallNth)
	i.Register4(engine.NewAtom("skip_max_list"), engine.SkipMaxList)
	i.Register2(engine.NewAtom("go_string"), func(vm *engine.VM, term, s engine.Term, k func(*engine.Env) *engine.Promise, env *engine.Env) *engine.Promise {
		return engine.Unify(vm, s, engine.NewAtom(fmt.Sprintf("%#v", term)), k, env)
	})
	return i
}
