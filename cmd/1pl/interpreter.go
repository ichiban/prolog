package main

import (
	"fmt"
	"io"
	"time"

	"github.com/ichiban/prolog"
	"github.com/ichiban/prolog/engine"
)

// New creates a prolog.Interpreter with some helper predicates.
func New(r io.Reader, w io.Writer) *prolog.Interpreter {
	i := prolog.New(r, w)
	i.Register2("call_nth", i.CallNth)
	i.Register4("skip_max_list", engine.SkipMaxList)
	i.Register2("go_string", func(term, s engine.Term, k func(*engine.Env) *engine.Promise, env *engine.Env) *engine.Promise {
		engine.GoStringEnv = env
		return engine.Unify(s, engine.Atom(fmt.Sprintf("%#v", term)), k, env)
	})
	i.Register2("time", func(goal, out engine.Term, k func(*engine.Env) *engine.Promise, env *engine.Env) *engine.Promise {
		start := time.Now()
		return i.Call(goal, func(env *engine.Env) *engine.Promise {
			end := time.Now()
			d := end.Sub(start)
			return engine.Unify(out, engine.Atom(d.String()), k, env)
		}, env)
	})
	return i
}
