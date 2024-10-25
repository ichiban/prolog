package main

import (
	"fmt"
	"time"

	"github.com/ichiban/prolog"
	"github.com/ichiban/prolog/engine"
)

func main() {
	p := prolog.New(nil, nil)

	// Define a custom predicate of arity 1: get_time(-Timestamp)
	p.Register1(engine.NewAtom("get_time"), func(_ *engine.VM, TimeStamp engine.Term, k engine.Cont, env *engine.Env) *engine.Promise {
		// Check if the input arguments are of the types you expected.
		_, ok := env.Resolve(TimeStamp).(engine.Variable)
		if !ok {
			return engine.Error(engine.TypeError(engine.NewAtom("variable"), TimeStamp, env))
		}

		now := int(time.Now().Unix())

		// Return values by unification with the output arguments.
		env, ok = env.Unify(TimeStamp, engine.Integer(now))
		if !ok {
			return engine.Bool(false)
		}

		return k(env)
	})

	// Query with the custom predicate get_time/1 but parameterize the first argument.
	sols, err := p.Query(`get_time(Timestamp).`)
	if err != nil {
		panic(err)
	}
	defer func() {
		if err := sols.Close(); err != nil {
			panic(err)
		}
	}()

	if !sols.Next() {
		panic("no solutions")
	}

	var s struct {
		Timestamp int
	}
	if err := sols.Scan(&s); err != nil {
		panic(err)
	}

	fmt.Printf("%+v\n", s)
}
