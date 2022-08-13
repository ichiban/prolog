package main

import (
	"fmt"
	"net/http"

	"github.com/ichiban/prolog"
	"github.com/ichiban/prolog/engine"
)

func main() {
	p := prolog.New(nil, nil)

	// Define a custom predicate of arity 2.
	p.Register2("get_status", func(url, status engine.Term, k func(*engine.Env) *engine.Promise, env *engine.Env) *engine.Promise {
		// Check if the input arguments are of the types you expected.
		u, ok := env.Resolve(url).(engine.Atom)
		if !ok {
			return engine.Error(fmt.Errorf("%s is not an atom", url))
		}

		// Do whatever you want with the given inputs.
		resp, err := http.Get(string(u))
		if err != nil {
			return engine.Error(err)
		}

		// Return values by unification with the output arguments.
		env, ok = env.Unify(status, engine.Integer(resp.StatusCode), false)
		if !ok {
			return engine.Bool(false)
		}

		// Tell Prolog to continue with the given continuation and environment.
		return k(env)
	})

	// Query with the custom predicate get_status/2 but parameterize the first argument.
	sols, err := p.Query(`get_status(?, Status).`, "https://httpbin.org/status/200")
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
		Status int
	}
	if err := sols.Scan(&s); err != nil {
		panic(err)
	}

	fmt.Printf("%+v\n", s)
}
