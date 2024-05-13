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
	m := p.TypeInModule()
	m.Register2("get_status", func(_ *engine.VM, url, status engine.Term, k engine.Cont, env *engine.Env) *engine.Promise {
		// Check if the input arguments are of the types you expected.
		u, ok := env.Resolve(url).(engine.Atom)
		if !ok {
			return engine.Error(engine.TypeError(engine.NewAtom("atom"), url, env))
		}

		// Do whatever you want with the given inputs.
		resp, err := http.Get(u.String())
		if err != nil {
			return engine.Error(err)
		}

		// Return values by unification with the output arguments.
		env, ok = env.Unify(status, engine.Integer(resp.StatusCode))
		if !ok {
			return engine.Bool(false)
		}

		// Tell Prolog to continue with the given continuation and environment.
		return k(env)
	})

	// Treat a string argument as an atom.
	if err := p.QuerySolution(`set_prolog_flag(double_quotes, atom).`).Err(); err != nil {
		panic(err)
	}

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
