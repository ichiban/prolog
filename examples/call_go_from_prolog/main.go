package main

import (
	"fmt"
	"net/http"

	"github.com/ichiban/prolog"
	"github.com/ichiban/prolog/nondet"
	"github.com/ichiban/prolog/term"
)

func main() {
	p := prolog.New(nil, nil)

	// Define a custom predicate of arity 2.
	p.Register2("get_status", func(url, status term.Interface, k func(*term.Env) *nondet.Promise, env *term.Env) *nondet.Promise {
		// Check if the input arguments are of the types you expected.
		u, ok := env.Resolve(url).(term.Atom)
		if !ok {
			return nondet.Error(fmt.Errorf("%s is not an atom", url))
		}

		// Do whatever you want with the given inputs.
		resp, err := http.Get(string(u))
		if err != nil {
			return nondet.Error(err)
		}

		// Return values by unification with the output arguments.
		env, ok = status.Unify(term.Integer(resp.StatusCode), false, env)
		if !ok {
			return nondet.Bool(false)
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
