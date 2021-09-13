# ichiban/prolog

A Go embeddable Prolog.

## Getting Started

### Prerequisites

You need Go 1.6 or above installed.

### Installation

```console
go get -u github.com/ichiban/prolog@latest
```

## Usage

### Embed Prolog into Go

```go
package main

import (
	"fmt"

	"github.com/ichiban/prolog"
)

// http://www.cse.unsw.edu.au/~billw/dictionaries/prolog/cut.html
func main() {
	p := prolog.New(nil, nil)
	if err := p.Exec(`
teaches(dr_fred, history).
teaches(dr_fred, english).
teaches(dr_fred, drama).
teaches(dr_fiona, physics).

studies(alice, english).
studies(angus, english).
studies(amelia, drama).
studies(alex, physics).
`); err != nil {
		panic(err)
	}

	for _, q := range []string{
		`teaches(dr_fred, Course), studies(Student, Course).`,
		`teaches(dr_fred, Course), !, studies(Student, Course).`,
		`teaches(dr_fred, Course), studies(Student, Course), !.`,
		`!, teaches(dr_fred, Course), studies(Student, Course).`,
	} {
		fmt.Printf("%s\n", q)

		sols, err := p.Query(q)
		if err != nil {
			panic(err)
		}

		for sols.Next() {
			var s struct {
				Course  string
				Student string
			}
			if err := sols.Scan(&s); err != nil {
				panic(err)
			}
			fmt.Printf("\t%+v\n", s)
		}

		fmt.Printf("\n")
		if err := sols.Close(); err != nil {
			panic(err)
		}
	}
}
```

```console
$ go run examples/embed_prolog_into_go/main.go 
teaches(dr_fred, Course), studies(Student, Course).
        {Course:english Student:alice}
        {Course:english Student:angus}
        {Course:drama Student:amelia}

teaches(dr_fred, Course), !, studies(Student, Course).

teaches(dr_fred, Course), studies(Student, Course), !.
        {Course:english Student:alice}

!, teaches(dr_fred, Course), studies(Student, Course).
        {Course:english Student:alice}
        {Course:english Student:angus}
        {Course:drama Student:amelia}

```

### Call Go from Prolog

```go
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
```

```console
$ go run examples/call_go_from_prolog/main.go 
{Status:200}
```

## License

Distributed under the MIT license. See `LICENSE` for more information.

## Contributing

See `ARCHITECTURE.md` for architecture details.

1. Fork it (https://github.com/ichiban/prolog/fork)
1. Create your feature branch (git checkout -b feature/fooBar)
1. Commit your changes (git commit -am 'Add some fooBar')
1. Push to the branch (git push origin feature/fooBar)
1. Create a new Pull Request