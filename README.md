# ![prolog - the only reasonable scripting engine for Go](prolog.gif)

[![Go Reference](https://pkg.go.dev/badge/github.com/ichiban/prolog.svg)](https://pkg.go.dev/github.com/ichiban/prolog)
[![Actions Status](https://github.com/ichiban/prolog/actions/workflows/go.yml/badge.svg)](https://github.com/ichiban/prolog/actions)
[![Go Report Card](https://goreportcard.com/badge/github.com/ichiban/prolog)](https://goreportcard.com/report/github.com/ichiban/prolog)
[![codecov](https://codecov.io/gh/ichiban/prolog/branch/main/graph/badge.svg?token=2FC3PZY7LN)](https://codecov.io/gh/ichiban/prolog)
[![Mentioned in Awesome Go](https://awesome.re/mentioned-badge.svg)](https://github.com/avelino/awesome-go)

## What is this?

`ichiban/prolog` is an embeddable **ISO Prolog** interpreter in **Go**.

- **Standards-compliant:**
  - [ISO/IEC 13211-1:1995 Information technology — Programming languages — Prolog — Part 1: General core](https://www.iso.org/standard/21413.html)
  - [A Prologue for Prolog](http://www.complang.tuwien.ac.at/ulrich/iso-prolog/prologue)
  - [DCGs](https://www.complang.tuwien.ac.at/ulrich/iso-prolog/dcgs/dcgsdraft-2019-06-03.pdf)
- **Easy to integrate:** `database/sql`-like Go API
- **Highly customizable:**
  - Sandboxing
  - Custom predicates in Go
  - Custom terms (data types) in Go

## Comparison with Other Libraries

|             | **prolog**             | otto            | go-lua          |
| ----------- | ---------------------- | --------------- | --------------- |
| Language    | ISO Prolog             | ECMA Script     | Lua             |
| Paradigm    | 🎓 Logic               | Object-oriented | Object-oriented |
| Go API      | 😻 `database/sql`-like | original        | original        |
| Declarative | ✅                     | ❌              | ❌              |
| Sandboxing  | ✅                     | ❌              | ✅              |

## Getting started

### Install latest version

```console
go get -u github.com/ichiban/prolog
```

### Usage

#### Instantiate an interpreter

```go
p := prolog.New(os.Stdin, os.Stdout) // Or `prolog.New(nil, nil)` if you don't need user_input/user_output.
```

Or, if you want a sandbox interpreter without any built-in predicates:

```go
// See examples/sandboxing/main.go for details.
p := new(prolog.Interpreter)
```

#### Load a Prolog program

```go
if err := p.Exec(`
	human(socrates).       % This is a fact.
	mortal(X) :- human(X). % This is a rule.
`); err != nil {
	panic(err)
}
```

Similar to `database/sql`, you can use placeholder `?` to insert Go data as Prolog data.

```go
if err := p.Exec(`human(?).`, "socrates"); err != nil { // Same as p.Exec(`human("socrates").`)
	panic(err)
}
```

#### Run the Prolog program

```go
sols, err := p.Query(`mortal(?).`, "socrates") // Same as p.Query(`mortal("socrates").`)
if err != nil {
	panic(err)
}
defer sols.Close()

// Iterates over solutions.
for sols.Next() {
	fmt.Printf("Yes.\n") // ==> Yes.
}

// Check if an error occurred while querying.
if err := sols.Err(); err != nil {
	panic(err)
}
```

Or, if you want to query for the variable values for each solution:

```go
sols, err := p.Query(`mortal(Who).`)
if err != nil {
	panic(err)
}
defer sols.Close()

// Iterates over solutions.
for sols.Next() {
	// Prepare a struct with fields which name corresponds with a variable in the query.
	var s struct {
		Who string
	}
	if err := sols.Scan(&s); err != nil {
		panic(err)
	}
	fmt.Printf("Who = %s\n", s.Who) // ==> Who = socrates
}

// Check if an error occurred while querying.
if err := sols.Err(); err != nil {
	panic(err)
}
```

## The Default Language

`ichiban/prolog` adheres the ISO standard and comes with the ISO predicates as well as the Prologue for Prolog and DCG predicates.

See [the Wiki](https://github.com/ichiban/prolog/wiki) for the directives and the built-in predicates.

### Top Level

`1pl` is an experimental top level command for testing the default language and its compliance to the ISO standard.

You can install it with `go install`:

```console
go install github.com/ichiban/prolog/cmd/1pl@latest
```

Then, you can enter the top level with `1pl`:

```console
$(go env GOPATH)/bin/1pl [<file>...]
```

## Extensions

- **[predicates](https://github.com/guregu/predicates):** Native predicates for ichiban/prolog.
- **[kagomelog](https://github.com/ichiban/kagomelog):** a Japanese morphological analyzing predicate.

## License

Distributed under the MIT license. See `LICENSE` for more information.

## Contributing

See `ARCHITECTURE.md` for architecture details.

1. Fork it (https://github.com/ichiban/prolog/fork)
2. Create your feature branch (git checkout -b feature/fooBar)
3. Commit your changes (git commit -am 'Add some fooBar')
4. Push to the branch (git push origin feature/fooBar)
5. Create a new Pull Request

## Acknowledgments

We would like to extend our thanks to the following individuals for their contributions to this project:

- [guregu](https://github.com/guregu) for contributing code and ideas
- [Markus Triska](https://github.com/triska) for his deep knowledge and insights on modern Prolog, as evidenced by his insightful comments on issues
- [Prof. Ulrich Neumerkel](https://github.com/UWN) for valuable guidance, his [works on ISO standards](http://www.complang.tuwien.ac.at/ulrich/iso-prolog/), [the Prologue for Prolog](http://www.complang.tuwien.ac.at/ulrich/iso-prolog/prologue), and DCGs.

We are grateful for the support and contributions of everyone involved in this project. Arigatou gozaimasu!
