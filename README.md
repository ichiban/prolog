# ![prolog - the only reasonable scripting engine for Go](prolog.gif)

[![Go Reference](https://pkg.go.dev/badge/github.com/ichiban/prolog.svg)](https://pkg.go.dev/github.com/ichiban/prolog)
[![Actions Status](https://github.com/ichiban/prolog/actions/workflows/go.yml/badge.svg)](https://github.com/ichiban/prolog/actions)
[![Go Report Card](https://goreportcard.com/badge/github.com/ichiban/prolog)](https://goreportcard.com/report/github.com/ichiban/prolog)
[![codecov](https://codecov.io/gh/ichiban/prolog/branch/main/graph/badge.svg?token=2FC3PZY7LN)](https://codecov.io/gh/ichiban/prolog)

## What is this?

`ichiban/prolog` is an embeddable scripting language for Go.
Unlike any other scripting engines, `ichiban/prolog` implements logic programming language Prolog.

- **Easy to reason about:** based on first-order logic
- **Easy to adopt:** `database/sql`-like Go API
- **Intelligent:** full-featured Prolog implementation
- **Highly customizable:** sandboxing, custom predicates 

## `ichiban/prolog` vs [otto](https://github.com/robertkrimen/otto) vs [go-lua](https://github.com/Shopify/go-lua)

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
go get -u github.com/ichiban/prolog@latest
```

### Usage

#### Instantiate an interpreter

```go
p := prolog.New(nil, nil)
```

Or, if you want to expose standard input/output to your interpreter:

```go
p := prolog.New(os.Stdin, os.Stdout)
```

Or, if you want a secure interpreter without any builtin predicates:

```go
// See examples/sandboxing/main.go for details.
p := new(prolog.Interpreter)
```

#### Load a Prolog program

```go
if err := p.Exec(`
	:- [abc].              % Load file 'abc' or 'abc.pl'.

	:- ['/path/to/abc'].   % Load file '/path/to/abc' or '/path/to/abc.pl'.
	                       % You need to quote to contain / in the path.

	:- [library(dcg)].     % Load library 'library(dcg)'.
	                       % See examples/dcg/main.go for a complete example.

	human(socrates).       % This is a fact.
	mortal(X) :- human(X). % This is a rule.
`); err != nil {
	panic(err)
}
```

#### Run the Prolog program

```go
// Prolog program invocation takes a form of query.
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
	fmt.Printf("Who = %s\n", s.Who)
	// ==> Who = socrates
}
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
