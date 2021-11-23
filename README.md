# ![prolog - the only reasonable scripting engine for Go](prolog.gif)

[![Go Reference](https://pkg.go.dev/badge/github.com/ichiban/prolog.svg)](https://pkg.go.dev/github.com/ichiban/prolog)
[![Actions Status](https://github.com/ichiban/prolog/actions/workflows/go.yml/badge.svg)](https://github.com/ichiban/prolog/actions)
[![Go Report Card](https://goreportcard.com/badge/github.com/ichiban/prolog)](https://goreportcard.com/report/github.com/ichiban/prolog)

## What is this?

`ichiban/prolog` is an embeddable scripting language for Go.
Unlike any other scripting engines, `ichiban/prolog` implements logic programming language Prolog.

- **Easy to reason about:** based on first-order logic
- **Easy to adopt:** `database/sql`-like Go API
- **Intelligent:** full-featured Prolog implementation
- **Highly customizable:** sandboxing, custom predicates 

## `ichiban/prolog` vs [otto](https://github.com/robertkrimen/otto) vs [go-lua](https://github.com/Shopify/go-lua)

|                              | **prolog**          | otto            | go-lua               |
| ---------------------------- | ------------------- | --------------- | -------------------- |
| Language                     | ISO Prolog          | ECMA Script     | Lua                  |
| Paradigm                     | Logic               | Object-oriented | Object-oriented      |
| Go API                       | `database/sql`-like | original        | original             |
| Declarative?                 | ✅                  | ❌              | ❌                   |
| Easy to set a value from Go? | ✅                  | ✅              | ✅                   |
| Easy to get a value to Go?   | ✅                  | ❌              | ✅                   |
| Sandboxing                   | ✅                  | ❌              | ✅                   |

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
p := &prolog.Interpreter{}
```

#### Load a Prolog program

```go
if err := p.Exec(`
	% This is a comment.
	human(socrates).       % This is a fact.
	mortal(X) :- human(X). % This is a rule.
`); err != nil {
	panic(err)
}
```

Or, from a file:

```go
// Load file `abc` or `abc.pl` in the current directory.
if err := p.Exec(`:- [abc].`); err != nil {
	panic(err)
}
```

```go
// Load file `/path/to/abc` or `/path/to/abc.pl`.
// You need to quote to contain / in the path.
if err := p.Exec(`:- ['/path/to/abc'].`); err != nil {
	panic(err)
}
```

Or, from a library:

```go
// See examples/dcg/main.go for a complete example.
if err := p.Exec(`:- [library(dcg)].`); err != nil {
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

// Iterates over every possibility of solutions.
for sols.Next() {
	var s struct {
		Who string
	}
	if err := sols.Scan(&s); err != nil {
		panic(err)
	}
	fmt.Printf("Who = %s\n", s.Who)
	// ==> "Who = socrates\n"
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
