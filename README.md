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
  - Modules
- **Easy to integrate:** idiomatic Go API
- **Highly customizable:**
  - Sandboxing
  - Custom predicates in Go

## Comparison with Other Libraries

|             | **prolog**             | otto            | go-lua          |
| ----------- | ---------------------- | --------------- | --------------- |
| Language    | ISO Prolog             | ECMA Script     | Lua             |
| Paradigm    | 🎓 Logic               | Object-oriented | Object-oriented |
| Declarative | ✅                     | ❌              | ❌              |
| Sandboxing  | ✅                     | ❌              | ✅              |

## Getting started

### Install latest version

```console
go get -u github.com/ichiban/prolog/v2
```

### Usage

#### Instantiate a processor

```go
p := prolog.New()
```

#### Load a Prolog program

```go
// Mount a fs.FS that contains a Prolog file.
p.Mount(fs)

// Load the file. 
if err := p.Load(ctx, "human.pl"); err != nil {
	panic(err)
}
```

#### Run the Prolog program

```go
// Define a struct type with fields which name corresponds with a variable in the query.
type sol struct{
	Who string
}

// Iterates over solutions.
for s, err := range prolog.Query[sol](ctx, p, `mortal(Who).`) {
	// Check if an error occurred while querying.
	if err != nil {
		panic(err)
	}
	
	fmt.Printf("Who = %s\n", s.Who) // ==> Who = socrates
}
```

## The Default Language

`ichiban/prolog` adheres the ISO standard and comes with the ISO predicates as well as the Prologue for Prolog and DCG predicates.

See [the Wiki](https://github.com/ichiban/prolog/wiki) for the directives and the built-in predicates.

### Top Level

`1pl` is an experimental top level command for testing the default language and its compliance to the ISO standard.

You can install it with `go install`:

```console
go install github.com/ichiban/prolog/v2/cmd/1pl@latest
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
