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

## Built-in Predicates

| Category           | Indicator                   | ISO? | Description                                                                                |
|--------------------|-----------------------------|:----:|--------------------------------------------------------------------------------------------|
| Control Constructs | `true/0`                    |  *   | Always succeeds.                                                                           |
|                    | `fail/0`                    |  *   | Always fails.                                                                              |
|                    | `false/0`                   |      | Synonym for fail/0.                                                                        |
|                    | `call/1`                    |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.Call                             |
|                    | `!/0`                       |  *   | Cut.                                                                                       |
|                    | `(,)/2`                     |  *   | Conjunction.                                                                               |
|                    | `(;)/2`                     |  *   | Not only disjunction but also If->Then;Else is supported.                                  |
|                    | `(->)/2`                    |  *   | If->Then.                                                                                  |
|                    | `catch/3`                   |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.Catch                            |
|                    | `throw/1`                   |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#Throw                                  |
|                    | `(\+)/1`                    |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.Negation                         |
|                    | `once/1`                    |  *   |                                                                                            |
|                    | `repeat/0`                  |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#Repeat                                 |
|                    | `halt/1`                    |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#Halt                                   |
|                    | `halt/0`                    |  *   | Equivalent to `halt(0)`.                                                                   |
| Unification        | `(=)/2`                     |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#Unify                                  |
|                    | `unify_with_occurs_check/2` |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#UnifyWithOccursCheck                   |
|                    | `(\=)/2`                    |  *   | Not unifiable.                                                                             |
| Type Testing       | `var/1`                     |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#TypeVar                                |
|                    | `atom/1`                    |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#TypeAtom                               |
|                    | `integer/1`                 |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#TypeInteger                            |
|                    | `float/1`                   |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#TypeFloat                              |
|                    | `atomic/1`                  |  *   |                                                                                            |
|                    | `compound/1`                |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#TypeCompound                           |
|                    | `nonvar/1`                  |  *   |                                                                                            |
|                    | `number/1`                  |  *   | `number(N)` if either`integer(N)` or `float(N)`.                                           |
| Term Processing    | `functor/3`                 |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#Functor                                |
|                    | `arg/3`                     |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#Arg                                    |
|                    | `(=..)/2`                   |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#Univ                                   |
|                    | `copy_term/2`               |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#CopyTerm                               |
|                    | `compare/3`                 |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#Compare                                |
|                    | `(@=<)/2`                   |  *   |                                                                                            |
|                    | `(==)/2`                    |  *   |                                                                                            |
|                    | `(\==)/2`                   |  *   |                                                                                            |
|                    | `(@<)/2`                    |  *   |                                                                                            |
|                    | `(@>)/2`                    |  *   |                                                                                            |
|                    | `(@>=)/2`                   |  *   |                                                                                            |
| Arithmetic         | `is/2`                      |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#FunctionSet.Is                         |
|                    | `(=:=)/2`                   |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#FunctionSet.Equal                      |
|                    | `(=\=)/2`                   |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#FunctionSet.NotEqual                   |
|                    | `(<)/2`                     |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#FunctionSet.LessThan                   |
|                    | `(=<)/2`                    |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#FunctionSet.LessThanOrEqual            |
|                    | `(>)/2`                     |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#FunctionSet.LessThanOrEqual            |
|                    | `(>=)/2`                    |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#FunctionSet.GreaterThanOrEqual         |
| Clause             | `dynamic/1`                 |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.Dynamic                          |
|                    | `built_in/1`                |      | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.BuiltIn                          |
|                    | `clause/2`                  |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.Clause                           |
|                    | `current_predicate/1`       |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.CurrentPredicate                 |
|                    | `asserta/1`                 |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.Asserta                          |
|                    | `assertz/1`                 |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.Assertz                          |
|                    | `retract/1`                 |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.Retract                          |
|                    | `abolish/1`                 |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.Abolish                          |
| All Solutions      | `findall/3`                 |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.FindAll                          |
|                    | `bagof/3`                   |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.BagOf                            |
|                    | `setof/3`                   |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.SetOf                            |
| Stream             | `current_input/1`           |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.CurrentInput                     |
|                    | `current_output/1`          |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.CurrentOutput                    |
|                    | `set_input/1`               |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.SetInput                         |
|                    | `set_output/1`              |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.SetOutput                        |
|                    | `open/4`                    |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.Open                             |
|                    | `open/3`                    |  *   | `open/4` without options.                                                                  |
|                    | `close/2`                   |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.Open                             |
|                    | `close/1`                   |  *   | `close/2` without options.                                                                 |
|                    | `stream_property/2`         |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.StreamProperty                   |
|                    | `at_end_of_stream/1`        |  *   |                                                                                            |
|                    | `at_end_of_stream/0`        |  *   |                                                                                            |
|                    | `set_stream_position/2`     |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.SetStreamPosition                |
| Character I/O      | `get_char/2`                |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.GetChar                          |
|                    | `get_char/1`                |  *   |                                                                                            |
|                    | `get_code/2`                |  *   |                                                                                            |
|                    | `get_code/1`                |  *   |                                                                                            |
|                    | `peek_char/2`               |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.PeekChar                         |
|                    | `peek_char/1`               |  *   |                                                                                            |
|                    | `peek_code/2`               |  *   |                                                                                            |
|                    | `peek_code/1`               |  *   |                                                                                            |
|                    | `put_char/2`                |  *   |                                                                                            |
|                    | `put_char/1`                |  *   |                                                                                            |
|                    | `put_code/2`                |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.PutCode                          |
|                    | `put_code/1`                |  *   |                                                                                            |
|                    | `nl/1`                      |  *   |                                                                                            |
|                    | `nl/0`                      |  *   |                                                                                            |
| Binary I/O         | `get_byte/2`                |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.GetByte                          |
|                    | `get_byte/1`                |  *   |                                                                                            |
|                    | `peek_byte/2`               |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.PeekByte                         |
|                    | `peek_byte/1`               |  *   |                                                                                            |
|                    | `put_byte/2`                |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.PutByte                          |
|                    | `put_byte/1`                |  *   |                                                                                            |
| Term I/O           | `read_term/3`               |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.ReadTerm                         |
|                    | `read_term/2`               |  *   |                                                                                            |
|                    | `read/2`                    |  *   |                                                                                            |
|                    | `read/1`                    |  *   |                                                                                            |
|                    | `write_term/3`              |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.WriteTerm                        |
|                    | `write_term/2`              |  *   |                                                                                            |
|                    | `write/2`                   |  *   |                                                                                            |
|                    | `write/1`                   |  *   |                                                                                            |
|                    | `writeq/2`                  |  *   |                                                                                            |
|                    | `writeq/1`                  |  *   |                                                                                            |
|                    | `write_canonical/2`         |  *   |                                                                                            |
|                    | `write_canonical/1`         |  *   |                                                                                            |
| Operator           | `op/3`                      |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.Op                               |
|                    | `current_op/3`              |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.CurrentOp                        |
| Char Conversion    | `char_conversion/2`         |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.CharConversion                   |
|                    | `current_char_conversion/2` |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.CurrentCharConversion            |
| Atom Processing    | `atom_length/2`             |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#AtomLength                             |
|                    | `atom_concat/3`             |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#AtomLength                             |
|                    | `sub_atom/5`                |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#SubAtom                                |
|                    | `atom_chars/2`              |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#AtomChars                              |
|                    | `atom_codes/2`              |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#AtomCodes                              |
|                    | `char_code/2`               |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#AtomCodes                              |
|                    | `number_chars/2`            |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#NumberChars                            |
|                    | `number_codes/2`            |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#NumberCodes                            |
| Flag               | `set_prolog_flag/2`         |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.SetPrologFlag                    |
|                    | `current_prolog_flag/2`     |  *   | https://pkg.go.dev/github.com/ichiban/prolog/engine#State.CurrentPrologFlag                |
| Program            | `consult/1`                 |      | Load files or libraries.                                                                   |
|                    | `(.)/1`                     |      | `[foo, bar]` is equivalent to `consult([foo, bar])`.                                       |
| List Processing    | `append/3`                  |      | `append(ListA, ListB, ListAB)` means `ListAB` is the concatination of `ListA` and `ListB`. |
|                    | `member/2`                  |      | `member(Elem, List)` means `Elem` is a member of `List`.                                   |
|                    | `length/2`                  |      | `length(List, N)` means `N` is the length of `List`.                                       |


## License

Distributed under the MIT license. See `LICENSE` for more information.

## Contributing

See `ARCHITECTURE.md` for architecture details.

1. Fork it (https://github.com/ichiban/prolog/fork)
1. Create your feature branch (git checkout -b feature/fooBar)
1. Commit your changes (git commit -am 'Add some fooBar')
1. Push to the branch (git push origin feature/fooBar)
1. Create a new Pull Request
