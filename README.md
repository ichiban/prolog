# ![prolog - the only reasonable scripting engine for Go](prolog.gif)

[![Go Reference](https://pkg.go.dev/badge/github.com/ichiban/prolog.svg)](https://pkg.go.dev/github.com/ichiban/prolog)
[![Actions Status](https://github.com/ichiban/prolog/actions/workflows/go.yml/badge.svg)](https://github.com/ichiban/prolog/actions)
[![Go Report Card](https://goreportcard.com/badge/github.com/ichiban/prolog)](https://goreportcard.com/report/github.com/ichiban/prolog)
[![codecov](https://codecov.io/gh/ichiban/prolog/branch/main/graph/badge.svg?token=2FC3PZY7LN)](https://codecov.io/gh/ichiban/prolog)
[![Mentioned in Awesome Go](https://awesome.re/mentioned-badge.svg)](https://github.com/avelino/awesome-go)

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
if err := p.Exec(`human(?).`, "socrates"); err != nil { // Same as p.Exec(`human(socrates).`)
	panic(err)
}
```

#### Run the Prolog program

```go
sols, err := p.Query(`mortal(?).`, "socrates") // Same as p.Query(`mortal(socrates).`)
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

### Top Level

`1pl` is an experimental top level command for testing the default language and its compliance to the ISO standard.

You can install it with `go install`:

```console
go install github.com/ichiban/prolog/cmd/1pl@latest
```

Then, you can test the language:

```console
$(go env GOPATH)/bin/1pl [<file>...]
```

### Directives
- **`:- dynamic(PI)`** Specifies the predicates indicated by `PI` are dynamic. *ISO*
- **`:- multifile(PI)`** Not supported yet. *ISO*
- **`:- discontiguous(PI)`** Not supported yet. *ISO*
- **`:- op(Priority, Specifier, Op)`** Alters the operator table. *ISO*
- **`:- char_conversion(In, Out)`** Alters the character conversion mapping. *ISO*
- **`:- initialization(T)`** Not supported yet. *ISO*
- **`:- include(F)`** Not supported yet. *ISO*
- **`:- ensure_loaded(P)`** Not supported yet. *ISO*
- **`:- set_prolog_flag(Flag, Value)`** Alters the value for the Prolog flag. *ISO*
- **`:- built_in(PI)`** Specifies the predicates indicated by `PI` are built-in.

### Predicates

#### Control constructs
- **`true`** Always succeeds. *ISO*
- **`fail`** Always fails. *ISO*
- **`call(Goal)`** Calls `Goal`. *ISO*
- **`!`** Cut. *ISO*
- **`P, Q`** Conjunction. *ISO*
- **`P; Q`** Disjunction. *ISO*
- **`If -> Then`** If-then. *ISO*
- **`If -> Then; Else`** If-then-else. *ISO*
- **`catch(Goal, Catcher, Recovery)`** Calls `Goal`. If an exception is raised and unifies with `Catcher`, calls `Recovery`. *ISO*
- **`throw(Ball)`** Raises an exception `Ball`. *ISO*
#### Term unification
- **`X = Y`** Unifies `X` with `Y`. *ISO*
- **`unify_with_occurs_check(X, Y)`** Unifies `X` with `Y` with occurs check. *ISO*
- **`X \= Y`** Succeeds iff `X` and `Y` are not unifiable. *ISO*
- **`subsumes_term(General, Specific)`** Succeeds iff there's a substitution `θ` such that `Generalθ = Specificθ` and `Specificθ = Specific`. *ISO*
#### Type testing
- **`var(X)`** Succeeds iff `X` is a variable. *ISO*
- **`atom(X)`** Succeeds iff `X` is an atom. *ISO*
- **`integer(X)`** Succeeds iff `X` is an integer. *ISO*
- **`float(X)`** Succeeds iff `X` is a float. *ISO*
- **`atomic(X)`** Succeeds iff `X` is neither a variable nor a compound. *ISO*
- **`compound(X)`** Succeeds iff `X` is a compound. *ISO*
- **`nonvar(X)`** Succeeds iff `X` is not a variable. *ISO*
- **`number(X)`** Succeeds iff `X` is either an integer or a float. *ISO*
- **`callable(X)`** Succeeds iff `X` is either an atom or a compound. *ISO*
- **`ground(X)`** Succeeds iff `X` is a ground term. *ISO*
- **`acyclic_term(X)`** Succeeds iff `X` is acyclic. *ISO*
#### Term comparison
- **`X @=< Y`** Either `X == Y` or `X @< Y`. *ISO*
- **`X == Y`** Equivalent to `compare(=, X, Y)`. *ISO*
- **`X \== Y`** Equivalent to `\+compare(=, X, Y)`. *ISO*
- **`X @< Y`** Equivalent to `compare(<, X, Y)`. *ISO*
- **`X @> Y`** Equivalent to `compare(>, X, Y)`. *ISO*
- **`X @>= Y`** Either `X == Y` or `X @> Y`. *ISO*
- **`compare(Order, X, Y)`** Compares `X` and `Y` and unifies `Order` with either `<`, `=`, or `>`. *ISO*
- **`sort(List, Sorted)`** Succeeds iff `Sorted` unifies with a sorted list of `List`. *ISO*
- **`keysort(Pairs, Sorted)`** Succeeds iff `Pairs` is a list of `Key-Value` pairs and `Sorted` unifies with a permutation of `Pairs` sorted by `Key`. *ISO*
#### Term creation and decomposition
- **`functor(Term, Name, Arity)`** Succeeds iff `Term` ia either a compound term of `Name` and `Arity` or an atom of `Name` and `Arity = 0`. *ISO*
- **`arg(N, Term, Arg)`** Succeeds iff `Arg` is the `N`-th argument of `Term`. *ISO*
- **`Term =.. List`** Succeeds iff `List` is a list of the functor and arguments of `Term`. *ISO*
- **`copy_term(Term1, Term2)`** Creates a copy of `Term1` and unifies it with `Term2`. *ISO*
- **`term_variables(Term, Vars)`** Succeeds iff `Vars` is a list of variables appear in `Term`. *ISO*
#### Arithmetic evaluation
- **`Result is Expression`** Evaluates `Expression` and unifies it with `Result`. *ISO*
  - `Expression` is either:
    - integer,
    - float, or
    - <details><summary>evaluable functors</summary>

        - `X + Y`
        - `X - Y`
        - `X * Y`
        - `X // Y`
        - `X / Y`
        - `X rem Y`
        - `X mod Y`
        - `-X`
        - `abs(X)`
        - `sign(X)`
        - `float_integer_part(X)`
        - `float_fractional_part(X)`
        - `float(X)`
        - `floor(X)`
        - `truncate(X)`
        - `round(X)`
        - `ceiling(X)`
        - `+(X)`
        - `X div Y`
        - `X ** Y`
        - `sin(X)`
        - `cos(X)`
        - `atan(X)`
        - `exp(X)`
        - `log(X)`
        - `sqrt(X)`
        - `max(X, Y)`
        - `min(X, Y)`
        - `X ^ Y`
        - `asin(X)`
        - `acos(X)`
        - `atan2(X, Y)`
        - `tan(X)`
        - `pi`
        - `X >> Y`
        - `X << Y`
        - `X /\ Y`
        - `X \/ Y`
        - `\X`
        - `xor(X, Y)`
    
</details>

- **`succ(X, S)`** Succeeds iff `S` is the successor of the non-negative integer `X`. *prologue*

#### Arithmetic comparison
- **`E1 =:= E2`** Succeeds iff `E1` and `E2` are evaluated to `EV1` and `EV2` respectively and `EV1` equals to `EV2`. *ISO*
- **`E1 =\= E2`** Succeeds iff `E1` and `E2` are evaluated to `EV1` and `EV2` respectively and `EV1` does not equal to `EV2`. *ISO*
- **`E1 < E2`** Succeeds iff `E1` and `E2` are evaluated to `EV1` and `EV2` respectively and `EV1` is greater than `EV2`. *ISO*
- **`E1 =< E2`** Succeeds iff `E1` and `E2` are evaluated to `EV1` and `EV2` respectively and `EV1` is greater than or equal to `EV2`. *ISO*
- **`E1 > E2`** Succeeds iff `E1` and `E2` are evaluated to `EV1` and `EV2` respectively and `EV1` is less than `EV2`. *ISO*
- **`E1 >= E2`** Succeeds iff `E1` and `E2` are evaluated to `EV1` and `EV2` respectively and `EV1` is less than or equal to `EV2`. *ISO*
#### Clause retrieval and information
- **`clause(Head, Body)`** Succeeds iff `Head :- Body` unifies with a clause in the DB. *ISO*
- **`current_predicate(PI)`** Succeeds iff the predicate indicated by `PI` is in the DB. *ISO*
#### Clause creation and destruction
- **`asserta(Clause)`** Prepends `Clause` to the DB. *ISO*
- **`assertz(Clause)`** Appends `Clause` to the DB. *ISO*
- **`retract(Clause)`** Removes a clause that unifies with`Clause` from the DB. *ISO*
- **`abolish(PI)`** Removes the predicate indicated by `PI` from the DB. *ISO*
- **`retractall(Head)`** Removes all the clauses which head unifies with `Head` from the DB. *ISO*
#### All solutions
- **`findall(Template, Goal, Instances)`** Succeeds iff `Instances` unifies with a list of `Template` for each solution of `Goal`. *ISO*
- **`bagof(Template, Goal, Instances)`** Succeeds iff `Instances` unifies with a bag (multiset) of `Template` for each solution of `Goal`. *ISO*
- **`setof(Template, Goal, Instances)`** Succeeds iff `Instances` unifies with a set of `Template` for each solution of `Goal`. *ISO*
#### Stream selection and control
- **`current_input(Stream)`** Succeeds iff `Stream` unifies with the current input stream. *ISO*
- **`current_output(Stream)`** Succeeds iff `Stream` unifies with the current output stream. *ISO*
- **`set_input(S_or_a)`** Sets the current input stream to the stream indicated by `S_or_a` which is either the stream itself or its alias. *ISO*
- **`set_output(S_or_a)`** Sets the current output stream to the stream indicated by `S_or_a` which is either the stream itself or its alias. *ISO*
- **`open(File, Mode, Stream, Options)`** Opens the file `File` in `Mode` with `Options` and unifies the stream with `Stream`. *ISO*
  - `Mode` is either: `read`, `write`, or `append`.
  - `Options` is a list of stream options listed below:
    - **`type(T)`** Specifies the type of the stream. `T` is either `text` (default) or `binary`.
    - **`reposition(Bool)`** Specifies if the stream can be repositions. `Bool` is either `true` or `false`.
    - **`alias(A)`** Specifies the alias for the stream. `A` is an atom.
    - **`eof_action(Action)`** Specifies the action that will be taken when the input stream reached to the end. `Action` is either:
      - `error` which throws an exception,
      - `eof_code` which returns a value indicating the end of stream (default), or
      - `reset` which resets the stream.
- **`open(File, Mode, Stream)`** Equivalent to `open(File, Mode, Stream, [])`. *ISO*
- **`close(S_or_a, Options)`** Closes the stream indicated by `S_or_a` which is the stream itself or its alias. *ISO*
  - `Options` is a list of:
    - **`force(Bool)`** Specifies if an exception will be raised when it failed to close the stream. `Bool` is either `false` (default) or `true`.
- **`close(S_or_a)`** Equivalent to `close(S_or_a, [])`. *ISO*
- **`flush_output(S_or_a)`** Sends any buffered output to the stream indicated by `S_or_a` which is either the stream itself or its alias. *ISO*
- **`flush_output`** Equivalent to `current_output(S), flush_output(S)`. *ISO*
- **`stream_property(Stream, Property)`** Succeeds iff the stream `Stream` has the property `Property`. *ISO*
  - `Property` is either:
    - **`file_name(F)`**
    - **`mode(M)`**
    - **`input`**
    - **`output`**
    - **`alias(A)`**
    - **`position(P)`**
    - **`end_of_stream(E)`**
    - **`eof_action(A)`**
    - **`reposition(Bool)`**
    - **`type(T)`**
- **`at_end_of_stream`** Equivalent to `current_input(S), at_end_of_stream(S)`. *ISO*
- **`at_end_of_stream(S_or_a)`** Succeeds iff the stream indicated by `S_or_a` which is either a stream or an alias has the property either `end_of_stream(at)` or `end_of_stream(past)`. *ISO*
- **`set_stream_position(S_or_a, Position)`** Sets the position of the stream indicated by `S_or_a` which is either a stream or an alias to the position `Position`. *ISO*
#### Character input/output
- **`get_char(S_or_a, Char)`** Succeeds iff `Char` unifies with the next character from the stream indicated by `S_or_a` which is either a stream or an alias. *ISO*
- **`get_char(Char)`** Equivalent to `current_input(S), get_char(S, Char)`. *ISO*
- **`get_code(S_or_a, Code)`** Succeeds iff `Char` unifies with the next code from the stream indicated by `S_or_a` which is either a stream or an alias.  *ISO*
- **`get_code(Code)`** Equivalent to `current_input(S), get_code(S, Code)`. *ISO*
- **`peek_char(S_or_a, Char)`** Similar to `get_char(S_or_a, Char)` but doesn't consume the character. *ISO*
- **`peek_char(Char)`** Equivalent to `current_input(S), peek_char(S, Char)`. *ISO*
- **`peek_code(S_or_a, Code)`** Similar to `get_code(S_or_a, Code)` but doesn't consume the code. *ISO*
- **`peek_code(Code)`** Equivalent to `current_input(S), peek_code(S, Code)`. *ISO*
- **`put_char(S_or_a, Char)`** Outputs the character `Char` to the stream indicated by `S_or_a` which is either a stream or an alias. *ISO*
- **`put_char(Char)`** Equivalent to `current_output(S), put_char(S, Char)`. *ISO*
- **`put_code(S_or_a, Code)`** Outputs the code `Code` to the stream indicated by `S_or_a` which is either a stream or an alias. *ISO*
- **`put_code(Code)`** Equivalent to `current_output(S), put_code(S, Code)`. *ISO*
- **`nl(S_or_a)`** Outputs a newline to the stream indicated by `S_or_a` which is either a stream or an alias. *ISO*
- **`nl`** Equivalent to `current_output(S), nl(S)`. *ISO*
#### Byte input/output
- **`get_byte(S_or_a, Byte)`** Succeeds iff `Byte` unifies with the next byte from the stream indicated by `S_or_a` which is a stream or an alias. *ISO*
- **`get_byte(Byte)`** Equivalent to `current_input(S), get_byte(S, Byte)`. *ISO*
- **`peek_byte(S_or_a, Byte)`** Similar to `get_byte(S_or_a, Byte)` but doesn't consume the byte. *ISO*
- **`peek_byte(Byte)`** Equivalent to `current_input(S), peek_byte(S, Byte)`. *ISO*
- **`put_byte(S_or_a, Byte)`** Outputs the byte `Byte` to the stream indicated by `S_or_a` which is either a stream or an alias. *ISO*
- **`put_byte(Byte)`** Equivalent to `current_output(S), put_byte(S, Byte)`. *ISO*
#### Term input/output
- **`read_term(S_or_a, Term, Options)`** Succeeds iff `Term` unifies with the next term from the stream indicated by `S_or_a` which is a stream or an alias. *ISO*
  - `Options` is a list of read options listed below:
    - **`variables(Vars)`** the list of variables appeared in `Term`
    - **`variable_names(VN_list)`** the list of `A = V` where `A` is an atom which name is the string representation of `V` and `V` is a variable appeared in `Term`
    - **`singletons(VN_list)`** similar to `variable_names(VN_list)` but those of variables that appeared once.
- **`read_term(Term, Options)`** Equivalent to `current_input(S), read_term(S, Term, Options)`. *ISO*
- **`read(S_or_a, Term)`** Equivalent to `read_term(S_or_a, Term, [])`. *ISO*
- **`read(Term)`** Equivalent to `current_input(S), read(S, Term)`. *ISO*
- **`write_term(S_or_a, Term, Options)`** Outputs `Term` to the stream indicated by `S_or_a` which is either a stream or an alias. *ISO*
  - `Options` is a list of write options listed below:
    - **`quoted(Bool)`** `Bool` is either `true` or `false`. If `true`, atoms and functors will be quoted as needed.
    - **`ignore_ops(Bool)`** `Bool` is either `true` or `false`. If `true`, operators will be written in functional notation.
    - **`numbervars(Bool)`** `Bool` is either `true` or `false`. If `true`, terms `'$VAR'(0)`, `'$VAR'(1)`, ... will be written as `A`, `B`, ...
- **`write_term(Term, Options)`** Equivalent to `current_output(S), write_term(S, Term, Options)`. *ISO*
- **`write(S_or_a, Term)`** Equivalent to `write_term(S_or_a, Term, [numbervars(true)])`. *ISO*
- **`write(Term)`** Equivalent to `current_output(S), write(S, Term)`. *ISO*
- **`writeq(S_or_a, Term)`** Equivalent to `write_temr(S_or_a, Term, [quoted(true), numbervars(true)])`. *ISO*
- **`writeq(Term)`** Equivalent to `current_output(S), writeq(S, Term)`. *ISO*
- **`write_canonical(S_or_a, Term)`** Equivalent to `write_term(S_or_a, Term, [quoted(true), ignore_ops(true)])`. *ISO*
- **`write_canonical(Term)`** Equivalent to `current_output(S), write_canonical(S, Term)`. *ISO*
- **`op(Priority, Specifier, Operator)`** Alters the operator table. *ISO*
- **`current_op(Priority, Specifier, Operator)`** Succeeds iff the operator indicated by `Priority`, `Specifier`, `Operator` is in the operator table. *ISO*
- **`char_conversion(In, Out)`** Alters the character conversion mapping. *ISO*
- **`current_char_conversion(In, Out)`** Succeeds iff the character conversion from `In` to `Out` is in the conversion mapping. *ISO*
#### Logic and control
- **`\+Goal`** Succeeds iff `call(Goal)` fails. *ISO*
- **`once(Goal)`** Calls `Goal` but never redoes. *ISO*
- **`repeat`** Repeats the following code until it succeeds. *ISO*
- **`call(Closure, Arg1, ..., ArgN)`** `N = 1..7`. Succeeds iff `call(Goal)` where `Goal` is `Closure` with additional arguments `Arg1, ..., ArgN`. *ISO*
- **`false`** Equivalent to `fail`. *ISO*
- **`between(Lower, Upper, X)`** Succeeds iff `Lower <= X <= Upper`. *prologue*
#### Atomic term processing
- **`atom_length(Atom, Length)`** Succeeds iff `Length` is the number of runes in `Atom`. *ISO*
- **`atom_concat(Atom1, Atom2, Atom3)`** Succeeds iff `Atom3` is a concatenation of `Atom1` and `Atom2`. *ISO*
- **`sub_atom(Atom, Before, Length, After, SubAtom)`** Succeeds iff `SubAtom` is a sub atom of `Atom` where `Before` is the number of runes before `SubAtom`, `Length` is the length of `SubAtom`, and `After` is the number of runes after `SubAtom`. *ISO*
- **`atom_chars(Atom, List)`** Succeeds iff `List` is the list of single-rune atoms that `Atom` consists of. *ISO*
- **`atom_codes(Atom, List)`** Succeeds iff `List` is the list of runes that `Atom` consists of. *ISO*
- **`char_code(Char, Code)`** Succeeds iff `Char` is a single-rune atom which rune is `Code`. *ISO*
- **`number_chars(Number, List)`** Succeeds iff `List` is the list of single-rune atoms that represents `Number`. *ISO*
- **`number_codes(Number, List)`** Succeeds iff `List` is the list of runes that represents `Number`. *ISO*
#### Implementation defined hooks
- **`set_prolog_flag(Flag, Value)`** Sets the Prolog flag `Flag` to `Value`. *ISO*
  - `Flag` is either:
    - **`char_conversion`** `Value` is either `on` or `off` (default).
    - **`debug`** `Value` is either `on` or `off` (default).
    - **`unknown`** `Value` is either `error` (default), `fail`, or `warning`.
    - **`double_quotes`** `Value` is either `chars` (default), `codes`, or `atom`.
- **`current_prolog_flag(Flag, Value)`** Succeeds iff `Value` is the current value for the Prolog flag `Flag`. *ISO*
  - `Flag` is either:
    - **`bounded`** `Value` is always `true`.
    - **`max_integer`** `Value` is always `9223372036854775807`.
    - **`min_integer`** `Value` is always `-9223372036854775808`.
    - **`integer_rounding_function`** `Value` is always `toward_zero`.
    - **`char_conversion`** `Value` is either `on` or `off` (default).
    - **`debug`** `Value` is either `on` or `off` (default).
    - **`max_arity`** `Value` is always `unbounded`.
    - **`unknown`** `Value` is either `error` (default), `fail`, or `warning`.
    - **`double_quotes`** `Value` is either `chars` (default), `codes`, or `atom`.
- **`halt(X)`** Exists the host program with the status code of `X`. *ISO*
- **`halt`** Equivalent to `halt(0)`. *ISO*
#### Definite clause grammars
- **`expand_term(In, Out)`** Succeeds iff `Out` is an expansion of the term `In`.
- **`phrase(Phrase, List, Remainder)`** Succeeds iff the different list `List-Remainder` satisfies the grammar rule `Phrase`.
- **`phrase(Phrase, List)`** Equivalent to `phrase(Phrase, List, [])`.
#### List processing
- **`member(X, L)`** Succeeds iff `X` is a member of the list `L`. *prologue*
- **`append(Xs, Ys, Zs)`** Succeeds iff `Zs` is the concatenation of `Xs` and `Ys`. *prologue*
- **`length(List, Length)`** Succeeds iff `Length` is the length of `List`. *prologue*
- **`select(X, Xs, Ys)`** Succeeds iff `X` is an element of `Xs` and `Ys` is `Xs` with one occurence of `X` removed. *prologue*
- **`maplist(Goal, List1, ..., Listn)`** `n = 1..7`. Succeeds iff `List1, ..., Listn` are the list of the same length and `call(Goal, List1_i, ..., Listn_i)` succeeds for all the `i`-th elements of `List1, ..., Listn`. *prologue*
- **`nth0(N, List, Elem)`** Succeeds iff `Elem` is the `N`-th element of `List` counting from 0.
- **`nth1(N, List, Elem)`** Succeeds iff `Elem` is the `N`-th element of `List` counting from 1.
#### Program
- **`consult(File)`** Loads Prolog program files indicated by `File`. `File` is either an atom or a list of atoms. An atom `abc` indicates a Prolog program file `./abc` or `./abc.pl`.
- **`[File|Files]`** Equivalent to `consult([File|Files])`.
#### Operating system interface
- **`environ(Name, Value)`** Succeeds iff the environment variable `Name` has the value `Value`.

## Extensions

- **[predicates](https://github.com/guregu/predicates):** Native predicates for ichiban/prolog.

## License

Distributed under the MIT license. See `LICENSE` for more information.

## Contributing

See `ARCHITECTURE.md` for architecture details.

1. Fork it (https://github.com/ichiban/prolog/fork)
1. Create your feature branch (git checkout -b feature/fooBar)
1. Commit your changes (git commit -am 'Add some fooBar')
1. Push to the branch (git push origin feature/fooBar)
1. Create a new Pull Request

## Acknowledgments

- [A PORTABLE PROLOG COMPILER (Bowen et al. 83)](http://www.softwarepreservation.org/projects/prolog/lisbon/lpw83/p74-Bowen.pdf)
- [ISO Prolog works](http://www.complang.tuwien.ac.at/ulrich/iso-prolog/) by Prof. Ulrich Neumerkel
- [SWI Prolog](https://www.swi-prolog.org/) and its documentation
- [GNU Prolog](http://www.gprolog.org/) and its documentation