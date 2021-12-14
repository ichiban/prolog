# Introduction

This document is a reference manual for the Prolog dialect `ichiban/prolog` implements.

`ichiban/prolog` is an embeddable scripting engine for Go.
For the details of the library itself, please refer to [the project GitHub page](https://github.com/ichiban/prolog).

Note that the Prolog dialect described in this document is one that `ichiban/prolog` implements by default.
Since the library is highly configurable, the authors of the host program can alter most of the aspects of the language.
Please refer to the documents of the host program for details.

## Notations

`Name/Arity`

```prolog
:- dynamic(foo/1).
foo(bar).
foo(baz) :- true.
```

```prolog
?- foo(X).
X = bar ;
X = baz .
```

## Syntax

### Facts

### Rules

### Queries

### Directives

# Control structures

## `true/0`

```prolog
?- true.
true.
```

## `fail/0`, `false/0`

```prolog
?- fail.
false.
?- false.
false.
```

## `!/0`
## `(,)/2`

```prolog
?- true, true.
true.
?- true, false.
false.
?- false, true.
false.
?- false, false.
false.
```

## `(;)/2`

```prolog
?- true; true.
true.
?- true; false.
true.
?- false; true.
true.
?- false; false.
false.
```

```prolog
?- true -> true; true.
true.
?- true -> true; false.
true.
?- true -> false; true.
false.
?- true -> false; false.
false.
?- false -> true; true.
true.
?- false -> true; false.
false.
?- false -> false; true. 
true.
?- false -> false; false.
false.
```

## `(->)/2`

```prolog
?- true -> true.         
true.
?- true -> false.
false.
?- false -> true. 
false.
?- false -> false.
false.
```

## `(\+)/1`

```prolog
?- \+true.        
false.
?- \+false.
true.
```

## `call/1`
## `catch/3`
## `throw/1`
## `once/1`
## `repeat/0`
## `halt/0`
## `halt/1`

# Data types

## Variables

### `var/1`
### `nonvar/1`

## Atoms

### `atom/1`

### `atom_chars/2`
### `atom_codes/2`
### `atom_concat/3`
### `atom_length/2`
### `char_code/2`
### `sub_atom/5`

## Numbers

### `number/1`
### `number_chars/2`
### `number_codes/2`

### `integer/1`
### `float/1`

## Compounds

### `compound/1`
### `atomic/1`
### `append/3`
### `length/2`
### `member/2`

## Streams

### `current_input/1`
### `current_output/1`
### `set_input/1`
### `set_output/1`
### `open/3`
### `open/4`
### `close/1`
### `close/2`
### `flush_output/0`
### `flush_output/1`
### `stream_property/2`
### `set_stream_position/2`
### `at_end_of_stream/0`
### `at_end_of_stream/1`

# Unification

## `(=)/2`
## `unify_with_occurs_check/2`
## `(\=)/2`

# Term comparison

## `(==)/2`
## `(\==)/2`
## `(@<)/2`
## `(@=<)/2`
## `(@>)/2`
## `(@>=)/2`
## `compare/3`

# Term processing

## `functor/3`
## `arg/3`
## `(=..)/2`
## `copy_term/2`

# Arithmetic

## `is/2`
## `(=:=)/2`
## `(=\=)/2`
## `(<)/2`
## `(=<)/2`
## `(>)/2`
## `(>=)/2`

# Clause management

## `asserta/1`
## `assertz/1`
## `retract/1`
## `clause/2`
## `abolish/1`
## `dynamic/1`
## `current_predicate/1`
## `built_in/1`

# Aggregation

## `bagof/3`
## `setof/3`
## `findall/3`

# Text I/O

## `get_char/1`
## `get_char/2`
## `get_code/1`
## `get_code/2`
## `peek_char/1`
## `peek_char/2`
## `peek_code/1`
## `peek_code/2`
## `put_char/1`
## `put_char/2`
## `put_code/1`
## `put_code/2`
## `read/1`
## `read/2`
## `read_term/2`
## `read_term/3`
## `write/1`
## `write/2`
## `write_canonical/1`
## `write_canonical/2`
## `write_term/2`
## `write_term/3`
## `writeq/1`
## `writeq/2`
## `nl/0`
## `nl/1`

# Binary I/O

## `get_byte/1`
## `get_byte/2`
## `peek_byte/1`
## `peek_byte/2`
## `put_byte/1`
## `put_byte/2`

# Term expansion

## `expand_term/2`

# Program

## `(.)/2`
## `consult/1`

# Char conversion

## `char_conversion/2`
## `current_char_conversion/2`

# Operator definition

## `op/3`
## `current_op/3`

# Flags

## `set_prolog_flag/2`
## `current_prolog_flag/2`
