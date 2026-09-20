# Architecture

## Overview

This library consists of several packages:

- **`prolog`:** high-level interface for the processor
- **`prolog/internal/db`:** clause database
- **`prolog/internal/ir`:** Intermediate Representation
- **`prolog/internal/ring`:** ring buffer and buffered rune reader
- **`prolog/internal/runtime`:** compiler, virtual machine, and built-in predicates
- **`prolog/internal/syntax`:** parser and formatter
- **`prolog/internal/term`:** terms and heap
- **`prolog/internal/wam`:** BinWAM instructions and code image
- **`prolog/cmd/1pl`:** simple toplevel to explore the implementation
- **`prolog/examples`:** runnable examples, each its own module

## Virtual Machine

We based our virtual machine on BinWAM, a variant of WAM.

### Registers

- $`X_0`$ Temporary for inline builtins
- $`X_1`$, $`A_1`$ First argument `p(X, _, _, ..., _)`
- $`X_2`$, $`A_2`$ Second argument `p(_, X, _, ..., _)`
- ...
- $`X_n`$, $`A_n`$ Last argument `p(_, _, _, ..., X)`
- $`X_{n+1}`$ Variable
- ...

## Module System

The module system follows *Implementing a Module System for SICStus Prolog*
(Stefan Andersson, SICS technical report T91:08), which is Quintus compatible:
**procedure based**, so atoms and functors stay global and only predicates are
local to a module, and **flat**, so every module is visible to every other.
There are two predefined modules, `prolog`, where the built-ins live, and
`user`, the default.

A predicate is named by `term.Procedure`, a module plus a functor, and that is
what keys `Image.Predicates` and what the `execute` and `switch` operands index.
A call resolves in this order:

1. the calling module's own definition;
2. what it imports, following a re-export to the module that defines it;
3. the `prolog` module, whose exported built-ins are visible everywhere.

The report gives each module a copy of the origin's definition record so that a
call is one lookup. `Engine.Lookup` walks the chain instead: a local call still
costs one map lookup, a built-in costs two, and nothing can go stale. Caching
the result is the upgrade path if a profile ever asks for it.

### Continuations carry their module

This is the one place where a BinWAM needs more than the report describes.

Section 1.2 says that in a procedure based system, a term used as a procedure
reference must carry a module name. A binarized continuation is exactly such a
term: `p :- q, r` compiles to `p(C) :- q(r(C))`, and `r(C)` is executed by
`true/1` an arbitrary number of calls later, in a clause that knows nothing
about where `r` was written. Neither register nor choice point can hold that
module, because a continuation outlives both.

So every continuation link is prefixed with the module of the clause that built
it, and `true/1` strips the prefix to resolve the goal. Links of the `prolog`
module stay bare, and a bare link is read as `prolog`'s: those are the bulk of
every continuation, which keeps the system library and every built-in call free
of the extra term. A user module's clause pays three heap cells per body goal
after the first. The first goal is the clause's own `execute`, whose module the
image records, so it stays bare too.

`(:)/2` is a built-in rather than a compiled form, so an explicit `M:G` in a
body becomes a call to it. When it, or any metacall, is handed a control
construct, the module is distributed over the branches and the construct itself
left bare — a clause like `(If -> Then; Else)` matches on the shape.

### Meta expansion

`meta_predicate` says which arguments of a call are module name expanded. The
compiler prefixes them with the source module, so a goal handed to `findall/3`
or a clause handed to `assertz/1` is called where it was written rather than in
whatever module happens to be running. A variable that appears in a meta
expandable position of the clause head is left alone: the caller expanded it
already.

The control constructs are deliberately **not** declared, though the report
declares them. This machine compiles `,/2`, `;/2`, `->/2` and `\+/1` into the
clause, so their goals already run where they were written, and a prefix would
turn a direct call into a metacall. Distribution at metacall time covers the
case where one survives as a term.

Goals issued at the top level or as a directive reach the system unexpanded, so
a metacall expands what it is about to call as well. That costs one map lookup
for a goal that is not a meta predicate, and nothing for a goal already
expanded, whose arguments carry a prefix.

While a text is being loaded, the type-in module is the module the text goes
to, as the report has it. That is what makes `dynamic/1`, `op/3` and an
asserting directive act on the right module without a declaration of their own.

### Not implemented

| Report | Why |
| ------ | --- |
| Erasing a module's predicates when its file is reloaded (2.4) | The image is append-only; a reload adds to the module. Marked `ponytail:` in `declareModule`. |
| `predicate_property/2` extensions (2.11) | The predicate itself doesn't exist here yet. |
| File-to-file compilation, the Emacs interface, delayed goals (2.8, 2.9, 2.13) | No counterpart in this system. |

## Provenance

Most of this repository is original work under the MIT license (`LICENSE`). Some
parts derive from Paul Tarau's [binprolog](https://github.com/ptarau/binprolog),
which is Apache-2.0 (`LICENSE-binprolog`). Those files carry a header naming the
upstream source and stating that the file was modified, as Apache-2.0 §4(b)
requires. Upstream ships no `NOTICE` file, so §4(d) adds nothing.

This section records which is which, so that the question doesn't have to be
re-derived from git history each time a file is split or moved.

### Ported from binprolog

These carry an attribution header. Keep it when moving code between them, and add
it to any new file split out of them.

| File | Upstream |
| ---- | -------- |
| `internal/runtime/execution.go` | `src/engine.c` |
| `internal/runtime/compiler.go` | `src/co.pl` |
| `internal/runtime/builtin.go` | `src/extra.pl` |
| `internal/runtime/bootstrap.pl` | `src/lib.pl`, `src/extra.pl` |

Note that `builtin.go` holds original code alongside ported code under a single
header, so the header claims more for upstream than is strictly true. That errs in
the safe direction, but it means a split of `builtin.go` needs the provenance of
the moved code checked rather than assumed.

### Implemented from the BinWAM design

`internal/wam` implements BinWAM's core instruction set rather than a textbook
WAM, and uses its names. Of its 31 opcodes, 27 correspond to instructions in
binprolog's `src/global.h` — two of them renamed, `Move` for `MOVE_REG` and
`TrustMe` for `TRUST_ME_ELSE`. `PutValue` and `GetVariable` are ordinary WAM;
only `Nop` and `Builtin` are new here. The correspondence is not incidental:
`PushCut`/`PutCut`/`GetCut`, `Nondet`, and the `Write*` family are binprolog's
rather than Warren's, and the absence of `allocate`/`deallocate`/`call` is
binarization, BinWAM's defining property.

`internal/wam/instruction.go` and `internal/ir/instruction.go` carry a header
saying so. It is deliberately *not* the Apache-2.0 header used by the ported
files above: these are original implementations, and claiming otherwise would
both misstate the fact and cast doubt on the MIT licensing of original work.

What it is not is a transliteration of `src/global.h`. It takes the core and
leaves behind binprolog's specialization and compression families
(`GET_UNIFY_VAR_VAR`, `PUT_WRITE_VAL_VAL`, `EXEC_SWITCH`, and the rest of
`STRUCT_COMPRESS` and `JUMP_COMPRESS`), its `LOAD_*` instructions, and its
C-chunk interface. An instruction set is a design, and BinWAM's was published to
be implemented; this is an implementation of it in that design's vocabulary,
written in Go against `internal/ir`.

### Original to this repository

| Path | Note |
| ---- | ---- |
| `internal/runtime/engine.go` | Module loading and code emission. No counterpart in `src/engine.c` or `src/load.c`; binprolog loads pre-compiled bytecode instead. |
| `internal/runtime/type.go` | ISO type and domain checks. |
| `internal/ir` | Orthogonal `OpCode` × `Type` encoding, where binprolog enumerates each combination as a distinct opcode. This is why `internal/wam` needs far fewer opcodes than `src/global.h`. The vocabulary is BinWAM's, so `instruction.go` carries a design-acknowledgment header. |
| `internal/term` | 13 cell tags against binprolog's four `PUTTAG`-encoded ones, and concepts binprolog has no counterpart for (streams, packed strings). Tagged cells are Warren's, not Tarau's. |
| `internal/wam/image.go` | `Predicate` and `Image` layout. binprolog keeps predicates in a `predmark` hash table and code in a flat cell array; the sidecar `Constants`/`Functors` tables here exist because Go has no union types. |
| `internal/runtime/module.go` | Module records, name resolution, importation and meta expansion. binprolog has no module system; see the Module System section above for what it follows instead. |
| `internal/runtime/arithmetic.go` | From v1's `engine/number.go`, `engine/integer.go` and `engine/float.go`. It passed through `builtin.go` during the rewrite, so git history makes it look binprolog-derived; it isn't. |

When splitting a file, check whether the code came from binprolog before deciding
whether the new file needs a header. Comparing against v1 settles most cases.
