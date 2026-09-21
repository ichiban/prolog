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
| `internal/runtime/arithmetic.go` | From v1's `engine/number.go`, `engine/integer.go` and `engine/float.go`. It passed through `builtin.go` during the rewrite, so git history makes it look binprolog-derived; it isn't. |

When splitting a file, check whether the code came from binprolog before deciding
whether the new file needs a header. Comparing against v1 settles most cases.
