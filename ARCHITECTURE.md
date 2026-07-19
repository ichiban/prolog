# Architecture

## Overview

This library consists of several packages:

- **`prolog`:** high-level interface for the processor
- **`prolog/internal/backend`:** IR to WAM compilation
- **`prolog/internal/frontend`:** AST to IR compilation
- **`prolog/internal/ir`:** Intermediate Representation
- **`prolog/internal/ring`:** ring buffer
- **`prolog/internal/syntax`:** parser and formatter
- **`prolog/internal/term`:** terms and heap
- **`prolog/internal/wam`:** Warren Abstract Machine
- **`prolog/cmd/1pl`:** simple toplevel to explore the implementation

## Virtual Machine

We based our virtual machine on BinWAM, a variant of WAM.

### Registers

- $`X_0`$ Temporary for inline/arithmetic builtins
- $`X_1`$, $`A_1`$ First argument `p(X, _, _, ..., _)`
- $`X_2`$, $`A_2`$ Second argument `p(_, X, _, ..., _)`
- ...
- $`X_n`$, $`A_n`$ Last argument `p(_, _, _, ..., X)`
- $`X_{n+1}`$ Variable
- ...