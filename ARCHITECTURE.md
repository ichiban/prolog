# Architecture

## Overview

This library consists of several packages:

- **`prolog`:** `database/sql`-like high-level interface for interpreter
- **`prolog/engine`:** virtual machine and other implementation details
- **`prolog/dcg`:** Definite Clause Grammar library
- **`prolog/cmd/1pl`:** simple toplevel
- **`prolog/examples`:** example programs

## Virtual Machine

We based our VM on one described in [A PORTABLE PROLOG COMPILER (Bowen et al. 83)](http://www.softwarepreservation.org/projects/prolog/lisbon/lpw83/p74-Bowen.pdf) which is presumably referred as ZIP by others.

ZIP is a high-level VM written in Prolog.
Thus, we had to bring missing control features in Go like nondeterminism and cut.
Hence, our deviation from the original ZIP arose.

### Instructions

We use the same VM opcodes you can find in the original paper:

- `opConst`
- `opVar`
- `opFunctor`
- `opPop`
- `opEnter`
- `opCall`
- `opExit`

Also, we've added some extra opcodes:

- `opCut` to perform cut operation

### Registers

We use the same registers you can find in the original paper:

- `pc`
- `xr`
- `vars`
- `cont`
- `args`
- `astack`

Also, we've added some extra registers:

- `pi` to store procedure indicators instead of `xr`
- `env` to keep track of variable bindings (environment)
- `cutParent` to keep track of cut parent
