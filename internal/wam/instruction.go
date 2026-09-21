// The instruction set follows BinWAM, the abstract machine of BinProlog
// (github.com/ptarau/binprolog, src/global.h) by Paul Tarau: of the
// opcodes below, all but Nop, Builtin, PutValue and GetVariable correspond to
// one of BinWAM's. This is an independent implementation of that design rather
// than a port of binprolog's code. See the Provenance section of
// ARCHITECTURE.md.

package wam

import "fmt"

type Instruction struct {
	Op OpCode
	I  uint16 // Operand for Xi, Ai
	N  uint32 // Operand for c, f/n, L, An
}

// OpCode tells what action to perform.
type OpCode uint16

func (o OpCode) String() string {
	if o >= OpBuiltin0 {
		return fmt.Sprintf("builtin %d", o-OpBuiltin0)
	}
	return OpCodeNames[o]
}

const (
	OpNop OpCode = iota

	OpPutVariable
	OpPutValue
	OpPutStructure
	OpPutConstant

	OpGetVariable
	OpGetValue
	OpGetConstant
	OpGetStructure

	OpUnifyVariable
	OpUnifyValue
	OpUnifyConstant
	OpUnifyVoid

	OpWriteVariable
	OpWriteValue
	OpWriteConstant
	OpWriteVoid

	OpPushVariable
	OpPushStructure

	OpExecute
	OpProceed

	OpTryMeElse
	OpRetryMeElse
	OpTrustMe

	OpSwitch
	OpNondet

	OpMove

	OpPutCut
	OpGetCut
	OpPushCut

	// OpBuiltin0 marks the beginning of the builtin section.
	OpBuiltin0
)

var OpCodeNames = [...]string{
	OpNop:           "nop",
	OpPutVariable:   "put_variable",
	OpPutValue:      "put_value",
	OpPutStructure:  "put_structure",
	OpPutConstant:   "put_constant",
	OpGetVariable:   "get_variable",
	OpGetValue:      "get_value",
	OpGetConstant:   "get_constant",
	OpGetStructure:  "get_structure",
	OpUnifyVariable: "unify_variable",
	OpUnifyValue:    "unify_value",
	OpUnifyConstant: "unify_constant",
	OpUnifyVoid:     "unify_void",
	OpWriteVariable: "write_variable",
	OpWriteValue:    "write_value",
	OpWriteConstant: "write_constant",
	OpWriteVoid:     "write_void",
	OpPushVariable:  "push_variable",
	OpPushStructure: "push_structure",
	OpExecute:       "execute",
	OpProceed:       "proceed",
	OpTryMeElse:     "try_me_else",
	OpRetryMeElse:   "retry_me_else",
	OpTrustMe:       "trust_me",
	OpSwitch:        "switch",
	OpNondet:        "nondet",
	OpMove:          "move",
	OpPutCut:        "put_cut",
	OpGetCut:        "get_cut",
	OpPushCut:       "push_cut",
}

type Mode uint8

const (
	ModeRead Mode = iota
	ModeWrite
)
