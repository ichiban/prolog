package wam

import "fmt"

type Instruction struct {
	Op OpCode
	I  uint8  // Operand for Xi, Ai
	N  uint16 // Operand for f/n, L
}

type OpCode uint8

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

	OpWriteVariable
	OpWriteValue
	OpWriteConstant

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

	OpLoadVariable
	OpLoadValue
	OpLoadConstant
	OpBuiltin0

	/* TODO: We're not going to define those specific opcodes but need to define Builtins for them.
	OpEvaluate
	OpEqual
	OpNotEqual
	OpLessThan
	OpLessThanOrEqual
	OpGreaterThan
	OpGreaterThanOrEqual
	OpAdd
	OpSubtract
	OpMultiply
	OpIntDivide
	OpDivide
	OpRemainder
	OpModulo
	OpNegate
	OpAbsolute
	OpSign
	OpFloatIntegerPart
	OpFloatFractionalPart
	OpFloat
	OpFloor
	OpTruncate
	OpRound
	OpCeiling
	OpPower
	OpSine
	OpCosine
	OpArcTangent
	OpExponential
	OpLogarithm
	OpSquareRoot
	OpMaximum
	OpMinimum
	OpIntegerPower
	OpArcSine
	OpArcCosine
	OpArcTangent2
	OpTangent
	OpPi
	OpBitwiseRightShift
	OpBitwiseLeftShift
	OpBitwiseAnd
	OpBitwiseOr
	OpBitwiseComplement
	OpBitwiseXor
	*/
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
	OpWriteVariable: "write_variable",
	OpWriteValue:    "write_value",
	OpWriteConstant: "write_constant",
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
	OpLoadVariable:  "load_variable",
	OpLoadValue:     "load_value",
	OpLoadConstant:  "load_constant",
}

type Mode uint8

const (
	ModeRead Mode = iota
	ModeWrite
)
