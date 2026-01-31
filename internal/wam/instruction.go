package wam

type Instruction struct {
	Op OpCode
	I  uint8  // Operand for Xi, Ai
	N  uint16 // Operand for f/n, L
}

type OpCode uint8

const (
	OpNop OpCode = iota
	OpPutVariable
	OpPutStructure
	OpGetStructure
	OpUnifyVariable
	OpWriteVariable
	OpUnifyValue
	OpWriteValue
	OpExecute
	OpBuiltin
	OpProceed
	OpTryMeElse
	OpRetryMeElse
	OpTrustMe
	OpMove
	OpNondet
	OpSwitch
	OpPushCut
	OpPutCut
	OpGetCut
)

type Mode uint8

const (
	ModeRead Mode = iota
	ModeWrite
)
