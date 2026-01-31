package ir

type Instruction struct {
	Op   Op
	Type Type
	A, B Operand
}

type Op int8

const (
	OpInvalid Op = iota
	OpPut
	OpGet
	OpWrite
	OpUnify
	OpInline
	OpLoad
	OpArithmetic
)

type Type int8

const (
	TypeUnknown Type = iota
	TypeVariable
	TypeValue
	TypeConstant
	TypeStructure
	TypeCut
)

type Operand struct {
	Kind  OperandKind
	Value int
}

type OperandKind int8

const (
	OperandKindNone OperandKind = iota
	OperandKindArg
	OperandKindTemp
	OperandKindImmediate
)
