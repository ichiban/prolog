package ir

import (
	"fmt"

	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
)

type Instruction struct {
	OpCode OpCode
	Type   Type
	A      Operand
	B      Operand
}

func (i Instruction) String() string {
	return fmt.Sprintf("%s_%s %s %s", i.OpCode, i.Type, i.A, i.B)
}

type OpCode int8

const (
	OpInvalid OpCode = iota
	OpBuiltin
	OpPut
	OpGet
	OpWrite
	OpUnify
	OpPush
	OpInline
	OpLoad
	OpArithmetic
)

var opCodeNames = [...]string{
	OpInvalid:    "invalid",
	OpBuiltin:    "builtin",
	OpPut:        "put",
	OpGet:        "get",
	OpWrite:      "write",
	OpUnify:      "unify",
	OpPush:       "push",
	OpInline:     "inline",
	OpLoad:       "load",
	OpArithmetic: "arith",
}

func (o OpCode) String() string {
	return opCodeNames[o]
}

type Type int8

const (
	TypeUnknown Type = iota
	TypeVoid
	TypeVariable
	TypeValue
	TypeConstant
	TypeStructure
	TypeCut
)

var typeNames = [...]string{
	TypeUnknown:   "?",
	TypeVoid:      "void",
	TypeVariable:  "variable",
	TypeValue:     "value",
	TypeConstant:  "constant",
	TypeStructure: "structure",
	TypeCut:       "cut",
}

func (o Type) String() string {
	return typeNames[o]
}

type Operand struct {
	Kind    OperandKind
	Index   int
	Functor term.Functor
	Term    term.Handle
}

func (o Operand) String() string {
	switch o.Kind {
	case OperandKindArgument:
		return fmt.Sprintf("arg(%d)", o.Index)
	case OperandKindTerm:
		return fmt.Sprintf("%s", &syntax.Formatter{Term: o.Term})
	case OperandKindOccurrence:
		return fmt.Sprintf("occ(%s, %d)", &syntax.Formatter{Term: o.Term}, o.Index)
	case OperandKindFunctor:
		f := o.Functor
		return fmt.Sprintf("%s/%d", f.Name(), f.Arity())
	case OperandKindBuiltin:
		return fmt.Sprintf("builtin(%d)", o.Index)
	case OperandKindCutArg:
		return fmt.Sprintf("cutarg(%d)", o.Index)
	case OperandKindTemp:
		return fmt.Sprintf("temp(%d)", o.Index)
	case OperandKindGet:
		return "get"
	case OperandKindPut:
		return "put"
	case OperandKindRegister:
		return fmt.Sprintf("reg(%d)", o.Index)
	default:
		return ""
	}
}

type OperandKind int8

const (
	OperandKindEmpty OperandKind = iota
	OperandKindArgument
	OperandKindTerm
	OperandKindOccurrence
	OperandKindFunctor
	OperandKindBuiltin
	OperandKindCutArg
	OperandKindTemp
	OperandKindGet
	OperandKindPut
	OperandKindRegister
)
