package engine

import (
	"bytes"
)

// Exception is an error represented by a prolog term.
type Exception struct {
	term Term
}

// NewException creates an exception from a copy of the given term.
func NewException(term Term, env *Env) Exception {
	return Exception{term: copyTerm(term, nil, env)}
}

// Term returns the underlying term of the exception.
func (e Exception) Term() Term {
	return e.term
}

func (e Exception) Error() string {
	var buf bytes.Buffer
	_ = Write(&buf, e.term, nil, WithQuoted(true))
	return buf.String()
}

// InstantiationError returns an instantiation error exception.
func InstantiationError(env *Env) Exception {
	return NewException(&Compound{
		Functor: "error",
		Args: []Term{
			Atom("instantiation_error"),
			varContext,
		},
	}, env)
}

type ValidType uint8

const (
	ValidTypeAtom ValidType = iota
	ValidTypeAtomic
	ValidTypeByte
	ValidTypeCallable
	ValidTypeCharacter
	ValidTypeCompound
	ValidTypeEvaluable
	ValidTypeInByte
	ValidTypeInCharacter
	ValidTypeInteger
	ValidTypeList
	ValidTypeNumber
	ValidTypePredicateIndicator
	ValidTypePair
	ValidTypeFloat
)

func (t ValidType) Term() Term {
	return [...]Atom{
		ValidTypeAtom:               "atom",
		ValidTypeAtomic:             "atomic",
		ValidTypeByte:               "byte",
		ValidTypeCallable:           "callable",
		ValidTypeCharacter:          "character",
		ValidTypeCompound:           "compound",
		ValidTypeEvaluable:          "evaluable",
		ValidTypeInByte:             "in_byte",
		ValidTypeInCharacter:        "in_character",
		ValidTypeInteger:            "integer",
		ValidTypeList:               "list",
		ValidTypeNumber:             "number",
		ValidTypePredicateIndicator: "predicate_indicator",
		ValidTypePair:               "pair",
		ValidTypeFloat:              "float",
	}[t]
}

// TypeError creates a new type error exception.
func TypeError(validType ValidType, culprit Term, env *Env) Exception {
	return NewException(&Compound{
		Functor: "error",
		Args: []Term{
			&Compound{
				Functor: "type_error",
				Args:    []Term{validType.Term(), culprit},
			},
			varContext,
		},
	}, env)
}

type ValidDomain uint8

const (
	ValidDomainCharacterCodeList ValidDomain = iota
	ValidDomainCloseOption
	ValidDomainFlagValue
	ValidDomainIOMode
	ValidDomainNonEmptyList
	ValidDomainNotLessThanZero
	ValidDomainOperatorPriority
	ValidDomainOperatorSpecifier
	ValidDomainPrologFlag
	ValidDomainReadOption
	ValidDomainSourceSink
	ValidDomainStream
	ValidDomainStreamOption
	ValidDomainStreamOrAlias
	ValidDomainStreamPosition
	ValidDomainStreamProperty
	ValidDomainWriteOption

	ValidDomainOrder
)

func (vd ValidDomain) Term() Term {
	return [...]Atom{
		ValidDomainCharacterCodeList: "character_code_list",
		ValidDomainCloseOption:       "close_option",
		ValidDomainFlagValue:         "flag_value",
		ValidDomainIOMode:            "io_mode",
		ValidDomainNonEmptyList:      "non_empty_list",
		ValidDomainNotLessThanZero:   "not_less_than_zero",
		ValidDomainOperatorPriority:  "operator_priority",
		ValidDomainOperatorSpecifier: "operator_specifier",
		ValidDomainPrologFlag:        "prolog_flag",
		ValidDomainReadOption:        "read_option",
		ValidDomainSourceSink:        "source_sink",
		ValidDomainStream:            "stream",
		ValidDomainStreamOption:      "stream_option",
		ValidDomainStreamOrAlias:     "stream_or_alias",
		ValidDomainStreamPosition:    "stream_position",
		ValidDomainStreamProperty:    "stream_property",
		ValidDomainWriteOption:       "write_option",
		ValidDomainOrder:             "order",
	}[vd]
}

// DomainError creates a new domain error exception.
func DomainError(validDomain ValidDomain, culprit Term, env *Env) Exception {
	return NewException(&Compound{
		Functor: "error",
		Args: []Term{
			&Compound{
				Functor: "domain_error",
				Args:    []Term{validDomain.Term(), culprit},
			},
			varContext,
		},
	}, env)
}

type ObjectType uint8

const (
	ObjectTypeProcedure ObjectType = iota
	ObjectTypeSourceSink
	ObjectTypeStream
)

func (ot ObjectType) Term() Term {
	return [...]Atom{
		ObjectTypeProcedure:  "procedure",
		ObjectTypeSourceSink: "source_sink",
		ObjectTypeStream:     "stream",
	}[ot]
}

// ExistenceError creates a new existence error exception.
func ExistenceError(objectType ObjectType, culprit Term, env *Env) Exception {
	return NewException(&Compound{
		Functor: "error",
		Args: []Term{
			&Compound{
				Functor: "existence_error",
				Args:    []Term{objectType.Term(), culprit},
			},
			varContext,
		},
	}, env)
}

type Operation uint8

const (
	OperationAccess Operation = iota
	OperationCreate
	OperationInput
	OperationModify
	OperationOpen
	OperationOutput
	OperationReposition
)

func (o Operation) Term() Term {
	return [...]Atom{
		OperationAccess:     "access",
		OperationCreate:     "create",
		OperationInput:      "input",
		OperationModify:     "modify",
		OperationOpen:       "open",
		OperationOutput:     "output",
		OperationReposition: "reposition",
	}[o]
}

type PermissionType uint8

const (
	PermissionTypeBinaryStream PermissionType = iota
	PermissionTypeFlag
	PermissionTypeOperator
	PermissionTypePastEndOfStream
	PermissionTypePrivateProcedure
	PermissionTypeStaticProcedure
	PermissionTypeSourceSink
	PermissionTypeStream
	PermissionTypeTextStream
)

func (pt PermissionType) Term() Term {
	return [...]Atom{
		PermissionTypeBinaryStream:     "binary_stream",
		PermissionTypeFlag:             "flag",
		PermissionTypeOperator:         "operator",
		PermissionTypePastEndOfStream:  "past_enf_of_stream",
		PermissionTypePrivateProcedure: "private_procedure",
		PermissionTypeStaticProcedure:  "static_procedure",
		PermissionTypeSourceSink:       "source_sink",
		PermissionTypeStream:           "stream",
		PermissionTypeTextStream:       "text_stream",
	}[pt]
}

// PermissionError creates a new permission error exception.
func PermissionError(operation Operation, permissionType PermissionType, culprit Term, env *Env) Exception {
	return NewException(&Compound{
		Functor: "error",
		Args: []Term{
			&Compound{
				Functor: "permission_error",
				Args:    []Term{operation.Term(), permissionType.Term(), culprit},
			},
			varContext,
		},
	}, env)
}

type Flag uint8

const (
	FlagCharacter Flag = iota
	FlagCharacterCode
	FlagInCharacterCode
	FlagMaxArity
	FlagMaxInteger
	FlagMinInteger
)

func (f Flag) Term() Term {
	return [...]Atom{
		FlagCharacter:       "character",
		FlagCharacterCode:   "character_code",
		FlagInCharacterCode: "in_character_code",
		FlagMaxArity:        "max_arity",
		FlagMaxInteger:      "max_integer",
		FlagMinInteger:      "min_integer",
	}[f]
}

// RepresentationError creates a new representation error exception.
func RepresentationError(limit Flag, env *Env) Exception {
	return NewException(&Compound{
		Functor: "error",
		Args: []Term{
			&Compound{
				Functor: "representation_error",
				Args:    []Term{limit.Term()},
			},
			varContext,
		},
	}, env)
}

type Resource uint8

const (
	ResourceFiniteMemory Resource = iota
)

func (r Resource) Term() Term {
	return [...]Atom{
		ResourceFiniteMemory: "finite_memory",
	}[r]
}

// ResourceError creates a new resource error exception.
func ResourceError(resource Resource, env *Env) Exception {
	return NewException(&Compound{
		Functor: "error",
		Args: []Term{
			&Compound{
				Functor: "resource_error",
				Args:    []Term{resource.Term()},
			},
			varContext,
		},
	}, env)
}

// SyntaxError creates a new syntax error exception.
func SyntaxError(err error, env *Env) Exception {
	return NewException(&Compound{
		Functor: "error",
		Args: []Term{
			&Compound{
				Functor: "syntax_error",
				Args:    []Term{Atom(err.Error())},
			},
			varContext,
		},
	}, env)
}

// SystemError creates a new system error exception.
func SystemError(err error) Exception {
	return NewException(&Compound{
		Functor: "error",
		Args: []Term{
			Atom("system_error"),
			Atom(err.Error()),
		},
	}, nil)
}

type ExceptionalValue uint8

const (
	ExceptionalValueFloatOverflow ExceptionalValue = iota
	ExceptionalValueIntOverflow
	ExceptionalValueUnderflow
	ExceptionalValueZeroDivisor
	ExceptionalValueUndefined
)

func (ev ExceptionalValue) Error() string {
	return string(ev.Term().(Atom))
}

func (ev ExceptionalValue) Term() Term {
	return [...]Atom{
		ExceptionalValueFloatOverflow: "float_overflow",
		ExceptionalValueIntOverflow:   "int_overflow",
		ExceptionalValueUnderflow:     "underflow",
		ExceptionalValueZeroDivisor:   "zero_divisor",
		ExceptionalValueUndefined:     "undefined",
	}[ev]
}

// EvaluationError creates a new evaluation error exception.
func EvaluationError(ev ExceptionalValue, env *Env) Exception {
	return NewException(&Compound{
		Functor: "error",
		Args: []Term{
			&Compound{
				Functor: "evaluation_error",
				Args:    []Term{ev.Term()},
			},
			varContext,
		},
	}, env)
}
