package engine

import (
	"bytes"
	"fmt"
)

var (
	// ErrInstantiation is an instantiation error exception.
	ErrInstantiation = &Exception{
		term: &Compound{
			Functor: "error",
			Args: []Term{
				Atom("instantiation_error"),
				Atom("Arguments are not sufficiently instantiated."),
			},
		},
	}

	// ErrZeroDivisor is an exception that will be raised when an operation divided by zero.
	ErrZeroDivisor = evaluationError(Atom("zero_divisor"), Atom("Divided by zero."))

	// ErrIntOverflow is an exception that will be raised when an integer overflowed, either positively or negatively.
	ErrIntOverflow = evaluationError(Atom("int_overflow"), Atom("Integer overflow."))

	// ErrFloatOverflow is an exception that will be raised when a float overflowed, either positively or negatively.
	ErrFloatOverflow = evaluationError(Atom("float_overflow"), Atom("Float overflow."))

	// ErrUnderflow is an exception that will be raised when a float is too small to be represented by engine.Float.
	ErrUnderflow = evaluationError(Atom("underflow"), Atom("Underflow."))

	// ErrUndefined is an exception that will be raised when a function value for the arguments is undefined.
	ErrUndefined = evaluationError(Atom("undefined"), Atom("Undefined."))
)

// Exception is an error represented by a prolog term.
type Exception struct {
	term Term
}

// NewException creates an exception from a copy of the given term.
func NewException(term Term, env *Env) *Exception {
	return &Exception{term: copyTerm(term, nil, env)}
}

// Term returns the underlying term of the exception.
func (e *Exception) Term() Term {
	return e.term
}

func (e *Exception) Error() string {
	var buf bytes.Buffer
	_ = Write(&buf, e.term, nil, WithQuoted(true))
	return buf.String()
}

// TypeErrorAtom returns a type error exception for atom.
func TypeErrorAtom(culprit Term, env *Env) *Exception {
	return TypeError("atom", culprit, env)
}

// TypeErrorAtomic returns a type error exception for atomic.
func TypeErrorAtomic(culprit Term, env *Env) *Exception {
	return TypeError("atomic", culprit, env)
}

// TypeErrorByte returns a type error exception for byte.
func TypeErrorByte(culprit Term, env *Env) *Exception {
	return TypeError("byte", culprit, env)
}

// TypeErrorCallable returns a type error exception for callable.
func TypeErrorCallable(culprit Term, env *Env) *Exception {
	return TypeError("callable", culprit, env)
}

// TypeErrorCharacter returns a type error exception for character.
func TypeErrorCharacter(culprit Term, env *Env) *Exception {
	return TypeError("character", culprit, env)
}

// TypeErrorCompound returns a type error exception for compound.
func TypeErrorCompound(culprit Term, env *Env) *Exception {
	return TypeError("compound", culprit, env)
}

// TypeErrorEvaluable returns a type error exception for evaluable.
func TypeErrorEvaluable(culprit Term, env *Env) *Exception {
	return TypeError("evaluable", culprit, env)
}

// TypeErrorInByte returns a type error exception for in_byte.
func TypeErrorInByte(culprit Term, env *Env) *Exception {
	return TypeError("in_byte", culprit, env)
}

// TypeErrorInCharacter returns a type error exception for in_character.
func TypeErrorInCharacter(culprit Term, env *Env) *Exception {
	return TypeError("in_character", culprit, env)
}

// TypeErrorInteger returns a type error exception for integer.
func TypeErrorInteger(culprit Term, env *Env) *Exception {
	return TypeError("integer", culprit, env)
}

// TypeErrorList returns a type error exception for list.
func TypeErrorList(culprit Term, env *Env) *Exception {
	return TypeError("list", culprit, env)
}

// TypeErrorNumber returns a type error exception for number.
func TypeErrorNumber(culprit Term, env *Env) *Exception {
	return TypeError("number", culprit, env)
}

// TypeErrorPredicateIndicator returns a type error exception for predicate_indicator.
func TypeErrorPredicateIndicator(culprit Term, env *Env) *Exception {
	return TypeError("predicate_indicator", culprit, env)
}

// TypeErrorPair returns a type error exception for pair.
func TypeErrorPair(culprit Term, env *Env) *Exception {
	return TypeError("pair", culprit, env)
}

// TypeErrorFloat returns a type error exception for float.
func TypeErrorFloat(culprit Term, env *Env) *Exception {
	return TypeError("float", culprit, env)
}

// TypeError creates a new type error exception.
func TypeError(validType Atom, culprit Term, env *Env) *Exception {
	return NewException(&Compound{
		Functor: "error",
		Args: []Term{
			&Compound{
				Functor: "type_error",
				Args:    []Term{validType, culprit},
			},
			Atom(fmt.Sprintf("Expected %s, found %T.", validType, culprit)),
		},
	}, env)
}

func domainErrorFlagValue(culprit Term, env *Env) *Exception {
	return DomainError("flag_value", culprit, env)
}

func domainErrorIOMode(culprit Term, env *Env) *Exception {
	return DomainError("io_mode", culprit, env)
}

func domainErrorNotEmptyList(culprit Term, env *Env) *Exception {
	return DomainError("not_empty_list", culprit, env)
}

func domainErrorNotLessThanZero(culprit Term, env *Env) *Exception {
	return DomainError("not_less_than_zero", culprit, env)
}

func domainErrorOperatorPriority(culprit Term, env *Env) *Exception {
	return DomainError("operator_priority", culprit, env)
}

func domainErrorOperatorSpecifier(culprit Term, env *Env) *Exception {
	return DomainError("operator_specifier", culprit, env)
}

func domainErrorPrologFlag(culprit Term, env *Env) *Exception {
	return DomainError("prolog_flag", culprit, env)
}

func domainErrorReadOption(culprit Term, env *Env) *Exception {
	return DomainError("read_option", culprit, env)
}

func domainErrorSourceSink(culprit Term, env *Env) *Exception {
	return DomainError("source_sink", culprit, env)
}

func domainErrorStream(culprit Term, env *Env) *Exception {
	return DomainError("stream", culprit, env)
}

func domainErrorStreamOption(culprit Term, env *Env) *Exception {
	return DomainError("stream_option", culprit, env)
}

func domainErrorStreamOrAlias(culprit Term, env *Env) *Exception {
	return DomainError("stream_or_alias", culprit, env)
}

func domainErrorStreamProperty(culprit Term, env *Env) *Exception {
	return DomainError("stream_property", culprit, env)
}

func domainErrorWriteOption(culprit Term, env *Env) *Exception {
	return DomainError("write_option", culprit, env)
}

func domainErrorOrder(culprit Term, env *Env) *Exception {
	return DomainError("order", culprit, env)
}

// DomainError creates a new domain error exception.
func DomainError(validDomain Atom, culprit Term, env *Env) *Exception {
	return NewException(&Compound{
		Functor: "error",
		Args: []Term{
			&Compound{
				Functor: "domain_error",
				Args:    []Term{validDomain, culprit},
			},
			Atom(fmt.Sprintf("Invalid value for %s.", validDomain)),
		},
	}, env)
}

func existenceErrorProcedure(culprit Term, env *Env) *Exception {
	return ExistenceError("procedure", culprit, env)
}

func existenceErrorSourceSink(culprit Term, env *Env) *Exception {
	return ExistenceError("source_sink", culprit, env)
}

func existenceErrorStream(culprit Term, env *Env) *Exception {
	return ExistenceError("stream", culprit, env)
}

// ExistenceError creates a new existence error exception.
func ExistenceError(objectType Atom, culprit Term, env *Env) *Exception {
	return NewException(&Compound{
		Functor: "error",
		Args: []Term{
			&Compound{
				Functor: "existence_error",
				Args:    []Term{objectType, culprit},
			},
			Atom(fmt.Sprintf("Unknown %s.", objectType)),
		},
	}, env)
}

func permissionErrorModifyStaticProcedure(culprit Term, env *Env) *Exception {
	return PermissionError("modify", "static_procedure", culprit, env)
}

func permissionErrorAccessPrivateProcedure(culprit Term, env *Env) *Exception {
	return PermissionError("access", "private_procedure", culprit, env)
}

func permissionErrorOutputStream(culprit Term, env *Env) *Exception {
	return PermissionError("output", "stream", culprit, env)
}

func permissionErrorOutputBinaryStream(culprit Term, env *Env) *Exception {
	return PermissionError("output", "binary_stream", culprit, env)
}

func permissionErrorOutputTextStream(culprit Term, env *Env) *Exception {
	return PermissionError("output", "text_stream", culprit, env)
}

func permissionErrorInputStream(culprit Term, env *Env) *Exception {
	return PermissionError("input", "stream", culprit, env)
}

func permissionErrorInputBinaryStream(culprit Term, env *Env) *Exception {
	return PermissionError("input", "binary_stream", culprit, env)
}

func permissionErrorInputTextStream(culprit Term, env *Env) *Exception {
	return PermissionError("input", "text_stream", culprit, env)
}

func permissionErrorInputPastEndOfStream(culprit Term, env *Env) *Exception {
	return PermissionError("input", "past_end_of_stream", culprit, env)
}

// PermissionError creates a new permission error exception.
func PermissionError(operation, permissionType Atom, culprit Term, env *Env) *Exception {
	return NewException(&Compound{
		Functor: "error",
		Args: []Term{
			&Compound{
				Functor: "permission_error",
				Args:    []Term{operation, permissionType, culprit},
			},
			Atom(fmt.Sprintf("Operation %s not allowed for %s.", operation, permissionType)),
		},
	}, env)
}

func representationError(limit Atom) *Exception {
	return &Exception{
		term: &Compound{
			Functor: "error",
			Args: []Term{
				&Compound{
					Functor: "representation_error",
					Args:    []Term{limit},
				},
				Atom(fmt.Sprintf("Invalid %s.", limit)),
			},
		},
	}
}

func resourceError(resource, info Term, env *Env) *Exception {
	return NewException(&Compound{
		Functor: "error",
		Args: []Term{
			&Compound{
				Functor: "resource_error",
				Args:    []Term{resource},
			},
			info,
		},
	}, env)
}

func syntaxErrorNotANumber() *Exception {
	return syntaxError(Atom("not_a_number"), Atom("Not a number."))
}

func syntaxErrorUnexpectedToken(info Term) *Exception {
	return syntaxError(Atom("unexpected_token"), info)
}

func syntaxErrorInsufficient() *Exception {
	return syntaxError(Atom("insufficient"), Atom("Not enough input."))
}

func syntaxError(detail, info Term) *Exception {
	return &Exception{
		term: &Compound{
			Functor: "error",
			Args: []Term{
				&Compound{
					Functor: "syntax_error",
					Args:    []Term{detail},
				},
				info,
			},
		},
	}
}

// SystemError creates a new system error exception.
func SystemError(err error) *Exception {
	return &Exception{
		term: &Compound{
			Functor: "error",
			Args: []Term{
				Atom("system_error"),
				Atom(err.Error()),
			},
		},
	}
}

func evaluationError(error, info Term) *Exception {
	return &Exception{
		term: &Compound{
			Functor: "error",
			Args: []Term{
				&Compound{
					Functor: "evaluation_error",
					Args:    []Term{error},
				},
				info,
			},
		},
	}
}
