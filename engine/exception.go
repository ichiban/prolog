package engine

import (
	"bytes"
	"fmt"
)

// Exception is an error represented by a prolog term.
type Exception struct {
	Term Term
}

func (e *Exception) Error() string {
	var buf bytes.Buffer
	_ = Write(&buf, e.Term, nil, WithQuoted(true))
	return buf.String()
}

// ErrInstantiation is an instantiation error exception.
var ErrInstantiation = &Exception{
	Term: &Compound{
		Functor: "error",
		Args: []Term{
			Atom("instantiation_error"),
			Atom("Arguments are not sufficiently instantiated."),
		},
	},
}

func typeErrorAtom(culprit Term) *Exception {
	return TypeError("atom", culprit)
}

func typeErrorByte(culprit Term) *Exception {
	return TypeError("byte", culprit)
}

func typeErrorCallable(culprit Term) *Exception {
	return TypeError("callable", culprit)
}

func typeErrorCharacter(culprit Term) *Exception {
	return TypeError("character", culprit)
}

func typeErrorInByte(culprit Term) *Exception {
	return TypeError("in_byte", culprit)
}

func typeErrorInCharacter(culprit Term) *Exception {
	return TypeError("in_character", culprit)
}

func typeErrorEvaluable(culprit Term) *Exception {
	return TypeError("evaluable", culprit)
}

func typeErrorInteger(culprit Term) *Exception {
	return TypeError("integer", culprit)
}

func typeErrorList(culprit Term) *Exception {
	return TypeError("list", culprit)
}

func typeErrorNumber(culprit Term) *Exception {
	return TypeError("number", culprit)
}

func typeErrorPredicateIndicator(culprit Term) *Exception {
	return TypeError("predicate_indicator", culprit)
}

func typeErrorVariable(culprit Term) *Exception {
	return TypeError("variable", culprit)
}

func typeErrorCompound(culprit Term) *Exception {
	return TypeError("compound", culprit)
}

func typeErrorAtomic(culprit Term) *Exception {
	return TypeError("atomic", culprit)
}

func typeErrorPair(culprit Term) *Exception {
	return TypeError("pair", culprit)
}

// TypeError creates a new type error exception.
func TypeError(validType Atom, culprit Term) *Exception {
	return &Exception{
		Term: &Compound{
			Functor: "error",
			Args: []Term{
				&Compound{
					Functor: "type_error",
					Args:    []Term{validType, culprit},
				},
				Atom(fmt.Sprintf("'%s' expected, found '%T'.", validType, culprit)),
			},
		},
	}
}

func domainErrorFlagValue(culprit Term) *Exception {
	return DomainError("flag_value", culprit, "%s is not a flag value.", culprit)
}

func domainErrorIOMode(culprit Term) *Exception {
	return DomainError("io_mode", culprit, "%s is not an I/O mode.", culprit)
}

func domainErrorNotEmptyList(culprit Term) *Exception {
	return DomainError("not_empty_list", culprit, "%s is an empty list.", culprit)
}

func domainErrorNotLessThanZero(culprit Term) *Exception {
	return DomainError("not_less_than_zero", culprit, "%s is less than zero.", culprit)
}

func domainErrorOperatorPriority(culprit Term) *Exception {
	return DomainError("operator_priority", culprit, "%s is not between 0 and 1200.", culprit)
}

func domainErrorOperatorSpecifier(culprit Term) *Exception {
	return DomainError("operator_specifier", culprit, "%s is neither xf, yf, xfx, xfy, yfx, fx, nor fy.", culprit)
}

func domainErrorPrologFlag(culprit Term) *Exception {
	return DomainError("prolog_flag", culprit, "%s is not a prolog flag.", culprit)
}

func domainErrorReadOption(culprit Term) *Exception {
	return DomainError("read_option", culprit, "%s is not a read option.", culprit)
}

func domainErrorSourceSink(culprit Term) *Exception {
	return DomainError("source_sink", culprit, "%s is not a source/sink.", culprit)
}

func domainErrorStream(culprit Term) *Exception {
	return DomainError("stream", culprit, "%s is not a stream.", culprit)
}

func domainErrorStreamOption(culprit Term) *Exception {
	return DomainError("stream_option", culprit, "%s is not a stream option.", culprit)
}

func domainErrorStreamOrAlias(culprit Term) *Exception {
	return DomainError("stream_or_alias", culprit, "%s is neither a stream nor an alias.", culprit)
}

func domainErrorStreamProperty(culprit Term) *Exception {
	return DomainError("stream_property", culprit, "%s is not a stream property.", culprit)
}

func domainErrorWriteOption(culprit Term) *Exception {
	return DomainError("write_option", culprit, "%s is not a write option.", culprit)
}

func domainErrorOrder(culprit Term) *Exception {
	return DomainError("order", culprit, "%s is neither <, =, nor >.", culprit)
}

// DomainError creates a new domain error exception.
func DomainError(validDomain Atom, culprit Term, format string, args ...interface{}) *Exception {
	return &Exception{
		Term: &Compound{
			Functor: "error",
			Args: []Term{
				&Compound{
					Functor: "domain_error",
					Args:    []Term{validDomain, culprit},
				},
				Atom(fmt.Sprintf(format, args...)),
			},
		},
	}
}

func existenceErrorProcedure(culprit Term) *Exception {
	return ExistenceError("procedure", culprit, "procedure %s is not defined.", culprit)
}

func existenceErrorSourceSink(culprit Term) *Exception {
	return ExistenceError("source_sink", culprit, "file %s doesn't exist.", culprit)
}

func existenceErrorStream(culprit Term) *Exception {
	return ExistenceError("stream", culprit, "stream %s doesn't exist.", culprit)
}

// ExistenceError creates a new existence error exception.
func ExistenceError(objectType Atom, culprit Term, format string, args ...interface{}) *Exception {
	return &Exception{
		Term: &Compound{
			Functor: "error",
			Args: []Term{
				&Compound{
					Functor: "existence_error",
					Args:    []Term{objectType, culprit},
				},
				Atom(fmt.Sprintf(format, args...)),
			},
		},
	}
}

func permissionErrorModifyStaticProcedure(culprit Term) *Exception {
	return PermissionError("modify", "static_procedure", culprit)
}

func permissionErrorAccessPrivateProcedure(culprit Term) *Exception {
	return PermissionError("access", "private_procedure", culprit)
}

func permissionErrorOutputStream(culprit Term) *Exception {
	return PermissionError("output", "stream", culprit)
}

func permissionErrorOutputBinaryStream(culprit Term) *Exception {
	return PermissionError("output", "binary_stream", culprit)
}

func permissionErrorOutputTextStream(culprit Term) *Exception {
	return PermissionError("output", "text_stream", culprit)
}

func permissionErrorInputStream(culprit Term) *Exception {
	return PermissionError("input", "stream", culprit)
}

func permissionErrorInputBinaryStream(culprit Term) *Exception {
	return PermissionError("input", "binary_stream", culprit)
}

func permissionErrorInputTextStream(culprit Term) *Exception {
	return PermissionError("input", "text_stream", culprit)
}

func permissionErrorInputPastEndOfStream(culprit Term) *Exception {
	return PermissionError("input", "past_end_of_stream", culprit)
}

// PermissionError creates a new permission error exception.
func PermissionError(operation, permissionType Atom, culprit Term) *Exception {
	return &Exception{
		Term: &Compound{
			Functor: "error",
			Args: []Term{
				&Compound{
					Functor: "permission_error",
					Args:    []Term{operation, permissionType, culprit},
				},
				Atom(fmt.Sprintf("Operation %s not allowed for %s.", operation, permissionType)),
			},
		},
	}
}

func representationError(limit Atom) *Exception {
	return &Exception{
		Term: &Compound{
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

func evaluationErrorZeroDivisor() *Exception {
	return evaluationError(Atom("zero_divisor"), Atom("divided by zero."))
}

func evaluationError(error, info Term) *Exception {
	return &Exception{
		Term: &Compound{
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

func resourceError(resource, info Term) *Exception {
	return &Exception{
		Term: &Compound{
			Functor: "error",
			Args: []Term{
				&Compound{
					Functor: "resource_error",
					Args:    []Term{resource},
				},
				info,
			},
		},
	}
}

func syntaxErrorNotANumber() *Exception {
	return syntaxError(Atom("not_a_number"), Atom("Not a number."))
}

func syntaxErrorUnexpectedChar(info Term) *Exception {
	return syntaxError(Atom("unexpected_char"), info)
}

func syntaxErrorUnexpectedToken(info Term) *Exception {
	return syntaxError(Atom("unexpected_token"), info)
}

func syntaxErrorInsufficient() *Exception {
	return syntaxError(Atom("insufficient"), Atom("Not enough input."))
}

func syntaxError(detail, info Term) *Exception {
	return &Exception{
		Term: &Compound{
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
		Term: &Compound{
			Functor: "error",
			Args: []Term{
				Atom("system_error"),
				Atom(err.Error()),
			},
		},
	}
}
