package engine

import (
	"fmt"

	"github.com/ichiban/prolog/term"
)

// Exception is an error represented by a prolog term.
type Exception struct {
	Term term.Interface
}

func (e *Exception) Error() string {
	return e.Term.String()
}

// InstantiationError creates a new instantiation error excdption.
func InstantiationError(culprit term.Interface) *Exception {
	return &Exception{
		Term: &term.Compound{
			Functor: "error",
			Args: []term.Interface{
				term.Atom("instantiation_error"),
				term.Atom(fmt.Sprintf("%s is not instantiated.", culprit)),
			},
		},
	}
}

func typeErrorAtom(culprit term.Interface) *Exception {
	return TypeError("atom", culprit, "%s is not an atom.", culprit)
}

func typeErrorByte(culprit term.Interface) *Exception {
	return TypeError("byte", culprit, "%s is not a byte.", culprit)
}

func typeErrorCallable(culprit term.Interface) *Exception {
	return TypeError("callable", culprit, "%s is not callable.", culprit)
}

func typeErrorCharacter(culprit term.Interface) *Exception {
	return TypeError("character", culprit, "%s is not a character.", culprit)
}

func typeErrorInByte(culprit term.Interface) *Exception {
	return TypeError("in_byte", culprit, "%s is not a byte.", culprit)
}

func typeErrorInCharacter(culprit term.Interface) *Exception {
	return TypeError("in_character", culprit, "%s is not a character.", culprit)
}

func typeErrorEvaluable(culprit term.Interface) *Exception {
	return TypeError("evaluable", culprit, "%s is not evaluable.", culprit)
}

func typeErrorInteger(culprit term.Interface) *Exception {
	return TypeError("integer", culprit, "%s is not an integer.", culprit)
}

func typeErrorList(culprit term.Interface) *Exception {
	return TypeError("list", culprit, "%s is not a list.", culprit)
}

func typeErrorNumber(culprit term.Interface) *Exception {
	return TypeError("number", culprit, "%s is not a number.", culprit)
}

func typeErrorPredicateIndicator(culprit term.Interface) *Exception {
	return TypeError("predicate_indicator", culprit, "%s is not a predicate indicator.", culprit)
}

func typeErrorVariable(culprit term.Interface) *Exception {
	return TypeError("variable", culprit, "%s is not a variable.", culprit)
}

func typeErrorCompound(culprit term.Interface) *Exception {
	return TypeError("compound", culprit, "%s is not a compound.", culprit)
}

func typeErrorAtomic(culprit term.Interface) *Exception {
	return TypeError("atomic", culprit, "%s is not atomic.", culprit)
}

// TypeError creates a new type error exception.
func TypeError(validType term.Atom, culprit term.Interface, format string, args ...interface{}) *Exception {
	return &Exception{
		Term: &term.Compound{
			Functor: "error",
			Args: []term.Interface{
				&term.Compound{
					Functor: "type_error",
					Args:    []term.Interface{validType, culprit},
				},
				term.Atom(fmt.Sprintf(format, args...)),
			},
		},
	}
}

func domainErrorFlagValue(culprit term.Interface) *Exception {
	return DomainError("flag_value", culprit, "%s is not a flag value.", culprit)
}

func domainErrorIOMode(culprit term.Interface) *Exception {
	return DomainError("io_mode", culprit, "%s is not an I/O mode.", culprit)
}

func domainErrorNotEmptyList(culprit term.Interface) *Exception {
	return DomainError("not_empty_list", culprit, "%s is an empty list.", culprit)
}

func domainErrorNotLessThanZero(culprit term.Interface) *Exception {
	return DomainError("not_less_than_zero", culprit, "%s is less than zero.", culprit)
}

func domainErrorOperatorPriority(culprit term.Interface) *Exception {
	return DomainError("operator_priority", culprit, "%s is not between 0 and 1200.", culprit)
}

func domainErrorOperatorSpecifier(culprit term.Interface) *Exception {
	return DomainError("operator_specifier", culprit, "%s is neither xf, yf, xfx, xfy, yfx, fx, nor fy.", culprit)
}

func domainErrorPrologFlag(culprit term.Interface) *Exception {
	return DomainError("prolog_flag", culprit, "%s is not a prolog flag.", culprit)
}

func domainErrorReadOption(culprit term.Interface) *Exception {
	return DomainError("read_option", culprit, "%s is not a read option.", culprit)
}

func domainErrorSourceSink(culprit term.Interface) *Exception {
	return DomainError("source_sink", culprit, "%s is not a source/sink.", culprit)
}

func domainErrorStream(culprit term.Interface) *Exception {
	return DomainError("stream", culprit, "%s is not a stream.", culprit)
}

func domainErrorStreamOption(culprit term.Interface) *Exception {
	return DomainError("stream_option", culprit, "%s is not a stream option.", culprit)
}

func domainErrorStreamOrAlias(culprit term.Interface) *Exception {
	return DomainError("stream_or_alias", culprit, "%s is neither a stream nor an alias.", culprit)
}

func domainErrorStreamProperty(culprit term.Interface) *Exception {
	return DomainError("stream_property", culprit, "%s is not a stream property.", culprit)
}

func domainErrorWriteOption(culprit term.Interface) *Exception {
	return DomainError("write_option", culprit, "%s is not a write option.", culprit)
}

func domainErrorOrder(culprit term.Interface) *Exception {
	return DomainError("order", culprit, "%s is neither <, =, nor >.", culprit)
}

// DomainError creates a new domain error exception.
func DomainError(validDomain term.Atom, culprit term.Interface, format string, args ...interface{}) *Exception {
	return &Exception{
		Term: &term.Compound{
			Functor: "error",
			Args: []term.Interface{
				&term.Compound{
					Functor: "domain_error",
					Args:    []term.Interface{validDomain, culprit},
				},
				term.Atom(fmt.Sprintf(format, args...)),
			},
		},
	}
}

func existenceErrorProcedure(culprit term.Interface) *Exception {
	return ExistenceError("procedure", culprit, "procedure %s is not defined.", culprit)
}

func existenceErrorSourceSink(culprit term.Interface) *Exception {
	return ExistenceError("source_sink", culprit, "file %s doesn't exist.", culprit)
}

func existenceErrorStream(culprit term.Interface) *Exception {
	return ExistenceError("stream", culprit, "stream %s doesn't exist.", culprit)
}

// ExistenceError creates a new existence error exception.
func ExistenceError(objectType term.Atom, culprit term.Interface, format string, args ...interface{}) *Exception {
	return &Exception{
		Term: &term.Compound{
			Functor: "error",
			Args: []term.Interface{
				&term.Compound{
					Functor: "existence_error",
					Args:    []term.Interface{objectType, culprit},
				},
				term.Atom(fmt.Sprintf(format, args...)),
			},
		},
	}
}

func permissionErrorModifyStaticProcedure(culprit term.Interface) *Exception {
	return PermissionError("modify", "static_procedure", culprit, "%s is static.", culprit)
}

func permissionErrorAccessPrivateProcedure(culprit term.Interface) *Exception {
	return PermissionError("access", "private_procedure", culprit, "%s is private.", culprit)
}

func permissionErrorOutputStream(culprit term.Interface) *Exception {
	return PermissionError("output", "stream", culprit, "%s is not an output stream.", culprit)
}

func permissionErrorOutputBinaryStream(culprit term.Interface) *Exception {
	return PermissionError("output", "binary_stream", culprit, "%s is a binary stream.", culprit)
}

func permissionErrorOutputTextStream(culprit term.Interface) *Exception {
	return PermissionError("output", "text_stream", culprit, "%s is a text stream.", culprit)
}

func permissionErrorInputStream(culprit term.Interface) *Exception {
	return PermissionError("input", "stream", culprit, "%s is not an input stream.", culprit)
}

func permissionErrorInputBufferedStream(culprit term.Interface) *Exception {
	return PermissionError("input", "buffered_stream", culprit, "%s is not a buffered stream.", culprit)
}

func permissionErrorInputBinaryStream(culprit term.Interface) *Exception {
	return PermissionError("input", "binary_stream", culprit, "%s is a binary stream.", culprit)
}

func permissionErrorInputTextStream(culprit term.Interface) *Exception {
	return PermissionError("input", "text_stream", culprit, "%s is a text stream.", culprit)
}

func permissionErrorInputPastEndOfStream(culprit term.Interface) *Exception {
	return PermissionError("input", "past_end_of_stream", culprit, "%s has past end of stream.", culprit)
}

// PermissionError creates a new permission error exception.
func PermissionError(operation, permissionType term.Atom, culprit term.Interface, format string, args ...interface{}) *Exception {
	return &Exception{
		Term: &term.Compound{
			Functor: "error",
			Args: []term.Interface{
				&term.Compound{
					Functor: "permission_error",
					Args:    []term.Interface{operation, permissionType, culprit},
				},
				term.Atom(fmt.Sprintf(format, args...)),
			},
		},
	}
}

func representationError(limit, info term.Interface) *Exception {
	return &Exception{
		Term: &term.Compound{
			Functor: "error",
			Args: []term.Interface{
				&term.Compound{
					Functor: "representation_error",
					Args:    []term.Interface{limit},
				},
				info,
			},
		},
	}
}

func evaluationErrorZeroDivisor() *Exception {
	return evaluationError(term.Atom("zero_divisor"), term.Atom("divided by zero."))
}

func evaluationError(error, info term.Interface) *Exception {
	return &Exception{
		Term: &term.Compound{
			Functor: "error",
			Args: []term.Interface{
				&term.Compound{
					Functor: "evaluation_error",
					Args:    []term.Interface{error},
				},
				info,
			},
		},
	}
}

func resourceError(resource, info term.Interface) *Exception {
	return &Exception{
		Term: &term.Compound{
			Functor: "error",
			Args: []term.Interface{
				&term.Compound{
					Functor: "resource_error",
					Args:    []term.Interface{resource},
				},
				info,
			},
		},
	}
}

func syntaxErrorNotANumber() *Exception {
	return syntaxError(term.Atom("not_a_number"), term.Atom("Not a number."))
}

func syntaxErrorUnexpectedChar(info term.Interface) *Exception {
	return syntaxError(term.Atom("unexpected_char"), info)
}

func syntaxErrorUnexpectedToken(info term.Interface) *Exception {
	return syntaxError(term.Atom("unexpected_token"), info)
}

func syntaxErrorInsufficient() *Exception {
	return syntaxError(term.Atom("insufficient"), term.Atom("Not enough input."))
}

func syntaxError(detail, info term.Interface) *Exception {
	return &Exception{
		Term: &term.Compound{
			Functor: "error",
			Args: []term.Interface{
				&term.Compound{
					Functor: "syntax_error",
					Args:    []term.Interface{detail},
				},
				info,
			},
		},
	}
}

func SystemError(err error) *Exception {
	return &Exception{
		Term: &term.Compound{
			Functor: "error",
			Args: []term.Interface{
				term.Atom("system_error"),
				term.Atom(err.Error()),
			},
		},
	}
}
