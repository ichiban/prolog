package engine

import (
	"fmt"

	"github.com/ichiban/prolog/term"
)

// Exception is an error represented by a prolog term.
type Exception struct {
	Term term.Interface
	Env  term.Env
}

func (e *Exception) Error() string {
	return e.Term.String()
}

func instantiationError(culprit term.Interface) *Exception {
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
	return typeError(term.Atom("atom"), culprit, term.Atom(fmt.Sprintf("%s is not an atom.", culprit)))
}

func typeErrorByte(culprit term.Interface) *Exception {
	return typeError(term.Atom("byte"), culprit, term.Atom(fmt.Sprintf("%s is not a byte.", culprit)))
}

func typeErrorCallable(culprit term.Interface) *Exception {
	return typeError(term.Atom("callable"), culprit, term.Atom(fmt.Sprintf("%s is not callable.", culprit)))
}

func typeErrorCharacter(culprit term.Interface) *Exception {
	return typeError(term.Atom("character"), culprit, term.Atom(fmt.Sprintf("%s is not a character.", culprit)))
}

func typeErrorInByte(culprit term.Interface) *Exception {
	return typeError(term.Atom("in_byte"), culprit, term.Atom(fmt.Sprintf("%s is not a byte.", culprit)))
}

func typeErrorInCharacter(culprit term.Interface) *Exception {
	return typeError(term.Atom("in_character"), culprit, term.Atom(fmt.Sprintf("%s is not a character.", culprit)))
}

func typeErrorEvaluable(culprit term.Interface) *Exception {
	return typeError(term.Atom("evaluable"), culprit, term.Atom(fmt.Sprintf("%s is not evaluable.", culprit)))
}

func typeErrorInteger(culprit term.Interface) *Exception {
	return typeError(term.Atom("integer"), culprit, term.Atom(fmt.Sprintf("%s is not an integer.", culprit)))
}

func typeErrorList(culprit term.Interface) *Exception {
	return typeError(term.Atom("list"), culprit, term.Atom(fmt.Sprintf("%s is not a list.", culprit)))
}

func typeErrorNumber(culprit term.Interface) *Exception {
	return typeError(term.Atom("number"), culprit, term.Atom(fmt.Sprintf("%s is not a number.", culprit)))
}

func typeErrorPredicateIndicator(culprit term.Interface) *Exception {
	return typeError(term.Atom("predicate_indicator"), culprit, term.Atom(fmt.Sprintf("%s is not a predicate indicator.", culprit)))
}

func typeErrorVariable(culprit term.Interface) *Exception {
	return typeError(term.Atom("variable"), culprit, term.Atom(fmt.Sprintf("%s is not a variable.", culprit)))
}

func typeErrorCompound(culprit term.Interface) *Exception {
	return typeError(term.Atom("compound"), culprit, term.Atom(fmt.Sprintf("%s is not a compound.", culprit)))
}

func typeError(validType, culprit, info term.Interface) *Exception {
	return &Exception{
		Term: &term.Compound{
			Functor: "error",
			Args: []term.Interface{
				&term.Compound{
					Functor: "type_error",
					Args:    []term.Interface{validType, culprit},
				},
				info,
			},
		},
	}
}

func domainErrorFlagValue(culprit term.Interface) *Exception {
	return domainError(term.Atom("flag_value"), culprit, term.Atom(fmt.Sprintf("%s is not a flag value.", culprit)))
}

func domainErrorIOMode(culprit term.Interface) *Exception {
	return domainError(term.Atom("io_mode"), culprit, term.Atom(fmt.Sprintf("%s is not an I/O mode.", culprit)))
}

func domainErrorNotEmptyList(culprit term.Interface) *Exception {
	return domainError(term.Atom("not_empty_list"), culprit, term.Atom(fmt.Sprintf("%s is an empty list.", culprit)))
}

func domainErrorNotLessThanZero(culprit term.Interface) *Exception {
	return domainError(term.Atom("not_less_than_zero"), culprit, term.Atom(fmt.Sprintf("%s is less than zero.", culprit)))
}

func domainErrorOperatorPriority(culprit term.Interface) *Exception {
	return domainError(term.Atom("operator_priority"), culprit, term.Atom(fmt.Sprintf("%s is not between 0 and 1200.", culprit)))
}

func domainErrorOperatorSpecifier(culprit term.Interface) *Exception {
	return domainError(term.Atom("operator_specifier"), culprit, term.Atom(fmt.Sprintf("%s is neither xf, yf, xfx, xfy, yfx, fx, nor fy.", culprit)))
}

func domainErrorPrologFlag(culprit term.Interface) *Exception {
	return domainError(term.Atom("prolog_flag"), culprit, term.Atom(fmt.Sprintf("%s is not a prolog flag.", culprit)))
}

func domainErrorReadOption(culprit term.Interface) *Exception {
	return domainError(term.Atom("read_option"), culprit, term.Atom(fmt.Sprintf("%s is not a read option.", culprit)))
}

func domainErrorSourceSink(culprit term.Interface) *Exception {
	return domainError(term.Atom("source_sink"), culprit, term.Atom(fmt.Sprintf("%s is not a source/sink.", culprit)))
}

func domainErrorStream(culprit term.Interface) *Exception {
	return domainError(term.Atom("stream"), culprit, term.Atom(fmt.Sprintf("%s is not a stream.", culprit)))
}

func domainErrorStreamOption(culprit term.Interface) *Exception {
	return domainError(term.Atom("stream_option"), culprit, term.Atom(fmt.Sprintf("%s is not a stream option.", culprit)))
}

func domainErrorStreamOrAlias(culprit term.Interface) *Exception {
	return domainError(term.Atom("stream_or_alias"), culprit, term.Atom(fmt.Sprintf("%s is neither a stream nor an alias.", culprit)))
}

func domainErrorStreamProperty(culprit term.Interface) *Exception {
	return domainError(term.Atom("stream_property"), culprit, term.Atom(fmt.Sprintf("%s is not a stream property.", culprit)))
}

func domainErrorWriteOption(culprit term.Interface) *Exception {
	return domainError(term.Atom("write_option"), culprit, term.Atom(fmt.Sprintf("%s is not a write option.", culprit)))
}

func domainErrorOrder(culprit term.Interface) *Exception {
	return domainError(term.Atom("order"), culprit, term.Atom(fmt.Sprintf("%s is neither <, =, nor >.", culprit)))
}

func domainError(validDomain, culprit, info term.Interface) *Exception {
	return &Exception{
		Term: &term.Compound{
			Functor: "error",
			Args: []term.Interface{
				&term.Compound{
					Functor: "domain_error",
					Args:    []term.Interface{validDomain, culprit},
				},
				info,
			},
		},
	}
}

func existenceErrorProcedure(culprit term.Interface) *Exception {
	return existenceError(term.Atom("procedure"), culprit, term.Atom(fmt.Sprintf("procedure %s is not defined.", culprit)))
}

func existenceErrorSourceSink(culprit term.Interface) *Exception {
	return existenceError(term.Atom("source_sink"), culprit, term.Atom(fmt.Sprintf("file %s doesn't exist.", culprit)))
}

func existenceErrorStream(culprit term.Interface) *Exception {
	return existenceError(term.Atom("stream"), culprit, term.Atom(fmt.Sprintf("stream %s doesn't exist.", culprit)))
}

func existenceError(objectType, culprit, info term.Interface) *Exception {
	return &Exception{
		Term: &term.Compound{
			Functor: "error",
			Args: []term.Interface{
				&term.Compound{
					Functor: "existence_error",
					Args:    []term.Interface{objectType, culprit},
				},
				info,
			},
		},
	}
}

func permissionErrorModifyStaticProcedure(culprit term.Interface) *Exception {
	return permissionError(term.Atom("modify"), term.Atom("static_procedure"), culprit, term.Atom(fmt.Sprintf("%s is static.", culprit)))
}

func permissionErrorOutputStream(culprit term.Interface) *Exception {
	return permissionError(term.Atom("output"), term.Atom("stream"), culprit, term.Atom(fmt.Sprintf("%s is not an output stream.", culprit)))
}

func permissionErrorOutputBinaryStream(culprit term.Interface) *Exception {
	return permissionError(term.Atom("output"), term.Atom("binary_stream"), culprit, term.Atom(fmt.Sprintf("%s is a binary stream.", culprit)))
}

func permissionErrorOutputTextStream(culprit term.Interface) *Exception {
	return permissionError(term.Atom("output"), term.Atom("text_stream"), culprit, term.Atom(fmt.Sprintf("%s is a text stream.", culprit)))
}

func permissionErrorInputStream(culprit term.Interface) *Exception {
	return permissionError(term.Atom("input"), term.Atom("stream"), culprit, term.Atom(fmt.Sprintf("%s is not an input stream.", culprit)))
}

func permissionErrorInputBufferedStream(culprit term.Interface) *Exception {
	return permissionError(term.Atom("input"), term.Atom("buffered_stream"), culprit, term.Atom(fmt.Sprintf("%s is not a buffered stream.", culprit)))
}

func permissionErrorInputBinaryStream(culprit term.Interface) *Exception {
	return permissionError(term.Atom("input"), term.Atom("binary_stream"), culprit, term.Atom(fmt.Sprintf("%s is a binary stream.", culprit)))
}

func permissionErrorInputTextStream(culprit term.Interface) *Exception {
	return permissionError(term.Atom("input"), term.Atom("text_stream"), culprit, term.Atom(fmt.Sprintf("%s is a text stream.", culprit)))
}

func permissionErrorInputPastEndOfStream(culprit term.Interface) *Exception {
	return permissionError(term.Atom("input"), term.Atom("past_end_of_stream"), culprit, term.Atom(fmt.Sprintf("%s has past end of stream.", culprit)))
}

func permissionError(operation, permissionType, culprit, info term.Interface) *Exception {
	return &Exception{
		Term: &term.Compound{
			Functor: "error",
			Args: []term.Interface{
				&term.Compound{
					Functor: "permission_error",
					Args:    []term.Interface{operation, permissionType, culprit},
				},
				info,
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

func systemError(err error) *Exception {
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
