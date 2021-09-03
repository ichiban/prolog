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

func instantiationError(culprit term.Interface, env term.Env) *Exception {
	return &Exception{
		Term: &term.Compound{
			Functor: "error",
			Args: []term.Interface{
				term.Atom("instantiation_error"),
				term.Atom(fmt.Sprintf("%s is not instantiated.", culprit)),
			},
		},
		Env: env,
	}
}

func typeErrorAtom(culprit term.Interface, env term.Env) *Exception {
	return typeError(term.Atom("atom"), culprit, term.Atom(fmt.Sprintf("%s is not an atom.", culprit)), env)
}

func typeErrorByte(culprit term.Interface, env term.Env) *Exception {
	return typeError(term.Atom("byte"), culprit, term.Atom(fmt.Sprintf("%s is not a byte.", culprit)), env)
}

func typeErrorCallable(culprit term.Interface, env term.Env) *Exception {
	return typeError(term.Atom("callable"), culprit, term.Atom(fmt.Sprintf("%s is not callable.", culprit)), env)
}

func typeErrorCharacter(culprit term.Interface, env term.Env) *Exception {
	return typeError(term.Atom("character"), culprit, term.Atom(fmt.Sprintf("%s is not a character.", culprit)), env)
}

func typeErrorInByte(culprit term.Interface, env term.Env) *Exception {
	return typeError(term.Atom("in_byte"), culprit, term.Atom(fmt.Sprintf("%s is not a byte.", culprit)), env)
}

func typeErrorInCharacter(culprit term.Interface, env term.Env) *Exception {
	return typeError(term.Atom("in_character"), culprit, term.Atom(fmt.Sprintf("%s is not a character.", culprit)), env)
}

func typeErrorEvaluable(culprit term.Interface, env term.Env) *Exception {
	return typeError(term.Atom("evaluable"), culprit, term.Atom(fmt.Sprintf("%s is not evaluable.", culprit)), env)
}

func typeErrorInteger(culprit term.Interface, env term.Env) *Exception {
	return typeError(term.Atom("integer"), culprit, term.Atom(fmt.Sprintf("%s is not an integer.", culprit)), env)
}

func typeErrorList(culprit term.Interface, env term.Env) *Exception {
	return typeError(term.Atom("list"), culprit, term.Atom(fmt.Sprintf("%s is not a list.", culprit)), env)
}

func typeErrorNumber(culprit term.Interface, env term.Env) *Exception {
	return typeError(term.Atom("number"), culprit, term.Atom(fmt.Sprintf("%s is not a number.", culprit)), env)
}

func typeErrorPredicateIndicator(culprit term.Interface, env term.Env) *Exception {
	return typeError(term.Atom("predicate_indicator"), culprit, term.Atom(fmt.Sprintf("%s is not a predicate indicator.", culprit)), env)
}

func typeErrorVariable(culprit term.Interface, env term.Env) *Exception {
	return typeError(term.Atom("variable"), culprit, term.Atom(fmt.Sprintf("%s is not a variable.", culprit)), env)
}

func typeErrorCompound(culprit term.Interface, env term.Env) *Exception {
	return typeError(term.Atom("compound"), culprit, term.Atom(fmt.Sprintf("%s is not a compound.", culprit)), env)
}

func typeError(validType, culprit, info term.Interface, env term.Env) *Exception {
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
		Env: env,
	}
}

func domainErrorFlagValue(culprit term.Interface, env term.Env) *Exception {
	return domainError(term.Atom("flag_value"), culprit, term.Atom(fmt.Sprintf("%s is not a flag value.", culprit)), env)
}

func domainErrorIOMode(culprit term.Interface, env term.Env) *Exception {
	return domainError(term.Atom("io_mode"), culprit, term.Atom(fmt.Sprintf("%s is not an I/O mode.", culprit)), env)
}

func domainErrorNotEmptyList(culprit term.Interface, env term.Env) *Exception {
	return domainError(term.Atom("not_empty_list"), culprit, term.Atom(fmt.Sprintf("%s is an empty list.", culprit)), env)
}

func domainErrorNotLessThanZero(culprit term.Interface, env term.Env) *Exception {
	return domainError(term.Atom("not_less_than_zero"), culprit, term.Atom(fmt.Sprintf("%s is less than zero.", culprit)), env)
}

func domainErrorOperatorPriority(culprit term.Interface, env term.Env) *Exception {
	return domainError(term.Atom("operator_priority"), culprit, term.Atom(fmt.Sprintf("%s is not between 0 and 1200.", culprit)), env)
}

func domainErrorOperatorSpecifier(culprit term.Interface, env term.Env) *Exception {
	return domainError(term.Atom("operator_specifier"), culprit, term.Atom(fmt.Sprintf("%s is neither xf, yf, xfx, xfy, yfx, fx, nor fy.", culprit)), env)
}

func domainErrorPrologFlag(culprit term.Interface, env term.Env) *Exception {
	return domainError(term.Atom("prolog_flag"), culprit, term.Atom(fmt.Sprintf("%s is not a prolog flag.", culprit)), env)
}

func domainErrorReadOption(culprit term.Interface, env term.Env) *Exception {
	return domainError(term.Atom("read_option"), culprit, term.Atom(fmt.Sprintf("%s is not a read option.", culprit)), env)
}

func domainErrorSourceSink(culprit term.Interface, env term.Env) *Exception {
	return domainError(term.Atom("source_sink"), culprit, term.Atom(fmt.Sprintf("%s is not a source/sink.", culprit)), env)
}

func domainErrorStream(culprit term.Interface, env term.Env) *Exception {
	return domainError(term.Atom("stream"), culprit, term.Atom(fmt.Sprintf("%s is not a stream.", culprit)), env)
}

func domainErrorStreamOption(culprit term.Interface, env term.Env) *Exception {
	return domainError(term.Atom("stream_option"), culprit, term.Atom(fmt.Sprintf("%s is not a stream option.", culprit)), env)
}

func domainErrorStreamOrAlias(culprit term.Interface, env term.Env) *Exception {
	return domainError(term.Atom("stream_or_alias"), culprit, term.Atom(fmt.Sprintf("%s is neither a stream nor an alias.", culprit)), env)
}

func domainErrorStreamProperty(culprit term.Interface, env term.Env) *Exception {
	return domainError(term.Atom("stream_property"), culprit, term.Atom(fmt.Sprintf("%s is not a stream property.", culprit)), env)
}

func domainErrorWriteOption(culprit term.Interface, env term.Env) *Exception {
	return domainError(term.Atom("write_option"), culprit, term.Atom(fmt.Sprintf("%s is not a write option.", culprit)), env)
}

func domainErrorOrder(culprit term.Interface, env term.Env) *Exception {
	return domainError(term.Atom("order"), culprit, term.Atom(fmt.Sprintf("%s is neither <, =, nor >.", culprit)), env)
}

func domainError(validDomain, culprit, info term.Interface, env term.Env) *Exception {
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
		Env: env,
	}
}

func existenceErrorProcedure(culprit term.Interface, env term.Env) *Exception {
	return existenceError(term.Atom("procedure"), culprit, term.Atom(fmt.Sprintf("procedure %s is not defined.", culprit)), env)
}

func existenceErrorSourceSink(culprit term.Interface, env term.Env) *Exception {
	return existenceError(term.Atom("source_sink"), culprit, term.Atom(fmt.Sprintf("file %s doesn't exist.", culprit)), env)
}

func existenceErrorStream(culprit term.Interface, env term.Env) *Exception {
	return existenceError(term.Atom("stream"), culprit, term.Atom(fmt.Sprintf("stream %s doesn't exist.", culprit)), env)
}

func existenceError(objectType, culprit, info term.Interface, env term.Env) *Exception {
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
		Env: env,
	}
}

func permissionErrorModifyStaticProcedure(culprit term.Interface, env term.Env) *Exception {
	return permissionError(term.Atom("modify"), term.Atom("static_procedure"), culprit, term.Atom(fmt.Sprintf("%s is static.", culprit)), env)
}

func permissionErrorOutputStream(culprit term.Interface, env term.Env) *Exception {
	return permissionError(term.Atom("output"), term.Atom("stream"), culprit, term.Atom(fmt.Sprintf("%s is not an output stream.", culprit)), env)
}

func permissionErrorOutputBinaryStream(culprit term.Interface, env term.Env) *Exception {
	return permissionError(term.Atom("output"), term.Atom("binary_stream"), culprit, term.Atom(fmt.Sprintf("%s is a binary stream.", culprit)), env)
}

func permissionErrorOutputTextStream(culprit term.Interface, env term.Env) *Exception {
	return permissionError(term.Atom("output"), term.Atom("text_stream"), culprit, term.Atom(fmt.Sprintf("%s is a text stream.", culprit)), env)
}

func permissionErrorInputStream(culprit term.Interface, env term.Env) *Exception {
	return permissionError(term.Atom("input"), term.Atom("stream"), culprit, term.Atom(fmt.Sprintf("%s is not an input stream.", culprit)), env)
}

func permissionErrorInputBufferedStream(culprit term.Interface, env term.Env) *Exception {
	return permissionError(term.Atom("input"), term.Atom("buffered_stream"), culprit, term.Atom(fmt.Sprintf("%s is not a buffered stream.", culprit)), env)
}

func permissionErrorInputBinaryStream(culprit term.Interface, env term.Env) *Exception {
	return permissionError(term.Atom("input"), term.Atom("binary_stream"), culprit, term.Atom(fmt.Sprintf("%s is a binary stream.", culprit)), env)
}

func permissionErrorInputTextStream(culprit term.Interface, env term.Env) *Exception {
	return permissionError(term.Atom("input"), term.Atom("text_stream"), culprit, term.Atom(fmt.Sprintf("%s is a text stream.", culprit)), env)
}

func permissionErrorInputPastEndOfStream(culprit term.Interface, env term.Env) *Exception {
	return permissionError(term.Atom("input"), term.Atom("past_end_of_stream"), culprit, term.Atom(fmt.Sprintf("%s has past end of stream.", culprit)), env)
}

func permissionError(operation, permissionType, culprit, info term.Interface, env term.Env) *Exception {
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
		Env: env,
	}
}

func representationError(limit, info term.Interface, env term.Env) *Exception {
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
		Env: env,
	}
}

func evaluationErrorZeroDivisor(env term.Env) *Exception {
	return evaluationError(term.Atom("zero_divisor"), term.Atom("divided by zero."), env)
}

func evaluationError(error, info term.Interface, env term.Env) *Exception {
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
		Env: env,
	}
}

func resourceError(resource, info term.Interface, env term.Env) *Exception {
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
		Env: env,
	}
}

func syntaxErrorNotANumber(env term.Env) *Exception {
	return syntaxError(term.Atom("not_a_number"), term.Atom("Not a number."), env)
}

func syntaxErrorUnexpectedChar(info term.Interface, env term.Env) *Exception {
	return syntaxError(term.Atom("unexpected_char"), info, env)
}

func syntaxErrorUnexpectedToken(info term.Interface, env term.Env) *Exception {
	return syntaxError(term.Atom("unexpected_token"), info, env)
}

func syntaxErrorInsufficient(env term.Env) *Exception {
	return syntaxError(term.Atom("insufficient"), term.Atom("Not enough input."), env)
}

func syntaxError(detail, info term.Interface, env term.Env) *Exception {
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
		Env: env,
	}
}

func systemError(err error, env term.Env) *Exception {
	return &Exception{
		Term: &term.Compound{
			Functor: "error",
			Args: []term.Interface{
				term.Atom("system_error"),
				term.Atom(err.Error()),
			},
		},
		Env: env,
	}
}
