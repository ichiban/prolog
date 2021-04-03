package engine

import "fmt"

// Exception is an error represented by a prolog term.
type Exception struct {
	Term Term
}

func (e *Exception) Error() string {
	return e.Term.String()
}

func instantiationError(culprit Term) *Exception {
	return &Exception{
		Term: &Compound{
			Functor: "error",
			Args: []Term{
				Atom("instantiation_error"),
				Atom(fmt.Sprintf("%s is not instantiated.", culprit)),
			},
		},
	}
}

func typeErrorAtom(culprit Term) *Exception {
	return typeError(Atom("atom"), culprit, Atom(fmt.Sprintf("%s is not an atom.", culprit)))
}

func typeErrorAtomic(culprit Term) *Exception {
	return typeError(Atom("atomic"), culprit, Atom(fmt.Sprintf("%s is not atomic.", culprit)))
}

func typeErrorByte(culprit Term) *Exception {
	return typeError(Atom("byte"), culprit, Atom(fmt.Sprintf("%s is not a byte.", culprit)))
}

func typeErrorCallable(culprit Term) *Exception {
	return typeError(Atom("callable"), culprit, Atom(fmt.Sprintf("%s is not callable.", culprit)))
}

func typeErrorCharacter(culprit Term) *Exception {
	return typeError(Atom("character"), culprit, Atom(fmt.Sprintf("%s is not a character.", culprit)))
}

func typeErrorInByte(culprit Term) *Exception {
	return typeError(Atom("in_byte"), culprit, Atom(fmt.Sprintf("%s is not a byte.", culprit)))
}

func typeErrorInCharacter(culprit Term) *Exception {
	return typeError(Atom("in_character"), culprit, Atom(fmt.Sprintf("%s is not a character.", culprit)))
}

func typeErrorEvaluable(culprit Term) *Exception {
	return typeError(Atom("evaluable"), culprit, Atom(fmt.Sprintf("%s is not evaluable.", culprit)))
}

func typeErrorInteger(culprit Term) *Exception {
	return typeError(Atom("integer"), culprit, Atom(fmt.Sprintf("%s is not an integer.", culprit)))
}

func typeErrorList(culprit Term) *Exception {
	return typeError(Atom("list"), culprit, Atom(fmt.Sprintf("%s is not a list.", culprit)))
}

func typeErrorNumber(culprit Term) *Exception {
	return typeError(Atom("number"), culprit, Atom(fmt.Sprintf("%s is not a number.", culprit)))
}

func typeErrorPredicateIndicator(culprit Term) *Exception {
	return typeError(Atom("predicate_indicator"), culprit, Atom(fmt.Sprintf("%s is not a predicate indicator.", culprit)))
}

func typeErrorVariable(culprit Term) *Exception {
	return typeError(Atom("variable"), culprit, Atom(fmt.Sprintf("%s is not a variable.", culprit)))
}

func typeErrorCompound(culprit Term) *Exception {
	return typeError(Atom("compound"), culprit, Atom(fmt.Sprintf("%s is not a compound.", culprit)))
}

func typeError(validType, culprit, info Term) *Exception {
	return &Exception{
		Term: &Compound{
			Functor: "error",
			Args: []Term{
				&Compound{
					Functor: "type_error",
					Args:    []Term{validType, culprit},
				},
				info,
			},
		},
	}
}

func domainErrorCharacterCodeList(culprit Term) *Exception {
	return domainError(Atom("character_code_list"), culprit, Atom(fmt.Sprintf("%s is not a character code list.", culprit)))
}

func domainErrorCloseOption(culprit Term) *Exception {
	return domainError(Atom("close_option"), culprit, Atom(fmt.Sprintf("%s is not a close option.", culprit)))
}

func domainErrorFlagValue(culprit Term) *Exception {
	return domainError(Atom("flag_value"), culprit, Atom(fmt.Sprintf("%s is not a flag value.", culprit)))
}

func domainErrorIOMode(culprit Term) *Exception {
	return domainError(Atom("io_mode"), culprit, Atom(fmt.Sprintf("%s is not an I/O mode.", culprit)))
}

func domainErrorNotEmptyList(culprit Term) *Exception {
	return domainError(Atom("not_empty_list"), culprit, Atom(fmt.Sprintf("%s is an empty list.", culprit)))
}

func domainErrorNotLessThanZero(culprit Term) *Exception {
	return domainError(Atom("not_less_than_zero"), culprit, Atom(fmt.Sprintf("%s is less than zero.", culprit)))
}

func domainErrorOperatorPriority(culprit Term) *Exception {
	return domainError(Atom("operator_priority"), culprit, Atom(fmt.Sprintf("%s is not between 0 and 1200.", culprit)))
}

func domainErrorOperatorSpecifier(culprit Term) *Exception {
	return domainError(Atom("operator_specifier"), culprit, Atom(fmt.Sprintf("%s is neither xf, yf, xfx, xfy, yfx, fx, nor fy.", culprit)))
}

func domainErrorPrologFlag(culprit Term) *Exception {
	return domainError(Atom("prolog_flag"), culprit, Atom(fmt.Sprintf("%s is not a prolog flag.", culprit)))
}

func domainErrorReadOption(culprit Term) *Exception {
	return domainError(Atom("read_option"), culprit, Atom(fmt.Sprintf("%s is not a read option.", culprit)))
}

func domainErrorSourceSink(culprit Term) *Exception {
	return domainError(Atom("source_sink"), culprit, Atom(fmt.Sprintf("%s is not a source/sink.", culprit)))
}

func domainErrorStream(culprit Term) *Exception {
	return domainError(Atom("stream"), culprit, Atom(fmt.Sprintf("%s is not a stream.", culprit)))
}

func domainErrorStreamOption(culprit Term) *Exception {
	return domainError(Atom("stream_option"), culprit, Atom(fmt.Sprintf("%s is not a stream option.", culprit)))
}

func domainErrorStreamOrAlias(culprit Term) *Exception {
	return domainError(Atom("stream_or_alias"), culprit, Atom(fmt.Sprintf("%s is neither a stream nor an alias.", culprit)))
}

func domainErrorStreamPosition(culprit Term) *Exception {
	return domainError(Atom("stream_position"), culprit, Atom(fmt.Sprintf("%s is not a stream position.", culprit)))
}

func domainErrorStreamProperty(culprit Term) *Exception {
	return domainError(Atom("stream_property"), culprit, Atom(fmt.Sprintf("%s is not a stream property.", culprit)))
}

func domainErrorWriteOption(culprit Term) *Exception {
	return domainError(Atom("write_option"), culprit, Atom(fmt.Sprintf("%s is not a write option.", culprit)))
}

func domainErrorOrder(culprit Term) *Exception {
	return domainError(Atom("order"), culprit, Atom(fmt.Sprintf("%s is neither <, =, nor >.", culprit)))
}

func domainError(validDomain, culprit, info Term) *Exception {
	return &Exception{
		Term: &Compound{
			Functor: "error",
			Args: []Term{
				&Compound{
					Functor: "domain_error",
					Args:    []Term{validDomain, culprit},
				},
				info,
			},
		},
	}
}

func existenceErrorProcedure(culprit Term) *Exception {
	return existenceError(Atom("procedure"), culprit, Atom(fmt.Sprintf("procedure %s is not defined.", culprit)))
}

func existenceErrorSourceSink(culprit Term) *Exception {
	return existenceError(Atom("source_sink"), culprit, Atom(fmt.Sprintf("file %s doesn't exist.", culprit)))
}

func existenceErrorStream(culprit Term) *Exception {
	return existenceError(Atom("stream"), culprit, Atom(fmt.Sprintf("stream %s doesn't exist.", culprit)))
}

func existenceError(objectType, culprit, info Term) *Exception {
	return &Exception{
		Term: &Compound{
			Functor: "error",
			Args: []Term{
				&Compound{
					Functor: "existence_error",
					Args:    []Term{objectType, culprit},
				},
				info,
			},
		},
	}
}

func permissionErrorModifyStaticProcedure(culprit Term) *Exception {
	return permissionError(Atom("modify"), Atom("static_procedure"), culprit, Atom(fmt.Sprintf("%s is static.", culprit)))
}

func permissionErrorOutputStream(culprit Term) *Exception {
	return permissionError(Atom("output"), Atom("stream"), culprit, Atom(fmt.Sprintf("%s is not an output stream.", culprit)))
}

func permissionErrorOutputBinaryStream(culprit Term) *Exception {
	return permissionError(Atom("output"), Atom("binary_stream"), culprit, Atom(fmt.Sprintf("%s is a binary stream.", culprit)))
}

func permissionErrorOutputTextStream(culprit Term) *Exception {
	return permissionError(Atom("output"), Atom("text_stream"), culprit, Atom(fmt.Sprintf("%s is a text stream.", culprit)))
}

func permissionErrorInputStream(culprit Term) *Exception {
	return permissionError(Atom("input"), Atom("stream"), culprit, Atom(fmt.Sprintf("%s is not an input stream.", culprit)))
}

func permissionErrorInputBufferedStream(culprit Term) *Exception {
	return permissionError(Atom("input"), Atom("buffered_stream"), culprit, Atom(fmt.Sprintf("%s is not a buffered stream.", culprit)))
}

func permissionErrorInputBinaryStream(culprit Term) *Exception {
	return permissionError(Atom("input"), Atom("binary_stream"), culprit, Atom(fmt.Sprintf("%s is a binary stream.", culprit)))
}

func permissionErrorInputTextStream(culprit Term) *Exception {
	return permissionError(Atom("input"), Atom("text_stream"), culprit, Atom(fmt.Sprintf("%s is a text stream.", culprit)))
}

func permissionErrorInputPastEndOfStream(culprit Term) *Exception {
	return permissionError(Atom("input"), Atom("past_end_of_stream"), culprit, Atom(fmt.Sprintf("%s has past end of stream.", culprit)))
}

func permissionError(operation, permissionType, culprit, info Term) *Exception {
	return &Exception{
		Term: &Compound{
			Functor: "error",
			Args: []Term{
				&Compound{
					Functor: "permission_error",
					Args:    []Term{operation, permissionType, culprit},
				},
				info,
			},
		},
	}
}

func representationError(limit, info Term) *Exception {
	return &Exception{
		Term: &Compound{
			Functor: "error",
			Args: []Term{
				&Compound{
					Functor: "representation_error",
					Args:    []Term{limit},
				},
				info,
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

func syntaxErrorNotANumber(culprit Term) *Exception {
	return syntaxError(Atom("not_a_number"), Atom(fmt.Sprintf("%s is not a number", culprit)))
}

func syntaxErrorInsufficient() *Exception {
	return syntaxError(Atom("insufficient"), Atom("Not enough input for a term."))
}

func syntaxErrorInvalidToken(info Term) *Exception {
	return syntaxError(Atom("invalid_token"), info)
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

func systemError(err error) *Exception {
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
