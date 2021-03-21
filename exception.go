package prolog

import "fmt"

// Exception is an error represented by a prolog term.
type Exception struct {
	Term Term
}

func (e *Exception) Error() string {
	return e.Term.String()
}

// InstantiationError creates an Exception which describes a certain argument contains a free variable.
func InstantiationError(culprit Term) *Exception {
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

// TypeErrorAtom creates an Exception which describes culprit is not an atom.
func TypeErrorAtom(culprit Term) *Exception {
	return typeError(Atom("atom"), culprit, Atom(fmt.Sprintf("%s is not an atom.", culprit)))
}

// TypeErrorAtomic creates an Exception which describes culprit is not atomic.
func TypeErrorAtomic(culprit Term) *Exception {
	return typeError(Atom("atomic"), culprit, Atom(fmt.Sprintf("%s is not atomic.", culprit)))
}

// TypeErrorByte creates an Exception which describes culprit is not a byte.
func TypeErrorByte(culprit Term) *Exception {
	return typeError(Atom("byte"), culprit, Atom(fmt.Sprintf("%s is not a byte.", culprit)))
}

// TypeErrorCallable creates an Exception which describes culprit is not callable.
func TypeErrorCallable(culprit Term) *Exception {
	return typeError(Atom("callable"), culprit, Atom(fmt.Sprintf("%s is not callable.", culprit)))
}

// TypeErrorCharacter creates an Exception which describes culprit is not a character.
func TypeErrorCharacter(culprit Term) *Exception {
	return typeError(Atom("character"), culprit, Atom(fmt.Sprintf("%s is not a character.", culprit)))
}

// TypeErrorInByte creates an Exception which describes culprit is not a byte.
func TypeErrorInByte(culprit Term) *Exception {
	return typeError(Atom("in_byte"), culprit, Atom(fmt.Sprintf("%s is not a byte.", culprit)))
}

// TypeErrorInCharacter creates an Exception which describes culprit is not a byte.
func TypeErrorInCharacter(culprit Term) *Exception {
	return typeError(Atom("in_character"), culprit, Atom(fmt.Sprintf("%s is not a character.", culprit)))
}

// TypeErrorEvaluable creates an Exception which describes culprit is not callable.
func TypeErrorEvaluable(culprit Term) *Exception {
	return typeError(Atom("evaluable"), culprit, Atom(fmt.Sprintf("%s is not evaluable.", culprit)))
}

// TypeErrorInteger creates an Exception which describes culprit is not an integer.
func TypeErrorInteger(culprit Term) *Exception {
	return typeError(Atom("integer"), culprit, Atom(fmt.Sprintf("%s is not an integer.", culprit)))
}

// TypeErrorList creates an Exception which describes culprit is not a list.
func TypeErrorList(culprit Term) *Exception {
	return typeError(Atom("list"), culprit, Atom(fmt.Sprintf("%s is not a list.", culprit)))
}

// TypeErrorNumber creates an Exception which describes culprit is not a number.
func TypeErrorNumber(culprit Term) *Exception {
	return typeError(Atom("number"), culprit, Atom(fmt.Sprintf("%s is not a number.", culprit)))
}

// TypeErrorPredicateIndicator creates an Exception which describes culprit is not a predicate indicator.
func TypeErrorPredicateIndicator(culprit Term) *Exception {
	return typeError(Atom("predicate_indicator"), culprit, Atom(fmt.Sprintf("%s is not a predicate indicator.", culprit)))
}

// TypeErrorVariable creates an Exception which describes culprit is not a variable.
func TypeErrorVariable(culprit Term) *Exception {
	return typeError(Atom("variable"), culprit, Atom(fmt.Sprintf("%s is not a variable.", culprit)))
}

// TypeErrorCompound creates an Exception which describes culprit is not a compound.
func TypeErrorCompound(culprit Term) *Exception {
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

// DomainErrorCharacterCodeList creates an Exception which describes culprit is not a character code list.
func DomainErrorCharacterCodeList(culprit Term) *Exception {
	return domainError(Atom("character_code_list"), culprit, Atom(fmt.Sprintf("%s is not a character code list.", culprit)))
}

// DomainErrorCloseOption creates an Exception which describes culprit is not a close option.
func DomainErrorCloseOption(culprit Term) *Exception {
	return domainError(Atom("close_option"), culprit, Atom(fmt.Sprintf("%s is not a close option.", culprit)))
}

// DomainErrorFlagValue creates an Exception which describes culprit is not a flag value.
func DomainErrorFlagValue(culprit Term) *Exception {
	return domainError(Atom("flag_value"), culprit, Atom(fmt.Sprintf("%s is not a flag value.", culprit)))
}

// DomainErrorIOMode creates an Exception which describes culprit is not an I/O mode.
func DomainErrorIOMode(culprit Term) *Exception {
	return domainError(Atom("io_mode"), culprit, Atom(fmt.Sprintf("%s is not an I/O mode.", culprit)))
}

// DomainErrorNotEmptyList creates an Exception which describes culprit is an empty list.
func DomainErrorNotEmptyList(culprit Term) *Exception {
	return domainError(Atom("not_empty_list"), culprit, Atom(fmt.Sprintf("%s is an empty list.", culprit)))
}

// DomainErrorNotLessThanZero creates an Exception which describes culprit is less than zero.
func DomainErrorNotLessThanZero(culprit Term) *Exception {
	return domainError(Atom("not_less_than_zero"), culprit, Atom(fmt.Sprintf("%s is less than zero.", culprit)))
}

// DomainErrorOperatorPriority creates an Exception which describes culprit is not an operator priority.
func DomainErrorOperatorPriority(culprit Term) *Exception {
	return domainError(Atom("operator_priority"), culprit, Atom(fmt.Sprintf("%s is not between 0 and 1200.", culprit)))
}

// DomainErrorOperatorSpecifier creates an Exception which describes culprit is not an operator specifier.
func DomainErrorOperatorSpecifier(culprit Term) *Exception {
	return domainError(Atom("operator_specifier"), culprit, Atom(fmt.Sprintf("%s is neither xf, yf, xfx, xfy, yfx, fx, nor fy.", culprit)))
}

// DomainErrorPrologFlag creates an Exception which describes culprit is not a prolog flag.
func DomainErrorPrologFlag(culprit Term) *Exception {
	return domainError(Atom("prolog_flag"), culprit, Atom(fmt.Sprintf("%s is not a prolog flag.", culprit)))
}

// DomainErrorReadOption creates an Exception which describes culprit is not a read option.
func DomainErrorReadOption(culprit Term) *Exception {
	return domainError(Atom("read_option"), culprit, Atom(fmt.Sprintf("%s is not a read option.", culprit)))
}

// DomainErrorSourceSink creates an Exception which describes culprit is not a source/sink.
func DomainErrorSourceSink(culprit Term) *Exception {
	return domainError(Atom("source_sink"), culprit, Atom(fmt.Sprintf("%s is not a source/sink.", culprit)))
}

// DomainErrorStream creates an Exception which describes culprit is not a stream.
func DomainErrorStream(culprit Term) *Exception {
	return domainError(Atom("stream"), culprit, Atom(fmt.Sprintf("%s is not a stream.", culprit)))
}

// DomainErrorStreamOption creates an Exception which describes culprit is not a stream option.
func DomainErrorStreamOption(culprit Term) *Exception {
	return domainError(Atom("stream_option"), culprit, Atom(fmt.Sprintf("%s is not a stream option.", culprit)))
}

// DomainErrorStreamOrAlias creates an Exception which describes culprit is neither a stream nor an alias.
func DomainErrorStreamOrAlias(culprit Term) *Exception {
	return domainError(Atom("stream_or_alias"), culprit, Atom(fmt.Sprintf("%s is neither a stream nor an alias.", culprit)))
}

// DomainErrorStreamPosition creates an Exception which describes culprit is not a stream position.
func DomainErrorStreamPosition(culprit Term) *Exception {
	return domainError(Atom("stream_position"), culprit, Atom(fmt.Sprintf("%s is not a stream position.", culprit)))
}

// DomainErrorStreamProperty creates an Exception which describes culprit is not a stream property.
func DomainErrorStreamProperty(culprit Term) *Exception {
	return domainError(Atom("stream_property"), culprit, Atom(fmt.Sprintf("%s is not a stream property.", culprit)))
}

// DomainErrorWriteOption creates an Exception which describes culprit is not a write option.
func DomainErrorWriteOption(culprit Term) *Exception {
	return domainError(Atom("write_option"), culprit, Atom(fmt.Sprintf("%s is not a write option.", culprit)))
}

// DomainErrorOrder creates an Exception which describes culprit is not an order.
func DomainErrorOrder(culprit Term) *Exception {
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

// ExistenceErrorProcedure creates an Exception which describes culprit of procedure doesn't exist.
func ExistenceErrorProcedure(culprit Term) *Exception {
	return existenceError(Atom("procedure"), culprit, Atom(fmt.Sprintf("procedure %s is not defined.", culprit)))
}

// ExistenceErrorSourceSink creates an Exception which describes culprit of source/sink doesn't exist.
func ExistenceErrorSourceSink(culprit Term) *Exception {
	return existenceError(Atom("source_sink"), culprit, Atom(fmt.Sprintf("file %s doesn't exist.", culprit)))
}

// ExistenceErrorStream creates an Exception which describes culprit of stream doesn't exist.
func ExistenceErrorStream(culprit Term) *Exception {
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

// PermissionError creates an Exception which describes it's not permitted to do operation on culprit of permissionType.
func PermissionError(operation, permissionType, culprit, info Term) *Exception {
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

// RepresentationError creates an Exception which describes an implementation limit has been breached.
func RepresentationError(limit, info Term) *Exception {
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

// EvaluationErrorZeroDivisor creates an Exception which descirbes zero division caused by culprit.
func EvaluationErrorZeroDivisor() *Exception {
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

// ResourceError creates an Exception which describes an insufficient resource.
func ResourceError(resource, info Term) *Exception {
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

// SyntaxError creates an Exception which describes an error on syntax.
func SyntaxError(detail, info Term) *Exception {
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

// SystemError creates an Exception which describes a system error.
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
