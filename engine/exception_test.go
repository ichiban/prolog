package engine

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestException_Error(t *testing.T) {
	e := Exception{term: Atom("foo")}
	assert.Equal(t, "foo", e.Error())
}

func TestInstantiationError(t *testing.T) {
	assert.Equal(t, Exception{
		term: &compound{
			functor: "error",
			args: []Term{
				Atom("instantiation_error"),
				rootContext,
			},
		},
	}, InstantiationError(nil))
}

func TestTypeError(t *testing.T) {
	assert.Equal(t, Exception{
		term: &compound{
			functor: "error",
			args: []Term{
				&compound{
					functor: "type_error",
					args: []Term{
						Atom("atom"),
						Integer(0),
					},
				},
				rootContext,
			},
		},
	}, TypeError(ValidTypeAtom, Integer(0), nil))
}

func TestDomainError(t *testing.T) {
	assert.Equal(t, Exception{
		term: &compound{
			functor: "error",
			args: []Term{
				&compound{
					functor: "domain_error",
					args: []Term{
						Atom("not_less_than_zero"),
						Integer(-1),
					},
				},
				rootContext,
			},
		},
	}, DomainError(ValidDomainNotLessThanZero, Integer(-1), nil))
}

func TestExistenceError(t *testing.T) {
	pi := ProcedureIndicator{Name: "foo", Arity: 0}.Term()
	assert.Equal(t, Exception{
		term: &compound{
			functor: "error",
			args: []Term{
				&compound{
					functor: "existence_error",
					args: []Term{
						Atom("procedure"),
						pi,
					},
				},
				rootContext,
			},
		},
	}, ExistenceError(ObjectTypeProcedure, pi, nil))
}

func TestPermissionError(t *testing.T) {
	pi := ProcedureIndicator{Name: "foo", Arity: 0}.Term()
	assert.Equal(t, Exception{
		term: &compound{
			functor: "error",
			args: []Term{
				&compound{
					functor: "permission_error",
					args: []Term{
						Atom("modify"),
						Atom("static_procedure"),
						pi,
					},
				},
				rootContext,
			},
		},
	}, PermissionError(OperationModify, PermissionTypeStaticProcedure, pi, nil))
}

func TestRepresentationError(t *testing.T) {
	assert.Equal(t, Exception{
		term: &compound{
			functor: "error",
			args: []Term{
				&compound{
					functor: "representation_error",
					args: []Term{
						Atom("max_integer"),
					},
				},
				rootContext,
			},
		},
	}, RepresentationError(FlagMaxInteger, nil))
}

func TestResourceError(t *testing.T) {
	assert.Equal(t, Exception{
		term: &compound{
			functor: "error",
			args: []Term{
				&compound{
					functor: "resource_error",
					args: []Term{
						Atom("finite_memory"),
					},
				},
				rootContext,
			},
		},
	}, ResourceError(ResourceFiniteMemory, nil))
}

func TestSyntaxError(t *testing.T) {
	assert.Equal(t, Exception{
		term: &compound{
			functor: "error",
			args: []Term{
				&compound{
					functor: "syntax_error",
					args: []Term{
						Atom("foo"),
					},
				},
				rootContext,
			},
		},
	}, SyntaxError(errors.New("foo"), nil))
}

func TestSystemError(t *testing.T) {
	assert.Equal(t, Exception{
		term: &compound{
			functor: "error",
			args: []Term{
				Atom("system_error"),
				Atom("foo"),
			},
		},
	}, SystemError(errors.New("foo")))
}

func TestEvaluationError(t *testing.T) {
	assert.Equal(t, Exception{
		term: &compound{
			functor: "error",
			args: []Term{
				&compound{
					functor: "evaluation_error",
					args: []Term{
						Atom("int_overflow"),
					},
				},
				rootContext,
			},
		},
	}, EvaluationError(ExceptionalValueIntOverflow, nil))
}

func TestExceptionalValue_Error(t *testing.T) {
	assert.Equal(t, "int_overflow", ExceptionalValueIntOverflow.Error())
}
