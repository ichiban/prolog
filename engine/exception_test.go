package engine

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestException_Error(t *testing.T) {
	e := Exception{term: NewAtom("foo")}
	assert.Equal(t, "foo", e.Error())
}

func TestInstantiationError(t *testing.T) {
	assert.Equal(t, Exception{
		term: &compound{
			functor: atomError,
			args: []Term{
				NewAtom("instantiation_error"),
				rootContext,
			},
		},
	}, InstantiationError(nil))
}

func TestTypeError(t *testing.T) {
	assert.Equal(t, Exception{
		term: &compound{
			functor: atomError,
			args: []Term{
				&compound{
					functor: NewAtom("type_error"),
					args: []Term{
						atomAtom,
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
			functor: atomError,
			args: []Term{
				&compound{
					functor: NewAtom("domain_error"),
					args: []Term{
						NewAtom("not_less_than_zero"),
						Integer(-1),
					},
				},
				rootContext,
			},
		},
	}, DomainError(ValidDomainNotLessThanZero, Integer(-1), nil))
}

func TestExistenceError(t *testing.T) {
	pi := procedureIndicator{name: NewAtom("foo"), arity: 0}
	assert.Equal(t, Exception{
		term: &compound{
			functor: atomError,
			args: []Term{
				&compound{
					functor: NewAtom("existence_error"),
					args: []Term{
						NewAtom("procedure"),
						pi,
					},
				},
				rootContext,
			},
		},
	}, ExistenceError(ObjectTypeProcedure, pi, nil))
}

func TestPermissionError(t *testing.T) {
	pi := procedureIndicator{name: NewAtom("foo"), arity: 0}
	assert.Equal(t, Exception{
		term: &compound{
			functor: atomError,
			args: []Term{
				&compound{
					functor: NewAtom("permission_error"),
					args: []Term{
						NewAtom("modify"),
						NewAtom("static_procedure"),
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
			functor: atomError,
			args: []Term{
				&compound{
					functor: NewAtom("representation_error"),
					args: []Term{
						atomMaxInteger,
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
			functor: atomError,
			args: []Term{
				&compound{
					functor: NewAtom("resource_error"),
					args: []Term{
						NewAtom("finite_memory"),
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
			functor: atomError,
			args: []Term{
				&compound{
					functor: NewAtom("syntax_error"),
					args: []Term{
						NewAtom("foo"),
					},
				},
				rootContext,
			},
		},
	}, SyntaxError(errors.New("foo"), nil))
}

func TestEvaluationError(t *testing.T) {
	assert.Equal(t, Exception{
		term: &compound{
			functor: atomError,
			args: []Term{
				&compound{
					functor: NewAtom("evaluation_error"),
					args: []Term{
						NewAtom("int_overflow"),
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
