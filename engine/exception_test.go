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
	}, instantiationError(nil))
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
	}, typeError(validTypeAtom, Integer(0), nil))
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
	}, domainError(validDomainNotLessThanZero, Integer(-1), nil))
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
	}, existenceError(objectTypeProcedure, pi, nil))
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
	}, permissionError(operationModify, permissionTypeStaticProcedure, pi, nil))
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
	}, representationError(flagMaxInteger, nil))
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
	}, resourceError(resourceFiniteMemory, nil))
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
	}, syntaxError(errors.New("foo"), nil))
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
	}, evaluationError(exceptionalValueIntOverflow, nil))
}

func TestExceptionalValue_Error(t *testing.T) {
	assert.Equal(t, "int_overflow", exceptionalValueIntOverflow.Error())
}
