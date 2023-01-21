package engine

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestException_Error(t *testing.T) {
	e := Exception{term: NewAtom("foo")}
	assert.Equal(t, "foo", e.Error())
}

func TestInstantiationError(t *testing.T) {
	assert.Equal(t, Exception{
		term: atomError.Apply(atomInstantiationError, rootContext),
	}, InstantiationError(nil))
}

func TestDomainError(t *testing.T) {
	assert.Equal(t, Exception{
		term: atomError.Apply(
			atomDomainError.Apply(atomNotLessThanZero, Integer(-1)),
			rootContext,
		),
	}, DomainError(atomNotLessThanZero, Integer(-1), nil))
}

func TestTypeError(t *testing.T) {
	assert.Equal(t, Exception{
		term: atomError.Apply(
			atomTypeError.Apply(atomAtom, Integer(0)),
			rootContext,
		),
	}, TypeError(atomAtom, Integer(0), nil))
}

func TestExceptionalValue_Error(t *testing.T) {
	assert.Equal(t, "int_overflow", exceptionalValueIntOverflow.Error())
}
