package engine

import (
	"context"
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
	}, InstantiationError(context.Background()))
}

func TestDomainError(t *testing.T) {
	assert.Equal(t, Exception{
		term: atomError.Apply(
			atomDomainError.Apply(atomNotLessThanZero, Integer(-1)),
			rootContext,
		),
	}, DomainError(context.Background(), atomNotLessThanZero, Integer(-1)))
}

func TestTypeError(t *testing.T) {
	assert.Equal(t, Exception{
		term: atomError.Apply(
			atomTypeError.Apply(atomAtom, Integer(0)),
			rootContext,
		),
	}, TypeError(context.Background(), atomAtom, Integer(0)))
}

func TestExceptionalValue_Error(t *testing.T) {
	assert.Equal(t, "int_overflow", exceptionalValueIntOverflow.Error())
}
