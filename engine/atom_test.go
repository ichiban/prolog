package engine

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestAtom_Compare(t *testing.T) {
	assert.Equal(t, int64(-1), Atom("a").Compare(Atom("b"), nil))
	assert.Equal(t, int64(0), Atom("a").Compare(Atom("a"), nil))
	assert.Equal(t, int64(1), Atom("b").Compare(Atom("a"), nil))
	assert.Equal(t, int64(1), Atom("a").Compare(Variable("X"), nil))
	assert.Equal(t, int64(1), Atom("a").Compare(Float(0), nil))
	assert.Equal(t, int64(1), Atom("a").Compare(Integer(0), nil))
}
