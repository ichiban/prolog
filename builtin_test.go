package prolog

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestCopyTerm(t *testing.T) {
	in := &Variable{Ref: Atom("a")}
	out := &Variable{}
	k := func() (bool, error) {
		return true, nil
	}
	ok, err := CopyTerm(in, out, k)
	assert.NoError(t, err)
	assert.True(t, ok)
	assert.Equal(t, Atom("a"), out.Ref)
}

func TestRepeat(t *testing.T) {
	c := 3
	ok, err := Repeat(func() (bool, error) {
		c--
		return c == 0, nil
	})
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Repeat(func() (bool, error) {
		return false, errors.New("")
	})
	assert.Error(t, err)
	assert.False(t, ok)

	ok, err = Repeat(func() (bool, error) {
		return true, errCut
	})
	assert.True(t, errors.Is(err, errCut))
	assert.True(t, ok)
}
