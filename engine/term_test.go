package engine

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestContains(t *testing.T) {
	var env *Env
	assert.True(t, Contains(Atom("a"), Atom("a"), env))
	assert.False(t, Contains(NewVariable(), Atom("a"), env))
	v := Variable("V")
	env = env.Bind(v, Atom("a"))
	assert.True(t, Contains(v, Atom("a"), env))
	assert.True(t, Contains(&Compound{Functor: "a"}, Atom("a"), env))
	assert.True(t, Contains(&Compound{Functor: "f", Args: []Term{Atom("a")}}, Atom("a"), env))
	assert.False(t, Contains(&Compound{Functor: "f"}, Atom("a"), env))
}

func TestRulify(t *testing.T) {
	assert.Equal(t, &Compound{
		Functor: ":-",
		Args:    []Term{Atom("a"), Atom("true")},
	}, Rulify(Atom("a"), nil))
	v := Variable("V")
	env := NewEnv().
		Bind(v, Atom("a"))
	assert.Equal(t, &Compound{
		Functor: ":-",
		Args:    []Term{Atom("a"), Atom("true")},
	}, Rulify(v, env))
	assert.Equal(t, &Compound{
		Functor: ":-",
		Args:    []Term{Atom("a"), Atom("b")},
	}, Rulify(&Compound{
		Functor: ":-",
		Args:    []Term{Atom("a"), Atom("b")},
	}, nil))
}

func TestErrWriter_Write(t *testing.T) {
	var failed = errors.New("failed")

	var m mockWriter
	m.On("Write", []byte("foo")).Return(0, failed).Once()
	defer m.AssertExpectations(t)

	ew := errWriter{w: &m}
	_, err := ew.Write([]byte("foo"))
	assert.NoError(t, err)
	_, err = ew.Write([]byte("bar"))
	assert.NoError(t, err)
	_, err = ew.Write([]byte("baz"))
	assert.NoError(t, err)
	assert.Equal(t, failed, ew.err)
}
