package engine

import (
	"bytes"
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
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

func TestWrite(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		var buf bytes.Buffer
		assert.NoError(t, Write(&buf, Atom("f").Apply(Atom("a"), Atom("b")), nil))
		assert.Equal(t, "f(a, b)", buf.String())
	})

	t.Run("ng", func(t *testing.T) {
		var w mockWriter
		w.On("Write", mock.Anything).Return(0, errors.New("failed"))
		defer w.AssertExpectations(t)

		assert.Error(t, Write(&w, Atom("f").Apply(Atom("a"), Atom("b")), nil))
	})
}
