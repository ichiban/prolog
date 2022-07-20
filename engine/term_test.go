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

func Test_iteratedGoalTerm(t *testing.T) {
	tests := []struct {
		t, g Term
	}{
		{t: &Compound{
			Functor: "^",
			Args: []Term{
				Variable("X"),
				&Compound{
					Functor: "foo",
					Args:    []Term{Variable("X")},
				},
			},
		}, g: &Compound{
			Functor: "foo",
			Args:    []Term{Variable("X")},
		}},
		{
			t: Atom("^").Apply(NewVariable(), Atom("^").Apply(NewVariable(), Atom("=").Apply(Variable("X"), Integer(1)))),
			g: Atom("=").Apply(Variable("X"), Integer(1)),
		},
	}

	for _, tt := range tests {
		assert.Equal(t, tt.g, iteratedGoalTerm(tt.t, nil))
	}
}

func Test_variant(t *testing.T) {
	tests := []struct {
		t1, t2 Term
	}{
		{
			t1: &Compound{Functor: "f", Args: []Term{Variable("A"), Variable("B"), Variable("A")}},
			t2: &Compound{Functor: "f", Args: []Term{Variable("X"), Variable("Y"), Variable("X")}},
		},
		{
			t1: &Compound{Functor: "g", Args: []Term{Variable("A"), Variable("B")}},
			t2: &Compound{Functor: "g", Args: []Term{NewVariable(), NewVariable()}},
		},
		{
			t1: &Compound{Functor: "+", Args: []Term{Variable("P"), Variable("Q")}},
			t2: &Compound{Functor: "+", Args: []Term{Variable("P"), Variable("Q")}},
		},
	}

	for _, tt := range tests {
		assert.True(t, variant(tt.t1, tt.t2, nil))
	}
}
