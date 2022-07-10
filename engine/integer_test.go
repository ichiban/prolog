package engine

import (
	"bytes"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestInteger_Unify(t *testing.T) {
	unit := Integer(1)

	t.Run("atom", func(t *testing.T) {
		env, ok := unit.Unify(Atom("foo"), false, nil)
		assert.False(t, ok)
		assert.Nil(t, env)
	})

	t.Run("integer", func(t *testing.T) {
		env, ok := unit.Unify(Integer(1), false, nil)
		assert.True(t, ok)
		assert.Nil(t, env)

		env, ok = unit.Unify(Integer(0), false, nil)
		assert.False(t, ok)
		assert.Nil(t, env)
	})

	t.Run("variable", func(t *testing.T) {
		t.Run("free", func(t *testing.T) {
			v := Variable("X")
			env, ok := unit.Unify(v, false, nil)
			assert.True(t, ok)
			assert.Equal(t, unit, env.Resolve(v))
		})
		t.Run("bound to the same value", func(t *testing.T) {
			v := Variable("X")
			env := NewEnv().
				Bind(v, unit)
			_, ok := unit.Unify(v, false, env)
			assert.True(t, ok)
		})
		t.Run("bound to a different value", func(t *testing.T) {
			v := Variable("X")
			env := NewEnv().
				Bind(v, Integer(0))
			_, ok := unit.Unify(v, false, env)
			assert.False(t, ok)
		})
	})

	t.Run("compound", func(t *testing.T) {
		_, ok := unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("foo")},
		}, false, nil)
		assert.False(t, ok)
	})
}

func TestInteger_WriteTerm(t *testing.T) {
	tests := []struct {
		title   string
		integer Integer
		before  operator
		output  string
	}{
		{title: "positive", integer: 33, output: `33`},
		{title: "positive following unary minus", integer: 33, before: operator{name: "-", specifier: operatorSpecifierFX}, output: ` (33)`},
		{title: "negative", integer: -33, output: `-33`},
	}

	var buf bytes.Buffer
	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			buf.Reset()
			assert.NoError(t, tt.integer.WriteTerm(&buf, &WriteOptions{left: tt.before}, nil))
			assert.Equal(t, tt.output, buf.String())
		})
	}
}

func TestInteger_Compare(t *testing.T) {
	var m mockTerm
	defer m.AssertExpectations(t)

	assert.Equal(t, int64(-1), Integer(0).Compare(&m, nil))
	assert.Equal(t, int64(-1), Integer(0).Compare(Integer(1), nil))
	assert.Equal(t, int64(0), Integer(0).Compare(Integer(0), nil))
	assert.Equal(t, int64(1), Integer(1).Compare(Integer(0), nil))
	assert.Equal(t, int64(1), Integer(0).Compare(Float(0), nil))
	assert.Equal(t, int64(1), Integer(0).Compare(Variable("X"), nil))
}
