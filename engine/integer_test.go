package engine

import (
	"math"
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

func TestInteger_Unparse(t *testing.T) {
	t.Run("positive", func(t *testing.T) {
		var tokens []Token
		Integer(33).Unparse(func(token Token) {
			tokens = append(tokens, token)
		}, nil)
		assert.Equal(t, []Token{
			{Kind: TokenInteger, Val: "33"},
		}, tokens)
	})

	t.Run("negative", func(t *testing.T) {
		var tokens []Token
		Integer(-33).Unparse(func(token Token) {
			tokens = append(tokens, token)
		}, nil)
		assert.Equal(t, []Token{
			{Kind: TokenGraphic, Val: "-"},
			{Kind: TokenInteger, Val: "33"},
		}, tokens)

		t.Run("math.MinInt64", func(t *testing.T) {
			var tokens []Token
			Integer(math.MinInt64).Unparse(func(token Token) {
				tokens = append(tokens, token)
			}, nil)
			assert.Equal(t, []Token{
				{Kind: TokenGraphic, Val: "-"},
				{Kind: TokenInteger, Val: "9223372036854775808"},
			}, tokens)
		})
	})
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
