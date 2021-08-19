package term

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestInteger_Unify(t *testing.T) {
	unit := Integer(1)

	t.Run("atom", func(t *testing.T) {
		assert.False(t, unit.Unify(Atom("foo"), false, nil))
	})

	t.Run("integer", func(t *testing.T) {
		assert.True(t, unit.Unify(Integer(1), false, nil))
		assert.False(t, unit.Unify(Integer(0), false, nil))
	})

	t.Run("variable", func(t *testing.T) {
		t.Run("free", func(t *testing.T) {
			env := Env{}
			v := Variable("X")
			assert.True(t, unit.Unify(v, false, &env))
			assert.Equal(t, unit, env.Resolve(v))
		})
		t.Run("bound to the same value", func(t *testing.T) {
			v := Variable("X")
			env := Env{
				{
					Variable: v,
					Value:    unit,
				},
			}
			assert.True(t, unit.Unify(v, false, &env))
		})
		t.Run("bound to a different value", func(t *testing.T) {
			v := Variable("X")
			env := Env{
				{
					Variable: v,
					Value:    Integer(0),
				},
			}
			assert.False(t, unit.Unify(v, false, &env))
		})
	})

	t.Run("compound", func(t *testing.T) {
		assert.False(t, unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Interface{Atom("foo")},
		}, false, nil))
	})
}
