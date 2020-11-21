package prolog

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestAtom_Unify(t *testing.T) {
	unit := Atom("foo")

	t.Run("atom", func(t *testing.T) {
		assert.True(t, unit.Unify(Atom("foo")))
		assert.False(t, unit.Unify(Atom("bar")))
	})

	t.Run("integer", func(t *testing.T) {
		assert.False(t, unit.Unify(Integer(1)))
	})

	t.Run("variable", func(t *testing.T) {
		t.Run("free", func(t *testing.T) {
			v := Variable{Name: "X"}
			assert.True(t, unit.Unify(&v))
			assert.Equal(t, unit, v.Ref)
		})
		t.Run("bound to the same value", func(t *testing.T) {
			v := Variable{Name: "X", Ref: unit}
			assert.True(t, unit.Unify(&v))
			assert.Equal(t, unit, v.Ref)
		})
		t.Run("bound to a different value", func(t *testing.T) {
			v := Variable{Name: "X", Ref: Atom("bar")}
			assert.False(t, unit.Unify(&v))
		})
	})

	t.Run("compound", func(t *testing.T) {
		assert.False(t, unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("foo")},
		}))
	})
}

func TestInteger_Unify(t *testing.T) {
	unit := Integer(1)

	t.Run("atom", func(t *testing.T) {
		assert.False(t, unit.Unify(Atom("foo")))
	})

	t.Run("integer", func(t *testing.T) {
		assert.True(t, unit.Unify(Integer(1)))
		assert.False(t, unit.Unify(Integer(0)))
	})

	t.Run("variable", func(t *testing.T) {
		t.Run("free", func(t *testing.T) {
			v := Variable{Name: "X"}
			assert.True(t, unit.Unify(&v))
			assert.Equal(t, unit, v.Ref)
		})
		t.Run("bound to the same value", func(t *testing.T) {
			v := Variable{Name: "X", Ref: unit}
			assert.True(t, unit.Unify(&v))
		})
		t.Run("bound to a different value", func(t *testing.T) {
			v := Variable{Name: "X", Ref: Integer(0)}
			assert.False(t, unit.Unify(&v))
		})
	})

	t.Run("compound", func(t *testing.T) {
		assert.False(t, unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("foo")},
		}))
	})
}

func TestCompound_Unify(t *testing.T) {
	unit := Compound{
		Functor: "foo",
		Args:    []Term{Atom("bar")},
	}

	t.Run("atom", func(t *testing.T) {
		assert.False(t, unit.Unify(Atom("foo")))
	})

	t.Run("integer", func(t *testing.T) {
		assert.False(t, unit.Unify(Integer(1)))
	})

	t.Run("variable", func(t *testing.T) {
		t.Run("free", func(t *testing.T) {
			v := Variable{Name: "X"}
			assert.True(t, unit.Unify(&v))
			assert.Equal(t, &unit, v.Ref)
		})
		t.Run("bound to the same value", func(t *testing.T) {
			v := Variable{Name: "X", Ref: &unit}
			assert.True(t, unit.Unify(&v))
		})
		t.Run("bound to a different value", func(t *testing.T) {
			v := Variable{Name: "X", Ref: &Compound{
				Functor: "foo",
				Args:    []Term{Atom("baz")},
			}}
			assert.False(t, unit.Unify(&v))
		})
	})

	t.Run("compound", func(t *testing.T) {
		assert.True(t, unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("bar")},
		}))
		assert.False(t, unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("bar"), Atom("baz")},
		}))
		assert.False(t, unit.Unify(&Compound{
			Functor: "baz",
			Args:    []Term{Atom("bar")},
		}))
		assert.False(t, unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("baz")},
		}))
		v := &Variable{Name: "X"}
		assert.True(t, unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Term{v},
		}))
		assert.Equal(t, Atom("bar"), v.Ref)
	})
}

func TestCompound_String(t *testing.T) {
	c := Compound{
		Functor: "/",
		Args:    []Term{Atom("append"), Integer(3)},
	}
	assert.Equal(t, "append/3", c.TermString(operators{
		{Precedence: 400, Type: `yfx`, Name: `/`},
	}))
}
