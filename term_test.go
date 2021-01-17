package prolog

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestAtom_Unify(t *testing.T) {
	unit := Atom("foo")

	t.Run("atom", func(t *testing.T) {
		assert.True(t, unit.Unify(Atom("foo"), false))
		assert.False(t, unit.Unify(Atom("bar"), false))
	})

	t.Run("integer", func(t *testing.T) {
		assert.False(t, unit.Unify(Integer(1), false))
	})

	t.Run("variable", func(t *testing.T) {
		t.Run("free", func(t *testing.T) {
			v := Variable{Name: "X"}
			assert.True(t, unit.Unify(&v, false))
			assert.Equal(t, unit, v.Ref)
		})
		t.Run("bound to the same value", func(t *testing.T) {
			v := Variable{Name: "X", Ref: unit}
			assert.True(t, unit.Unify(&v, false))
			assert.Equal(t, unit, v.Ref)
		})
		t.Run("bound to a different value", func(t *testing.T) {
			v := Variable{Name: "X", Ref: Atom("bar")}
			assert.False(t, unit.Unify(&v, false))
		})
	})

	t.Run("compound", func(t *testing.T) {
		assert.False(t, unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("foo")},
		}, false))
	})
}

func TestAtom_Copy(t *testing.T) {
	unit := Atom("foo")
	assert.Equal(t, unit, unit.Copy())
}

func TestInteger_Unify(t *testing.T) {
	unit := Integer(1)

	t.Run("atom", func(t *testing.T) {
		assert.False(t, unit.Unify(Atom("foo"), false))
	})

	t.Run("integer", func(t *testing.T) {
		assert.True(t, unit.Unify(Integer(1), false))
		assert.False(t, unit.Unify(Integer(0), false))
	})

	t.Run("variable", func(t *testing.T) {
		t.Run("free", func(t *testing.T) {
			v := Variable{Name: "X"}
			assert.True(t, unit.Unify(&v, false))
			assert.Equal(t, unit, v.Ref)
		})
		t.Run("bound to the same value", func(t *testing.T) {
			v := Variable{Name: "X", Ref: unit}
			assert.True(t, unit.Unify(&v, false))
		})
		t.Run("bound to a different value", func(t *testing.T) {
			v := Variable{Name: "X", Ref: Integer(0)}
			assert.False(t, unit.Unify(&v, false))
		})
	})

	t.Run("compound", func(t *testing.T) {
		assert.False(t, unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("foo")},
		}, false))
	})
}

func TestInteger_Copy(t *testing.T) {
	unit := Integer(1)
	assert.Equal(t, unit, unit.Copy())
}

func TestCompound_Unify(t *testing.T) {
	unit := Compound{
		Functor: "foo",
		Args:    []Term{Atom("bar")},
	}

	t.Run("atom", func(t *testing.T) {
		assert.False(t, unit.Unify(Atom("foo"), false))
	})

	t.Run("integer", func(t *testing.T) {
		assert.False(t, unit.Unify(Integer(1), false))
	})

	t.Run("variable", func(t *testing.T) {
		t.Run("free", func(t *testing.T) {
			v := Variable{Name: "X"}
			assert.True(t, unit.Unify(&v, false))
			assert.Equal(t, &unit, v.Ref)
		})
		t.Run("bound to the same value", func(t *testing.T) {
			v := Variable{Name: "X", Ref: &unit}
			assert.True(t, unit.Unify(&v, false))
		})
		t.Run("bound to a different value", func(t *testing.T) {
			v := Variable{Name: "X", Ref: &Compound{
				Functor: "foo",
				Args:    []Term{Atom("baz")},
			}}
			assert.False(t, unit.Unify(&v, false))
		})
	})

	t.Run("compound", func(t *testing.T) {
		assert.True(t, unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("bar")},
		}, false))
		assert.False(t, unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("bar"), Atom("baz")},
		}, false))
		assert.False(t, unit.Unify(&Compound{
			Functor: "baz",
			Args:    []Term{Atom("bar")},
		}, false))
		assert.False(t, unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("baz")},
		}, false))
		v := &Variable{Name: "X"}
		assert.True(t, unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Term{v},
		}, false))
		assert.Equal(t, Atom("bar"), v.Ref)
	})
}

func TestCompound_String(t *testing.T) {
	t.Run("no parenthesises", func(t *testing.T) {
		c := Compound{
			Functor: "/",
			Args:    []Term{Atom("append"), Integer(3)},
		}
		var stop []*Variable
		assert.Equal(t, "append/3", c.TermString(operators{
			{Precedence: 400, Type: `yfx`, Name: `/`},
		}, &stop))
	})

	t.Run("parenthesises", func(t *testing.T) {
		c := Compound{
			Functor: "/",
			Args:    []Term{Atom("=="), Integer(2)},
		}
		var stop []*Variable
		assert.Equal(t, "(==)/2", c.TermString(operators{
			{Precedence: 400, Type: `yfx`, Name: `/`},
		}, &stop))
	})
}

func TestContains(t *testing.T) {
	assert.True(t, Contains(Atom("a"), Atom("a")))
	assert.False(t, Contains(&Variable{}, Atom("a")))
	assert.True(t, Contains(&Variable{Ref: Atom("a")}, Atom("a")))
	assert.True(t, Contains(&Compound{Functor: "a"}, Atom("a")))
	assert.True(t, Contains(&Compound{Functor: "f", Args: []Term{Atom("a")}}, Atom("a")))
	assert.False(t, Contains(&Compound{Functor: "f"}, Atom("a")))
}

func TestVariable_TermString(t *testing.T) {
	v := Variable{
		Name: "X",
		Ref: &Variable{
			Ref: &Variable{
				Ref: &Compound{
					Functor: "f",
					Args:    []Term{nil},
				},
			},
		},
	}
	v.Ref.(*Variable).Ref.(*Variable).Ref.(*Compound).Args[0] = &v

	var stop []*Variable
	assert.Equal(t, "X = f(X)", v.TermString(nil, &stop))
}
