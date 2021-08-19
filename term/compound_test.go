package term

import (
	"bytes"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestCompound_Unify(t *testing.T) {
	unit := Compound{
		Functor: "foo",
		Args:    []Interface{Atom("bar")},
	}

	t.Run("atom", func(t *testing.T) {
		assert.False(t, unit.Unify(Atom("foo"), false, nil))
	})

	t.Run("integer", func(t *testing.T) {
		assert.False(t, unit.Unify(Integer(1), false, nil))
	})

	t.Run("variable", func(t *testing.T) {
		t.Run("free", func(t *testing.T) {
			env := Env{}
			v := Variable("X")
			assert.True(t, unit.Unify(v, false, &env))
			assert.Equal(t, &unit, env.Resolve(v))
		})
		t.Run("bound to the same value", func(t *testing.T) {
			v := Variable("X")
			env := Env{
				{
					Variable: v,
					Value:    &unit,
				},
			}
			assert.True(t, unit.Unify(v, false, &env))
		})
		t.Run("bound to a different value", func(t *testing.T) {
			v := Variable("X")
			env := Env{
				{
					Variable: v,
					Value: &Compound{
						Functor: "foo",
						Args:    []Interface{Atom("baz")},
					},
				},
			}
			assert.False(t, unit.Unify(&v, false, &env))
		})
	})

	t.Run("compound", func(t *testing.T) {
		assert.True(t, unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Interface{Atom("bar")},
		}, false, nil))
		assert.False(t, unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Interface{Atom("bar"), Atom("baz")},
		}, false, nil))
		assert.False(t, unit.Unify(&Compound{
			Functor: "baz",
			Args:    []Interface{Atom("bar")},
		}, false, nil))
		assert.False(t, unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Interface{Atom("baz")},
		}, false, nil))
		env := Env{}
		v := Variable("X")
		assert.True(t, unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Interface{v},
		}, false, &env))
		assert.Equal(t, Atom("bar"), env.Resolve(v))
	})
}

func TestCompound_String(t *testing.T) {
	t.Run("no parenthesises", func(t *testing.T) {
		c := Compound{
			Functor: "/",
			Args:    []Interface{Atom("append"), Integer(3)},
		}
		assert.Equal(t, "append/3", c.String())
	})

	t.Run("parenthesises", func(t *testing.T) {
		c := Compound{
			Functor: "/",
			Args:    []Interface{Atom("=="), Integer(2)},
		}
		assert.Equal(t, "(==)/2", c.String())
	})
}

func TestCompound_WriteTerm(t *testing.T) {
	t.Run("ignore_ops", func(t *testing.T) {
		c := Compound{
			Functor: "+",
			Args: []Interface{
				Integer(2),
				&Compound{
					Functor: "-",
					Args:    []Interface{Integer(2)},
				},
			},
		}

		t.Run("false", func(t *testing.T) {
			ops := Operators{
				{Priority: 500, Specifier: "yfx", Name: "+"},
				{Priority: 200, Specifier: "fy", Name: "-"},
			}

			var buf bytes.Buffer
			assert.NoError(t, c.WriteTerm(&buf, WriteTermOptions{Ops: ops}, nil))
			assert.Equal(t, "2+(-2)", buf.String())
		})

		t.Run("true", func(t *testing.T) {
			var buf bytes.Buffer
			assert.NoError(t, c.WriteTerm(&buf, WriteTermOptions{Ops: nil}, nil))
			assert.Equal(t, "+(2, -(2))", buf.String())
		})
	})

	t.Run("numbervars", func(t *testing.T) {
		c := Compound{
			Functor: "f",
			Args: []Interface{
				&Compound{Functor: "$VAR", Args: []Interface{Integer(1)}},
				&Compound{Functor: "$VAR", Args: []Interface{Integer(2)}},
				&Compound{Functor: "$VAR", Args: []Interface{Integer(3)}},
				&Compound{Functor: "$VAR", Args: []Interface{Integer(27)}},
			},
		}

		t.Run("false", func(t *testing.T) {
			var buf bytes.Buffer
			assert.NoError(t, c.WriteTerm(&buf, WriteTermOptions{NumberVars: false}, nil))
			assert.Equal(t, "f($VAR(1), $VAR(2), $VAR(3), $VAR(27))", buf.String())
		})

		t.Run("true", func(t *testing.T) {
			var buf bytes.Buffer
			assert.NoError(t, c.WriteTerm(&buf, WriteTermOptions{NumberVars: true}, nil))
			assert.Equal(t, "f(A, B, C, AA)", buf.String())
		})
	})
}

func TestSet(t *testing.T) {
	assert.Equal(t, List(), Set())
	assert.Equal(t, List(Atom("a")), Set(Atom("a")))
	assert.Equal(t, List(Atom("a")), Set(Atom("a"), Atom("a"), Atom("a")))
	assert.Equal(t, List(Atom("a"), Atom("b"), Atom("c")), Set(Atom("c"), Atom("b"), Atom("a")))
}
