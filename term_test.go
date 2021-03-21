package prolog

import (
	"bytes"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestVariable_Unify(t *testing.T) {
	var v1, v2 Variable
	assert.True(t, v1.Unify(&v2, false))
	assert.True(t, v1.Unify(Atom("foo"), false))
	assert.Equal(t, &Variable{Ref: Atom("foo")}, v1.Ref)
	assert.Equal(t, &Variable{Ref: Atom("foo")}, v2.Ref)

	var v3, v4 Variable
	assert.True(t, v3.Unify(&v4, false))
	assert.True(t, v4.Unify(Atom("bar"), false))
	assert.Equal(t, &Variable{Ref: Atom("bar")}, v3.Ref)
	assert.Equal(t, &Variable{Ref: Atom("bar")}, v4.Ref)
}

func TestVariable_WriteTerm(t *testing.T) {
	t.Run("named", func(t *testing.T) {
		v := Variable{Name: "X", Ref: Integer(1)}
		var buf bytes.Buffer
		assert.NoError(t, v.WriteTerm(&buf, WriteTermOptions{}))
		assert.Equal(t, "X", buf.String())
	})

	t.Run("unnamed", func(t *testing.T) {
		v := Variable{Ref: Integer(1)}
		var buf bytes.Buffer
		assert.NoError(t, v.WriteTerm(&buf, WriteTermOptions{}))
		assert.Regexp(t, `\A_0x[[:xdigit:]]+\z`, buf.String())
	})
}

func TestAtom_String(t *testing.T) {
	assert.Equal(t, `abc`, Atom("abc").String())
	assert.Equal(t, `'\a'`, Atom("\a").String())
	assert.Equal(t, `'\b'`, Atom("\b").String())
	assert.Equal(t, `'\f'`, Atom("\f").String())
	assert.Equal(t, `'\n'`, Atom("\n").String())
	assert.Equal(t, `'\r'`, Atom("\r").String())
	assert.Equal(t, `'\t'`, Atom("\t").String())
	assert.Equal(t, `'\v'`, Atom("\v").String())
	assert.Equal(t, `'\x0\'`, Atom("\x00").String())
	assert.Equal(t, `'\\\a'`, Atom("\\\a").String()) // '\' by itself doesn't require quotation.
	assert.Equal(t, `'\''`, Atom("'").String())
	assert.Equal(t, `'\"'`, Atom("\"").String())
	assert.Equal(t, "'\\`'", Atom("`").String())
}

func TestAtom_WriteTerm(t *testing.T) {
	t.Run("not quoted", func(t *testing.T) {
		t.Run("no need to quote", func(t *testing.T) {
			var buf bytes.Buffer
			assert.NoError(t, Atom("a").WriteTerm(&buf, WriteTermOptions{Quoted: false}))
			assert.Equal(t, `a`, buf.String())
		})

		t.Run("need to quote", func(t *testing.T) {
			var buf bytes.Buffer
			assert.NoError(t, Atom("\a\b\f\n\r\t\v\x00\\'\"`").WriteTerm(&buf, WriteTermOptions{Quoted: false}))
			assert.Equal(t, "\a\b\f\n\r\t\v\x00\\'\"`", buf.String())
		})
	})

	t.Run("quoted", func(t *testing.T) {
		t.Run("no need to quote", func(t *testing.T) {
			var buf bytes.Buffer
			assert.NoError(t, Atom("a").WriteTerm(&buf, WriteTermOptions{Quoted: true}))
			assert.Equal(t, `a`, buf.String())
		})

		t.Run("need to quote", func(t *testing.T) {
			var buf bytes.Buffer
			assert.NoError(t, Atom("\a\b\f\n\r\t\v\x00\\'\"`").WriteTerm(&buf, WriteTermOptions{Quoted: true}))
			assert.Equal(t, "'\\a\\b\\f\\n\\r\\t\\v\\x0\\\\\\\\'\\\"\\`'", buf.String())
		})
	})
}

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
		assert.Equal(t, "append/3", c.String())
	})

	t.Run("parenthesises", func(t *testing.T) {
		c := Compound{
			Functor: "/",
			Args:    []Term{Atom("=="), Integer(2)},
		}
		assert.Equal(t, "(==)/2", c.String())
	})
}

func TestCompound_WriteTerm(t *testing.T) {
	t.Run("ignore_ops", func(t *testing.T) {
		c := Compound{
			Functor: "+",
			Args: []Term{
				Integer(2),
				&Compound{
					Functor: "-",
					Args:    []Term{Integer(2)},
				},
			},
		}

		t.Run("false", func(t *testing.T) {
			ops := Operators{
				{Priority: 500, Specifier: "yfx", Name: "+"},
				{Priority: 200, Specifier: "fy", Name: "-"},
			}

			var buf bytes.Buffer
			assert.NoError(t, c.WriteTerm(&buf, WriteTermOptions{Ops: ops}))
			assert.Equal(t, "2+(-2)", buf.String())
		})

		t.Run("true", func(t *testing.T) {
			var buf bytes.Buffer
			assert.NoError(t, c.WriteTerm(&buf, WriteTermOptions{Ops: nil}))
			assert.Equal(t, "+(2, -(2))", buf.String())
		})
	})

	t.Run("numbervars", func(t *testing.T) {
		c := Compound{
			Functor: "f",
			Args: []Term{
				&Compound{Functor: "$VAR", Args: []Term{Integer(1)}},
				&Compound{Functor: "$VAR", Args: []Term{Integer(2)}},
				&Compound{Functor: "$VAR", Args: []Term{Integer(3)}},
				&Compound{Functor: "$VAR", Args: []Term{Integer(27)}},
			},
		}

		t.Run("false", func(t *testing.T) {
			var buf bytes.Buffer
			assert.NoError(t, c.WriteTerm(&buf, WriteTermOptions{NumberVars: false}))
			assert.Equal(t, "f($VAR(1), $VAR(2), $VAR(3), $VAR(27))", buf.String())
		})

		t.Run("true", func(t *testing.T) {
			var buf bytes.Buffer
			assert.NoError(t, c.WriteTerm(&buf, WriteTermOptions{NumberVars: true}))
			assert.Equal(t, "f(A, B, C, AA)", buf.String())
		})
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

func TestRulify(t *testing.T) {
	assert.Equal(t, &Compound{
		Functor: ":-",
		Args:    []Term{Atom("a"), Atom("true")},
	}, Rulify(Atom("a")))
	assert.Equal(t, &Compound{
		Functor: ":-",
		Args:    []Term{Atom("a"), Atom("true")},
	}, Rulify(&Variable{Ref: Atom("a")}))
	assert.Equal(t, &Compound{
		Functor: ":-",
		Args:    []Term{Atom("a"), Atom("b")},
	}, Rulify(&Compound{
		Functor: ":-",
		Args:    []Term{Atom("a"), Atom("b")},
	}))
}

func TestSet(t *testing.T) {
	assert.Equal(t, List(), Set())
	assert.Equal(t, List(Atom("a")), Set(Atom("a")))
	assert.Equal(t, List(Atom("a")), Set(Atom("a"), Atom("a"), Atom("a")))
	assert.Equal(t, List(Atom("a"), Atom("b"), Atom("c")), Set(Atom("c"), Atom("b"), Atom("a")))
}
