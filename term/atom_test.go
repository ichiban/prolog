package term

import (
	"bytes"
	"testing"

	"github.com/stretchr/testify/assert"
)

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
			assert.NoError(t, Atom("a").WriteTerm(&buf, WriteTermOptions{Quoted: false}, nil))
			assert.Equal(t, `a`, buf.String())
		})

		t.Run("need to quote", func(t *testing.T) {
			var buf bytes.Buffer
			assert.NoError(t, Atom("\a\b\f\n\r\t\v\x00\\'\"`").WriteTerm(&buf, WriteTermOptions{Quoted: false}, nil))
			assert.Equal(t, "\a\b\f\n\r\t\v\x00\\'\"`", buf.String())
		})
	})

	t.Run("quoted", func(t *testing.T) {
		t.Run("no need to quote", func(t *testing.T) {
			var buf bytes.Buffer
			assert.NoError(t, Atom("a").WriteTerm(&buf, WriteTermOptions{Quoted: true}, nil))
			assert.Equal(t, `a`, buf.String())
		})

		t.Run("need to quote", func(t *testing.T) {
			var buf bytes.Buffer
			assert.NoError(t, Atom("\a\b\f\n\r\t\v\x00\\'\"`").WriteTerm(&buf, WriteTermOptions{Quoted: true}, nil))
			assert.Equal(t, "'\\a\\b\\f\\n\\r\\t\\v\\x0\\\\\\\\'\\\"\\`'", buf.String())
		})
	})
}

func TestAtom_Unify(t *testing.T) {
	unit := Atom("foo")

	t.Run("atom", func(t *testing.T) {
		assert.True(t, unit.Unify(Atom("foo"), false, nil))
		assert.False(t, unit.Unify(Atom("bar"), false, nil))
	})

	t.Run("integer", func(t *testing.T) {
		assert.False(t, unit.Unify(Integer(1), false, nil))
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
			assert.Equal(t, unit, env.Resolve(v))
		})
		t.Run("bound to a different value", func(t *testing.T) {
			v := Variable("X")
			env := Env{
				{
					Variable: v,
					Value:    Atom("bar"),
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
