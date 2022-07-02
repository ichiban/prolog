package engine

import (
	"bytes"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestAtom_Unify(t *testing.T) {
	unit := Atom("foo")

	t.Run("atom", func(t *testing.T) {
		env, ok := unit.Unify(Atom("foo"), false, nil)
		assert.True(t, ok)
		assert.Nil(t, env)

		env, ok = unit.Unify(Atom("bar"), false, env)
		assert.False(t, ok)
		assert.Nil(t, env)
	})

	t.Run("integer", func(t *testing.T) {
		env, ok := unit.Unify(Integer(1), false, nil)
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
			env, ok := unit.Unify(v, false, env)
			assert.True(t, ok)
			assert.Equal(t, unit, env.Resolve(v))
		})
		t.Run("bound to a different value", func(t *testing.T) {
			v := Variable("X")
			env := NewEnv().
				Bind(v, Atom("bar"))
			_, ok := unit.Unify(v, false, env)
			assert.False(t, ok)
		})
	})

	t.Run("compound", func(t *testing.T) {
		env, ok := unit.Unify(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("foo")},
		}, false, nil)
		assert.False(t, ok)
		assert.Nil(t, env)
	})
}

func TestAtom_WriteTerm(t *testing.T) {
	ops := operators{
		{name: "+"},
		{name: "-"},
	}

	tests := []struct {
		atom          Atom
		quoted        bool
		before, after operator
		output        string
	}{
		{atom: `a`, quoted: false, output: `a`},
		{atom: `a`, quoted: true, output: `a`},
		{atom: "\a\b\f\n\r\t\v\x00\\'\"`", quoted: false, output: "\a\b\f\n\r\t\v\x00\\'\"`"},
		{atom: "\a\b\f\n\r\t\v\x00\\'\"`", quoted: true, output: "'\\a\\b\\f\\n\\r\\t\\v\\x0\\\\\\\\'\\\"\\`'"},
		{atom: `,`, quoted: false, output: `,`},
		{atom: `,`, quoted: true, output: `','`},
		{atom: `[]`, quoted: false, output: `[]`},
		{atom: `[]`, quoted: true, output: `[]`},
		{atom: `{}`, quoted: false, output: `{}`},
		{atom: `{}`, quoted: true, output: `{}`},

		{atom: `-`, output: `-`},
		{atom: `-`, before: operator{name: "+"}, output: `(-)`},
		{atom: `-`, after: operator{name: "+"}, output: `(-)`},
	}

	var buf bytes.Buffer
	for _, tt := range tests {
		t.Run(string(tt.atom), func(t *testing.T) {
			buf.Reset()
			assert.NoError(t, tt.atom.WriteTerm(&buf, &WriteOptions{
				Quoted: tt.quoted,
				ops:    ops,
				before: tt.before,
				after:  tt.after,
			}, nil))
			assert.Equal(t, tt.output, buf.String())
		})
	}
}

func TestAtom_Compare(t *testing.T) {
	var m mockTerm
	defer m.AssertExpectations(t)

	assert.Equal(t, int64(-1), Atom("a").Compare(&m, nil))
	assert.Equal(t, int64(-1), Atom("a").Compare(Atom("b"), nil))
	assert.Equal(t, int64(0), Atom("a").Compare(Atom("a"), nil))
	assert.Equal(t, int64(1), Atom("b").Compare(Atom("a"), nil))
	assert.Equal(t, int64(1), Atom("a").Compare(Variable("X"), nil))
	assert.Equal(t, int64(1), Atom("a").Compare(Float(0), nil))
	assert.Equal(t, int64(1), Atom("a").Compare(Integer(0), nil))
}
