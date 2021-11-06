package term

import (
	"testing"

	"github.com/ichiban/prolog/syntax"

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
			Args:    []Interface{Atom("foo")},
		}, false, nil)
		assert.False(t, ok)
		assert.Nil(t, env)
	})
}

func TestAtom_Unparse(t *testing.T) {
	t.Run("not quoted", func(t *testing.T) {
		t.Run("no need to quote", func(t *testing.T) {
			var tokens []syntax.Token
			Atom("a").Unparse(func(token syntax.Token) {
				tokens = append(tokens, token)
			}, WriteTermOptions{Quoted: false}, nil)
			assert.Equal(t, []syntax.Token{
				{Kind: syntax.TokenIdent, Val: `a`},
			}, tokens)
		})

		t.Run("need to quote", func(t *testing.T) {
			var tokens []syntax.Token
			Atom("\a\b\f\n\r\t\v\x00\\'\"`").Unparse(func(token syntax.Token) {
				tokens = append(tokens, token)
			}, WriteTermOptions{Quoted: false}, nil)
			assert.Equal(t, []syntax.Token{
				{Kind: syntax.TokenIdent, Val: "\a\b\f\n\r\t\v\x00\\'\"`"},
			}, tokens)
		})
	})

	t.Run("quoted", func(t *testing.T) {
		t.Run("no need to quote", func(t *testing.T) {
			var tokens []syntax.Token
			Atom("a").Unparse(func(token syntax.Token) {
				tokens = append(tokens, token)
			}, WriteTermOptions{Quoted: true}, nil)
			assert.Equal(t, []syntax.Token{
				{Kind: syntax.TokenIdent, Val: `a`},
			}, tokens)
		})

		t.Run("need to quote", func(t *testing.T) {
			var tokens []syntax.Token
			Atom("\a\b\f\n\r\t\v\x00\\'\"`").Unparse(func(token syntax.Token) {
				tokens = append(tokens, token)
			}, WriteTermOptions{Quoted: true}, nil)
			assert.Equal(t, []syntax.Token{
				{Kind: syntax.TokenQuotedIdent, Val: "'\\a\\b\\f\\n\\r\\t\\v\\x0\\\\\\\\'\\\"\\`'"},
			}, tokens)
		})
	})

	t.Run("comma", func(t *testing.T) {
		var tokens []syntax.Token
		Atom(",").Unparse(func(token syntax.Token) {
			tokens = append(tokens, token)
		}, WriteTermOptions{Quoted: true}, nil)
		assert.Equal(t, []syntax.Token{
			{Kind: syntax.TokenComma, Val: ","},
		}, tokens)
	})

	t.Run("nil", func(t *testing.T) {
		var tokens []syntax.Token
		Atom("[]").Unparse(func(token syntax.Token) {
			tokens = append(tokens, token)
		}, WriteTermOptions{Quoted: true}, nil)
		assert.Equal(t, []syntax.Token{
			{Kind: syntax.TokenIdent, Val: "[]"},
		}, tokens)
	})

	t.Run("empty block", func(t *testing.T) {
		var tokens []syntax.Token
		Atom("{}").Unparse(func(token syntax.Token) {
			tokens = append(tokens, token)
		}, WriteTermOptions{Quoted: true}, nil)
		assert.Equal(t, []syntax.Token{
			{Kind: syntax.TokenIdent, Val: "{}"},
		}, tokens)
	})
}
