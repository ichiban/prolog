package prolog

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestLexer_Next(t *testing.T) {
	t.Run("clause", func(t *testing.T) {
		l := NewLexer("append(nil,L,L).")

		assert.Equal(t, Token{Kind: TokenAtom, Val: "append"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "("}, l.Next())
		assert.Equal(t, Token{Kind: TokenAtom, Val: "nil"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ","}, l.Next())
		assert.Equal(t, Token{Kind: TokenVariable, Val: "L"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ","}, l.Next())
		assert.Equal(t, Token{Kind: TokenVariable, Val: "L"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ")"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, l.Next())
		assert.Equal(t, Token{Kind: TokenEOS}, l.Next())
		assert.Equal(t, Token{Kind: TokenEOS}, l.Next())
	})

	t.Run("conjunction", func(t *testing.T) {
		l := NewLexer("p(X, Y), p(Y, X).")

		assert.Equal(t, Token{Kind: TokenAtom, Val: "p"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "("}, l.Next())
		assert.Equal(t, Token{Kind: TokenVariable, Val: "X"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ","}, l.Next())
		assert.Equal(t, Token{Kind: TokenVariable, Val: "Y"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ")"}, l.Next())
		assert.Equal(t, Token{Kind: TokenAtom, Val: ","}, l.Next()) // operator
		assert.Equal(t, Token{Kind: TokenAtom, Val: "p"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "("}, l.Next())
		assert.Equal(t, Token{Kind: TokenVariable, Val: "Y"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ","}, l.Next())
		assert.Equal(t, Token{Kind: TokenVariable, Val: "X"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ")"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, l.Next())
		assert.Equal(t, Token{Kind: TokenEOS}, l.Next())
		assert.Equal(t, Token{Kind: TokenEOS}, l.Next())
	})

	t.Run("nil", func(t *testing.T) {
		l := NewLexer("[]")

		assert.Equal(t, Token{Kind: TokenAtom, Val: "[]"}, l.Next())
		assert.Equal(t, Token{Kind: TokenEOS}, l.Next())
		assert.Equal(t, Token{Kind: TokenEOS}, l.Next())
	})

	t.Run("list", func(t *testing.T) {
		l := NewLexer("[a, b|c]")

		assert.Equal(t, Token{Kind: TokenSeparator, Val: "["}, l.Next())
		assert.Equal(t, Token{Kind: TokenAtom, Val: "a"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ","}, l.Next())
		assert.Equal(t, Token{Kind: TokenAtom, Val: "b"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "|"}, l.Next())
		assert.Equal(t, Token{Kind: TokenAtom, Val: "c"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "]"}, l.Next())
		assert.Equal(t, Token{Kind: TokenEOS}, l.Next())
		assert.Equal(t, Token{Kind: TokenEOS}, l.Next())
	})

	t.Run("comma", func(t *testing.T) {
		l := NewLexer("p(x,,), q((,)).")

		assert.Equal(t, Token{Kind: TokenAtom, Val: "p"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "("}, l.Next())
		assert.Equal(t, Token{Kind: TokenAtom, Val: "x"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ","}, l.Next())
		assert.Equal(t, Token{Kind: TokenAtom, Val: ","}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ")"}, l.Next())
		assert.Equal(t, Token{Kind: TokenAtom, Val: ","}, l.Next())
		assert.Equal(t, Token{Kind: TokenAtom, Val: "q"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "("}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "("}, l.Next())
		assert.Equal(t, Token{Kind: TokenAtom, Val: ","}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ")"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ")"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, l.Next())
		assert.Equal(t, Token{Kind: TokenEOS}, l.Next())
		assert.Equal(t, Token{Kind: TokenEOS}, l.Next())
	})
}
