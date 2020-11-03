package prolog

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestLexer_Next(t *testing.T) {
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
}
