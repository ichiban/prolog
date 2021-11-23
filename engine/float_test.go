package engine

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFloat_Unparse(t *testing.T) {
	t.Run("positive", func(t *testing.T) {
		var tokens []Token
		Float(33.0).Unparse(func(token Token) {
			tokens = append(tokens, token)
		}, WriteTermOptions{}, nil)
		assert.Equal(t, []Token{
			{Kind: TokenFloat, Val: "33.0"},
		}, tokens)
	})

	t.Run("negative", func(t *testing.T) {
		var tokens []Token
		Float(-33.0).Unparse(func(token Token) {
			tokens = append(tokens, token)
		}, WriteTermOptions{}, nil)
		assert.Equal(t, []Token{
			{Kind: TokenSign, Val: "-"},
			{Kind: TokenFloat, Val: "33.0"},
		}, tokens)
	})
}
