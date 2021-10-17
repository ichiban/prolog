package term

import (
	"testing"

	"github.com/ichiban/prolog/syntax"

	"github.com/stretchr/testify/assert"
)

func TestFloat_Unparse(t *testing.T) {
	t.Run("positive", func(t *testing.T) {
		var tokens []syntax.Token
		Float(33.0).Unparse(func(token syntax.Token) {
			tokens = append(tokens, token)
		}, WriteTermOptions{}, nil)
		assert.Equal(t, []syntax.Token{
			{Kind: syntax.TokenFloat, Val: "33.0"},
		}, tokens)
	})

	t.Run("negative", func(t *testing.T) {
		var tokens []syntax.Token
		Float(-33.0).Unparse(func(token syntax.Token) {
			tokens = append(tokens, token)
		}, WriteTermOptions{}, nil)
		assert.Equal(t, []syntax.Token{
			{Kind: syntax.TokenSign, Val: "-"},
			{Kind: syntax.TokenFloat, Val: "33.0"},
		}, tokens)
	})
}
