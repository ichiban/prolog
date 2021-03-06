package internal

import (
	"bufio"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestLexer_Next(t *testing.T) {
	t.Run("clause", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader("append(nil,L,L).")), map[rune]rune{})

		token, err := l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "append"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "("}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "nil"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenVariable, Val: "L"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenVariable, Val: "L"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ")"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenEOS}, token)
	})

	t.Run("conjunction", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader("p(X, Y), p(Y, X).")), map[rune]rune{})

		token, err := l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "p"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "("}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenVariable, Val: "X"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenVariable, Val: "Y"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ")"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: ","}, token) // operator

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "p"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "("}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenVariable, Val: "Y"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenVariable, Val: "X"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ")"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenEOS}, token)
	})

	t.Run("nil", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader("[]")), map[rune]rune{})

		token, err := l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "[]"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenEOS}, token)
	})

	t.Run("list", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader("[a, b|c]")), map[rune]rune{})

		token, err := l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "["}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "a"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "b"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "|"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "c"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "]"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenEOS}, token)
	})

	t.Run("comma", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader(",(x, y), p(x,,), q((,)).")), map[rune]rune{})

		token, err := l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "("}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "x"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "y"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ")"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "p"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "("}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "x"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ")"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "q"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "("}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "("}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ")"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ")"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenEOS}, token)
	})

	t.Run("single line comment", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader("% comment\nfoo.")), map[rune]rune{})

		token, err := l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "foo"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenEOS}, token)
	})

	t.Run("multi line comment", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader("/* comment \n * also comment \n */foo.")), map[rune]rune{})

		token, err := l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "foo"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenEOS}, token)
	})

	t.Run("quoted atom", func(t *testing.T) {
		t.Run("no escape", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'abc'.`)), map[rune]rune{})

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "abc"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("double single quotes", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'dont''t panic'.`)), map[rune]rune{})

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "dont't panic"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("backslash at the very end of the line", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'this is \
an atom'.`)), map[rune]rune{})

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "this is an atom"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("alert", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\a'.`)), map[rune]rune{})

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "\a"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("backspace", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\b'.`)), map[rune]rune{})

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "\b"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("formfeed", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\f'.`)), map[rune]rune{})

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "\f"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("newline", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\n'.`)), map[rune]rune{})

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "\n"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("return", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\r'.`)), map[rune]rune{})

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "\r"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("tab", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\t'.`)), map[rune]rune{})

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "\t"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("vertical tab", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\v'.`)), map[rune]rune{})

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "\v"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("hex code", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\x23\'.`)), map[rune]rune{})

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "#"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("oct code", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\43\'.`)), map[rune]rune{})

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "#"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("backslash", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\\'.`)), map[rune]rune{})

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: `\`}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("single quote", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\''.`)), map[rune]rune{})

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: `'`}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("double quote", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\"'.`)), map[rune]rune{})

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: `"`}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("backquote", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader("'\\`'.")), map[rune]rune{})

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "`"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})
	})

	t.Run("integer then period", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader("X is 1 + 2.")), map[rune]rune{})

		token, err := l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenVariable, Val: "X"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "is"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenInteger, Val: "1"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "+"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenInteger, Val: "2"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenEOS}, token)
	})

	t.Run("multiple terms", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader(`
foo(a).
foo(b).
foo(c).
`)), map[rune]rune{})

		token, err := l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "foo"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "("}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "a"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ")"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "foo"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "("}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "b"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ")"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "foo"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "("}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "c"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ")"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)
	})

	t.Run("char conversions", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader(`abc('abc').`)), map[rune]rune{
			'b': 'a',
		})

		token, err := l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "aac"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "("}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "abc"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ")"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, token)
	})
}
