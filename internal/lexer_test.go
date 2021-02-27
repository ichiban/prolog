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
		l := NewLexer(bufio.NewReader(strings.NewReader("p(X, Y), p(Y, X).")), map[rune]rune{})

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
		l := NewLexer(bufio.NewReader(strings.NewReader("[]")), map[rune]rune{})

		assert.Equal(t, Token{Kind: TokenAtom, Val: "[]"}, l.Next())
		assert.Equal(t, Token{Kind: TokenEOS}, l.Next())
		assert.Equal(t, Token{Kind: TokenEOS}, l.Next())
	})

	t.Run("list", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader("[a, b|c]")), map[rune]rune{})

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
		l := NewLexer(bufio.NewReader(strings.NewReader(",(x, y), p(x,,), q((,)).")), map[rune]rune{})

		assert.Equal(t, Token{Kind: TokenAtom, Val: ","}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "("}, l.Next())
		assert.Equal(t, Token{Kind: TokenAtom, Val: "x"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ","}, l.Next())
		assert.Equal(t, Token{Kind: TokenAtom, Val: "y"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ")"}, l.Next())
		assert.Equal(t, Token{Kind: TokenAtom, Val: ","}, l.Next())
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

	t.Run("single line comment", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader("% comment\n")), map[rune]rune{})
		assert.Equal(t, Token{Kind: TokenEOS}, l.Next())
		assert.Equal(t, Token{Kind: TokenEOS}, l.Next())
	})

	t.Run("multi line comment", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader("/* comment \n * also comment \n */")), map[rune]rune{})
		assert.Equal(t, Token{Kind: TokenEOS}, l.Next())
		assert.Equal(t, Token{Kind: TokenEOS}, l.Next())
	})

	t.Run("quoted atom", func(t *testing.T) {
		t.Run("no escape", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'abc'`)), map[rune]rune{})
			assert.Equal(t, Token{Kind: TokenAtom, Val: "abc"}, l.Next())
		})

		t.Run("double single quotes", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'dont''t panic'`)), map[rune]rune{})
			assert.Equal(t, Token{Kind: TokenAtom, Val: "dont't panic"}, l.Next())
		})

		t.Run("backslash at the very end of the line", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'this is \
an atom'`)), map[rune]rune{})
			assert.Equal(t, Token{Kind: TokenAtom, Val: "this is an atom"}, l.Next())
		})

		t.Run("alert", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\a'`)), map[rune]rune{})
			assert.Equal(t, Token{Kind: TokenAtom, Val: "\a"}, l.Next())
		})

		t.Run("backspace", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\b'`)), map[rune]rune{})
			assert.Equal(t, Token{Kind: TokenAtom, Val: "\b"}, l.Next())
		})

		t.Run("formfeed", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\f'`)), map[rune]rune{})
			assert.Equal(t, Token{Kind: TokenAtom, Val: "\f"}, l.Next())
		})

		t.Run("newline", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\n'`)), map[rune]rune{})
			assert.Equal(t, Token{Kind: TokenAtom, Val: "\n"}, l.Next())
		})

		t.Run("return", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\r'`)), map[rune]rune{})
			assert.Equal(t, Token{Kind: TokenAtom, Val: "\r"}, l.Next())
		})

		t.Run("tab", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\t'`)), map[rune]rune{})
			assert.Equal(t, Token{Kind: TokenAtom, Val: "\t"}, l.Next())
		})

		t.Run("vertical tab", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\v'`)), map[rune]rune{})
			assert.Equal(t, Token{Kind: TokenAtom, Val: "\v"}, l.Next())
		})

		t.Run("hex code", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\x23\'`)), map[rune]rune{})
			assert.Equal(t, Token{Kind: TokenAtom, Val: "#"}, l.Next())
		})

		t.Run("oct code", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\43\'`)), map[rune]rune{})
			assert.Equal(t, Token{Kind: TokenAtom, Val: "#"}, l.Next())
		})

		t.Run("backslash", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\\'`)), map[rune]rune{})
			assert.Equal(t, Token{Kind: TokenAtom, Val: `\`}, l.Next())
		})

		t.Run("single quote", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\''`)), map[rune]rune{})
			assert.Equal(t, Token{Kind: TokenAtom, Val: `'`}, l.Next())
		})

		t.Run("double quote", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\"'`)), map[rune]rune{})
			assert.Equal(t, Token{Kind: TokenAtom, Val: `"`}, l.Next())
		})

		t.Run("backquote", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader("'\\`'")), map[rune]rune{})
			assert.Equal(t, Token{Kind: TokenAtom, Val: "`"}, l.Next())
		})
	})

	t.Run("integer then period", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader("X is 1 + 2.")), map[rune]rune{})
		assert.Equal(t, Token{Kind: TokenVariable, Val: "X"}, l.Next())
		assert.Equal(t, Token{Kind: TokenAtom, Val: "is"}, l.Next())
		assert.Equal(t, Token{Kind: TokenInteger, Val: "1"}, l.Next())
		assert.Equal(t, Token{Kind: TokenAtom, Val: "+"}, l.Next())
		assert.Equal(t, Token{Kind: TokenInteger, Val: "2"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, l.Next())
	})

	t.Run("multiple terms", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader(`
foo(a).
foo(b).
foo(c).
`)), map[rune]rune{})
		assert.Equal(t, Token{Kind: TokenAtom, Val: "foo"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "("}, l.Next())
		assert.Equal(t, Token{Kind: TokenAtom, Val: "a"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ")"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, l.Next())

		assert.Equal(t, Token{Kind: TokenAtom, Val: "foo"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "("}, l.Next())
		assert.Equal(t, Token{Kind: TokenAtom, Val: "b"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ")"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, l.Next())

		assert.Equal(t, Token{Kind: TokenAtom, Val: "foo"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "("}, l.Next())
		assert.Equal(t, Token{Kind: TokenAtom, Val: "c"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ")"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, l.Next())
	})

	t.Run("char conversions", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader(`abc('abc').`)), map[rune]rune{
			'b': 'a',
		})
		assert.Equal(t, Token{Kind: TokenAtom, Val: "aac"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "("}, l.Next())
		assert.Equal(t, Token{Kind: TokenAtom, Val: "abc"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: ")"}, l.Next())
		assert.Equal(t, Token{Kind: TokenSeparator, Val: "."}, l.Next())
	})
}
