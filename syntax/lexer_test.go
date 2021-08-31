package syntax

import (
	"bufio"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestLexer_Next(t *testing.T) {
	t.Run("clause", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader("append(nil,L,L).")), nil)

		token, err := l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "append"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenL, Val: "("}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "nil"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenComma, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenVariable, Val: "L"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenComma, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenVariable, Val: "L"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenR, Val: ")"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenEOS}, token)
	})

	t.Run("conjunction", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader("p(X, Y), p(Y, X).")), nil)

		token, err := l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "p"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenL, Val: "("}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenVariable, Val: "X"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenComma, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenVariable, Val: "Y"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenR, Val: ")"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenComma, Val: ","}, token) // operator

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "p"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenL, Val: "("}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenVariable, Val: "Y"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenComma, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenVariable, Val: "X"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenR, Val: ")"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenEOS}, token)
	})

	t.Run("nil", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader("[]")), nil)

		token, err := l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "[]"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenEOS}, token)
	})

	t.Run("list", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader("[a, b|c]")), nil)

		token, err := l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenBracketL, Val: "["}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "a"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenComma, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "b"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenBar, Val: "|"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "c"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenBracketR, Val: "]"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenEOS}, token)
	})

	t.Run("comma", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader(",(x, y), p(x,,), q((,)).")), nil)

		token, err := l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenComma, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenL, Val: "("}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "x"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenComma, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "y"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenR, Val: ")"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenComma, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "p"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenL, Val: "("}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "x"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenComma, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenComma, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenR, Val: ")"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenComma, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "q"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenL, Val: "("}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenL, Val: "("}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenComma, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenR, Val: ")"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenR, Val: ")"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenEOS}, token)
	})

	t.Run("single line comment", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader("% comment\nfoo.")), nil)

		token, err := l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "foo"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenEOS}, token)
	})

	t.Run("multi line comment", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader("/* comment \n * also comment \n */foo.")), nil)

		token, err := l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "foo"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenEOS}, token)
	})

	t.Run("quoted atom", func(t *testing.T) {
		t.Run("no escape", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'abc'.`)), nil)

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "abc"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("double single quotes", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'dont''t panic'.`)), nil)

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "dont't panic"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("backslash at the very end of the line", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'this is \
an atom'.`)), nil)

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "this is an atom"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("alert", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\a'.`)), nil)

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "\a"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("backspace", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\b'.`)), nil)

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "\b"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("formfeed", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\f'.`)), nil)

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "\f"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("newline", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\n'.`)), nil)

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "\n"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("return", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\r'.`)), nil)

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "\r"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("tab", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\t'.`)), nil)

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "\t"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("vertical tab", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\v'.`)), nil)

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "\v"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("hex code", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\x23\'.`)), nil)

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "#"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("oct code", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\43\'.`)), nil)

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "#"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("backslash", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\\'.`)), nil)

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: `\`}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("single quote", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\''.`)), nil)

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: `'`}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("double quote", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`'\"'.`)), nil)

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: `"`}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})

		t.Run("backquote", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader("'\\`'.")), nil)

			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenAtom, Val: "`"}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)

			token, err = l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenEOS}, token)
		})
	})

	t.Run("integer", func(t *testing.T) {
		t.Run("decimal", func(t *testing.T) {
			t.Run("no sign", func(t *testing.T) {
				l := NewLexer(bufio.NewReader(strings.NewReader(`012345`)), nil)
				token, err := l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenInteger, Val: "012345"}, token)
			})

			t.Run("plus", func(t *testing.T) {
				l := NewLexer(bufio.NewReader(strings.NewReader(`+012345`)), nil)
				token, err := l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenSign, Val: "+"}, token)
				token, err = l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenInteger, Val: "012345"}, token)
			})

			t.Run("minus", func(t *testing.T) {
				l := NewLexer(bufio.NewReader(strings.NewReader(`-012345`)), nil)
				token, err := l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenSign, Val: "-"}, token)
				token, err = l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenInteger, Val: "012345"}, token)
			})
		})

		t.Run("octal", func(t *testing.T) {
			t.Run("no sign", func(t *testing.T) {
				l := NewLexer(bufio.NewReader(strings.NewReader(`0o567`)), nil)
				token, err := l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenInteger, Val: "0o567"}, token)
			})

			t.Run("plus", func(t *testing.T) {
				l := NewLexer(bufio.NewReader(strings.NewReader(`+0o567`)), nil)
				token, err := l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenSign, Val: "+"}, token)
				token, err = l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenInteger, Val: "0o567"}, token)
			})

			t.Run("minus", func(t *testing.T) {
				l := NewLexer(bufio.NewReader(strings.NewReader(`-0o567`)), nil)
				token, err := l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenSign, Val: "-"}, token)
				token, err = l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenInteger, Val: "0o567"}, token)
			})
		})

		t.Run("hexadecimal", func(t *testing.T) {
			t.Run("no sign", func(t *testing.T) {
				l := NewLexer(bufio.NewReader(strings.NewReader(`0x89ABC`)), nil)
				token, err := l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenInteger, Val: "0x89ABC"}, token)
			})

			t.Run("plus", func(t *testing.T) {
				l := NewLexer(bufio.NewReader(strings.NewReader(`+0x89ABC`)), nil)
				token, err := l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenSign, Val: "+"}, token)
				token, err = l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenInteger, Val: "0x89ABC"}, token)
			})

			t.Run("minus", func(t *testing.T) {
				l := NewLexer(bufio.NewReader(strings.NewReader(`-0x89ABC`)), nil)
				token, err := l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenSign, Val: "-"}, token)
				token, err = l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenInteger, Val: "0x89ABC"}, token)
			})
		})

		t.Run("binary", func(t *testing.T) {
			t.Run("no sign", func(t *testing.T) {
				l := NewLexer(bufio.NewReader(strings.NewReader(`0b10110101`)), nil)
				token, err := l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenInteger, Val: "0b10110101"}, token)
			})

			t.Run("plus", func(t *testing.T) {
				l := NewLexer(bufio.NewReader(strings.NewReader(`+0b10110101`)), nil)
				token, err := l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenSign, Val: "+"}, token)
				token, err = l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenInteger, Val: "0b10110101"}, token)
			})

			t.Run("minus", func(t *testing.T) {
				l := NewLexer(bufio.NewReader(strings.NewReader(`-0b10110101`)), nil)
				token, err := l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenSign, Val: "-"}, token)
				token, err = l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenInteger, Val: "0b10110101"}, token)
			})
		})

		t.Run("character", func(t *testing.T) {
			t.Run("normal", func(t *testing.T) {
				t.Run("no sign", func(t *testing.T) {
					l := NewLexer(bufio.NewReader(strings.NewReader(`0'a`)), nil)
					token, err := l.Next()
					assert.NoError(t, err)
					assert.Equal(t, Token{Kind: TokenInteger, Val: "0'a"}, token)
				})

				t.Run("plus", func(t *testing.T) {
					l := NewLexer(bufio.NewReader(strings.NewReader(`+0'a`)), nil)
					token, err := l.Next()
					assert.NoError(t, err)
					assert.Equal(t, Token{Kind: TokenSign, Val: "+"}, token)
					token, err = l.Next()
					assert.NoError(t, err)
					assert.Equal(t, Token{Kind: TokenInteger, Val: "0'a"}, token)
				})

				t.Run("minus", func(t *testing.T) {
					l := NewLexer(bufio.NewReader(strings.NewReader(`-0'a`)), nil)
					token, err := l.Next()
					assert.NoError(t, err)
					assert.Equal(t, Token{Kind: TokenSign, Val: "-"}, token)
					token, err = l.Next()
					assert.NoError(t, err)
					assert.Equal(t, Token{Kind: TokenInteger, Val: "0'a"}, token)
				})
			})

			t.Run("quote", func(t *testing.T) {
				t.Run("no sign", func(t *testing.T) {
					l := NewLexer(bufio.NewReader(strings.NewReader(`0''`)), nil)
					token, err := l.Next()
					assert.NoError(t, err)
					assert.Equal(t, Token{Kind: TokenInteger, Val: "0''"}, token)
				})

				t.Run("plus", func(t *testing.T) {
					l := NewLexer(bufio.NewReader(strings.NewReader(`+0''`)), nil)
					token, err := l.Next()
					assert.NoError(t, err)
					assert.Equal(t, Token{Kind: TokenSign, Val: "+"}, token)
					token, err = l.Next()
					assert.NoError(t, err)
					assert.Equal(t, Token{Kind: TokenInteger, Val: "0''"}, token)
				})

				t.Run("minus", func(t *testing.T) {
					l := NewLexer(bufio.NewReader(strings.NewReader(`-0''`)), nil)
					token, err := l.Next()
					assert.NoError(t, err)
					assert.Equal(t, Token{Kind: TokenSign, Val: "-"}, token)
					token, err = l.Next()
					assert.NoError(t, err)
					assert.Equal(t, Token{Kind: TokenInteger, Val: "0''"}, token)
				})
			})

			t.Run("escape sequence", func(t *testing.T) {
				t.Run("no sign", func(t *testing.T) {
					l := NewLexer(bufio.NewReader(strings.NewReader(`0'\n`)), nil)
					token, err := l.Next()
					assert.NoError(t, err)
					assert.Equal(t, Token{Kind: TokenInteger, Val: "0'\n"}, token)
				})

				t.Run("plus", func(t *testing.T) {
					l := NewLexer(bufio.NewReader(strings.NewReader(`+0'\n`)), nil)
					token, err := l.Next()
					assert.NoError(t, err)
					assert.Equal(t, Token{Kind: TokenSign, Val: "+"}, token)
					token, err = l.Next()
					assert.NoError(t, err)
					assert.Equal(t, Token{Kind: TokenInteger, Val: "0'\n"}, token)
				})

				t.Run("minus", func(t *testing.T) {
					l := NewLexer(bufio.NewReader(strings.NewReader(`-0'\n`)), nil)
					token, err := l.Next()
					assert.NoError(t, err)
					assert.Equal(t, Token{Kind: TokenSign, Val: "-"}, token)
					token, err = l.Next()
					assert.NoError(t, err)
					assert.Equal(t, Token{Kind: TokenInteger, Val: "0'\n"}, token)
				})
			})
		})

		t.Run("misc", func(t *testing.T) {
			t.Run("just 0", func(t *testing.T) {
				l := NewLexer(bufio.NewReader(strings.NewReader(`0`)), nil)
				token, err := l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenInteger, Val: "0"}, token)
			})
		})
	})

	t.Run("float", func(t *testing.T) {
		t.Run("without e", func(t *testing.T) {
			t.Run("no sign", func(t *testing.T) {
				l := NewLexer(bufio.NewReader(strings.NewReader(`2.34`)), nil)
				token, err := l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenFloat, Val: "2.34"}, token)
			})

			t.Run("plus", func(t *testing.T) {
				l := NewLexer(bufio.NewReader(strings.NewReader(`+2.34`)), nil)
				token, err := l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenSign, Val: "+"}, token)
				token, err = l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenFloat, Val: "2.34"}, token)
			})

			t.Run("minus", func(t *testing.T) {
				l := NewLexer(bufio.NewReader(strings.NewReader(`-2.34`)), nil)
				token, err := l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenSign, Val: "-"}, token)
				token, err = l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenFloat, Val: "2.34"}, token)
			})
		})

		t.Run("with e and no sign", func(t *testing.T) {
			t.Run("no sign", func(t *testing.T) {
				l := NewLexer(bufio.NewReader(strings.NewReader(`2.34E5`)), nil)
				token, err := l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenFloat, Val: "2.34E5"}, token)
			})

			t.Run("plus", func(t *testing.T) {
				l := NewLexer(bufio.NewReader(strings.NewReader(`+2.34E5`)), nil)
				token, err := l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenSign, Val: "+"}, token)
				token, err = l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenFloat, Val: "2.34E5"}, token)
			})

			t.Run("minus", func(t *testing.T) {
				l := NewLexer(bufio.NewReader(strings.NewReader(`-2.34E5`)), nil)
				token, err := l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenSign, Val: "-"}, token)
				token, err = l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenFloat, Val: "2.34E5"}, token)
			})
		})

		t.Run("with e and plus", func(t *testing.T) {
			t.Run("no sign", func(t *testing.T) {
				l := NewLexer(bufio.NewReader(strings.NewReader(`2.34E+5`)), nil)
				token, err := l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenFloat, Val: "2.34E+5"}, token)
			})

			t.Run("plus", func(t *testing.T) {
				l := NewLexer(bufio.NewReader(strings.NewReader(`+2.34E+5`)), nil)
				token, err := l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenSign, Val: "+"}, token)
				token, err = l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenFloat, Val: "2.34E+5"}, token)
			})

			t.Run("minus", func(t *testing.T) {
				l := NewLexer(bufio.NewReader(strings.NewReader(`-2.34E+5`)), nil)
				token, err := l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenSign, Val: "-"}, token)
				token, err = l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenFloat, Val: "2.34E+5"}, token)
			})
		})

		t.Run("with e and minus", func(t *testing.T) {
			t.Run("no sign", func(t *testing.T) {
				l := NewLexer(bufio.NewReader(strings.NewReader(`2.34E-10`)), nil)
				token, err := l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenFloat, Val: "2.34E-10"}, token)
			})

			t.Run("plus", func(t *testing.T) {
				l := NewLexer(bufio.NewReader(strings.NewReader(`+2.34E-10`)), nil)
				token, err := l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenSign, Val: "+"}, token)
				token, err = l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenFloat, Val: "2.34E-10"}, token)
			})

			t.Run("minus", func(t *testing.T) {
				l := NewLexer(bufio.NewReader(strings.NewReader(`-2.34E-10`)), nil)
				token, err := l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenSign, Val: "-"}, token)
				token, err = l.Next()
				assert.NoError(t, err)
				assert.Equal(t, Token{Kind: TokenFloat, Val: "2.34E-10"}, token)
			})
		})

		t.Run("begins with 0", func(t *testing.T) {
			l := NewLexer(bufio.NewReader(strings.NewReader(`0.333`)), nil)
			token, err := l.Next()
			assert.NoError(t, err)
			assert.Equal(t, Token{Kind: TokenFloat, Val: "0.333"}, token)
		})
	})

	t.Run("integer then period", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader("X is 1 + 2.")), nil)

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
		assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenEOS}, token)
	})

	t.Run("multiple terms", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader(`
foo(a).
foo(b).
foo(c).
`)), nil)

		token, err := l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "foo"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenL, Val: "("}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "a"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenR, Val: ")"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "foo"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenL, Val: "("}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "b"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenR, Val: ")"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "foo"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenL, Val: "("}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "c"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenR, Val: ")"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)
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
		assert.Equal(t, Token{Kind: TokenParenL, Val: "("}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "abc"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenR, Val: ")"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)
	})

	t.Run("trailing space in args", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader(`:- op( 20, xfx, <-- ).`)), nil)

		token, err := l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: ":-"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "op"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenL, Val: "("}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenInteger, Val: "20"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenComma, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "xfx"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenComma, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "<--"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenR, Val: ")"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)
	})

	t.Run("elems", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader(`[V<--Ans].`)), nil)

		token, err := l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenBracketL, Val: "["}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenVariable, Val: "V"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "<--"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenVariable, Val: "Ans"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenBracketR, Val: "]"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)
	})

	t.Run("trailing space in list elems", func(t *testing.T) {
		l := NewLexer(bufio.NewReader(strings.NewReader(`del_item(Item, [It |R], R) :-
      same_subst(Item, It), ! .`)), nil)

		token, err := l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "del_item"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenL, Val: "("}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenVariable, Val: "Item"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenComma, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenBracketL, Val: "["}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenVariable, Val: "It"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenBar, Val: "|"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenVariable, Val: "R"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenBracketR, Val: "]"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenComma, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenVariable, Val: "R"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenR, Val: ")"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: ":-"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "same_subst"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenL, Val: "("}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenVariable, Val: "Item"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenComma, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenVariable, Val: "It"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenParenR, Val: ")"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenComma, Val: ","}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenAtom, Val: "!"}, token)

		token, err = l.Next()
		assert.NoError(t, err)
		assert.Equal(t, Token{Kind: TokenPeriod, Val: "."}, token)
	})
}
