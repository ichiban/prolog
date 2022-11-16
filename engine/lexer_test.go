package engine

import (
	"errors"
	"io"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestLexer_Token(t *testing.T) {
	tests := []struct {
		input           string
		charConversions map[rune]rune
		token           Token
		err             error
	}{
		{input: ``, err: io.EOF},
		{input: `🙈`, err: errMonkey}, // In this test, we use a see-no-evil monkey emoji to denote a non-EOF error.

		{input: ".", token: Token{Kind: TokenEnd, Val: "."}},
		{input: ";", token: Token{Kind: TokenSemicolon, Val: ";"}},
		{input: "!", token: Token{Kind: TokenCut, Val: "!"}},
		{input: "(", token: Token{Kind: TokenOpenCT, Val: "("}},
		{input: " (", token: Token{Kind: TokenOpen, Val: "("}},
		{input: ")", token: Token{Kind: TokenClose, Val: ")"}},
		{input: "[", token: Token{Kind: TokenOpenList, Val: "["}},
		{input: "]", token: Token{Kind: TokenCloseList, Val: "]"}},
		{input: "{", token: Token{Kind: TokenOpenCurly, Val: "{"}},
		{input: "}", token: Token{Kind: TokenCloseCurly, Val: "}"}},
		{input: "|", token: Token{Kind: TokenBar, Val: "|"}},
		{input: ",", token: Token{Kind: TokenComma, Val: ","}},

		{input: "% comment\nfoo", token: Token{Kind: TokenLetterDigit, Val: "foo"}},
		{input: "% comment", err: io.EOF},
		{input: "/* comment \n * also comment \n */foo", token: Token{Kind: TokenLetterDigit, Val: "foo"}},
		{input: "/* comment ", err: io.EOF},
		{input: `/`, token: Token{Kind: TokenGraphic, Val: `/`}},
		{input: `/ *`, token: Token{Kind: TokenGraphic, Val: `/`}},
		{input: "/* comment *", err: io.EOF},
		{input: `/🙈`, err: errMonkey},

		{input: `改善`, token: Token{Kind: TokenLetterDigit, Val: `改善`}},
		{input: `プロログ`, token: Token{Kind: TokenLetterDigit, Val: `プロログ`}},
		{input: `ぷろろぐ`, token: Token{Kind: TokenLetterDigit, Val: `ぷろろぐ`}},
		{input: `프롤로그`, token: Token{Kind: TokenLetterDigit, Val: `프롤로그`}},
		{input: `برولوغ`, token: Token{Kind: TokenLetterDigit, Val: `برولوغ`}},
		{input: `פרולוג`, token: Token{Kind: TokenLetterDigit, Val: `פרולוג`}},
		{input: `ゴー`, token: Token{Kind: TokenLetterDigit, Val: `ゴー`}},
		{input: `prolog.`, token: Token{Kind: TokenLetterDigit, Val: `prolog`}},
		{input: `prolog🙈`, err: errMonkey},

		{input: `..`, token: Token{Kind: TokenGraphic, Val: `..`}},
		{input: `#`, token: Token{Kind: TokenGraphic, Val: `#`}},
		{input: `\`, token: Token{Kind: TokenGraphic, Val: `\`}},
		{input: `∀`, token: Token{Kind: TokenGraphic, Val: `∀`}},
		{input: `⨀`, token: Token{Kind: TokenGraphic, Val: `⨀`}},
		{input: `+🙈`, err: errMonkey},

		{input: `'abc'`, token: Token{Kind: TokenQuoted, Val: "'abc'"}},
		{input: `'abc'.`, token: Token{Kind: TokenQuoted, Val: "'abc'"}},
		{input: `'don''t panic'`, token: Token{Kind: TokenQuoted, Val: "'don''t panic'"}},
		{input: `'this is \
a quoted ident'`, token: Token{Kind: TokenQuoted, Val: "'this is \\\na quoted ident'"}},
		{input: `'\a'`, token: Token{Kind: TokenQuoted, Val: "'\\a'"}},
		{input: `'\b'`, token: Token{Kind: TokenQuoted, Val: "'\\b'"}},
		{input: `'\f'`, token: Token{Kind: TokenQuoted, Val: "'\\f'"}},
		{input: `'\n'`, token: Token{Kind: TokenQuoted, Val: "'\\n'"}},
		{input: `'\r'`, token: Token{Kind: TokenQuoted, Val: "'\\r'"}},
		{input: `'\t'`, token: Token{Kind: TokenQuoted, Val: "'\\t'"}},
		{input: `'\v'`, token: Token{Kind: TokenQuoted, Val: "'\\v'"}},
		{input: `'\xa3\'`, token: Token{Kind: TokenQuoted, Val: "'\\xa3\\'"}},
		{input: `'\xa333333333\'`, token: Token{Kind: TokenInvalid, Val: `'\xa333333333\'`}},
		{input: `'\xa333333333\'.`, token: Token{Kind: TokenInvalid, Val: `'\xa333333333\'`}},
		{input: `'\43333333\'`, token: Token{Kind: TokenInvalid, Val: `'\43333333\'`}},
		{input: `'\\'`, token: Token{Kind: TokenQuoted, Val: `'\\'`}},
		{input: `'\''`, token: Token{Kind: TokenQuoted, Val: `'\''`}},
		{input: `'\"'`, token: Token{Kind: TokenQuoted, Val: `'\"'`}},
		{input: "'`'", token: Token{Kind: TokenQuoted, Val: "'`'"}},
		{input: "'\\`'", token: Token{Kind: TokenQuoted, Val: "'\\`'"}},
		{input: `'`, err: io.EOF},
		{input: `'\`, err: io.EOF},
		{input: `'\x`, err: io.EOF},
		{input: `'\xG`, token: Token{Kind: TokenInvalid, Val: `'\xG`}},
		{input: `'\0`, err: io.EOF},
		{input: `'\08`, token: Token{Kind: TokenInvalid, Val: `'\08`}},
		{input: "'\x01'", token: Token{Kind: TokenInvalid, Val: "'\x01"}},
		{input: `'abc'🙈`, err: errMonkey},
		{input: `'this is \🙈'`, err: errMonkey},

		{input: `X`, token: Token{Kind: TokenVariable, Val: `X`}},
		{input: `X.`, token: Token{Kind: TokenVariable, Val: `X`}},
		{input: `_123`, token: Token{Kind: TokenVariable, Val: `_123`}},
		{input: `X🙈`, err: errMonkey},

		{input: `012345`, token: Token{Kind: TokenInteger, Val: "012345"}},
		{input: `012345,`, token: Token{Kind: TokenInteger, Val: "012345"}},
		{input: `012345..`, token: Token{Kind: TokenInteger, Val: "012345"}},
		{input: `0b10110101`, token: Token{Kind: TokenInteger, Val: "0b10110101"}},
		{input: `0b10110101.`, token: Token{Kind: TokenInteger, Val: "0b10110101"}},
		{input: `0b`, token: Token{Kind: TokenInteger, Val: "0"}},
		{input: `0b.`, token: Token{Kind: TokenInteger, Val: "0"}},
		{input: `0o567`, token: Token{Kind: TokenInteger, Val: "0o567"}},
		{input: `0o567.`, token: Token{Kind: TokenInteger, Val: "0o567"}},
		{input: `0o`, token: Token{Kind: TokenInteger, Val: "0"}},
		{input: `0o.`, token: Token{Kind: TokenInteger, Val: "0"}},
		{input: `0x89ABC`, token: Token{Kind: TokenInteger, Val: "0x89ABC"}},
		{input: `0x89ABC.`, token: Token{Kind: TokenInteger, Val: "0x89ABC"}},
		{input: `0x`, token: Token{Kind: TokenInteger, Val: "0"}},
		{input: `0x.`, token: Token{Kind: TokenInteger, Val: "0"}},
		{input: `0'a`, token: Token{Kind: TokenInteger, Val: "0'a"}},
		{input: `0'''`, token: Token{Kind: TokenInteger, Val: "0'''"}},
		{input: `0''`, token: Token{Kind: TokenInteger, Val: "0"}},
		{input: `0''.`, token: Token{Kind: TokenInteger, Val: "0"}},
		{input: `0'\n`, token: Token{Kind: TokenInteger, Val: `0'\n`}},
		{input: `0'\
`, token: Token{Kind: TokenInteger, Val: `0`}},
		{input: `0'\`, err: io.EOF},
		{input: `0'\q`, token: Token{Kind: TokenInvalid, Val: `0'\q`}},
		{input: `0'\😀`, token: Token{Kind: TokenInvalid, Val: `0'\😀`}},
		{input: `0'`, err: io.EOF},
		{input: "0'\x01", token: Token{Kind: TokenInvalid, Val: "0'\x01"}},
		{input: `0`, token: Token{Kind: TokenInteger, Val: "0"}},
		{input: `0.`, token: Token{Kind: TokenInteger, Val: "0"}},
		{input: `0🙈`, err: errMonkey},
		{input: `0'🙈`, err: errMonkey},
		{input: `0''🙈`, err: errMonkey},
		{input: `0'\🙈`, err: errMonkey},
		{input: `0b🙈`, err: errMonkey},
		{input: `0o🙈`, err: errMonkey},
		{input: `0x🙈`, err: errMonkey},
		{input: `012345🙈`, err: errMonkey},
		{input: `012345.🙈`, err: errMonkey},
		{input: `0b10110101🙈`, err: errMonkey},
		{input: `0o567🙈`, err: errMonkey},
		{input: `0x89ABC🙈`, err: errMonkey},

		{input: `2.34`, token: Token{Kind: TokenFloatNumber, Val: "2.34"}},
		{input: `2.34.`, token: Token{Kind: TokenFloatNumber, Val: "2.34"}},
		{input: `2.34E5`, token: Token{Kind: TokenFloatNumber, Val: "2.34E5"}},
		{input: `2.34E5.`, token: Token{Kind: TokenFloatNumber, Val: "2.34E5"}},
		{input: `2.34E`, token: Token{Kind: TokenFloatNumber, Val: "2.34"}},
		{input: `2.34E.`, token: Token{Kind: TokenFloatNumber, Val: "2.34"}},
		{input: `2.34E+5`, token: Token{Kind: TokenFloatNumber, Val: "2.34E+5"}},
		{input: `2.34E+5.`, token: Token{Kind: TokenFloatNumber, Val: "2.34E+5"}},
		{input: `2.34E+`, token: Token{Kind: TokenFloatNumber, Val: "2.34"}},
		{input: `2.34E+.`, token: Token{Kind: TokenFloatNumber, Val: "2.34"}},
		{input: `2.34E-10`, token: Token{Kind: TokenFloatNumber, Val: "2.34E-10"}},
		{input: `2.34E-10.`, token: Token{Kind: TokenFloatNumber, Val: "2.34E-10"}},
		{input: `2.34E-`, token: Token{Kind: TokenFloatNumber, Val: "2.34"}},
		{input: `2.34E-.`, token: Token{Kind: TokenFloatNumber, Val: "2.34"}},
		{input: `0.333`, token: Token{Kind: TokenFloatNumber, Val: "0.333"}},
		{input: `2.34🙈`, err: errMonkey},
		{input: `2.34E🙈`, err: errMonkey},
		{input: `2.34E+🙈`, err: errMonkey},
		{input: `2.34E-🙈`, err: errMonkey},
		{input: `2.34E5🙈`, err: errMonkey},
		{input: `2.34E+5🙈`, err: errMonkey},
		{input: `2.34E-10🙈`, err: errMonkey},

		{input: `"abc"`, token: Token{Kind: TokenDoubleQuotedList, Val: `"abc"`}},
		{input: `"abc".`, token: Token{Kind: TokenDoubleQuotedList, Val: `"abc"`}},
		{input: `"don""t panic"`, token: Token{Kind: TokenDoubleQuotedList, Val: `"don""t panic"`}},
		{input: `"this is \
a quoted ident"`, token: Token{Kind: TokenDoubleQuotedList, Val: `"this is \
a quoted ident"`}},
		{input: `"\a"`, token: Token{Kind: TokenDoubleQuotedList, Val: `"\a"`}},
		{input: `"\b"`, token: Token{Kind: TokenDoubleQuotedList, Val: `"\b"`}},
		{input: `"\f"`, token: Token{Kind: TokenDoubleQuotedList, Val: `"\f"`}},
		{input: `"\n"`, token: Token{Kind: TokenDoubleQuotedList, Val: `"\n"`}},
		{input: `"\r"`, token: Token{Kind: TokenDoubleQuotedList, Val: `"\r"`}},
		{input: `"\t"`, token: Token{Kind: TokenDoubleQuotedList, Val: `"\t"`}},
		{input: `"\v"`, token: Token{Kind: TokenDoubleQuotedList, Val: `"\v"`}},
		{input: `"\xa3\"`, token: Token{Kind: TokenDoubleQuotedList, Val: `"\xa3\"`}},
		{input: `"\xa3`, err: io.EOF},
		{input: `"\xa3g`, token: Token{Kind: TokenInvalid, Val: `"\xa3g`}},
		{input: `"\43\"`, token: Token{Kind: TokenDoubleQuotedList, Val: `"\43\"`}},
		{input: `"\43`, err: io.EOF},
		{input: `"\438`, token: Token{Kind: TokenInvalid, Val: `"\438`}},
		{input: `"\\"`, token: Token{Kind: TokenDoubleQuotedList, Val: `"\\"`}},
		{input: `"\'"`, token: Token{Kind: TokenDoubleQuotedList, Val: `"\'"`}},
		{input: `"\""`, token: Token{Kind: TokenDoubleQuotedList, Val: `"\""`}},
		{input: "\"\\`\"", token: Token{Kind: TokenDoubleQuotedList, Val: "\"\\`\""}},
		{input: `"`, err: io.EOF},
		{input: `"\`, err: io.EOF},
		{input: `"abc"🙈`, err: errMonkey},

		{input: "\x01", token: Token{Kind: TokenInvalid, Val: "\x01"}},

		{input: `abc`, charConversions: map[rune]rune{'b': 'a'}, token: Token{Kind: TokenLetterDigit, Val: "aac"}},
		{input: `'abc'`, charConversions: map[rune]rune{'b': 'a'}, token: Token{Kind: TokenQuoted, Val: "'abc'"}},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			l := Lexer{input: newRuneRingBuffer(noMonkeyReader{strings.NewReader(tt.input)}), charConversions: tt.charConversions}

			token, err := l.Token()
			assert.Equal(t, tt.token, token)
			assert.Equal(t, tt.err, err)
		})
	}
}

var errMonkey = errors.New("monkey")

type noMonkeyReader struct {
	io.RuneReader
}

func (n noMonkeyReader) ReadRune() (rune, int, error) {
	r, size, err := n.RuneReader.ReadRune()
	if r == '🙈' {
		return 0, 0, errMonkey
	}
	return r, size, err
}

func TestTokenKind_GoString(t *testing.T) {
	for i := TokenKind(0); i < tokenKindLen; i++ {
		assert.Equal(t, i.String(), i.GoString())
	}
}
