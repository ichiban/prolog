package engine

import (
	"io"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestParser_Term(t *testing.T) {
	ops := operators{}
	ops.define(1000, operatorSpecifierXFY, `,`)
	ops.define(500, operatorSpecifierYFX, `+`)
	ops.define(400, operatorSpecifierYFX, `*`)
	ops.define(200, operatorSpecifierFY, `-`)
	ops.define(200, operatorSpecifierYF, `--`)

	pvs := []ParsedVariable{
		{
			Name:     "X",
			Variable: NewNamedVariable("Z"),
		},
	}

	tests := []struct {
		input     string
		opts      []parserOption
		term      Term
		varOffset int64
		err       error
	}{
		{input: ``, err: io.EOF},
		{input: `foo`, err: ErrInsufficient},
		{input: `.`, err: unexpectedTokenError{actual: Token{Kind: TokenEnd, Val: "."}}},

		{input: `(foo).`, term: Atom("foo")},
		{input: `(a b).`, err: unexpectedTokenError{actual: Token{Kind: TokenLetterDigit, Val: "b"}}},

		{input: `foo.`, term: Atom("foo")},
		{input: `[].`, term: Atom("[]")},
		{input: `[ ].`, term: Atom("[]")},
		{input: `{}.`, term: Atom("{}")},
		{input: `{ }.`, term: Atom("{}")},
		{input: `'abc'.`, term: Atom("abc")},
		{input: `'don''t panic'.`, term: Atom("don't panic")},
		{input: "'this is \\\na quoted ident'.", term: Atom("this is a quoted ident")},
		{input: `'\a'.`, term: Atom("\a")},
		{input: `'\b'.`, term: Atom("\b")},
		{input: `'\f'.`, term: Atom("\f")},
		{input: `'\n'.`, term: Atom("\n")},
		{input: `'\r'.`, term: Atom("\r")},
		{input: `'\t'.`, term: Atom("\t")},
		{input: `'\v'.`, term: Atom("\v")},
		{input: `'\43\'.`, term: Atom("#")},
		{input: `'\xa3\'.`, term: Atom("£")},
		{input: `'\\'.`, term: Atom(`\`)},
		{input: `'\''.`, term: Atom(`'`)},
		{input: `'\"'.`, term: Atom(`"`)},
		{input: "'\\`'.", term: Atom("`")},

		{input: `1.`, term: Integer(1)},
		{input: `0'1.`, term: Integer(49)},
		{input: `0b1.`, term: Integer(1)},
		{input: `0o1.`, term: Integer(1)},
		{input: `0x1.`, term: Integer(1)},
		{input: `-1.`, term: Integer(-1)},
		{input: `- 1.`, term: Integer(-1)},
		{input: `'-'1.`, term: Integer(-1)},
		{input: `9223372036854775808.`, err: RepresentationError(FlagMaxInteger, nil)},
		{input: `-9223372036854775809.`, err: RepresentationError(FlagMinInteger, nil)},

		{input: `1.0.`, term: Float(1)},
		{input: `-1.0.`, term: Float(-1)},
		{input: `- 1.0.`, term: Float(-1)},
		{input: `'-'1.0.`, term: Float(-1)},

		{input: `X.`, term: NewNamedVariable("X")},
		{input: `_.`, varOffset: 1},
		{input: `X.`, opts: []parserOption{withParsedVars(&pvs)}, term: NewNamedVariable("Z")},
		{input: `Y.`, opts: []parserOption{withParsedVars(&pvs)}, varOffset: 1},

		{input: `foo(a, b).`, term: &compound{functor: "foo", args: []Term{Atom("a"), Atom("b")}}},
		{input: `foo(-(a)).`, term: &compound{functor: "foo", args: []Term{&compound{functor: "-", args: []Term{Atom("a")}}}}},
		{input: `foo(-).`, term: &compound{functor: "foo", args: []Term{Atom("-")}}},
		{input: `foo((), b).`, err: unexpectedTokenError{actual: Token{Kind: TokenClose, Val: ")"}}},
		{input: `foo([]).`, term: &compound{functor: "foo", args: []Term{Atom("[]")}}},
		{input: `foo(a, ()).`, err: unexpectedTokenError{actual: Token{Kind: TokenClose, Val: ")"}}},
		{input: `foo(a b).`, err: unexpectedTokenError{actual: Token{Kind: TokenLetterDigit, Val: "b"}}},
		{input: `foo(a, b`, err: ErrInsufficient},

		{input: `[a, b].`, term: List(Atom("a"), Atom("b"))},
		{input: `[(), b].`, err: unexpectedTokenError{actual: Token{Kind: TokenClose, Val: ")"}}},
		{input: `[a, ()].`, err: unexpectedTokenError{actual: Token{Kind: TokenClose, Val: ")"}}},
		{input: `[a b].`, err: unexpectedTokenError{actual: Token{Kind: TokenLetterDigit, Val: "b"}}},
		{input: `[a|X].`, term: Cons(Atom("a"), NewNamedVariable("X"))},
		{input: `[a, b|X].`, term: ListRest(NewNamedVariable("X"), Atom("a"), Atom("b"))},
		{input: `[a, b|()].`, err: unexpectedTokenError{actual: Token{Kind: TokenClose, Val: ")"}}},
		{input: `[a, b|c d].`, err: unexpectedTokenError{actual: Token{Kind: TokenLetterDigit, Val: "d"}}},

		{input: `{a}.`, term: &compound{functor: "{}", args: []Term{Atom("a")}}},
		{input: `{()}.`, err: unexpectedTokenError{actual: Token{Kind: TokenClose, Val: ")"}}},
		{input: `{a b}.`, err: unexpectedTokenError{actual: Token{Kind: TokenLetterDigit, Val: "b"}}},

		{input: `-a.`, term: &compound{functor: "-", args: []Term{Atom("a")}}},
		{input: `- .`, term: Atom("-")},

		{input: `a-- .`, term: &compound{functor: `--`, args: []Term{Atom(`a`)}}},

		{input: `a + b.`, term: &compound{functor: "+", args: []Term{Atom("a"), Atom("b")}}},
		{input: `a + ().`, err: unexpectedTokenError{actual: Token{Kind: TokenClose, Val: ")"}}},
		{input: `a * b + c.`, term: &compound{functor: "+", args: []Term{&compound{functor: "*", args: []Term{Atom("a"), Atom("b")}}, Atom("c")}}},
		{input: `a [] b.`, err: unexpectedTokenError{actual: Token{Kind: TokenOpenList, Val: "["}}},
		{input: `a {} b.`, err: unexpectedTokenError{actual: Token{Kind: TokenOpenCurly, Val: "{"}}},
		{input: `a, b.`, term: &compound{functor: ",", args: []Term{Atom("a"), Atom("b")}}},
		{input: `+ * + .`, err: unexpectedTokenError{actual: Token{Kind: TokenGraphic, Val: "+"}}},

		{input: `"abc".`, opts: []parserOption{withDoubleQuotes(doubleQuotesChars)}, term: charList("abc")},
		{input: `"abc".`, opts: []parserOption{withDoubleQuotes(doubleQuotesCodes)}, term: codeList("abc")},
		{input: `"abc".`, opts: []parserOption{withDoubleQuotes(doubleQuotesAtom)}, term: Atom("abc")},
		{input: `"don""t panic".`, opts: []parserOption{withDoubleQuotes(doubleQuotesAtom)}, term: Atom("don\"t panic")},
		{input: "\"this is \\\na double-quoted string\".", opts: []parserOption{withDoubleQuotes(doubleQuotesAtom)}, term: Atom("this is a double-quoted string")},
		{input: `"\a".`, opts: []parserOption{withDoubleQuotes(doubleQuotesAtom)}, term: Atom("\a")},
		{input: `"\b".`, opts: []parserOption{withDoubleQuotes(doubleQuotesAtom)}, term: Atom("\b")},
		{input: `"\f".`, opts: []parserOption{withDoubleQuotes(doubleQuotesAtom)}, term: Atom("\f")},
		{input: `"\n".`, opts: []parserOption{withDoubleQuotes(doubleQuotesAtom)}, term: Atom("\n")},
		{input: `"\r".`, opts: []parserOption{withDoubleQuotes(doubleQuotesAtom)}, term: Atom("\r")},
		{input: `"\t".`, opts: []parserOption{withDoubleQuotes(doubleQuotesAtom)}, term: Atom("\t")},
		{input: `"\v".`, opts: []parserOption{withDoubleQuotes(doubleQuotesAtom)}, term: Atom("\v")},
		{input: `"\xa3\".`, opts: []parserOption{withDoubleQuotes(doubleQuotesAtom)}, term: Atom("£")},
		{input: `"\43\".`, opts: []parserOption{withDoubleQuotes(doubleQuotesAtom)}, term: Atom("#")},
		{input: `"\\".`, opts: []parserOption{withDoubleQuotes(doubleQuotesAtom)}, term: Atom(`\`)},
		{input: `"\'".`, opts: []parserOption{withDoubleQuotes(doubleQuotesAtom)}, term: Atom(`'`)},
		{input: `"\"".`, opts: []parserOption{withDoubleQuotes(doubleQuotesAtom)}, term: Atom(`"`)},
		{input: "\"\\`\".", opts: []parserOption{withDoubleQuotes(doubleQuotesAtom)}, term: Atom("`")},

		// https://github.com/ichiban/prolog/issues/219#issuecomment-1200489336
		{input: `write('[]').`, term: &compound{functor: `write`, args: []Term{Atom(`[]`)}}},
		{input: `write('{}').`, term: &compound{functor: `write`, args: []Term{Atom(`{}`)}}},
	}

	for _, tc := range tests {
		t.Run(tc.input, func(t *testing.T) {
			offset := varCounter
			p := newParser(strings.NewReader(tc.input), append(tc.opts, withOperators(ops))...)
			term, err := p.Term()
			assert.Equal(t, tc.err, err)
			if tc.varOffset != 0 {
				assert.Equal(t, Variable(offset+tc.varOffset), term)
			} else {
				assert.Equal(t, tc.term, term)
			}
		})
	}
}

func TestParser_Replace(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		p := newParser(strings.NewReader(`[?, ?, ?, ?].`))
		assert.NoError(t, p.Replace("?", 1.0, 2, "foo", []string{"a", "b", "c"}))

		list, err := p.Term()
		assert.NoError(t, err)
		assert.Equal(t, List(Float(1.0), Integer(2), Atom("foo"), List(Atom("a"), Atom("b"), Atom("c"))), list)
	})

	t.Run("invalid argument", func(t *testing.T) {
		p := newParser(strings.NewReader(`[?].`))
		assert.Error(t, p.Replace("?", []struct{}{{}}))
	})

	t.Run("too few arguments", func(t *testing.T) {
		p := newParser(strings.NewReader(`[?, ?, ?, ?, ?].`))
		assert.NoError(t, p.Replace("?", 1.0, 2, "foo", []string{"a", "b", "c"}))

		_, err := p.Term()
		assert.Error(t, err)
	})

	t.Run("too many arguments", func(t *testing.T) {
		p := newParser(strings.NewReader(`[?, ?, ?, ?].`))
		assert.NoError(t, p.Replace("?", 1.0, 2, "foo", []string{"a", "b", "c"}, "extra"))

		_, err := p.Term()
		assert.Error(t, err)
	})
}

func TestParser_Number(t *testing.T) {
	tests := []struct {
		input  string
		number Number
		err    error
	}{
		{input: `33`, number: Integer(33)},
		{input: `-33`, number: Integer(-33)},
		{input: `- 33`, number: Integer(-33)},
		{input: `'-'33`, number: Integer(-33)},
		{input: ` 33`, number: Integer(33)},
		{input: `9223372036854775808.`, err: RepresentationError(FlagMaxInteger, nil)},
		{input: `-9223372036854775809.`, err: RepresentationError(FlagMinInteger, nil)},

		{input: `0'!`, number: Integer(33)},
		{input: `-0'!`, number: Integer(-33)},
		{input: `- 0'!`, number: Integer(-33)},
		{input: `'-'0'!`, number: Integer(-33)},

		{input: `0b1`, number: Integer(1)},
		{input: `0o1`, number: Integer(1)},
		{input: `0x1`, number: Integer(1)},

		{input: `3.3`, number: Float(3.3)},
		{input: `-3.3`, number: Float(-3.3)},
		{input: `- 3.3`, number: Float(-3.3)},
		{input: `'-'3.3`, number: Float(-3.3)},

		{input: `33 three`, err: errNotANumber},
		{input: `3 `, err: errNotANumber},
		{input: `3.`, err: errNotANumber},
		{input: `three`, err: errNotANumber},
		{input: `-`, err: errNotANumber},
		{input: `()`, err: errNotANumber},
	}

	for _, tc := range tests {
		t.Run(tc.input, func(t *testing.T) {
			p := newParser(strings.NewReader(tc.input))
			n, err := p.Number()
			assert.Equal(t, tc.err, err)
			assert.Equal(t, tc.number, n)
		})
	}
}

func TestParser_More(t *testing.T) {
	p := newParser(strings.NewReader(`foo. bar.`))
	term, err := p.Term()
	assert.NoError(t, err)
	assert.Equal(t, Atom("foo"), term)
	assert.True(t, p.More())
	term, err = p.Term()
	assert.NoError(t, err)
	assert.Equal(t, Atom("bar"), term)
	assert.False(t, p.More())
}
