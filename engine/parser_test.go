package engine

import (
	"errors"
	"io"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestParser_Term(t *testing.T) {
	ops := operators{}
	ops.define(1000, operatorSpecifierXFY, NewAtom(`,`))
	ops.define(500, operatorSpecifierYFX, NewAtom(`+`))
	ops.define(400, operatorSpecifierYFX, NewAtom(`*`))
	ops.define(200, operatorSpecifierFY, NewAtom(`-`))
	ops.define(200, operatorSpecifierYF, NewAtom(`--`))

	tests := []struct {
		input        string
		doubleQuotes doubleQuotes
		term         Term
		termLazy     func() Term
		vars         func() []ParsedVariable
		err          error
	}{
		{input: ``, err: io.EOF},
		{input: `foo`, err: io.EOF},
		{input: `.`, err: unexpectedTokenError{actual: Token{kind: tokenEnd, val: "."}}},

		{input: `(foo).`, term: NewAtom("foo")},
		{input: `(a b).`, err: unexpectedTokenError{actual: Token{kind: tokenLetterDigit, val: "b"}}},

		{input: `foo.`, term: NewAtom("foo")},
		{input: `[].`, term: atomEmptyList},
		{input: `[ ].`, term: atomEmptyList},
		{input: `{}.`, term: atomEmptyBlock},
		{input: `{ }.`, term: atomEmptyBlock},
		{input: `'abc'.`, term: NewAtom("abc")},
		{input: `'don''t panic'.`, term: NewAtom("don't panic")},
		{input: "'this is \\\na quoted ident'.", term: NewAtom("this is a quoted ident")},
		{input: `'\a'.`, term: NewAtom("\a")},
		{input: `'\b'.`, term: NewAtom("\b")},
		{input: `'\f'.`, term: NewAtom("\f")},
		{input: `'\n'.`, term: NewAtom("\n")},
		{input: `'\r'.`, term: NewAtom("\r")},
		{input: `'\t'.`, term: NewAtom("\t")},
		{input: `'\v'.`, term: NewAtom("\v")},
		{input: `'\43\'.`, term: NewAtom("#")},
		{input: `'\xa3\'.`, term: NewAtom("£")},
		{input: `'\\'.`, term: NewAtom(`\`)},
		{input: `'\''.`, term: NewAtom(`'`)},
		{input: `'\"'.`, term: NewAtom(`"`)},
		{input: "'\\`'.", term: NewAtom("`")},
		{input: `[`, err: io.EOF},
		{input: `{`, err: io.EOF},

		{input: `1.`, term: Integer(1)},
		{input: `0'1.`, term: Integer(49)},
		{input: `0b1.`, term: Integer(1)},
		{input: `0o1.`, term: Integer(1)},
		{input: `0x1.`, term: Integer(1)},
		{input: `-1.`, term: Integer(-1)},
		{input: `- 1.`, term: Integer(-1)},
		{input: `'-'1.`, term: Integer(-1)},
		{input: `9223372036854775808.`, err: representationError(flagMaxInteger, nil)},
		{input: `-9223372036854775809.`, err: representationError(flagMinInteger, nil)},
		{input: `-`, err: io.EOF},
		{input: `- -`, err: io.EOF},

		{input: `1.0.`, term: Float(1)},
		{input: `-1.0.`, term: Float(-1)},
		{input: `- 1.0.`, term: Float(-1)},
		{input: `'-'1.0.`, term: Float(-1)},

		{input: `_.`, termLazy: func() Term {
			return lastVariable()
		}},
		{input: `X.`, termLazy: func() Term {
			return lastVariable()
		}, vars: func() []ParsedVariable {
			return []ParsedVariable{
				{Name: NewAtom("X"), Variable: lastVariable(), Count: 1},
			}
		}},

		{input: `foo(a, b).`, term: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("a"), NewAtom("b")}}},
		{input: `foo(-(a)).`, term: &compound{functor: NewAtom("foo"), args: []Term{&compound{functor: atomMinus, args: []Term{NewAtom("a")}}}}},
		{input: `foo(-).`, term: &compound{functor: NewAtom("foo"), args: []Term{atomMinus}}},
		{input: `foo((), b).`, err: unexpectedTokenError{actual: Token{kind: tokenClose, val: ")"}}},
		{input: `foo([]).`, term: &compound{functor: NewAtom("foo"), args: []Term{atomEmptyList}}},
		{input: `foo(a, ()).`, err: unexpectedTokenError{actual: Token{kind: tokenClose, val: ")"}}},
		{input: `foo(a b).`, err: unexpectedTokenError{actual: Token{kind: tokenLetterDigit, val: "b"}}},
		{input: `foo(a, b`, err: io.EOF},

		{input: `[a, b].`, term: List(NewAtom("a"), NewAtom("b"))},
		{input: `[(), b].`, err: unexpectedTokenError{actual: Token{kind: tokenClose, val: ")"}}},
		{input: `[a, ()].`, err: unexpectedTokenError{actual: Token{kind: tokenClose, val: ")"}}},
		{input: `[a b].`, err: unexpectedTokenError{actual: Token{kind: tokenLetterDigit, val: "b"}}},
		{input: `[a|X].`, termLazy: func() Term {
			return Cons(NewAtom("a"), lastVariable())
		}, vars: func() []ParsedVariable {
			return []ParsedVariable{
				{Name: NewAtom("X"), Variable: lastVariable(), Count: 1},
			}
		}},
		{input: `[a, b|X].`, termLazy: func() Term {
			return PartialList(lastVariable(), NewAtom("a"), NewAtom("b"))
		}, vars: func() []ParsedVariable {
			return []ParsedVariable{
				{Name: NewAtom("X"), Variable: lastVariable(), Count: 1},
			}
		}},
		{input: `[a, b|()].`, err: unexpectedTokenError{actual: Token{kind: tokenClose, val: ")"}}},
		{input: `[a, b|c d].`, err: unexpectedTokenError{actual: Token{kind: tokenLetterDigit, val: "d"}}},
		{input: `[a `, err: io.EOF},

		{input: `{a}.`, term: &compound{functor: atomEmptyBlock, args: []Term{NewAtom("a")}}},
		{input: `{()}.`, err: unexpectedTokenError{actual: Token{kind: tokenClose, val: ")"}}},
		{input: `{a b}.`, err: unexpectedTokenError{actual: Token{kind: tokenLetterDigit, val: "b"}}},

		{input: `-a.`, term: &compound{functor: atomMinus, args: []Term{NewAtom("a")}}},
		{input: `- .`, term: atomMinus},

		{input: `a-- .`, term: &compound{functor: NewAtom(`--`), args: []Term{NewAtom(`a`)}}},

		{input: `a + b.`, term: &compound{functor: atomPlus, args: []Term{NewAtom("a"), NewAtom("b")}}},
		{input: `a + ().`, err: unexpectedTokenError{actual: Token{kind: tokenClose, val: ")"}}},
		{input: `a * b + c.`, term: &compound{functor: atomPlus, args: []Term{&compound{functor: NewAtom("*"), args: []Term{NewAtom("a"), NewAtom("b")}}, NewAtom("c")}}},
		{input: `a [] b.`, err: unexpectedTokenError{actual: Token{kind: tokenOpenList, val: "["}}},
		{input: `a {} b.`, err: unexpectedTokenError{actual: Token{kind: tokenOpenCurly, val: "{"}}},
		{input: `a, b.`, term: &compound{functor: atomComma, args: []Term{NewAtom("a"), NewAtom("b")}}},
		{input: `+ * + .`, err: unexpectedTokenError{actual: Token{kind: tokenGraphic, val: "+"}}},

		{input: `"abc".`, doubleQuotes: doubleQuotesChars, term: charList("abc")},
		{input: `"abc".`, doubleQuotes: doubleQuotesCodes, term: codeList("abc")},
		{input: `"abc".`, doubleQuotes: doubleQuotesAtom, term: NewAtom("abc")},
		{input: `"don""t panic".`, doubleQuotes: doubleQuotesAtom, term: NewAtom("don\"t panic")},
		{input: "\"this is \\\na double-quoted string\".", doubleQuotes: doubleQuotesAtom, term: NewAtom("this is a double-quoted string")},
		{input: `"\a".`, doubleQuotes: doubleQuotesAtom, term: NewAtom("\a")},
		{input: `"\b".`, doubleQuotes: doubleQuotesAtom, term: NewAtom("\b")},
		{input: `"\f".`, doubleQuotes: doubleQuotesAtom, term: NewAtom("\f")},
		{input: `"\n".`, doubleQuotes: doubleQuotesAtom, term: NewAtom("\n")},
		{input: `"\r".`, doubleQuotes: doubleQuotesAtom, term: NewAtom("\r")},
		{input: `"\t".`, doubleQuotes: doubleQuotesAtom, term: NewAtom("\t")},
		{input: `"\v".`, doubleQuotes: doubleQuotesAtom, term: NewAtom("\v")},
		{input: `"\xa3\".`, doubleQuotes: doubleQuotesAtom, term: NewAtom("£")},
		{input: `"\43\".`, doubleQuotes: doubleQuotesAtom, term: NewAtom("#")},
		{input: `"\\".`, doubleQuotes: doubleQuotesAtom, term: NewAtom(`\`)},
		{input: `"\'".`, doubleQuotes: doubleQuotesAtom, term: NewAtom(`'`)},
		{input: `"\"".`, doubleQuotes: doubleQuotesAtom, term: NewAtom(`"`)},
		{input: "\"\\`\".", doubleQuotes: doubleQuotesAtom, term: NewAtom("`")},

		// https://github.com/ichiban/prolog/issues/219#issuecomment-1200489336
		{input: `write('[]').`, term: &compound{functor: NewAtom(`write`), args: []Term{NewAtom(`[]`)}}},
		{input: `write('{}').`, term: &compound{functor: NewAtom(`write`), args: []Term{NewAtom(`{}`)}}},
	}

	for _, tc := range tests {
		t.Run(tc.input, func(t *testing.T) {
			vm := VM{
				moduleLocals: map[Atom]moduleLocal{
					atomUser: {
						operators:    ops,
						doubleQuotes: tc.doubleQuotes,
					},
				},
			}
			p := Parser{
				vm:     &vm,
				module: &atomUser,
				lexer: Lexer{
					input: newRuneRingBuffer(strings.NewReader(tc.input)),
				},
			}
			term, err := p.Term()
			assert.Equal(t, tc.err, err)
			if tc.termLazy == nil {
				assert.Equal(t, tc.term, term)
			} else {
				assert.Equal(t, tc.termLazy(), term)
			}
			if tc.vars == nil {
				assert.Empty(t, p.Vars)
			} else {
				assert.Equal(t, tc.vars(), p.Vars)
			}
		})
	}
}

func TestParser_Replace(t *testing.T) {
	tests := []struct {
		title        string
		doubleQuotes doubleQuotes
		input        string
		args         []interface{}
		err, termErr error
		term         Term
	}{
		{
			title:        "chars",
			doubleQuotes: doubleQuotesChars,
			input:        `[?, ?, ?, ?].`,
			args:         []interface{}{1.0, 2, "foo", []string{"a", "b", "c"}},
			term:         List(Float(1.0), Integer(2), CharList("foo"), List(CharList("a"), CharList("b"), CharList("c"))),
		},
		{
			title:        "codes",
			doubleQuotes: doubleQuotesCodes,
			input:        `[?, ?, ?, ?].`,
			args:         []interface{}{1.0, 2, "foo", []string{"a", "b", "c"}},
			term:         List(Float(1.0), Integer(2), CodeList("foo"), List(CodeList("a"), CodeList("b"), CodeList("c"))),
		},
		{
			title:        "atom",
			doubleQuotes: doubleQuotesAtom,
			input:        `[?, ?, ?, ?].`,
			args:         []interface{}{1.0, 2, "foo", []string{"a", "b", "c"}},
			term:         List(Float(1.0), Integer(2), NewAtom("foo"), List(NewAtom("a"), NewAtom("b"), NewAtom("c"))),
		},
		{
			title: "invalid argument",
			input: `[?].`,
			args:  []interface{}{nil},
			err:   errors.New("can't convert to term: <invalid reflect.Value>"),
		},
		{
			title:   "too few arguments",
			input:   `[?, ?, ?, ?, ?].`,
			args:    []interface{}{1.0, 2, "foo", []string{"a", "b", "c"}},
			termErr: errors.New("not enough arguments for placeholders"),
		},
		{
			title:   "too many arguments",
			input:   `[?, ?, ?, ?].`,
			args:    []interface{}{1.0, 2, "foo", []string{"a", "b", "c"}, "extra"},
			termErr: errors.New("too many arguments for placeholders: [extra]"),
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			vm := VM{
				moduleLocals: map[Atom]moduleLocal{
					atomUser: {
						doubleQuotes: tt.doubleQuotes,
					},
				},
			}
			p := Parser{
				vm:     &vm,
				module: &atomUser,
				lexer: Lexer{
					input: newRuneRingBuffer(strings.NewReader(tt.input)),
				},
			}
			err := p.SetPlaceholder(NewAtom("?"), tt.args...)
			assert.Equal(t, tt.err, err)

			if err != nil {
				return
			}

			term, err := p.Term()
			assert.Equal(t, tt.termErr, err)
			assert.Equal(t, tt.term, term)
		})
	}
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
		{input: `9223372036854775808.`, err: representationError(flagMaxInteger, nil)},
		{input: `-9223372036854775809.`, err: representationError(flagMinInteger, nil)},

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

		{input: ``, err: io.EOF},
		{input: `X`, err: errNotANumber},
		{input: `33 three`, err: errNotANumber},
		{input: `3 `, err: errNotANumber},
		{input: `3.`, err: errNotANumber},
		{input: `three`, err: errNotANumber},
		{input: `-`, err: errNotANumber},
		{input: `-a.`, err: errNotANumber},
		{input: `()`, err: errNotANumber},
	}

	for _, tc := range tests {
		t.Run(tc.input, func(t *testing.T) {
			p := Parser{
				lexer: Lexer{
					input: newRuneRingBuffer(strings.NewReader(tc.input)),
				},
			}
			n, err := p.number()
			assert.Equal(t, tc.err, err)
			assert.Equal(t, tc.number, n)
		})
	}
}

func TestParser_More(t *testing.T) {
	var vm VM
	p := Parser{
		vm:     &vm,
		module: &atomUser,
		lexer: Lexer{
			input: newRuneRingBuffer(strings.NewReader(`foo. bar.`)),
		},
	}
	term, err := p.Term()
	assert.NoError(t, err)
	assert.Equal(t, NewAtom("foo"), term)
	assert.True(t, p.More())
	term, err = p.Term()
	assert.NoError(t, err)
	assert.Equal(t, NewAtom("bar"), term)
	assert.False(t, p.More())
}
