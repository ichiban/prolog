package internal

import (
	"bytes"
	"errors"
	"github.com/ichiban/prolog/internal/ring"
	"io"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestParser_Term(t *testing.T) {
	ops := operators{}
	ops.define(1000, OperatorSpecifierXFY, NewAtom(`,`))
	ops.define(500, OperatorSpecifierYFX, NewAtom(`+`))
	ops.define(400, OperatorSpecifierYFX, NewAtom(`*`))
	ops.define(200, OperatorSpecifierFY, NewAtom(`-`))
	ops.define(200, OperatorSpecifierYF, NewAtom(`--`))

	pool := NewHeap(1024 * 1024)

	must := func(id Term, err error) Term {
		if err != nil {
			t.Fatal(err)
		}
		return id
	}

	lastVariable := func() Term {
		return must(pool.PutVariable(pool.env.lastVariable))
	}

	a := must(pool.PutAtom(Atom('a')))
	b := must(pool.PutAtom(Atom('b')))
	c := must(pool.PutAtom(Atom('c')))
	minusA := must(pool.PutCompound(Atom('-'), a))
	minus := must(pool.PutAtom(Atom('-')))
	emptyList := must(pool.PutAtom(atomEmptyList))
	minusMinusA := must(pool.PutCompound(NewAtom(`--`), a))

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

		{input: `(foo).`, term: must(pool.PutAtom(NewAtom("foo")))},
		{input: `(a b).`, err: unexpectedTokenError{actual: Token{kind: tokenLetterDigit, val: "b"}}},

		{input: `foo.`, term: must(pool.PutAtom(NewAtom("foo")))},
		{input: `[].`, term: must(pool.PutAtom(atomEmptyList))},
		{input: `[ ].`, term: must(pool.PutAtom(atomEmptyList))},
		{input: `{}.`, term: must(pool.PutAtom(atomEmptyBlock))},
		{input: `{ }.`, term: must(pool.PutAtom(atomEmptyBlock))},
		{input: `'abc'.`, term: must(pool.PutAtom(NewAtom("abc")))},
		{input: `'don''t panic'.`, term: must(pool.PutAtom(NewAtom("don't panic")))},
		{input: "'this is \\\na quoted ident'.", term: must(pool.PutAtom(NewAtom("this is a quoted ident")))},
		{input: `'\a'.`, term: must(pool.PutAtom(NewAtom("\a")))},
		{input: `'\b'.`, term: must(pool.PutAtom(NewAtom("\b")))},
		{input: `'\f'.`, term: must(pool.PutAtom(NewAtom("\f")))},
		{input: `'\n'.`, term: must(pool.PutAtom(NewAtom("\n")))},
		{input: `'\r'.`, term: must(pool.PutAtom(NewAtom("\r")))},
		{input: `'\t'.`, term: must(pool.PutAtom(NewAtom("\t")))},
		{input: `'\v'.`, term: must(pool.PutAtom(NewAtom("\v")))},
		{input: `'\43\'.`, term: must(pool.PutAtom(NewAtom("#")))},
		{input: `'\xa3\'.`, term: must(pool.PutAtom(NewAtom("£")))},
		{input: `'\\'.`, term: must(pool.PutAtom(NewAtom(`\`)))},
		{input: `'\''.`, term: must(pool.PutAtom(NewAtom(`'`)))},
		{input: `'\"'.`, term: must(pool.PutAtom(NewAtom(`"`)))},
		{input: "'\\`'.", term: must(pool.PutAtom(NewAtom("`")))},
		{input: `[`, err: io.EOF},
		{input: `{`, err: io.EOF},

		{input: `1.`, term: must(pool.PutInteger(1))},
		{input: `0'1.`, term: must(pool.PutInteger(49))},
		{input: `0b1.`, term: must(pool.PutInteger(1))},
		{input: `0o1.`, term: must(pool.PutInteger(1))},
		{input: `0x1.`, term: must(pool.PutInteger(1))},
		{input: `-1.`, term: must(pool.PutInteger(-1))},
		{input: `- 1.`, term: must(pool.PutInteger(-1))},
		{input: `'-'1.`, term: must(pool.PutInteger(-1))},
		{input: `9223372036854775808.`, err: errRepresentationMaxInteger},
		{input: `-9223372036854775809.`, err: errRepresentationMinInteger},
		{input: `-`, err: io.EOF},
		{input: `- -`, err: io.EOF},

		{input: `1.0.`, term: must(pool.PutFloat(1))},
		{input: `-1.0.`, term: must(pool.PutFloat(-1))},
		{input: `- 1.0.`, term: must(pool.PutFloat(-1))},
		{input: `'-'1.0.`, term: must(pool.PutFloat(-1))},

		{input: `_.`, termLazy: func() Term {
			return lastVariable()
		}},
		{input: `X.`, termLazy: func() Term {
			return lastVariable()
		}, vars: func() []ParsedVariable {
			return []ParsedVariable{
				{Name: Atom('X'), Variable: pool.env.lastVariable, Count: 1},
			}
		}},

		{input: `foo(a, b).`, term: must(pool.PutCompound(NewAtom("foo"), a, b))},
		{input: `foo(-(a)).`, term: must(pool.PutCompound(NewAtom("foo"), minusA))},
		{input: `foo(-).`, term: must(pool.PutCompound(NewAtom("foo"), minus))},
		{input: `foo((), b).`, err: unexpectedTokenError{actual: Token{kind: tokenClose, val: ")"}}},
		{input: `foo([]).`, term: must(pool.PutCompound(NewAtom("foo"), emptyList))},
		{input: `foo(a, ()).`, err: unexpectedTokenError{actual: Token{kind: tokenClose, val: ")"}}},
		{input: `foo(a b).`, err: unexpectedTokenError{actual: Token{kind: tokenLetterDigit, val: "b"}}},
		{input: `foo(a, b`, err: io.EOF},

		{input: `[a, b].`, term: must(pool.PutList(a, b))},
		{input: `[(), b].`, err: unexpectedTokenError{actual: Token{kind: tokenClose, val: ")"}}},
		{input: `[a, ()].`, err: unexpectedTokenError{actual: Token{kind: tokenClose, val: ")"}}},
		{input: `[a b].`, err: unexpectedTokenError{actual: Token{kind: tokenLetterDigit, val: "b"}}},
		{input: `[a|X].`, termLazy: func() Term {
			return must(pool.PutCompound(Atom('.'), a, lastVariable()))
		}, vars: func() []ParsedVariable {
			return []ParsedVariable{
				{Name: Atom('X'), Variable: pool.env.lastVariable, Count: 1},
			}
		}},
		{input: `[a, b|X].`, termLazy: func() Term {
			return must(pool.PutPartialList(lastVariable(), a, b))
		}, vars: func() []ParsedVariable {
			return []ParsedVariable{
				{Name: Atom('X'), Variable: pool.env.lastVariable, Count: 1},
			}
		}},
		{input: `[a, b|()].`, err: unexpectedTokenError{actual: Token{kind: tokenClose, val: ")"}}},
		{input: `[a, b|c d].`, err: unexpectedTokenError{actual: Token{kind: tokenLetterDigit, val: "d"}}},
		{input: `[a `, err: io.EOF},

		{input: `{a}.`, term: must(pool.PutCompound(atomEmptyBlock, a))},
		{input: `{()}.`, err: unexpectedTokenError{actual: Token{kind: tokenClose, val: ")"}}},
		{input: `{a b}.`, err: unexpectedTokenError{actual: Token{kind: tokenLetterDigit, val: "b"}}},

		{input: `-a.`, term: minusA},
		{input: `- .`, term: minus},

		{input: `a-- .`, term: minusMinusA},

		{input: `a + b.`, term: must(pool.PutCompound(Atom('+'), a, b))},
		{input: `a + ().`, err: unexpectedTokenError{actual: Token{kind: tokenClose, val: ")"}}},
		{input: `a * b + c.`, term: must(pool.PutCompound(Atom('+'), must(pool.PutCompound(Atom('*'), a, b)), c))},
		{input: `a [] b.`, err: unexpectedTokenError{actual: Token{kind: tokenOpenList, val: "["}}},
		{input: `a {} b.`, err: unexpectedTokenError{actual: Token{kind: tokenOpenCurly, val: "{"}}},
		{input: `a, b.`, term: must(pool.PutCompound(Atom(','), a, b))},
		{input: `+ * + .`, err: unexpectedTokenError{actual: Token{kind: tokenGraphic, val: "+"}}},

		{input: `"abc".`, doubleQuotes: doubleQuotesChars, term: must(pool.PutString("abc"))},
		{input: `"abc".`, doubleQuotes: doubleQuotesCodes, term: func() Term {
			s := "abc"
			ids := make([]Term, len(s))
			for i, r := range s {
				ids[i] = must(pool.PutInteger(int64(r)))
			}
			return must(pool.PutList(ids...))
		}()},
		{input: `"abc".`, doubleQuotes: doubleQuotesAtom, term: must(pool.PutAtom(NewAtom("abc")))},
		{input: `"don""t panic".`, doubleQuotes: doubleQuotesAtom, term: must(pool.PutAtom(NewAtom("don\"t panic")))},
		{input: "\"this is \\\na double-quoted string\".", doubleQuotes: doubleQuotesAtom, term: must(pool.PutAtom(NewAtom("this is a double-quoted string")))},
		{input: `"\a".`, doubleQuotes: doubleQuotesAtom, term: must(pool.PutAtom(NewAtom("\a")))},
		{input: `"\b".`, doubleQuotes: doubleQuotesAtom, term: must(pool.PutAtom(NewAtom("\b")))},
		{input: `"\f".`, doubleQuotes: doubleQuotesAtom, term: must(pool.PutAtom(NewAtom("\f")))},
		{input: `"\n".`, doubleQuotes: doubleQuotesAtom, term: must(pool.PutAtom(NewAtom("\n")))},
		{input: `"\r".`, doubleQuotes: doubleQuotesAtom, term: must(pool.PutAtom(NewAtom("\r")))},
		{input: `"\t".`, doubleQuotes: doubleQuotesAtom, term: must(pool.PutAtom(NewAtom("\t")))},
		{input: `"\v".`, doubleQuotes: doubleQuotesAtom, term: must(pool.PutAtom(NewAtom("\v")))},
		{input: `"\xa3\".`, doubleQuotes: doubleQuotesAtom, term: must(pool.PutAtom(NewAtom("£")))},
		{input: `"\43\".`, doubleQuotes: doubleQuotesAtom, term: must(pool.PutAtom(NewAtom("#")))},
		{input: `"\\".`, doubleQuotes: doubleQuotesAtom, term: must(pool.PutAtom(NewAtom(`\`)))},
		{input: `"\'".`, doubleQuotes: doubleQuotesAtom, term: must(pool.PutAtom(NewAtom(`'`)))},
		{input: `"\"".`, doubleQuotes: doubleQuotesAtom, term: must(pool.PutAtom(NewAtom(`"`)))},
		{input: "\"\\`\".", doubleQuotes: doubleQuotesAtom, term: must(pool.PutAtom(NewAtom("`")))},

		// https://github.com/ichiban/prolog/issues/219#issuecomment-1200489336
		{input: `write('[]').`, term: must(pool.PutCompound(NewAtom(`write`), emptyList))},
		{input: `write('{}').`, term: must(pool.PutCompound(NewAtom(`write`), must(pool.PutAtom(atomEmptyBlock))))},
	}

	for _, tc := range tests {
		t.Run(tc.input, func(t *testing.T) {
			p := Parser{
				Lexer: Lexer{
					module: func() *Module {
						return &Module{
							operators:    ops,
							doubleQuotes: tc.doubleQuotes,
						}
					},
					input: newRuneRingBuffer(strings.NewReader(tc.input)),
				},
				pool: &pool,
				buf:  ring.NewBuffer[Token](4),
			}

			term, err := p.Term()
			if !errors.Is(err, tc.err) {
				t.Errorf("expected error %q, got %q", tc.err, err)
			}

			if tc.termLazy != nil {
				tc.term = tc.termLazy()
			}
			o, err := pool.Compare(tc.term, term)
			if err != nil {
				t.Fatal(err)
			}
			if o != 0 {
				var b bytes.Buffer
				if err := pool.Write(&b, tc.term, nil); err != nil {
					t.Fatal(err)
				}
				want := b.String()
				b.Reset()

				if err := pool.Write(&b, term, nil); err != nil {
					t.Fatal(err)
				}
				actual := b.String()

				t.Errorf("expected %q, got %q", want, actual)
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
	pool := NewHeap(8 * 1024)

	must := func(id Term, err error) Term {
		if err != nil {
			t.Fatal(err)
		}
		return id
	}

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
			term: must(pool.PutList(
				must(pool.PutFloat(1.0)),
				must(pool.PutInteger(2)),
				must(pool.PutString("foo")),
				must(pool.PutList(
					must(pool.PutString("a")),
					must(pool.PutString("b")),
					must(pool.PutString("c")),
				)),
			)),
		},
		{
			title:        "codes",
			doubleQuotes: doubleQuotesCodes,
			input:        `[?, ?, ?, ?].`,
			args:         []interface{}{1.0, 2, "foo", []string{"a", "b", "c"}},
			term: must(pool.PutList(
				must(pool.PutFloat(1.0)),
				must(pool.PutInteger(2)),
				must(pool.PutString("foo")),
				must(pool.PutList(
					must(pool.PutInteger('a')),
					must(pool.PutInteger('b')),
					must(pool.PutInteger('c')),
				)),
			)),
		},
		{
			title:        "atom",
			doubleQuotes: doubleQuotesAtom,
			input:        `[?, ?, ?, ?].`,
			args:         []interface{}{1.0, 2, "foo", []string{"a", "b", "c"}},
			term: must(pool.PutList(
				must(pool.PutFloat(1.0)),
				must(pool.PutInteger(2)),
				must(pool.PutAtom(NewAtom("foo"))),
				must(pool.PutList(
					must(pool.PutAtom(Atom('a'))),
					must(pool.PutAtom(Atom('b'))),
					must(pool.PutAtom(Atom('c'))),
				)),
			)),
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
			termErr: errPlaceholder,
		},
		{
			title:   "too many arguments",
			input:   `[?, ?, ?, ?].`,
			args:    []interface{}{1.0, 2, "foo", []string{"a", "b", "c"}, "extra"},
			termErr: errPlaceholder,
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			p := Parser{
				Lexer: Lexer{
					module: func() *Module {
						return &Module{
							doubleQuotes: tt.doubleQuotes,
						}
					},
					input: newRuneRingBuffer(strings.NewReader(tt.input)),
				},
				buf:  ring.NewBuffer[Token](4),
				pool: &pool,
			}
			err := p.SetPlaceholder("?", tt.args...)
			if !errors.Is(err, tt.err) {
				t.Fatalf("unexpected error: %s", err)
			}

			if err != nil {
				return
			}

			term, err := p.Term()
			if !errors.Is(err, tt.termErr) {
				t.Fatalf("unexpected error: %s", err)
			}
			o, err := pool.Compare(term, tt.term)
			if err != nil {
				t.Fatal(err)
			}
			if o != 0 {
				t.Errorf("expected %d, got %d", tt.term, term)
			}
		})
	}
}

func TestParser_More(t *testing.T) {
	pool := NewHeap(1024)
	p := Parser{
		Lexer: Lexer{
			module: func() *Module {
				return &Module{}
			},
			input: newRuneRingBuffer(strings.NewReader(`foo. bar.`)),
		},
		pool: &pool,
		buf:  ring.NewBuffer[Token](4),
	}

	foo, err := pool.PutAtom(NewAtom("foo"))
	if err != nil {
		t.Fatal(err)
	}

	bar, err := pool.PutAtom(NewAtom("bar"))
	if err != nil {
		t.Fatal(err)
	}

	term, err := p.Term()
	if err != nil {
		t.Fatal(err)
	}
	o, err := pool.Compare(term, foo)
	if err != nil {
		t.Fatal(err)
	}
	if o != 0 {
		t.Errorf("expected 0, got %d", o)
	}
	if !p.More() {
		t.Error("expected more")
	}
	term, err = p.Term()
	if err != nil {
		t.Fatal(err)
	}
	o, err = pool.Compare(term, bar)
	if err != nil {
		t.Fatal(err)
	}
	if o != 0 {
		t.Errorf("expected 0, got %d", o)
	}
	if p.More() {
		t.Error("expected no more")
	}
}
