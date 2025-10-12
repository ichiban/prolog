package prolog

import (
	"io"
	"reflect"
	"strings"
	"testing"
)

func TestParser_Term(t *testing.T) {
	e := NewEngine()

	must := func(term Term, err error) Term {
		if err != nil {
			t.Fatal(err)
		}
		return term
	}

	var ops Operators
	ops.Define(1000, XFY, `,`)
	ops.Define(500, YFX, `+`)
	ops.Define(400, YFX, `*`)
	ops.Define(200, FY, `-`)
	ops.Define(200, YF, `--`)

	tests := []struct {
		input        string
		doubleQuotes doubleQuotes
		term         Term
		vars         []ParsedVariable
		err          error
	}{
		{input: ``, err: io.EOF},
		{input: `foo`, err: io.EOF},
		{input: `.`, err: &SyntaxError{impDepAtom: "unexpected token: end(.)"}},

		{input: `(foo).`, term: must(e.PutAtom("foo"))},
		{input: `(a b).`, err: &SyntaxError{impDepAtom: "unexpected token: letter digit(b)"}},

		{input: `foo.`, term: must(e.PutAtom("foo"))},
		{input: `[].`, term: must(e.PutAtom("[]"))},
		{input: `[ ].`, term: must(e.PutAtom("[]"))},
		{input: `{}.`, term: must(e.PutAtom("{}"))},
		{input: `{ }.`, term: must(e.PutAtom("{}"))},
		{input: `'abc'.`, term: must(e.PutAtom("abc"))},
		{input: `'don''t panic'.`, term: must(e.PutAtom("don't panic"))},
		{input: "'this is \\\na quoted ident'.", term: must(e.PutAtom("this is a quoted ident"))},
		{input: `'\a'.`, term: must(e.PutAtom("\a"))},
		{input: `'\b'.`, term: must(e.PutAtom("\b"))},
		{input: `'\f'.`, term: must(e.PutAtom("\f"))},
		{input: `'\n'.`, term: must(e.PutAtom("\n"))},
		{input: `'\r'.`, term: must(e.PutAtom("\r"))},
		{input: `'\t'.`, term: must(e.PutAtom("\t"))},
		{input: `'\v'.`, term: must(e.PutAtom("\v"))},
		{input: `'\43\'.`, term: must(e.PutAtom("#"))},
		{input: `'\xa3\'.`, term: must(e.PutAtom("£"))},
		{input: `'\\'.`, term: must(e.PutAtom(`\`))},
		{input: `'\''.`, term: must(e.PutAtom(`'`))},
		{input: `'\"'.`, term: must(e.PutAtom(`"`))},
		{input: "'\\`'.", term: must(e.PutAtom("`"))},
		{input: `[`, err: io.EOF},
		{input: `{`, err: io.EOF},

		{input: `1.`, term: must(e.PutInteger(1))},
		{input: `0'1.`, term: must(e.PutInteger(49))},
		{input: `0b1.`, term: must(e.PutInteger(1))},
		{input: `0o1.`, term: must(e.PutInteger(1))},
		{input: `0x1.`, term: must(e.PutInteger(1))},
		{input: `-1.`, term: must(e.PutInteger(-1))},
		{input: `- 1.`, term: must(e.PutInteger(-1))},
		{input: `'-'1.`, term: must(e.PutInteger(-1))},
		{input: `9223372036854775808.`, err: &RepresentationError{flag: "max_integer"}},
		{input: `-9223372036854775809.`, err: &RepresentationError{flag: "min_integer"}},
		{input: `-`, err: io.EOF},
		{input: `- -`, err: io.EOF},

		{input: `1.0.`, term: must(e.PutFloat(1))},
		{input: `-1.0.`, term: must(e.PutFloat(-1))},
		{input: `- 1.0.`, term: must(e.PutFloat(-1))},
		{input: `'-'1.0.`, term: must(e.PutFloat(-1))},

		{input: `_.`, term: Term{tag: termTagReference, value: 136}},
		{input: `X.`, term: Term{tag: termTagReference, value: 137}, vars: []ParsedVariable{
			{Name: "X", Variable: 137, Count: 1},
		}},

		{input: `foo(a, b).`, term: must(e.PutCompound("foo", must(e.PutAtom("a")), must(e.PutAtom("b"))))},
		{input: `foo(-(a)).`, term: must(e.PutCompound("foo", must(e.PutCompound("-", must(e.PutAtom("a"))))))},
		{input: `foo(-).`, term: must(e.PutCompound("foo", must(e.PutAtom("-"))))},
		{input: `foo((), b).`, err: &SyntaxError{impDepAtom: "unexpected token: close())"}},
		{input: `foo([]).`, term: must(e.PutCompound("foo", must(e.PutAtom("[]"))))},
		{input: `foo(a, ()).`, err: &SyntaxError{impDepAtom: "unexpected token: close())"}},
		{input: `foo(a b).`, err: &SyntaxError{impDepAtom: "unexpected token: letter digit(b)"}},
		{input: `foo(a, b`, err: io.EOF},

		{input: `[a, b].`, term: must(e.PutList(must(e.PutAtom("a")), must(e.PutAtom("b"))))},
		{input: `[(), b].`, err: &SyntaxError{impDepAtom: "unexpected token: close())"}},
		{input: `[a, ()].`, err: &SyntaxError{impDepAtom: "unexpected token: close())"}},
		{input: `[a b].`, err: &SyntaxError{impDepAtom: "unexpected token: letter digit(b)"}},
		{input: `[a|X].`, term: must(e.PutCompound(".", must(e.PutAtom("a")), Term{tag: termTagReference, value: 172})), vars: []ParsedVariable{
			{Name: "X", Variable: 172, Count: 1},
		}},
		{input: `[a, b|X].`, term: must(e.PutPartialList(Term{tag: termTagReference, value: 179}, must(e.PutAtom("a")), must(e.PutAtom("b")))), vars: []ParsedVariable{
			{Name: "X", Variable: 179, Count: 1},
		}},
		{input: `[a, b|()].`, err: &SyntaxError{impDepAtom: "unexpected token: close())"}},
		{input: `[a, b|c d].`, err: &SyntaxError{impDepAtom: "unexpected token: letter digit(d)"}},
		{input: `[a `, err: io.EOF},

		{input: `{a}.`, term: must(e.PutCompound("{}", must(e.PutAtom("a"))))},
		{input: `{()}.`, err: &SyntaxError{impDepAtom: "unexpected token: close())"}},
		{input: `{a b}.`, err: &SyntaxError{impDepAtom: "unexpected token: letter digit(b)"}},

		{input: `-a.`, term: must(e.PutCompound("-", must(e.PutAtom("a"))))},
		{input: `- .`, term: must(e.PutAtom("-"))},

		{input: `a-- .`, term: must(e.PutCompound("--", must(e.PutAtom("a"))))},

		{input: `a + b.`, term: must(e.PutCompound("+", must(e.PutAtom("a")), must(e.PutAtom("b"))))},
		{input: `a + ().`, err: &SyntaxError{impDepAtom: "unexpected token: close())"}},
		{input: `a * b + c.`, term: must(e.PutCompound("+", must(e.PutCompound("*", must(e.PutAtom("a")), must(e.PutAtom("b")))), must(e.PutAtom("c"))))},
		{input: `a [] b.`, err: &SyntaxError{impDepAtom: "unexpected token: open list([)"}},
		{input: `a {} b.`, err: &SyntaxError{impDepAtom: "unexpected token: open curly({)"}},
		{input: `a, b.`, term: must(e.PutCompound(",", must(e.PutAtom("a")), must(e.PutAtom("b"))))},
		{input: `+ * + .`, err: &SyntaxError{impDepAtom: "unexpected token: graphic(+)"}},

		{input: `"abc".`, doubleQuotes: doubleQuotesChars, term: must(e.PutCharList("abc"))},
		{input: `"abc".`, doubleQuotes: doubleQuotesCodes, term: must(e.PutCodeList("abc"))},
		{input: `"abc".`, doubleQuotes: doubleQuotesAtom, term: must(e.PutAtom("abc"))},
		{input: `"don""t panic".`, doubleQuotes: doubleQuotesAtom, term: must(e.PutAtom("don\"t panic"))},
		{input: "\"this is \\\na double-quoted string\".", doubleQuotes: doubleQuotesAtom, term: must(e.PutAtom("this is a double-quoted string"))},
		{input: `"\a".`, doubleQuotes: doubleQuotesAtom, term: must(e.PutAtom("\a"))},
		{input: `"\b".`, doubleQuotes: doubleQuotesAtom, term: must(e.PutAtom("\b"))},
		{input: `"\f".`, doubleQuotes: doubleQuotesAtom, term: must(e.PutAtom("\f"))},
		{input: `"\n".`, doubleQuotes: doubleQuotesAtom, term: must(e.PutAtom("\n"))},
		{input: `"\r".`, doubleQuotes: doubleQuotesAtom, term: must(e.PutAtom("\r"))},
		{input: `"\t".`, doubleQuotes: doubleQuotesAtom, term: must(e.PutAtom("\t"))},
		{input: `"\v".`, doubleQuotes: doubleQuotesAtom, term: must(e.PutAtom("\v"))},
		{input: `"\xa3\".`, doubleQuotes: doubleQuotesAtom, term: must(e.PutAtom("£"))},
		{input: `"\43\".`, doubleQuotes: doubleQuotesAtom, term: must(e.PutAtom("#"))},
		{input: `"\\".`, doubleQuotes: doubleQuotesAtom, term: must(e.PutAtom(`\`))},
		{input: `"\'".`, doubleQuotes: doubleQuotesAtom, term: must(e.PutAtom(`'`))},
		{input: `"\"".`, doubleQuotes: doubleQuotesAtom, term: must(e.PutAtom(`"`))},
		{input: "\"\\`\".", doubleQuotes: doubleQuotesAtom, term: must(e.PutAtom("`"))},

		// https://github.com/ichiban/prolog/issues/219#issuecomment-1200489336
		{input: `write('[]').`, term: must(e.PutCompound("write", must(e.PutAtom("[]"))))},
		{input: `write('{}').`, term: must(e.PutCompound("write", must(e.PutAtom("{}"))))},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			// TODO: reset heap? e.heap = e.heap[:0]

			p := NewParser(strings.NewReader(tt.input), &Module{
				operators:    ops,
				doubleQuotes: tt.doubleQuotes,
			})
			term, pvs, err := p.Term(e)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected error %q, got %q", tt.err, err)
			}
			if o := e.Compare(term, tt.term); o != 0 {
				t.Errorf("expected %4q, got %4q", &Formatter{Term: tt.term, Engine: e}, &Formatter{Term: term, Engine: e})
			}
			if !reflect.DeepEqual(pvs, tt.vars) {
				t.Errorf("expected %v, got %v", tt.vars, pvs)
			}
		})
	}
}

func TestParser_Number(t *testing.T) {
	e := NewEngine()

	must := func(term Term, err error) Term {
		if err != nil {
			t.Fatal(err)
		}
		return term
	}

	tests := []struct {
		input  string
		number Term
		err    error
	}{
		{input: `33`, number: must(e.PutInteger(33))},
		{input: `-33`, number: must(e.PutInteger(-33))},
		{input: `- 33`, number: must(e.PutInteger(-33))},
		{input: `'-'33`, number: must(e.PutInteger(-33))},
		{input: ` 33`, number: must(e.PutInteger(33))},
		{input: `9223372036854775808.`, err: &RepresentationError{flag: "max_integer"}},
		{input: `-9223372036854775809.`, err: &RepresentationError{flag: "min_integer"}},

		{input: `0'!`, number: must(e.PutInteger(33))},
		{input: `-0'!`, number: must(e.PutInteger(-33))},
		{input: `- 0'!`, number: must(e.PutInteger(-33))},
		{input: `'-'0'!`, number: must(e.PutInteger(-33))},

		{input: `0b1`, number: must(e.PutInteger(1))},
		{input: `0o1`, number: must(e.PutInteger(1))},
		{input: `0x1`, number: must(e.PutInteger(1))},

		{input: `3.3`, number: must(e.PutFloat(3.3))},
		{input: `-3.3`, number: must(e.PutFloat(-3.3))},
		{input: `- 3.3`, number: must(e.PutFloat(-3.3))},
		{input: `'-'3.3`, number: must(e.PutFloat(-3.3))},

		{input: ``, err: io.EOF},
		{input: `X`, err: &SyntaxError{impDepAtom: "not_a_number"}},
		{input: `33 three`, err: &SyntaxError{impDepAtom: "not_a_number"}},
		{input: `3 `, err: &SyntaxError{impDepAtom: "not_a_number"}},
		{input: `3.`, err: &SyntaxError{impDepAtom: "not_a_number"}},
		{input: `three`, err: &SyntaxError{impDepAtom: "not_a_number"}},
		{input: `-`, err: &SyntaxError{impDepAtom: "not_a_number"}},
		{input: `-a.`, err: &SyntaxError{impDepAtom: "not_a_number"}},
		{input: `()`, err: &SyntaxError{impDepAtom: "not_a_number"}},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			p := NewParser(strings.NewReader(tt.input), &Module{
				doubleQuotes: doubleQuotesChars,
			})
			n, err := p.Number(e)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected error %q, got %q", tt.err, err)
			}
			if o := e.Compare(n, tt.number); o != 0 {
				t.Errorf("expected %4q, got %4q", tt.number, n)
			}
		})
	}
}

func TestParser_More(t *testing.T) {
	e := NewEngine()
	p := NewParser(strings.NewReader(`foo. bar.`), &Module{
		doubleQuotes: doubleQuotesChars,
	})
	term, _, err := p.Term(e)
	if err != nil {
		t.Fatal(err)
	}
	if a, err := e.Atom(term); err != nil || a != "foo" {
		t.Errorf("expected foo, got %v", a)
	}

	if !p.More() {
		t.Fatal("expected more")
	}

	term, _, err = p.Term(e)
	if err != nil {
		t.Fatal(err)
	}

	if a, err := e.Atom(term); err != nil || a != "bar" {
		t.Errorf("expected bar, got %v", a)
	}

	if p.More() {
		t.Fatal("expected no more")
	}
}
