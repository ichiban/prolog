package prolog

import (
	"io"
	"reflect"
	"strings"
	"testing"
)

func TestParser_Term(t *testing.T) {
	h := NewHeap(nil)

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

		{input: `(foo).`, term: must(h.PutAtom("foo"))},
		{input: `(a b).`, err: &SyntaxError{impDepAtom: "unexpected token: letter digit(b)"}},

		{input: `foo.`, term: must(h.PutAtom("foo"))},
		{input: `[].`, term: must(h.PutAtom("[]"))},
		{input: `[ ].`, term: must(h.PutAtom("[]"))},
		{input: `{}.`, term: must(h.PutAtom("{}"))},
		{input: `{ }.`, term: must(h.PutAtom("{}"))},
		{input: `'abc'.`, term: must(h.PutAtom("abc"))},
		{input: `'don''t panic'.`, term: must(h.PutAtom("don't panic"))},
		{input: "'this is \\\na quoted ident'.", term: must(h.PutAtom("this is a quoted ident"))},
		{input: `'\a'.`, term: must(h.PutAtom("\a"))},
		{input: `'\b'.`, term: must(h.PutAtom("\b"))},
		{input: `'\f'.`, term: must(h.PutAtom("\f"))},
		{input: `'\n'.`, term: must(h.PutAtom("\n"))},
		{input: `'\r'.`, term: must(h.PutAtom("\r"))},
		{input: `'\t'.`, term: must(h.PutAtom("\t"))},
		{input: `'\v'.`, term: must(h.PutAtom("\v"))},
		{input: `'\43\'.`, term: must(h.PutAtom("#"))},
		{input: `'\xa3\'.`, term: must(h.PutAtom("£"))},
		{input: `'\\'.`, term: must(h.PutAtom(`\`))},
		{input: `'\''.`, term: must(h.PutAtom(`'`))},
		{input: `'\"'.`, term: must(h.PutAtom(`"`))},
		{input: "'\\`'.", term: must(h.PutAtom("`"))},
		{input: `[`, err: io.EOF},
		{input: `{`, err: io.EOF},

		{input: `1.`, term: must(h.PutInteger(1))},
		{input: `0'1.`, term: must(h.PutInteger(49))},
		{input: `0b1.`, term: must(h.PutInteger(1))},
		{input: `0o1.`, term: must(h.PutInteger(1))},
		{input: `0x1.`, term: must(h.PutInteger(1))},
		{input: `-1.`, term: must(h.PutInteger(-1))},
		{input: `- 1.`, term: must(h.PutInteger(-1))},
		{input: `'-'1.`, term: must(h.PutInteger(-1))},
		{input: `9223372036854775808.`, err: &RepresentationError{flag: "max_integer"}},
		{input: `-9223372036854775809.`, err: &RepresentationError{flag: "min_integer"}},
		{input: `-`, err: io.EOF},
		{input: `- -`, err: io.EOF},

		{input: `1.0.`, term: must(h.PutFloat(1))},
		{input: `-1.0.`, term: must(h.PutFloat(-1))},
		{input: `- 1.0.`, term: must(h.PutFloat(-1))},
		{input: `'-'1.0.`, term: must(h.PutFloat(-1))},

		{input: `_.`, term: Term{tag: termTagReference, value: 136}},
		{input: `X.`, term: Term{tag: termTagReference, value: 137}, vars: []ParsedVariable{
			{Name: "X", Variable: 137, Count: 1},
		}},

		{input: `foo(a, b).`, term: must(h.PutCompound("foo", must(h.PutAtom("a")), must(h.PutAtom("b"))))},
		{input: `foo(-(a)).`, term: must(h.PutCompound("foo", must(h.PutCompound("-", must(h.PutAtom("a"))))))},
		{input: `foo(-).`, term: must(h.PutCompound("foo", must(h.PutAtom("-"))))},
		{input: `foo((), b).`, err: &SyntaxError{impDepAtom: "unexpected token: close())"}},
		{input: `foo([]).`, term: must(h.PutCompound("foo", must(h.PutAtom("[]"))))},
		{input: `foo(a, ()).`, err: &SyntaxError{impDepAtom: "unexpected token: close())"}},
		{input: `foo(a b).`, err: &SyntaxError{impDepAtom: "unexpected token: letter digit(b)"}},
		{input: `foo(a, b`, err: io.EOF},

		{input: `[a, b].`, term: must(h.PutList(must(h.PutAtom("a")), must(h.PutAtom("b"))))},
		{input: `[(), b].`, err: &SyntaxError{impDepAtom: "unexpected token: close())"}},
		{input: `[a, ()].`, err: &SyntaxError{impDepAtom: "unexpected token: close())"}},
		{input: `[a b].`, err: &SyntaxError{impDepAtom: "unexpected token: letter digit(b)"}},
		{input: `[a|X].`, term: must(h.PutCompound(".", must(h.PutAtom("a")), Term{tag: termTagReference, value: 172})), vars: []ParsedVariable{
			{Name: "X", Variable: 172, Count: 1},
		}},
		{input: `[a, b|X].`, term: must(h.PutPartialList(Term{tag: termTagReference, value: 179}, must(h.PutAtom("a")), must(h.PutAtom("b")))), vars: []ParsedVariable{
			{Name: "X", Variable: 179, Count: 1},
		}},
		{input: `[a, b|()].`, err: &SyntaxError{impDepAtom: "unexpected token: close())"}},
		{input: `[a, b|c d].`, err: &SyntaxError{impDepAtom: "unexpected token: letter digit(d)"}},
		{input: `[a `, err: io.EOF},

		{input: `{a}.`, term: must(h.PutCompound("{}", must(h.PutAtom("a"))))},
		{input: `{()}.`, err: &SyntaxError{impDepAtom: "unexpected token: close())"}},
		{input: `{a b}.`, err: &SyntaxError{impDepAtom: "unexpected token: letter digit(b)"}},

		{input: `-a.`, term: must(h.PutCompound("-", must(h.PutAtom("a"))))},
		{input: `- .`, term: must(h.PutAtom("-"))},

		{input: `a-- .`, term: must(h.PutCompound("--", must(h.PutAtom("a"))))},

		{input: `a + b.`, term: must(h.PutCompound("+", must(h.PutAtom("a")), must(h.PutAtom("b"))))},
		{input: `a + ().`, err: &SyntaxError{impDepAtom: "unexpected token: close())"}},
		{input: `a * b + c.`, term: must(h.PutCompound("+", must(h.PutCompound("*", must(h.PutAtom("a")), must(h.PutAtom("b")))), must(h.PutAtom("c"))))},
		{input: `a [] b.`, err: &SyntaxError{impDepAtom: "unexpected token: open list([)"}},
		{input: `a {} b.`, err: &SyntaxError{impDepAtom: "unexpected token: open curly({)"}},
		{input: `a, b.`, term: must(h.PutCompound(",", must(h.PutAtom("a")), must(h.PutAtom("b"))))},
		{input: `+ * + .`, err: &SyntaxError{impDepAtom: "unexpected token: graphic(+)"}},

		{input: `"abc".`, doubleQuotes: doubleQuotesChars, term: must(h.PutCharList("abc"))},
		{input: `"abc".`, doubleQuotes: doubleQuotesCodes, term: must(h.PutCodeList("abc"))},
		{input: `"abc".`, doubleQuotes: doubleQuotesAtom, term: must(h.PutAtom("abc"))},
		{input: `"don""t panic".`, doubleQuotes: doubleQuotesAtom, term: must(h.PutAtom("don\"t panic"))},
		{input: "\"this is \\\na double-quoted string\".", doubleQuotes: doubleQuotesAtom, term: must(h.PutAtom("this is a double-quoted string"))},
		{input: `"\a".`, doubleQuotes: doubleQuotesAtom, term: must(h.PutAtom("\a"))},
		{input: `"\b".`, doubleQuotes: doubleQuotesAtom, term: must(h.PutAtom("\b"))},
		{input: `"\f".`, doubleQuotes: doubleQuotesAtom, term: must(h.PutAtom("\f"))},
		{input: `"\n".`, doubleQuotes: doubleQuotesAtom, term: must(h.PutAtom("\n"))},
		{input: `"\r".`, doubleQuotes: doubleQuotesAtom, term: must(h.PutAtom("\r"))},
		{input: `"\t".`, doubleQuotes: doubleQuotesAtom, term: must(h.PutAtom("\t"))},
		{input: `"\v".`, doubleQuotes: doubleQuotesAtom, term: must(h.PutAtom("\v"))},
		{input: `"\xa3\".`, doubleQuotes: doubleQuotesAtom, term: must(h.PutAtom("£"))},
		{input: `"\43\".`, doubleQuotes: doubleQuotesAtom, term: must(h.PutAtom("#"))},
		{input: `"\\".`, doubleQuotes: doubleQuotesAtom, term: must(h.PutAtom(`\`))},
		{input: `"\'".`, doubleQuotes: doubleQuotesAtom, term: must(h.PutAtom(`'`))},
		{input: `"\"".`, doubleQuotes: doubleQuotesAtom, term: must(h.PutAtom(`"`))},
		{input: "\"\\`\".", doubleQuotes: doubleQuotesAtom, term: must(h.PutAtom("`"))},

		// https://github.com/ichiban/prolog/issues/219#issuecomment-1200489336
		{input: `write('[]').`, term: must(h.PutCompound("write", must(h.PutAtom("[]"))))},
		{input: `write('{}').`, term: must(h.PutCompound("write", must(h.PutAtom("{}"))))},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			// TODO: reset heap? h.terms = h.terms[:0]

			p := NewParser(strings.NewReader(tt.input), &Module{
				operators:    ops,
				doubleQuotes: tt.doubleQuotes,
			})
			term, pvs, err := p.Term(h)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected error %q, got %q", tt.err, err)
			}
			if o := h.Compare(term, tt.term); o != 0 {
				t.Errorf("expected %4q, got %4q", &Formatter{Term: tt.term, Heap: h}, &Formatter{Term: term, Heap: h})
			}
			if !reflect.DeepEqual(pvs, tt.vars) {
				t.Errorf("expected %v, got %v", tt.vars, pvs)
			}
		})
	}
}

func TestParser_Number(t *testing.T) {
	h := NewHeap(nil)

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
		{input: `33`, number: must(h.PutInteger(33))},
		{input: `-33`, number: must(h.PutInteger(-33))},
		{input: `- 33`, number: must(h.PutInteger(-33))},
		{input: `'-'33`, number: must(h.PutInteger(-33))},
		{input: ` 33`, number: must(h.PutInteger(33))},
		{input: `9223372036854775808.`, err: &RepresentationError{flag: "max_integer"}},
		{input: `-9223372036854775809.`, err: &RepresentationError{flag: "min_integer"}},

		{input: `0'!`, number: must(h.PutInteger(33))},
		{input: `-0'!`, number: must(h.PutInteger(-33))},
		{input: `- 0'!`, number: must(h.PutInteger(-33))},
		{input: `'-'0'!`, number: must(h.PutInteger(-33))},

		{input: `0b1`, number: must(h.PutInteger(1))},
		{input: `0o1`, number: must(h.PutInteger(1))},
		{input: `0x1`, number: must(h.PutInteger(1))},

		{input: `3.3`, number: must(h.PutFloat(3.3))},
		{input: `-3.3`, number: must(h.PutFloat(-3.3))},
		{input: `- 3.3`, number: must(h.PutFloat(-3.3))},
		{input: `'-'3.3`, number: must(h.PutFloat(-3.3))},

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
			n, err := p.Number(h)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected error %q, got %q", tt.err, err)
			}
			if o := h.Compare(n, tt.number); o != 0 {
				t.Errorf("expected %4q, got %4q", tt.number, n)
			}
		})
	}
}

func TestParser_More(t *testing.T) {
	h := NewHeap(nil)
	p := NewParser(strings.NewReader(`foo. bar.`), &Module{
		doubleQuotes: doubleQuotesChars,
	})
	term, _, err := p.Term(h)
	if err != nil {
		t.Fatal(err)
	}
	if a, err := h.Atom(term); err != nil || a != "foo" {
		t.Errorf("expected foo, got %v", a)
	}

	if !p.More() {
		t.Fatal("expected more")
	}

	term, _, err = p.Term(h)
	if err != nil {
		t.Fatal(err)
	}

	if a, err := h.Atom(term); err != nil || a != "bar" {
		t.Errorf("expected bar, got %v", a)
	}

	if p.More() {
		t.Fatal("expected no more")
	}
}
