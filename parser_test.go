package prolog

import (
	"io"
	"reflect"
	"strings"
	"testing"
)

func TestParser_Term(t *testing.T) {
	h := NewHeap(11 * 1024)

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

		{input: `(foo).`, term: must(NewAtom(h, "foo"))},
		{input: `(a b).`, err: &SyntaxError{impDepAtom: "unexpected token: letter digit(b)"}},

		{input: `foo.`, term: must(NewAtom(h, "foo"))},
		{input: `[].`, term: must(NewAtom(h, "[]"))},
		{input: `[ ].`, term: must(NewAtom(h, "[]"))},
		{input: `{}.`, term: must(NewAtom(h, "{}"))},
		{input: `{ }.`, term: must(NewAtom(h, "{}"))},
		{input: `'abc'.`, term: must(NewAtom(h, "abc"))},
		{input: `'don''t panic'.`, term: must(NewAtom(h, "don't panic"))},
		{input: "'this is \\\na quoted ident'.", term: must(NewAtom(h, "this is a quoted ident"))},
		{input: `'\a'.`, term: must(NewAtom(h, "\a"))},
		{input: `'\b'.`, term: must(NewAtom(h, "\b"))},
		{input: `'\f'.`, term: must(NewAtom(h, "\f"))},
		{input: `'\n'.`, term: must(NewAtom(h, "\n"))},
		{input: `'\r'.`, term: must(NewAtom(h, "\r"))},
		{input: `'\t'.`, term: must(NewAtom(h, "\t"))},
		{input: `'\v'.`, term: must(NewAtom(h, "\v"))},
		{input: `'\43\'.`, term: must(NewAtom(h, "#"))},
		{input: `'\xa3\'.`, term: must(NewAtom(h, "£"))},
		{input: `'\\'.`, term: must(NewAtom(h, `\`))},
		{input: `'\''.`, term: must(NewAtom(h, `'`))},
		{input: `'\"'.`, term: must(NewAtom(h, `"`))},
		{input: "'\\`'.", term: must(NewAtom(h, "`"))},
		{input: `[`, err: io.EOF},
		{input: `{`, err: io.EOF},

		{input: `1.`, term: must(NewInteger(h, 1))},
		{input: `0'1.`, term: must(NewInteger(h, 49))},
		{input: `0b1.`, term: must(NewInteger(h, 1))},
		{input: `0o1.`, term: must(NewInteger(h, 1))},
		{input: `0x1.`, term: must(NewInteger(h, 1))},
		{input: `-1.`, term: must(NewInteger(h, -1))},
		{input: `- 1.`, term: must(NewInteger(h, -1))},
		{input: `'-'1.`, term: must(NewInteger(h, -1))},
		{input: `9223372036854775808.`, err: &RepresentationError{flag: "max_integer"}},
		{input: `-9223372036854775809.`, err: &RepresentationError{flag: "min_integer"}},
		{input: `-`, err: io.EOF},
		{input: `- -`, err: io.EOF},

		{input: `1.0.`, term: must(NewFloat(h, 1))},
		{input: `-1.0.`, term: must(NewFloat(h, -1))},
		{input: `- 1.0.`, term: must(NewFloat(h, -1))},
		{input: `'-'1.0.`, term: must(NewFloat(h, -1))},

		{input: `_.`, term: Term{tag: termTagVariable, payload: 1}},
		{input: `X.`, term: Term{tag: termTagVariable, payload: 1}, vars: []ParsedVariable{
			{Name: "X", Variable: 1, Count: 1},
		}},

		{input: `foo(a, b).`, term: must(NewCompound(h, "foo", must(NewAtom(h, "a")), must(NewAtom(h, "b"))))},
		{input: `foo(-(a)).`, term: must(NewCompound(h, "foo", must(NewCompound(h, "-", must(NewAtom(h, "a"))))))},
		{input: `foo(-).`, term: must(NewCompound(h, "foo", must(NewAtom(h, "-"))))},
		{input: `foo((), b).`, err: &SyntaxError{impDepAtom: "unexpected token: close())"}},
		{input: `foo([]).`, term: must(NewCompound(h, "foo", must(NewAtom(h, "[]"))))},
		{input: `foo(a, ()).`, err: &SyntaxError{impDepAtom: "unexpected token: close())"}},
		{input: `foo(a b).`, err: &SyntaxError{impDepAtom: "unexpected token: letter digit(b)"}},
		{input: `foo(a, b`, err: io.EOF},

		{input: `[a, b].`, term: must(NewList(h, must(NewAtom(h, "a")), must(NewAtom(h, "b"))))},
		{input: `[(), b].`, err: &SyntaxError{impDepAtom: "unexpected token: close())"}},
		{input: `[a, ()].`, err: &SyntaxError{impDepAtom: "unexpected token: close())"}},
		{input: `[a b].`, err: &SyntaxError{impDepAtom: "unexpected token: letter digit(b)"}},
		{input: `[a|X].`, term: must(NewCompound(h, ".", must(NewAtom(h, "a")), Term{tag: termTagVariable, payload: 1})), vars: []ParsedVariable{
			{Name: "X", Variable: 1, Count: 1},
		}},
		{input: `[a, b|X].`, term: must(NewPartialList(h, Term{tag: termTagVariable, payload: 1}, must(NewAtom(h, "a")), must(NewAtom(h, "b")))), vars: []ParsedVariable{
			{Name: "X", Variable: 1, Count: 1},
		}},
		{input: `[a, b|()].`, err: &SyntaxError{impDepAtom: "unexpected token: close())"}},
		{input: `[a, b|c d].`, err: &SyntaxError{impDepAtom: "unexpected token: letter digit(d)"}},
		{input: `[a `, err: io.EOF},

		{input: `{a}.`, term: must(NewCompound(h, "{}", must(NewAtom(h, "a"))))},
		{input: `{()}.`, err: &SyntaxError{impDepAtom: "unexpected token: close())"}},
		{input: `{a b}.`, err: &SyntaxError{impDepAtom: "unexpected token: letter digit(b)"}},

		{input: `-a.`, term: must(NewCompound(h, "-", must(NewAtom(h, "a"))))},
		{input: `- .`, term: must(NewAtom(h, "-"))},

		{input: `a-- .`, term: must(NewCompound(h, "--", must(NewAtom(h, "a"))))},

		{input: `a + b.`, term: must(NewCompound(h, "+", must(NewAtom(h, "a")), must(NewAtom(h, "b"))))},
		{input: `a + ().`, err: &SyntaxError{impDepAtom: "unexpected token: close())"}},
		{input: `a * b + c.`, term: must(NewCompound(h, "+", must(NewCompound(h, "*", must(NewAtom(h, "a")), must(NewAtom(h, "b")))), must(NewAtom(h, "c"))))},
		{input: `a [] b.`, err: &SyntaxError{impDepAtom: "unexpected token: open list([)"}},
		{input: `a {} b.`, err: &SyntaxError{impDepAtom: "unexpected token: open curly({)"}},
		{input: `a, b.`, term: must(NewCompound(h, ",", must(NewAtom(h, "a")), must(NewAtom(h, "b"))))},
		{input: `+ * + .`, err: &SyntaxError{impDepAtom: "unexpected token: graphic(+)"}},

		{input: `"abc".`, doubleQuotes: doubleQuotesChars, term: must(NewCharList(h, "abc"))},
		{input: `"abc".`, doubleQuotes: doubleQuotesCodes, term: must(NewCodeList(h, "abc"))},
		{input: `"abc".`, doubleQuotes: doubleQuotesAtom, term: must(NewAtom(h, "abc"))},
		{input: `"don""t panic".`, doubleQuotes: doubleQuotesAtom, term: must(NewAtom(h, "don\"t panic"))},
		{input: "\"this is \\\na double-quoted string\".", doubleQuotes: doubleQuotesAtom, term: must(NewAtom(h, "this is a double-quoted string"))},
		{input: `"\a".`, doubleQuotes: doubleQuotesAtom, term: must(NewAtom(h, "\a"))},
		{input: `"\b".`, doubleQuotes: doubleQuotesAtom, term: must(NewAtom(h, "\b"))},
		{input: `"\f".`, doubleQuotes: doubleQuotesAtom, term: must(NewAtom(h, "\f"))},
		{input: `"\n".`, doubleQuotes: doubleQuotesAtom, term: must(NewAtom(h, "\n"))},
		{input: `"\r".`, doubleQuotes: doubleQuotesAtom, term: must(NewAtom(h, "\r"))},
		{input: `"\t".`, doubleQuotes: doubleQuotesAtom, term: must(NewAtom(h, "\t"))},
		{input: `"\v".`, doubleQuotes: doubleQuotesAtom, term: must(NewAtom(h, "\v"))},
		{input: `"\xa3\".`, doubleQuotes: doubleQuotesAtom, term: must(NewAtom(h, "£"))},
		{input: `"\43\".`, doubleQuotes: doubleQuotesAtom, term: must(NewAtom(h, "#"))},
		{input: `"\\".`, doubleQuotes: doubleQuotesAtom, term: must(NewAtom(h, `\`))},
		{input: `"\'".`, doubleQuotes: doubleQuotesAtom, term: must(NewAtom(h, `'`))},
		{input: `"\"".`, doubleQuotes: doubleQuotesAtom, term: must(NewAtom(h, `"`))},
		{input: "\"\\`\".", doubleQuotes: doubleQuotesAtom, term: must(NewAtom(h, "`"))},

		// https://github.com/ichiban/prolog/issues/219#issuecomment-1200489336
		{input: `write('[]').`, term: must(NewCompound(h, "write", must(NewAtom(h, "[]"))))},
		{input: `write('{}').`, term: must(NewCompound(h, "write", must(NewAtom(h, "{}"))))},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			snapshot := *h
			defer func() {
				*h = snapshot
			}()

			p := NewParser(strings.NewReader(tt.input), ops, tt.doubleQuotes)
			term, pvs, err := p.Term(h)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected error %q, got %q", tt.err, err)
			}
			if o := term.Compare(h, tt.term); o != 0 {
				t.Errorf("expected %4q, got %4q", &Formatter{Term: tt.term, Heap: h}, &Formatter{Term: term, Heap: h})
			}
			if !reflect.DeepEqual(pvs, tt.vars) {
				t.Errorf("expected %v, got %v", tt.vars, pvs)
			}
		})
	}
}

func TestParser_Number(t *testing.T) {
	h := NewHeap(11 * 1024)

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
		{input: `33`, number: must(NewInteger(h, 33))},
		{input: `-33`, number: must(NewInteger(h, -33))},
		{input: `- 33`, number: must(NewInteger(h, -33))},
		{input: `'-'33`, number: must(NewInteger(h, -33))},
		{input: ` 33`, number: must(NewInteger(h, 33))},
		{input: `9223372036854775808.`, err: &RepresentationError{flag: "max_integer"}},
		{input: `-9223372036854775809.`, err: &RepresentationError{flag: "min_integer"}},

		{input: `0'!`, number: must(NewInteger(h, 33))},
		{input: `-0'!`, number: must(NewInteger(h, -33))},
		{input: `- 0'!`, number: must(NewInteger(h, -33))},
		{input: `'-'0'!`, number: must(NewInteger(h, -33))},

		{input: `0b1`, number: must(NewInteger(h, 1))},
		{input: `0o1`, number: must(NewInteger(h, 1))},
		{input: `0x1`, number: must(NewInteger(h, 1))},

		{input: `3.3`, number: must(NewFloat(h, 3.3))},
		{input: `-3.3`, number: must(NewFloat(h, -3.3))},
		{input: `- 3.3`, number: must(NewFloat(h, -3.3))},
		{input: `'-'3.3`, number: must(NewFloat(h, -3.3))},

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
			p := NewParser(strings.NewReader(tt.input), Operators{}, doubleQuotesChars)
			n, err := p.Number(h)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected error %q, got %q", tt.err, err)
			}
			if o := n.Compare(h, tt.number); o != 0 {
				t.Errorf("expected %4q, got %4q", tt.number, n)
			}
		})
	}
}

func TestParser_More(t *testing.T) {
	h := NewHeap(1024)
	p := NewParser(strings.NewReader(`foo. bar.`), Operators{}, doubleQuotesChars)
	term, _, err := p.Term(h)
	if err != nil {
		t.Fatal(err)
	}
	if a, err := term.Atom(h); err != nil || a != "foo" {
		t.Errorf("expected foo, got %v", a)
	}

	if !p.More() {
		t.Fatal("expected more")
	}

	term, _, err = p.Term(h)
	if err != nil {
		t.Fatal(err)
	}

	if a, err := term.Atom(h); err != nil || a != "bar" {
		t.Errorf("expected bar, got %v", a)
	}

	if p.More() {
		t.Fatal("expected no more")
	}
}
