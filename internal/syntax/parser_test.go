package syntax

import (
	"io"
	"reflect"
	"strings"
	"testing"

	"github.com/ichiban/prolog/v2/internal/term"
)

func TestParser_Term(t *testing.T) {
	heap := make(term.Heap, 0, 1024)

	must := func(term term.Handle, err error) term.Handle {
		t.Helper()
		if err != nil {
			t.Fatal(err)
		}
		return term
	}

	x := must(heap.PutVariable())
	heap = heap[:0]

	var ops OperatorSet
	ops.Define(1000, XFY, term.NewAtom(`,`))
	ops.Define(500, YFX, term.NewAtom(`+`))
	ops.Define(400, YFX, term.NewAtom(`*`))
	ops.Define(200, FY, term.NewAtom(`-`))
	ops.Define(200, YF, term.NewAtom(`--`))

	tests := []struct {
		input        string
		doubleQuotes doubleQuotes
		term         term.Handle
		vars         []ParsedVariable
		err          error
	}{
		{input: ``, err: io.EOF},
		{input: `foo`, err: io.EOF},
		{input: `.`, err: &UnexpectedTokenError{token: token{kind: tokenEnd, val: "."}}},

		{input: `(foo).`, term: must(heap.PutAtom(term.NewAtom("foo")))},
		{input: `(a b).`, err: &UnexpectedTokenError{token: token{kind: tokenLetterDigit, val: "b"}}},

		{input: `foo.`, term: must(heap.PutAtom(term.NewAtom("foo")))},
		{input: `[].`, term: must(heap.PutAtom(term.NewAtom("[]")))},
		{input: `[ ].`, term: must(heap.PutAtom(term.NewAtom("[]")))},
		{input: `{}.`, term: must(heap.PutAtom(term.NewAtom("{}")))},
		{input: `{ }.`, term: must(heap.PutAtom(term.NewAtom("{}")))},
		{input: `'abc'.`, term: must(heap.PutAtom(term.NewAtom("abc")))},
		{input: `'don''t panic'.`, term: must(heap.PutAtom(term.NewAtom("don't panic")))},
		{input: "'this is \\\na quoted ident'.", term: must(heap.PutAtom(term.NewAtom("this is a quoted ident")))},
		{input: `'\a'.`, term: must(heap.PutAtom(term.NewAtom("\a")))},
		{input: `'\b'.`, term: must(heap.PutAtom(term.NewAtom("\b")))},
		{input: `'\f'.`, term: must(heap.PutAtom(term.NewAtom("\f")))},
		{input: `'\n'.`, term: must(heap.PutAtom(term.NewAtom("\n")))},
		{input: `'\r'.`, term: must(heap.PutAtom(term.NewAtom("\r")))},
		{input: `'\t'.`, term: must(heap.PutAtom(term.NewAtom("\t")))},
		{input: `'\v'.`, term: must(heap.PutAtom(term.NewAtom("\v")))},
		{input: `'\43\'.`, term: must(heap.PutAtom(term.NewAtom("#")))},
		{input: `'\xa3\'.`, term: must(heap.PutAtom(term.NewAtom("£")))},
		{input: `'\\'.`, term: must(heap.PutAtom(term.NewAtom(`\`)))},
		{input: `'\''.`, term: must(heap.PutAtom(term.NewAtom(`'`)))},
		{input: `'\"'.`, term: must(heap.PutAtom(term.NewAtom(`"`)))},
		{input: "'\\`'.", term: must(heap.PutAtom(term.NewAtom("`")))},
		{input: `[`, err: io.EOF},
		{input: `{`, err: io.EOF},

		{input: `1.`, term: must(heap.PutInteger(1))},
		{input: `0'1.`, term: must(heap.PutInteger(49))},
		{input: `0b1.`, term: must(heap.PutInteger(1))},
		{input: `0o1.`, term: must(heap.PutInteger(1))},
		{input: `0x1.`, term: must(heap.PutInteger(1))},
		{input: `-1.`, term: must(heap.PutInteger(-1))},
		{input: `- 1.`, term: must(heap.PutInteger(-1))},
		{input: `'-'1.`, term: must(heap.PutInteger(-1))},
		{input: `9223372036854775808.`, err: ErrIntBelow},
		{input: `-9223372036854775809.`, err: ErrIntAbove},
		{input: `-`, err: io.EOF},
		{input: `- -`, err: io.EOF},

		{input: `1.0.`, term: must(heap.PutFloat(1))},
		{input: `-1.0.`, term: must(heap.PutFloat(-1))},
		{input: `- 1.0.`, term: must(heap.PutFloat(-1))},
		{input: `'-'1.0.`, term: must(heap.PutFloat(-1))},

		{input: `_.`, term: x},
		{input: `X.`, term: x, vars: []ParsedVariable{
			{Name: "X", Variable: x, Count: 1},
		}},

		{input: `foo(a, b).`, term: must(heap.PutCompound(term.NewAtom("foo"), must(heap.PutAtom(term.NewAtom("a"))), must(heap.PutAtom(term.NewAtom("b")))))},
		{input: `foo(-(a)).`, term: must(heap.PutCompound(term.NewAtom("foo"), must(heap.PutCompound(term.NewAtom("-"), must(heap.PutAtom(term.NewAtom("a"))), must(heap.PutAtom(term.NewAtom("b")))))))},
		{input: `foo(-).`, term: must(heap.PutCompound(term.NewAtom("foo"), must(heap.PutAtom(term.NewAtom("-")))))},
		{input: `foo((), b).`, err: &UnexpectedTokenError{token: token{kind: tokenClose, val: ")"}}},
		{input: `foo([]).`, term: must(heap.PutCompound(term.NewAtom("foo"), must(heap.PutAtom(term.NewAtom("[]")))))},
		{input: `foo(a, ()).`, err: &UnexpectedTokenError{token: token{kind: tokenClose, val: ")"}}},
		{input: `foo(a b).`, err: &UnexpectedTokenError{token: token{kind: tokenLetterDigit, val: "b"}}},
		{input: `foo(a, b`, err: io.EOF},

		{input: `[a, b].`, term: must(heap.PutList(must(heap.PutAtom(term.NewAtom("a"))), must(heap.PutAtom(term.NewAtom("b")))))},
		{input: `[(), b].`, err: &UnexpectedTokenError{token: token{kind: tokenClose, val: ")"}}},
		{input: `[a, ()].`, err: &UnexpectedTokenError{token: token{kind: tokenClose, val: ")"}}},
		{input: `[a b].`, err: &UnexpectedTokenError{token: token{kind: tokenLetterDigit, val: "b"}}},
		{input: `[a|X].`, term: must(heap.PutCompound(term.NewAtom("."), must(heap.PutAtom(term.NewAtom("a"))), x)), vars: []ParsedVariable{
			{Name: "X", Variable: x, Count: 1},
		}},
		{input: `[a, b|X].`, term: must(heap.PutPartialList(x, must(heap.PutAtom(term.NewAtom("a"))), must(heap.PutAtom(term.NewAtom("b"))))), vars: []ParsedVariable{
			{Name: "X", Variable: x, Count: 1},
		}},
		{input: `[a, b|()].`, err: &UnexpectedTokenError{token: token{kind: tokenClose, val: ")"}}},
		{input: `[a, b|c d].`, err: &UnexpectedTokenError{token: token{kind: tokenLetterDigit, val: "d"}}},
		{input: `[a `, err: io.EOF},

		{input: `{a}.`, term: must(heap.PutCompound(term.NewAtom("{}"), must(heap.PutAtom(term.NewAtom("a")))))},
		{input: `{()}.`, err: &UnexpectedTokenError{token: token{kind: tokenClose, val: ")"}}},
		{input: `{a b}.`, err: &UnexpectedTokenError{token: token{kind: tokenLetterDigit, val: "b"}}},

		{input: `-a.`, term: must(heap.PutCompound(term.NewAtom("-"), must(heap.PutAtom(term.NewAtom("a")))))},
		{input: `- .`, term: must(heap.PutAtom(term.NewAtom("-")))},

		{input: `a-- .`, term: must(heap.PutCompound(term.NewAtom("--"), must(heap.PutAtom(term.NewAtom("a")))))},

		{input: `a + b.`, term: must(heap.PutCompound(term.NewAtom("+"), must(heap.PutAtom(term.NewAtom("a"))), must(heap.PutAtom(term.NewAtom("b")))))},
		{input: `a + ().`, err: &UnexpectedTokenError{token: token{kind: tokenClose, val: ")"}}},
		{input: `a * b + c.`, term: must(heap.PutCompound(term.NewAtom("+"), must(heap.PutCompound(term.NewAtom("*"), must(heap.PutAtom(term.NewAtom("a"))), must(heap.PutAtom(term.NewAtom("b"))))), must(heap.PutAtom(term.NewAtom("c")))))},
		{input: `a [] b.`, err: &UnexpectedTokenError{token: token{kind: tokenOpenList, val: "["}}},
		{input: `a {} b.`, err: &UnexpectedTokenError{token: token{kind: tokenOpenCurly, val: "{"}}},
		{input: `a, b.`, term: must(heap.PutCompound(term.NewAtom(","), must(heap.PutAtom(term.NewAtom("a"))), must(heap.PutAtom(term.NewAtom("b")))))},
		{input: `+ * + .`, err: &UnexpectedTokenError{token: token{kind: tokenGraphic, val: "+"}}},

		{input: `"abc".`, doubleQuotes: doubleQuotesChars, term: must(heap.PutCharList("abc"))},
		{input: `"abc".`, doubleQuotes: doubleQuotesCodes, term: must(heap.PutCodeList("abc"))},
		{input: `"abc".`, doubleQuotes: doubleQuotesAtom, term: must(heap.PutAtom(term.NewAtom("abc")))},
		{input: `"don""t panic".`, doubleQuotes: doubleQuotesAtom, term: must(heap.PutAtom(term.NewAtom("don\"t panic")))},
		{input: "\"this is \\\na double-quoted string\".", doubleQuotes: doubleQuotesAtom, term: must(heap.PutAtom(term.NewAtom("this is a double-quoted string")))},
		{input: `"\a".`, doubleQuotes: doubleQuotesAtom, term: must(heap.PutAtom(term.NewAtom("\a")))},
		{input: `"\b".`, doubleQuotes: doubleQuotesAtom, term: must(heap.PutAtom(term.NewAtom("\b")))},
		{input: `"\f".`, doubleQuotes: doubleQuotesAtom, term: must(heap.PutAtom(term.NewAtom("\f")))},
		{input: `"\n".`, doubleQuotes: doubleQuotesAtom, term: must(heap.PutAtom(term.NewAtom("\n")))},
		{input: `"\r".`, doubleQuotes: doubleQuotesAtom, term: must(heap.PutAtom(term.NewAtom("\r")))},
		{input: `"\t".`, doubleQuotes: doubleQuotesAtom, term: must(heap.PutAtom(term.NewAtom("\t")))},
		{input: `"\v".`, doubleQuotes: doubleQuotesAtom, term: must(heap.PutAtom(term.NewAtom("\v")))},
		{input: `"\xa3\".`, doubleQuotes: doubleQuotesAtom, term: must(heap.PutAtom(term.NewAtom("£")))},
		{input: `"\43\".`, doubleQuotes: doubleQuotesAtom, term: must(heap.PutAtom(term.NewAtom("#")))},
		{input: `"\\".`, doubleQuotes: doubleQuotesAtom, term: must(heap.PutAtom(term.NewAtom(`\`)))},
		{input: `"\'".`, doubleQuotes: doubleQuotesAtom, term: must(heap.PutAtom(term.NewAtom(`'`)))},
		{input: `"\"".`, doubleQuotes: doubleQuotesAtom, term: must(heap.PutAtom(term.NewAtom(`"`)))},
		{input: "\"\\`\".", doubleQuotes: doubleQuotesAtom, term: must(heap.PutAtom(term.NewAtom("`")))},

		// https://github.com/ichiban/prolog/issues/219#issuecomment-1200489336
		{input: `write('[]').`, term: must(heap.PutCompound(term.NewAtom("write"), must(heap.PutAtom(term.NewAtom("[]")))))},
		{input: `write('{}').`, term: must(heap.PutCompound(term.NewAtom("write"), must(heap.PutAtom(term.NewAtom("{}")))))},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			heap = heap[:0]

			p := NewParser(strings.NewReader(tt.input), &heap, &ops, &tt.doubleQuotes)
			result, pvs, err := p.Term()
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected error %q, got %q", tt.err, err)
			}
			if result != tt.term {
				t.Errorf("expected %4q, got %4q", &Formatter{Term: tt.term}, &Formatter{Term: result})
			}
			if !reflect.DeepEqual(pvs, tt.vars) {
				t.Errorf("expected %v, got %v", tt.vars, pvs)
			}
		})
	}
}

func TestParser_Number(t *testing.T) {
	heap := make(term.Heap, 0, 1024)

	must := func(term term.Handle, err error) term.Handle {
		if err != nil {
			t.Fatal(err)
		}
		return term
	}

	tests := []struct {
		input  string
		number term.Handle
		err    error
	}{
		{input: `33`, number: must(heap.PutInteger(33))},
		{input: `-33`, number: must(heap.PutInteger(-33))},
		{input: `- 33`, number: must(heap.PutInteger(-33))},
		{input: `'-'33`, number: must(heap.PutInteger(-33))},
		{input: ` 33`, number: must(heap.PutInteger(33))},
		{input: `9223372036854775808.`, err: ErrIntAbove},
		{input: `-9223372036854775809.`, err: ErrIntBelow},

		{input: `0'!`, number: must(heap.PutInteger(33))},
		{input: `-0'!`, number: must(heap.PutInteger(-33))},
		{input: `- 0'!`, number: must(heap.PutInteger(-33))},
		{input: `'-'0'!`, number: must(heap.PutInteger(-33))},

		{input: `0b1`, number: must(heap.PutInteger(1))},
		{input: `0o1`, number: must(heap.PutInteger(1))},
		{input: `0x1`, number: must(heap.PutInteger(1))},

		{input: `3.3`, number: must(heap.PutFloat(3.3))},
		{input: `-3.3`, number: must(heap.PutFloat(-3.3))},
		{input: `- 3.3`, number: must(heap.PutFloat(-3.3))},
		{input: `'-'3.3`, number: must(heap.PutFloat(-3.3))},

		{input: ``, err: io.EOF},
		{input: `X`, err: ErrNotANumber},
		{input: `33 three`, err: ErrNotANumber},
		{input: `3 `, err: ErrNotANumber},
		{input: `3.`, err: ErrNotANumber},
		{input: `three`, err: ErrNotANumber},
		{input: `-`, err: ErrNotANumber},
		{input: `-a.`, err: ErrNotANumber},
		{input: `()`, err: ErrNotANumber},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			p := NewParser(strings.NewReader(tt.input), &heap, nil, nil)
			n, err := p.Number()
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected error %q, got %q", tt.err, err)
			}
			if n != tt.number {
				t.Errorf("expected %v, got %v", &Formatter{Term: tt.number}, &Formatter{Term: n})
			}
		})
	}
}

func TestParser_More(t *testing.T) {
	heap := make(term.Heap, 0, 1024)
	p := NewParser(strings.NewReader(`foo. bar.`), &heap, nil, nil)
	foo, _, err := p.Term()
	if err != nil {
		t.Fatal(err)
	}
	if a, ok := foo.Atom(); !ok || a != term.NewAtom("foo") {
		t.Errorf("expected foo, got %v", a)
	}

	if !p.More() {
		t.Fatal("expected more")
	}

	bar, _, err := p.Term()
	if err != nil {
		t.Fatal(err)
	}

	if a, ok := bar.Atom(); !ok || a != term.NewAtom("bar") {
		t.Errorf("expected bar, got %v", a)
	}

	if p.More() {
		t.Fatal("expected no more")
	}
}
