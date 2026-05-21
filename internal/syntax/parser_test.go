package syntax

import (
	"io"
	"reflect"
	"testing"

	"github.com/ichiban/prolog/v2/internal/term"
)

func Test_ParseTerm(t *testing.T) {
	arena := term.Arena{
		Heap: make(term.Heap, 0, 1024),
	}

	must := func(term term.Handle, err error) term.Handle {
		t.Helper()
		if err != nil {
			t.Fatal(err)
		}
		return term
	}

	x := must(arena.PutVariable())

	var ops OperatorSet
	ops.Define(1000, XFY, term.NewAtom(`,`))
	ops.Define(500, YFX, term.NewAtom(`+`))
	ops.Define(400, YFX, term.NewAtom(`*`))
	ops.Define(200, FY, term.NewAtom(`-`))
	ops.Define(200, YF, term.NewAtom(`--`))

	tests := []struct {
		input        string
		doubleQuotes DoubleQuotes
		term         term.Handle
		err          error
		vars         []ParsedVariable
	}{
		{input: ``, err: io.EOF},
		{input: `foo`, err: io.EOF},
		{input: `.`, err: &UnexpectedTokenError{token: token{kind: tokenEnd, val: "."}}},

		{input: `(foo).`, term: must(arena.PutAtom(term.NewAtom("foo")))},
		{input: `(a b).`, err: &UnexpectedTokenError{token: token{kind: tokenLetterDigit, val: "b"}}},

		{input: `foo.`, term: must(arena.PutAtom(term.NewAtom("foo")))},
		{input: `[].`, term: must(arena.PutAtom(term.NewAtom("[]")))},
		{input: `[ ].`, term: must(arena.PutAtom(term.NewAtom("[]")))},
		{input: `{}.`, term: must(arena.PutAtom(term.NewAtom("{}")))},
		{input: `{ }.`, term: must(arena.PutAtom(term.NewAtom("{}")))},
		{input: `'abc'.`, term: must(arena.PutAtom(term.NewAtom("abc")))},
		{input: `'don''t panic'.`, term: must(arena.PutAtom(term.NewAtom("don't panic")))},
		{input: "'this is \\\na quoted ident'.", term: must(arena.PutAtom(term.NewAtom("this is a quoted ident")))},
		{input: `'\a'.`, term: must(arena.PutAtom(term.NewAtom("\a")))},
		{input: `'\b'.`, term: must(arena.PutAtom(term.NewAtom("\b")))},
		{input: `'\f'.`, term: must(arena.PutAtom(term.NewAtom("\f")))},
		{input: `'\n'.`, term: must(arena.PutAtom(term.NewAtom("\n")))},
		{input: `'\r'.`, term: must(arena.PutAtom(term.NewAtom("\r")))},
		{input: `'\t'.`, term: must(arena.PutAtom(term.NewAtom("\t")))},
		{input: `'\v'.`, term: must(arena.PutAtom(term.NewAtom("\v")))},
		{input: `'\43\'.`, term: must(arena.PutAtom(term.NewAtom("#")))},
		{input: `'\xa3\'.`, term: must(arena.PutAtom(term.NewAtom("£")))},
		{input: `'\\'.`, term: must(arena.PutAtom(term.NewAtom(`\`)))},
		{input: `'\''.`, term: must(arena.PutAtom(term.NewAtom(`'`)))},
		{input: `'\"'.`, term: must(arena.PutAtom(term.NewAtom(`"`)))},
		{input: "'\\`'.", term: must(arena.PutAtom(term.NewAtom("`")))},
		{input: `[`, err: io.EOF},
		{input: `{`, err: io.EOF},

		{input: `1.`, term: must(arena.PutInteger(1))},
		{input: `0'1.`, term: must(arena.PutInteger(49))},
		{input: `0b1.`, term: must(arena.PutInteger(1))},
		{input: `0o1.`, term: must(arena.PutInteger(1))},
		{input: `0x1.`, term: must(arena.PutInteger(1))},
		{input: `-1.`, term: must(arena.PutInteger(-1))},
		{input: `- 1.`, term: must(arena.PutInteger(-1))},
		{input: `'-'1.`, term: must(arena.PutInteger(-1))},
		{input: `9223372036854775808.`, err: ErrIntBelow},
		{input: `-9223372036854775809.`, err: ErrIntAbove},
		{input: `-`, err: io.EOF},
		{input: `- -`, err: io.EOF},

		{input: `1.0.`, term: must(arena.PutFloat(1))},
		{input: `-1.0.`, term: must(arena.PutFloat(-1))},
		{input: `- 1.0.`, term: must(arena.PutFloat(-1))},
		{input: `'-'1.0.`, term: must(arena.PutFloat(-1))},

		{input: `_.`, term: x},
		{input: `X.`, term: x, vars: []ParsedVariable{
			{Name: "X", Variable: x, Count: 1},
		}},

		{input: `foo(a, b).`, term: must(arena.PutCompound(term.NewAtom("foo"), must(arena.PutAtom(term.NewAtom("a"))), must(arena.PutAtom(term.NewAtom("b")))))},
		{input: `foo(-(a)).`, term: must(arena.PutCompound(term.NewAtom("foo"), must(arena.PutCompound(term.NewAtom("-"), must(arena.PutAtom(term.NewAtom("a")))))))},
		{input: `foo(-).`, term: must(arena.PutCompound(term.NewAtom("foo"), must(arena.PutAtom(term.NewAtom("-")))))},
		{input: `foo((), b).`, err: &UnexpectedTokenError{token: token{kind: tokenClose, val: ")"}}},
		{input: `foo([]).`, term: must(arena.PutCompound(term.NewAtom("foo"), must(arena.PutAtom(term.NewAtom("[]")))))},
		{input: `foo(a, ()).`, err: &UnexpectedTokenError{token: token{kind: tokenClose, val: ")"}}},
		{input: `foo(a b).`, err: &UnexpectedTokenError{token: token{kind: tokenLetterDigit, val: "b"}}},
		{input: `foo(a, b`, err: io.EOF},

		{input: `[a, b].`, term: must(arena.PutList(must(arena.PutAtom(term.NewAtom("a"))), must(arena.PutAtom(term.NewAtom("b")))))},
		{input: `[(), b].`, err: &UnexpectedTokenError{token: token{kind: tokenClose, val: ")"}}},
		{input: `[a, ()].`, err: &UnexpectedTokenError{token: token{kind: tokenClose, val: ")"}}},
		{input: `[a b].`, err: &UnexpectedTokenError{token: token{kind: tokenLetterDigit, val: "b"}}},
		{input: `[a|X].`, term: must(arena.PutCompound(term.NewAtom("."), must(arena.PutAtom(term.NewAtom("a"))), x)), vars: []ParsedVariable{
			{Name: "X", Variable: x, Count: 1},
		}},
		{input: `[a, b|X].`, term: must(arena.PutPartialList(x, must(arena.PutAtom(term.NewAtom("a"))), must(arena.PutAtom(term.NewAtom("b"))))), vars: []ParsedVariable{
			{Name: "X", Variable: x, Count: 1},
		}},
		{input: `[a, b|()].`, err: &UnexpectedTokenError{token: token{kind: tokenClose, val: ")"}}},
		{input: `[a, b|c d].`, err: &UnexpectedTokenError{token: token{kind: tokenLetterDigit, val: "d"}}},
		{input: `[a `, err: io.EOF},

		{input: `{a}.`, term: must(arena.PutCompound(term.NewAtom("{}"), must(arena.PutAtom(term.NewAtom("a")))))},
		{input: `{()}.`, err: &UnexpectedTokenError{token: token{kind: tokenClose, val: ")"}}},
		{input: `{a b}.`, err: &UnexpectedTokenError{token: token{kind: tokenLetterDigit, val: "b"}}},

		{input: `-a.`, term: must(arena.PutCompound(term.NewAtom("-"), must(arena.PutAtom(term.NewAtom("a")))))},
		{input: `- .`, term: must(arena.PutAtom(term.NewAtom("-")))},

		{input: `a-- .`, term: must(arena.PutCompound(term.NewAtom("--"), must(arena.PutAtom(term.NewAtom("a")))))},

		{input: `a + b.`, term: must(arena.PutCompound(term.NewAtom("+"), must(arena.PutAtom(term.NewAtom("a"))), must(arena.PutAtom(term.NewAtom("b")))))},
		{input: `a + ().`, err: &UnexpectedTokenError{token: token{kind: tokenClose, val: ")"}}},
		{input: `a * b + c.`, term: must(arena.PutCompound(term.NewAtom("+"), must(arena.PutCompound(term.NewAtom("*"), must(arena.PutAtom(term.NewAtom("a"))), must(arena.PutAtom(term.NewAtom("b"))))), must(arena.PutAtom(term.NewAtom("c")))))},
		{input: `a [] b.`, err: &UnexpectedTokenError{token: token{kind: tokenOpenList, val: "["}}},
		{input: `a {} b.`, err: &UnexpectedTokenError{token: token{kind: tokenOpenCurly, val: "{"}}},
		{input: `a, b.`, term: must(arena.PutCompound(term.NewAtom(","), must(arena.PutAtom(term.NewAtom("a"))), must(arena.PutAtom(term.NewAtom("b")))))},
		{input: `+ * + .`, err: &UnexpectedTokenError{token: token{kind: tokenGraphic, val: "+"}}},

		{input: `"abc".`, doubleQuotes: DoubleQuotesChars, term: must(arena.PutCharList("abc"))},
		{input: `"abc".`, doubleQuotes: DoubleQuotesCodes, term: must(arena.PutCodeList("abc"))},
		{input: `"abc".`, doubleQuotes: DoubleQuotesAtom, term: must(arena.PutAtom(term.NewAtom("abc")))},
		{input: `"don""t panic".`, doubleQuotes: DoubleQuotesAtom, term: must(arena.PutAtom(term.NewAtom("don\"t panic")))},
		{input: "\"this is \\\na double-quoted string\".", doubleQuotes: DoubleQuotesAtom, term: must(arena.PutAtom(term.NewAtom("this is a double-quoted string")))},
		{input: `"\a".`, doubleQuotes: DoubleQuotesAtom, term: must(arena.PutAtom(term.NewAtom("\a")))},
		{input: `"\b".`, doubleQuotes: DoubleQuotesAtom, term: must(arena.PutAtom(term.NewAtom("\b")))},
		{input: `"\f".`, doubleQuotes: DoubleQuotesAtom, term: must(arena.PutAtom(term.NewAtom("\f")))},
		{input: `"\n".`, doubleQuotes: DoubleQuotesAtom, term: must(arena.PutAtom(term.NewAtom("\n")))},
		{input: `"\r".`, doubleQuotes: DoubleQuotesAtom, term: must(arena.PutAtom(term.NewAtom("\r")))},
		{input: `"\t".`, doubleQuotes: DoubleQuotesAtom, term: must(arena.PutAtom(term.NewAtom("\t")))},
		{input: `"\v".`, doubleQuotes: DoubleQuotesAtom, term: must(arena.PutAtom(term.NewAtom("\v")))},
		{input: `"\xa3\".`, doubleQuotes: DoubleQuotesAtom, term: must(arena.PutAtom(term.NewAtom("£")))},
		{input: `"\43\".`, doubleQuotes: DoubleQuotesAtom, term: must(arena.PutAtom(term.NewAtom("#")))},
		{input: `"\\".`, doubleQuotes: DoubleQuotesAtom, term: must(arena.PutAtom(term.NewAtom(`\`)))},
		{input: `"\'".`, doubleQuotes: DoubleQuotesAtom, term: must(arena.PutAtom(term.NewAtom(`'`)))},
		{input: `"\"".`, doubleQuotes: DoubleQuotesAtom, term: must(arena.PutAtom(term.NewAtom(`"`)))},
		{input: "\"\\`\".", doubleQuotes: DoubleQuotesAtom, term: must(arena.PutAtom(term.NewAtom("`")))},

		// https://github.com/ichiban/prolog/issues/219#issuecomment-1200489336
		{input: `write('[]').`, term: must(arena.PutCompound(term.NewAtom("write"), must(arena.PutAtom(term.NewAtom("[]")))))},
		{input: `write('{}').`, term: must(arena.PutCompound(term.NewAtom("write"), must(arena.PutAtom(term.NewAtom("{}")))))},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			var pvs []ParsedVariable
			result, err := ParseTerm(tt.input,
				Arena(&arena),
				Operators(&ops),
				DoubleQuote(&tt.doubleQuotes),
				Variables(&pvs),
				MakeVariable(func() (term.Handle, error) {
					return x, nil
				}),
			)
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected error %q, got %q", tt.err, err)
			}
			if arena.Compare(result, tt.term) != 0 {
				t.Errorf("expected %4q, got %4q", &Formatter{Term: tt.term}, &Formatter{Term: result})
			}
			if len(pvs) != len(tt.vars) {
				t.Errorf("expected %d, got %d", len(tt.vars), len(pvs))
			}
			for i := range len(pvs) {
				if pvs[i] != tt.vars[i] {
					t.Errorf("expected %v, got %v", tt.vars[i], pvs[i])
				}
			}
		})
	}
}

func Test_ParseNumber(t *testing.T) {
	arena := term.Arena{
		Heap: make(term.Heap, 0, 1024),
	}

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
		{input: `33`, number: must(arena.PutInteger(33))},
		{input: `-33`, number: must(arena.PutInteger(-33))},
		{input: `- 33`, number: must(arena.PutInteger(-33))},
		{input: `'-'33`, number: must(arena.PutInteger(-33))},
		{input: ` 33`, number: must(arena.PutInteger(33))},
		{input: `9223372036854775808.`, err: ErrIntBelow},
		{input: `-9223372036854775809.`, err: ErrIntAbove},

		{input: `0'!`, number: must(arena.PutInteger(33))},
		{input: `-0'!`, number: must(arena.PutInteger(-33))},
		{input: `- 0'!`, number: must(arena.PutInteger(-33))},
		{input: `'-'0'!`, number: must(arena.PutInteger(-33))},

		{input: `0b1`, number: must(arena.PutInteger(1))},
		{input: `0o1`, number: must(arena.PutInteger(1))},
		{input: `0x1`, number: must(arena.PutInteger(1))},

		{input: `3.3`, number: must(arena.PutFloat(3.3))},
		{input: `-3.3`, number: must(arena.PutFloat(-3.3))},
		{input: `- 3.3`, number: must(arena.PutFloat(-3.3))},
		{input: `'-'3.3`, number: must(arena.PutFloat(-3.3))},

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
			n, err := ParseNumber(tt.input, Arena(&arena))
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("expected error %q, got %q", tt.err, err)
			}
			if arena.Compare(n, tt.number) != 0 {
				t.Errorf("expected %v, got %v", &Formatter{Term: tt.number}, &Formatter{Term: n})
			}
		})
	}
}
