package syntax

import (
	"bytes"
	"reflect"
	"testing"

	"github.com/ichiban/prolog/v2/internal/term"
)

func TestFormatter_WriteTo(t *testing.T) {
	heap := make(term.Heap, 0, 1024)

	must := func(term term.Handle, err error) term.Handle {
		t.Helper()
		if err != nil {
			t.Fatal(err)
		}
		return term
	}

	x := must(heap.PutVariable()) // _0
	y := must(heap.PutVariable()) // _1
	a := must(heap.PutAtom(term.NewAtom("a")))
	b := must(heap.PutAtom(term.NewAtom("b")))
	c := must(heap.PutAtom(term.NewAtom("c")))
	X := must(heap.PutAtom(term.NewAtom("X")))
	rest := must(heap.PutAtom(term.NewAtom("rest")))
	escapeSequence := must(heap.PutAtom(term.NewAtom("\a\b\f\n\r\t\v\x00\\'\"`")))
	comma := must(heap.PutAtom(term.NewAtom(",")))
	emptyList := must(heap.PutAtom(term.NewAtom("[]")))
	emptyBlock := must(heap.PutAtom(term.NewAtom("{}")))
	minus := must(heap.PutAtom(term.NewAtom("-")))
	foo := must(heap.PutAtom(term.NewAtom("foo")))
	bar := must(heap.PutAtom(term.NewAtom("bar")))
	baz := must(heap.PutAtom(term.NewAtom("baz")))
	thirtyThree := must(heap.PutInteger(33))
	minusThirtyThree := must(heap.PutInteger(-33))
	zero := must(heap.PutInteger(0))
	one := must(heap.PutInteger(1))
	two := must(heap.PutInteger(2))
	twentyFive := must(heap.PutInteger(25))
	twentySix := must(heap.PutInteger(26))
	twentySeven := must(heap.PutInteger(27))
	minusTwo := must(heap.PutInteger(-2))
	floatThirtyThree := must(heap.PutFloat(33))
	floatWithE := must(heap.PutFloat(3.0e+100))
	floatMinusThirtyThree := must(heap.PutFloat(-33))
	list := must(heap.PutList(a, b, c))
	listish := must(heap.PutPartialList(rest, a, b))
	v := must(heap.PutVariable())
	circularList := must(heap.PutPartialList(v, a, b))
	if err := v.Bind(circularList); err != nil {
		t.Fatal(err)
	}
	curlyBrackets := must(heap.PutCompound(term.NewAtom("{}"), foo))
	ifFoo := must(heap.PutCompound(term.NewAtom(":-"), foo))
	ifIfFoo := must(heap.PutCompound(term.NewAtom(":-"), ifFoo))
	notFoo := must(heap.PutCompound(term.NewAtom(`\+`), foo))
	minusNotFoo := must(heap.PutCompound(term.NewAtom(`-`), notFoo))
	notMinusNotFoo := must(heap.PutCompound(term.NewAtom(`\+`), minusNotFoo))
	fiFoo := must(heap.PutCompound(term.NewAtom(`-:`), foo))
	fiFiFoo := must(heap.PutCompound(term.NewAtom(`-:`), fiFoo))
	tonFoo := must(heap.PutCompound(term.NewAtom(`+/`), foo))
	minusMinusTonFoo := must(heap.PutCompound(term.NewAtom(`--`), tonFoo))
	tonMinusMinusTonFoo := must(heap.PutCompound(term.NewAtom(`+/`), minusMinusTonFoo))
	ifBarBaz := must(heap.PutCompound(term.NewAtom(`:-`), bar, baz))
	ifFooIfBarBaz := must(heap.PutCompound(term.NewAtom(`:-`), foo, ifBarBaz))
	plusTwoTwo := must(heap.PutCompound(term.NewAtom(`+`), two, two))
	asteriskTwoPlusTwoTwo := must(heap.PutCompound(term.NewAtom(`*`), two, plusTwoTwo))
	barTwoTwo := must(heap.PutCompound(term.NewAtom(`|`), two, two))
	commaTwoBarTwoTwo := must(heap.PutCompound(term.NewAtom(`,`), two, barTwoTwo))
	plusTwoMinusTwo := must(heap.PutCompound(term.NewAtom(`+`), two, minusTwo))
	varZero := must(heap.PutCompound(term.NewAtom(`$VAR`), zero))
	varOne := must(heap.PutCompound(term.NewAtom(`$VAR`), one))
	varTwentyFive := must(heap.PutCompound(term.NewAtom(`$VAR`), twentyFive))
	varTwentySix := must(heap.PutCompound(term.NewAtom(`$VAR`), twentySix))
	varTwentySeven := must(heap.PutCompound(term.NewAtom(`$VAR`), twentySeven))
	fVars := must(heap.PutCompound(term.NewAtom(`f`), varZero, varOne, varTwentyFive, varTwentySix, varTwentySeven))
	minusB := must(heap.PutCompound(term.NewAtom(`-`), b))
	asteriskAMinusB := must(heap.PutCompound(term.NewAtom(`*`), a, minusB))
	tonA := must(heap.PutCompound(term.NewAtom(`+/`), a))
	minusTonA := must(heap.PutCompound(term.NewAtom(`-`), tonA))
	asteriskAB := must(heap.PutCompound(term.NewAtom(`*`), a, b))
	minusAsteriskAB := must(heap.PutCompound(term.NewAtom(`-`), asteriskAB))
	w := must(heap.PutVariable())
	r := must(heap.PutCompound(term.NewAtom("f"), w))
	if err := w.Bind(r); err != nil {
		t.Fatal(err)
	}
	isXY := must(heap.PutCompound(term.NewAtom("is"), x, y))
	minusMinus := must(heap.PutCompound(term.NewAtom("-"), minus))
	minusMinusMinus := must(heap.PutCompound(term.NewAtom("--"), minus))
	FXX := must(heap.PutCompound(term.NewAtom(`F`), X, X))
	isFooFoo := must(heap.PutCompound(term.NewAtom(`is`), foo, foo))
	unaryMinusThirtyThree := must(heap.PutCompound(term.NewAtom(`-`), thirtyThree))
	b0Zero := must(heap.PutCompound(term.NewAtom(`b0`), zero))
	o0Zero := must(heap.PutCompound(term.NewAtom(`o0`), zero))
	x0Zero := must(heap.PutCompound(term.NewAtom(`x0`), zero))
	FooZero := must(heap.PutCompound(term.NewAtom(`Foo`), zero))
	minusFloatThirtyThree := must(heap.PutCompound(term.NewAtom(`-`), floatThirtyThree))
	eFloatThirtyThree := must(heap.PutCompound(term.NewAtom(`e`), floatThirtyThree))

	var ops OperatorSet
	ops.Define(1200, XFX, term.NewAtom(`:-`))
	ops.Define(1200, FX, term.NewAtom(`:-`))
	ops.Define(1200, XF, term.NewAtom(`-:`))
	ops.Define(1105, XFY, term.NewAtom(`|`))
	ops.Define(1000, XFY, term.NewAtom(`,`))
	ops.Define(900, FY, term.NewAtom(`\+`))
	ops.Define(900, YF, term.NewAtom(`+/`))
	ops.Define(700, XFX, term.NewAtom(`is`))
	ops.Define(700, XFX, term.NewAtom(`F`))
	ops.Define(500, YFX, term.NewAtom(`+`))
	ops.Define(400, YFX, term.NewAtom(`*`))
	ops.Define(200, FY, term.NewAtom("+"))
	ops.Define(200, FY, term.NewAtom(`-`))
	ops.Define(200, YF, term.NewAtom(`--`))
	ops.Define(200, YF, term.NewAtom(`b0`))
	ops.Define(200, YF, term.NewAtom(`o0`))
	ops.Define(200, YF, term.NewAtom(`x0`))
	ops.Define(200, YF, term.NewAtom(`Foo`))
	ops.Define(200, YF, term.NewAtom(`e`))

	tests := []struct {
		title     string
		formatter Formatter
		output    string
		err       error
	}{
		{title: "variable: unnamed", formatter: Formatter{Term: x}, output: "_0"},
		{title: "variable: variable_names", formatter: Formatter{Term: x, VariableName: map[term.Handle]term.Atom{
			x: term.NewAtom("Foo"),
		}}, output: `Foo`},

		{title: "atom: a", formatter: Formatter{Term: a, Quoted: false}, output: `a`},
		{title: "atom: a with quoted", formatter: Formatter{Term: a, Quoted: true}, output: `a`},
		{title: "atom: escape sequence", formatter: Formatter{Term: escapeSequence, Quoted: false}, output: "\a\b\f\n\r\t\v\x00\\'\"`"},
		{title: "atom: escape sequence with quoted", formatter: Formatter{Term: escapeSequence, Quoted: true}, output: "'\\a\\b\\f\\n\\r\\t\\v\\x0\\\\\\\\'\"`'"},
		{title: "atom: comma", formatter: Formatter{Term: comma, Quoted: false}, output: `,`},
		{title: "atom: comma with quoted", formatter: Formatter{Term: comma, Quoted: true}, output: `','`},
		{title: "atom: empty list", formatter: Formatter{Term: emptyList, Quoted: false}, output: `[]`},
		{title: "atom: empty list with quoted", formatter: Formatter{Term: emptyList, Quoted: true}, output: `[]`},
		{title: "atom: empty block", formatter: Formatter{Term: emptyBlock, Quoted: false}, output: `{}`},
		{title: "atom: empty block with quoted", formatter: Formatter{Term: emptyBlock, Quoted: true}, output: `{}`},
		{title: "atom: minus", formatter: Formatter{Term: minus}, output: `-`},

		{title: "integer: positive", formatter: Formatter{Term: thirtyThree}, output: `33`},
		{title: "integer: negative", formatter: Formatter{Term: minusThirtyThree}, output: `-33`},

		{title: "float: positive", formatter: Formatter{Term: floatThirtyThree, Precision: -1}, output: `33.0`},
		{title: "float: with e", formatter: Formatter{Term: floatWithE, Precision: -1}, output: `3.0e+100`},
		{title: "float: negative", formatter: Formatter{Term: floatMinusThirtyThree, Precision: -1}, output: `-33.0`},

		{title: "compound: list", formatter: Formatter{Term: list}, output: `[a,b,c]`},
		{title: "compound: list-ish", formatter: Formatter{Term: listish}, output: `[a,b|rest]`},
		{title: "compound: circular list", formatter: Formatter{Term: circularList}, output: `[a,b,a|...]`},
		{title: "compound: curly brackets", formatter: Formatter{Term: curlyBrackets}, output: `{foo}`},
		{title: "compound: fx", formatter: Formatter{Term: ifIfFoo, Ops: &ops}, output: `:- (:-foo)`},
		{title: "compound: fy", formatter: Formatter{Term: notMinusNotFoo, Ops: &ops}, output: `\+ - (\+foo)`},
		{title: "compound: xf", formatter: Formatter{Term: fiFiFoo, Ops: &ops}, output: `(foo-:)-:`},
		{title: "compound: yf", formatter: Formatter{Term: tonMinusMinusTonFoo, Ops: &ops}, output: `(foo+/)-- +/`},
		{title: "compound: xfx", formatter: Formatter{Term: ifFooIfBarBaz, Ops: &ops}, output: `foo:-(bar:-baz)`},
		{title: "compound: yfx", formatter: Formatter{Term: asteriskTwoPlusTwoTwo, Ops: &ops}, output: `2*(2+2)`},
		{title: "compound: xfy", formatter: Formatter{Term: commaTwoBarTwoTwo, Ops: &ops}, output: `2,(2|2)`},
		{title: "compound: ignore_ops(false)", formatter: Formatter{Term: plusTwoMinusTwo, IgnoreOps: false, Ops: &ops}, output: `2+ -2`},
		{title: "compound: ignore_ops(true)", formatter: Formatter{Term: plusTwoMinusTwo, IgnoreOps: true, Ops: &ops}, output: `+(2,-2)`},
		{title: "compound: number_vars(false)", formatter: Formatter{Term: fVars, Quoted: true, NumberVars: false, Ops: &ops}, output: `f('$VAR'(0),'$VAR'(1),'$VAR'(25),'$VAR'(26),'$VAR'(27))`},
		{title: "compound: number_vars(true)", formatter: Formatter{Term: fVars, Quoted: true, NumberVars: true, Ops: &ops}, output: `f(A,B,Z,A1,B1)`},
		{title: "compound: prefix: spacing between operators", formatter: Formatter{Term: asteriskAMinusB, Ops: &ops}, output: `a* -b`},
		{title: "compound: postfix: spacing between unary minus and open/close", formatter: Formatter{Term: minusTonA, Ops: &ops}, output: `- (a+/)`},
		{title: "compound: infix: spacing between unary minus and open/close", formatter: Formatter{Term: minusAsteriskAB, Ops: &ops}, output: `- (a*b)`},
		{title: "compound: recursive", formatter: Formatter{Term: r}, output: `f(...)`},
		{title: "compound: variable following/followed by a letter-digit operator", formatter: Formatter{Term: isXY, Ops: &ops}, output: "_0 is _1"},
		{title: "compound: atom minus right after an operator", formatter: Formatter{Term: minusMinus, Ops: &ops}, output: `- (-)`},
		{title: "compound: atom minus right before an operator", formatter: Formatter{Term: minusMinusMinus, Ops: &ops}, output: `(-)--`},
		{title: "compound: atom X right before/after an operator that requires quotes", formatter: Formatter{Term: FXX, Quoted: true, Ops: &ops}, output: `'X' 'F' 'X'`},
		{title: "compound: atom foo right before/after a letter-digit operator", formatter: Formatter{Term: isFooFoo, Ops: &ops}, output: `foo is foo`}, // So that it won't be barfoo.
		{title: "compound: positive integer following unary minus", formatter: Formatter{Term: unaryMinusThirtyThree, Ops: &ops}, output: `- (33)`},
		{title: "compound: integer ambiguous 0b", formatter: Formatter{Term: b0Zero, Ops: &ops}, output: `0 b0`},                   // So that it won't be 0b0.
		{title: "compound: integer ambiguous 0o", formatter: Formatter{Term: o0Zero, Ops: &ops}, output: `0 o0`},                   // So that it won't be 0o0.
		{title: "compound: integer ambiguous 0x", formatter: Formatter{Term: x0Zero, Ops: &ops}, output: `0 x0`},                   // So that it won't be 0x0.
		{title: "compound: integer ambiguous 0'", formatter: Formatter{Term: FooZero, Quoted: true, Ops: &ops}, output: `0 'Foo'`}, // So that it won't be 0'Foo'.
		{title: "float: positive following unary minus", formatter: Formatter{Term: minusFloatThirtyThree, Ops: &ops, Precision: -1}, output: `- (33.0)`},
		{title: "float: ambiguous e", formatter: Formatter{Term: eFloatThirtyThree, Ops: &ops, Precision: -1}, output: `33.0 e`}, // So that it won't be 33.0e.
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			var buf bytes.Buffer
			_, err := tt.formatter.WriteTo(&buf)
			if !reflect.DeepEqual(tt.err, err) {
				t.Errorf("want %v, got %v", tt.err, err)
			}

			if tt.output != buf.String() {
				t.Errorf("want %s, got %s", tt.output, buf.String())
			}
		})
	}
}
