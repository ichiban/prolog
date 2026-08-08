package syntax

import (
	"bytes"
	"reflect"
	"testing"

	"github.com/ichiban/prolog/v2/internal/term"
)

func TestFormatter_WriteTo(t *testing.T) {
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

	x := must(arena.PutVariable()) // _0
	y := must(arena.PutVariable()) // _1
	a := must(arena.PutAtom(term.NewAtom("a")))
	b := must(arena.PutAtom(term.NewAtom("b")))
	c := must(arena.PutAtom(term.NewAtom("c")))
	X := must(arena.PutAtom(term.NewAtom("X")))
	rest := must(arena.PutAtom(term.NewAtom("rest")))
	escapeSequence := must(arena.PutAtom(term.NewAtom("\a\b\f\n\r\t\v\x00\\'\"`")))
	comma := must(arena.PutAtom(term.NewAtom(",")))
	emptyList := must(arena.PutAtom(term.NewAtom("[]")))
	emptyBlock := must(arena.PutAtom(term.NewAtom("{}")))
	minus := must(arena.PutAtom(term.NewAtom("-")))
	foo := must(arena.PutAtom(term.NewAtom("foo")))
	bar := must(arena.PutAtom(term.NewAtom("bar")))
	baz := must(arena.PutAtom(term.NewAtom("baz")))
	thirtyThree := must(arena.PutInteger(33))
	minusThirtyThree := must(arena.PutInteger(-33))
	zero := must(arena.PutInteger(0))
	one := must(arena.PutInteger(1))
	two := must(arena.PutInteger(2))
	twentyFive := must(arena.PutInteger(25))
	twentySix := must(arena.PutInteger(26))
	twentySeven := must(arena.PutInteger(27))
	minusTwo := must(arena.PutInteger(-2))
	floatThirtyThree := must(arena.PutFloat(33))
	floatWithE := must(arena.PutFloat(3.0e+100))
	floatMinusThirtyThree := must(arena.PutFloat(-33))
	list := must(arena.PutList(a, b, c))
	listish := must(arena.PutPartialList(rest, a, b))
	v := must(arena.PutVariable())
	circularList := must(arena.PutPartialList(v, a, b))
	if err := arena.Bind(v, circularList); err != nil {
		t.Fatal(err)
	}
	curlyBrackets := must(arena.PutCompound(term.NewAtom("{}"), foo))
	ifFoo := must(arena.PutCompound(term.NewAtom(":-"), foo))
	ifIfFoo := must(arena.PutCompound(term.NewAtom(":-"), ifFoo))
	notFoo := must(arena.PutCompound(term.NewAtom(`\+`), foo))
	minusNotFoo := must(arena.PutCompound(term.NewAtom(`-`), notFoo))
	notMinusNotFoo := must(arena.PutCompound(term.NewAtom(`\+`), minusNotFoo))
	fiFoo := must(arena.PutCompound(term.NewAtom(`-:`), foo))
	fiFiFoo := must(arena.PutCompound(term.NewAtom(`-:`), fiFoo))
	tonFoo := must(arena.PutCompound(term.NewAtom(`+/`), foo))
	minusMinusTonFoo := must(arena.PutCompound(term.NewAtom(`--`), tonFoo))
	tonMinusMinusTonFoo := must(arena.PutCompound(term.NewAtom(`+/`), minusMinusTonFoo))
	ifBarBaz := must(arena.PutCompound(term.NewAtom(`:-`), bar, baz))
	ifFooIfBarBaz := must(arena.PutCompound(term.NewAtom(`:-`), foo, ifBarBaz))
	plusTwoTwo := must(arena.PutCompound(term.NewAtom(`+`), two, two))
	asteriskTwoPlusTwoTwo := must(arena.PutCompound(term.NewAtom(`*`), two, plusTwoTwo))
	barTwoTwo := must(arena.PutCompound(term.NewAtom(`|`), two, two))
	commaTwoBarTwoTwo := must(arena.PutCompound(term.NewAtom(`,`), two, barTwoTwo))
	plusTwoMinusTwo := must(arena.PutCompound(term.NewAtom(`+`), two, minusTwo))
	varZero := must(arena.PutCompound(term.NewAtom(`$VAR`), zero))
	varOne := must(arena.PutCompound(term.NewAtom(`$VAR`), one))
	varTwentyFive := must(arena.PutCompound(term.NewAtom(`$VAR`), twentyFive))
	varTwentySix := must(arena.PutCompound(term.NewAtom(`$VAR`), twentySix))
	varTwentySeven := must(arena.PutCompound(term.NewAtom(`$VAR`), twentySeven))
	fVars := must(arena.PutCompound(term.NewAtom(`f`), varZero, varOne, varTwentyFive, varTwentySix, varTwentySeven))
	minusB := must(arena.PutCompound(term.NewAtom(`-`), b))
	asteriskAMinusB := must(arena.PutCompound(term.NewAtom(`*`), a, minusB))
	tonA := must(arena.PutCompound(term.NewAtom(`+/`), a))
	minusTonA := must(arena.PutCompound(term.NewAtom(`-`), tonA))
	asteriskAB := must(arena.PutCompound(term.NewAtom(`*`), a, b))
	minusAsteriskAB := must(arena.PutCompound(term.NewAtom(`-`), asteriskAB))
	w := must(arena.PutVariable())
	r := must(arena.PutCompound(term.NewAtom("f"), w))
	if err := arena.Bind(w, r); err != nil {
		t.Fatal(err)
	}
	isXY := must(arena.PutCompound(term.NewAtom("is"), x, y))
	minusMinus := must(arena.PutCompound(term.NewAtom("-"), minus))
	minusMinusMinus := must(arena.PutCompound(term.NewAtom("--"), minus))
	FXX := must(arena.PutCompound(term.NewAtom(`F`), X, X))
	isFooFoo := must(arena.PutCompound(term.NewAtom(`is`), foo, foo))
	unaryMinusThirtyThree := must(arena.PutCompound(term.NewAtom(`-`), thirtyThree))
	b0Zero := must(arena.PutCompound(term.NewAtom(`b0`), zero))
	o0Zero := must(arena.PutCompound(term.NewAtom(`o0`), zero))
	x0Zero := must(arena.PutCompound(term.NewAtom(`x0`), zero))
	FooZero := must(arena.PutCompound(term.NewAtom(`Foo`), zero))
	minusFloatThirtyThree := must(arena.PutCompound(term.NewAtom(`-`), floatThirtyThree))
	eFloatThirtyThree := must(arena.PutCompound(term.NewAtom(`e`), floatThirtyThree))

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
		{title: "variable: unnamed", formatter: Formatter{Arena: &arena, Term: x}, output: "_0"},
		{title: "variable: variable_names", formatter: Formatter{Arena: &arena, Term: x, VariableNames: []term.VariableName{
			{Variable: x, Name: "Foo"},
		}}, output: `Foo`},

		{title: "atom: a", formatter: Formatter{Arena: &arena, Term: a, Quoted: false}, output: `a`},
		{title: "atom: a with quoted", formatter: Formatter{Arena: &arena, Term: a, Quoted: true}, output: `a`},
		{title: "atom: escape sequence", formatter: Formatter{Arena: &arena, Term: escapeSequence, Quoted: false}, output: "\a\b\f\n\r\t\v\x00\\'\"`"},
		{title: "atom: escape sequence with quoted", formatter: Formatter{Arena: &arena, Term: escapeSequence, Quoted: true}, output: "'\\a\\b\\f\\n\\r\\t\\v\\x0\\\\\\\\'\"`'"},
		{title: "atom: comma", formatter: Formatter{Arena: &arena, Term: comma, Quoted: false}, output: `,`},
		{title: "atom: comma with quoted", formatter: Formatter{Arena: &arena, Term: comma, Quoted: true}, output: `','`},
		{title: "atom: empty list", formatter: Formatter{Arena: &arena, Term: emptyList, Quoted: false}, output: `[]`},
		{title: "atom: empty list with quoted", formatter: Formatter{Arena: &arena, Term: emptyList, Quoted: true}, output: `[]`},
		{title: "atom: empty block", formatter: Formatter{Arena: &arena, Term: emptyBlock, Quoted: false}, output: `{}`},
		{title: "atom: empty block with quoted", formatter: Formatter{Arena: &arena, Term: emptyBlock, Quoted: true}, output: `{}`},
		{title: "atom: minus", formatter: Formatter{Arena: &arena, Term: minus}, output: `-`},

		{title: "integer: positive", formatter: Formatter{Arena: &arena, Term: thirtyThree}, output: `33`},
		{title: "integer: negative", formatter: Formatter{Arena: &arena, Term: minusThirtyThree}, output: `-33`},

		{title: "float: positive", formatter: Formatter{Arena: &arena, Term: floatThirtyThree, Precision: -1}, output: `33.0`},
		{title: "float: with e", formatter: Formatter{Arena: &arena, Term: floatWithE, Precision: -1}, output: `3.0e+100`},
		{title: "float: negative", formatter: Formatter{Arena: &arena, Term: floatMinusThirtyThree, Precision: -1}, output: `-33.0`},

		{title: "compound: list", formatter: Formatter{Arena: &arena, Term: list}, output: `[a,b,c]`},
		{title: "compound: list-ish", formatter: Formatter{Arena: &arena, Term: listish}, output: `[a,b|rest]`},
		{title: "compound: circular list", formatter: Formatter{Arena: &arena, Term: circularList}, output: `[a,b,a|...]`},
		{title: "compound: curly brackets", formatter: Formatter{Arena: &arena, Term: curlyBrackets}, output: `{foo}`},
		{title: "compound: fx", formatter: Formatter{Arena: &arena, Term: ifIfFoo, Ops: &ops}, output: `:- (:-foo)`},
		{title: "compound: fy", formatter: Formatter{Arena: &arena, Term: notMinusNotFoo, Ops: &ops}, output: `\+ - (\+foo)`},
		{title: "compound: xf", formatter: Formatter{Arena: &arena, Term: fiFiFoo, Ops: &ops}, output: `(foo-:)-:`},
		{title: "compound: yf", formatter: Formatter{Arena: &arena, Term: tonMinusMinusTonFoo, Ops: &ops}, output: `(foo+/)-- +/`},
		{title: "compound: xfx", formatter: Formatter{Arena: &arena, Term: ifFooIfBarBaz, Ops: &ops}, output: `foo:-(bar:-baz)`},
		{title: "compound: yfx", formatter: Formatter{Arena: &arena, Term: asteriskTwoPlusTwoTwo, Ops: &ops}, output: `2*(2+2)`},
		{title: "compound: xfy", formatter: Formatter{Arena: &arena, Term: commaTwoBarTwoTwo, Ops: &ops}, output: `2,(2|2)`},
		{title: "compound: ignore_ops(false)", formatter: Formatter{Arena: &arena, Term: plusTwoMinusTwo, IgnoreOps: false, Ops: &ops}, output: `2+ -2`},
		{title: "compound: ignore_ops(true)", formatter: Formatter{Arena: &arena, Term: plusTwoMinusTwo, IgnoreOps: true, Ops: &ops}, output: `+(2,-2)`},
		{title: "compound: number_vars(false)", formatter: Formatter{Arena: &arena, Term: fVars, Quoted: true, NumberVars: false, Ops: &ops}, output: `f('$VAR'(0),'$VAR'(1),'$VAR'(25),'$VAR'(26),'$VAR'(27))`},
		{title: "compound: number_vars(true)", formatter: Formatter{Arena: &arena, Term: fVars, Quoted: true, NumberVars: true, Ops: &ops}, output: `f(A,B,Z,A1,B1)`},
		{title: "compound: prefix: spacing between operators", formatter: Formatter{Arena: &arena, Term: asteriskAMinusB, Ops: &ops}, output: `a* -b`},
		{title: "compound: postfix: spacing between unary minus and open/close", formatter: Formatter{Arena: &arena, Term: minusTonA, Ops: &ops}, output: `- (a+/)`},
		{title: "compound: infix: spacing between unary minus and open/close", formatter: Formatter{Arena: &arena, Term: minusAsteriskAB, Ops: &ops}, output: `- (a*b)`},
		{title: "compound: recursive", formatter: Formatter{Arena: &arena, Term: r}, output: `f(...)`},
		{title: "compound: variable following/followed by a letter-digit operator", formatter: Formatter{Arena: &arena, Term: isXY, Ops: &ops}, output: "_0 is _1"},
		{title: "compound: atom minus right after an operator", formatter: Formatter{Arena: &arena, Term: minusMinus, Ops: &ops}, output: `- (-)`},
		{title: "compound: atom minus right before an operator", formatter: Formatter{Arena: &arena, Term: minusMinusMinus, Ops: &ops}, output: `(-)--`},
		{title: "compound: atom X right before/after an operator that requires quotes", formatter: Formatter{Arena: &arena, Term: FXX, Quoted: true, Ops: &ops}, output: `'X' 'F' 'X'`},
		{title: "compound: atom foo right before/after a letter-digit operator", formatter: Formatter{Arena: &arena, Term: isFooFoo, Ops: &ops}, output: `foo is foo`}, // So that it won't be barfoo.
		{title: "compound: positive integer following unary minus", formatter: Formatter{Arena: &arena, Term: unaryMinusThirtyThree, Ops: &ops}, output: `- (33)`},
		{title: "compound: integer ambiguous 0b", formatter: Formatter{Arena: &arena, Term: b0Zero, Ops: &ops}, output: `0 b0`},                   // So that it won't be 0b0.
		{title: "compound: integer ambiguous 0o", formatter: Formatter{Arena: &arena, Term: o0Zero, Ops: &ops}, output: `0 o0`},                   // So that it won't be 0o0.
		{title: "compound: integer ambiguous 0x", formatter: Formatter{Arena: &arena, Term: x0Zero, Ops: &ops}, output: `0 x0`},                   // So that it won't be 0x0.
		{title: "compound: integer ambiguous 0'", formatter: Formatter{Arena: &arena, Term: FooZero, Quoted: true, Ops: &ops}, output: `0 'Foo'`}, // So that it won't be 0'Foo'.
		{title: "float: positive following unary minus", formatter: Formatter{Arena: &arena, Term: minusFloatThirtyThree, Ops: &ops, Precision: -1}, output: `- (33.0)`},
		{title: "float: ambiguous e", formatter: Formatter{Arena: &arena, Term: eFloatThirtyThree, Ops: &ops, Precision: -1}, output: `33.0 e`}, // So that it won't be 33.0e.
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
