package prolog

import (
	"bytes"
	"fmt"
	"reflect"
	"testing"
)

func TestFormatter_WriteTo(t *testing.T) {
	e := NewEngine()

	x, err := e.PutVariable()
	if err != nil {
		t.Fatal(err)
	}

	y, err := e.PutVariable()
	if err != nil {
		t.Fatal(err)
	}

	a, err := e.PutAtom("a")
	if err != nil {
		t.Fatal(err)
	}

	b, err := e.PutAtom("b")
	if err != nil {
		t.Fatal(err)
	}

	c, err := e.PutAtom("c")
	if err != nil {
		t.Fatal(err)
	}

	X, err := e.PutAtom("X")
	if err != nil {
		t.Fatal(err)
	}

	rest, err := e.PutAtom("rest")
	if err != nil {
		t.Fatal(err)
	}

	escapeSequence, err := e.PutAtom("\a\b\f\n\r\t\v\x00\\'\"`")
	if err != nil {
		t.Fatal(err)
	}

	comma, err := e.PutAtom(",")
	if err != nil {
		t.Fatal(err)
	}

	emptyList, err := e.PutAtom("[]")
	if err != nil {
		t.Fatal(err)
	}

	emptyBlock, err := e.PutAtom("{}")
	if err != nil {
		t.Fatal(err)
	}

	minus, err := e.PutAtom("-")
	if err != nil {
		t.Fatal(err)
	}

	foo, err := e.PutAtom("foo")
	if err != nil {
		t.Fatal(err)
	}

	bar, err := e.PutAtom("bar")
	if err != nil {
		t.Fatal(err)
	}

	baz, err := e.PutAtom("baz")
	if err != nil {
		t.Fatal(err)
	}

	thirtyThree, err := e.PutInteger(33)
	if err != nil {
		t.Fatal(err)
	}

	minusThirtyThree, err := e.PutInteger(-33)
	if err != nil {
		t.Fatal(err)
	}

	zero, err := e.PutInteger(0)
	if err != nil {
		t.Fatal(err)
	}

	one, err := e.PutInteger(1)
	if err != nil {
		t.Fatal(err)
	}

	two, err := e.PutInteger(2)
	if err != nil {
		t.Fatal(err)
	}

	twentyFive, err := e.PutInteger(25)
	if err != nil {
		t.Fatal(err)
	}

	twentySix, err := e.PutInteger(26)
	if err != nil {
		t.Fatal(err)
	}

	twentySeven, err := e.PutInteger(27)
	if err != nil {
		t.Fatal(err)
	}

	minusTwo, err := e.PutInteger(-2)
	if err != nil {
		t.Fatal(err)
	}

	floatThirtyThree, err := e.PutFloat(33)
	if err != nil {
		t.Fatal(err)
	}

	floatWithE, err := e.PutFloat(3.0e+100)
	if err != nil {
		t.Fatal(err)
	}

	floatMinusThirtyThree, err := e.PutFloat(-33)
	if err != nil {
		t.Fatal(err)
	}

	list, err := e.PutList(a, b, c)
	if err != nil {
		t.Fatal(err)
	}

	listish, err := e.PutPartialList(rest, a, b)
	if err != nil {
		t.Fatal(err)
	}

	v, err := e.PutVariable()
	if err != nil {
		t.Fatal(err)
	}
	circularList, err := e.PutPartialList(v, a, b)
	if err != nil {
		t.Fatal(err)
	}
	var trail []Variable
	if !e.bind(&trail, v, circularList, false) {
		t.Fatal(err)
	}

	curlyBrackets, err := e.PutCompound("{}", foo)
	if err != nil {
		t.Fatal(err)
	}

	ifFoo, err := e.PutCompound(":-", foo)
	if err != nil {
		t.Fatal(err)
	}
	ifIfFoo, err := e.PutCompound(":-", ifFoo)
	if err != nil {
		t.Fatal(err)
	}

	notFoo, err := e.PutCompound(`\+`, foo)
	if err != nil {
		t.Fatal(err)
	}

	minusNotFoo, err := e.PutCompound(`-`, notFoo)
	if err != nil {
		t.Fatal(err)
	}

	notMinusNotFoo, err := e.PutCompound(`\+`, minusNotFoo)
	if err != nil {
		t.Fatal(err)
	}

	fiFoo, err := e.PutCompound(`-:`, foo)
	if err != nil {
		t.Fatal(err)
	}

	fiFiFoo, err := e.PutCompound(`-:`, fiFoo)
	if err != nil {
		t.Fatal(err)
	}

	tonFoo, err := e.PutCompound(`+/`, foo)
	if err != nil {
		t.Fatal(err)
	}

	minusMinusTonFoo, err := e.PutCompound(`--`, tonFoo)
	if err != nil {
		t.Fatal(err)
	}

	tonMinusMinusTonFoo, err := e.PutCompound(`+/`, minusMinusTonFoo)
	if err != nil {
		t.Fatal(err)
	}

	ifBarBaz, err := e.PutCompound(`:-`, bar, baz)
	if err != nil {
		t.Fatal(err)
	}

	ifFooIfBarBaz, err := e.PutCompound(`:-`, foo, ifBarBaz)
	if err != nil {
		t.Fatal(err)
	}

	plusTwoTwo, err := e.PutCompound(`+`, two, two)
	if err != nil {
		t.Fatal(err)
	}

	asteriskTwoPlusTwoTwo, err := e.PutCompound(`*`, two, plusTwoTwo)
	if err != nil {
		t.Fatal(err)
	}

	barTwoTwo, err := e.PutCompound(`|`, two, two)
	if err != nil {
		t.Fatal(err)
	}

	commaTwoBarTwoTwo, err := e.PutCompound(`,`, two, barTwoTwo)
	if err != nil {
		t.Fatal(err)
	}

	plusTwoMinusTwo, err := e.PutCompound(`+`, two, minusTwo)
	if err != nil {
		t.Fatal(err)
	}

	varZero, err := e.PutCompound(`$VAR`, zero)
	if err != nil {
		t.Fatal(err)
	}

	varOne, err := e.PutCompound(`$VAR`, one)
	if err != nil {
		t.Fatal(err)
	}

	varTwentyFive, err := e.PutCompound(`$VAR`, twentyFive)
	if err != nil {
		t.Fatal(err)
	}

	varTwentySix, err := e.PutCompound(`$VAR`, twentySix)
	if err != nil {
		t.Fatal(err)
	}

	varTwentySeven, err := e.PutCompound(`$VAR`, twentySeven)
	if err != nil {
		t.Fatal(err)
	}

	fVars, err := e.PutCompound(`f`, varZero, varOne, varTwentyFive, varTwentySix, varTwentySeven)
	if err != nil {
		t.Fatal(err)
	}

	minusB, err := e.PutCompound(`-`, b)
	if err != nil {
		t.Fatal(err)
	}

	asteriskAMinusB, err := e.PutCompound(`*`, a, minusB)
	if err != nil {
		t.Fatal(err)
	}

	tonA, err := e.PutCompound(`+/`, a)
	if err != nil {
		t.Fatal(err)
	}

	minusTonA, err := e.PutCompound(`-`, tonA)
	if err != nil {
		t.Fatal(err)
	}

	asteriskAB, err := e.PutCompound(`*`, a, b)
	if err != nil {
		t.Fatal(err)
	}

	minusAsteriskAB, err := e.PutCompound(`-`, asteriskAB)
	if err != nil {
		t.Fatal(err)
	}

	w, err := e.PutVariable()
	if err != nil {
		t.Fatal(err)
	}
	r, err := e.PutCompound("f", w)
	if err != nil {
		t.Fatal(err)
	}
	if !e.bind(&trail, w, r, false) {
		t.Fatal(err)
	}

	isXY, err := e.PutCompound("is", x, y)
	if err != nil {
		t.Fatal(err)
	}

	minusMinus, err := e.PutCompound("-", minus)
	if err != nil {
		t.Fatal(err)
	}

	minusMinusMinus, err := e.PutCompound("--", minus)
	if err != nil {
		t.Fatal(err)
	}

	FXX, err := e.PutCompound(`F`, X, X)
	if err != nil {
		t.Fatal(err)
	}

	isFooFoo, err := e.PutCompound(`is`, foo, foo)
	if err != nil {
		t.Fatal(err)
	}

	unaryMinusThirtyThree, err := e.PutCompound(`-`, thirtyThree)
	if err != nil {
		t.Fatal(err)
	}

	b0Zero, err := e.PutCompound(`b0`, zero)
	if err != nil {
		t.Fatal(err)
	}

	o0Zero, err := e.PutCompound(`o0`, zero)
	if err != nil {
		t.Fatal(err)
	}

	x0Zero, err := e.PutCompound(`x0`, zero)
	if err != nil {
		t.Fatal(err)
	}

	FooZero, err := e.PutCompound(`Foo`, zero)
	if err != nil {
		t.Fatal(err)
	}

	minusFloatThirtyThree, err := e.PutCompound(`-`, floatThirtyThree)
	if err != nil {
		t.Fatal(err)
	}

	eFloatThirtyThree, err := e.PutCompound(`e`, floatThirtyThree)
	if err != nil {
		t.Fatal(err)
	}

	var ops Operators
	ops.Define(1200, XFX, `:-`)
	ops.Define(1200, FX, `:-`)
	ops.Define(1200, XF, `-:`)
	ops.Define(1105, XFY, `|`)
	ops.Define(1000, XFY, `,`)
	ops.Define(900, FY, `\+`)
	ops.Define(900, YF, `+/`)
	ops.Define(700, XFX, `is`)
	ops.Define(700, XFX, `F`)
	ops.Define(500, YFX, `+`)
	ops.Define(400, YFX, `*`)
	ops.Define(200, FY, "+")
	ops.Define(200, FY, `-`)
	ops.Define(200, YF, `--`)
	ops.Define(200, YF, `b0`)
	ops.Define(200, YF, `o0`)
	ops.Define(200, YF, `x0`)
	ops.Define(200, YF, `Foo`)
	ops.Define(200, YF, `e`)

	tests := []struct {
		title     string
		formatter Formatter
		output    string
		err       error
	}{
		{title: "variable: unnamed", formatter: Formatter{Term: x, Engine: e}, output: fmt.Sprintf("_%d", x.value)},
		{title: "variable: variable_names", formatter: Formatter{Term: x, Engine: e, VariableName: map[Variable]Atom{
			Variable(x.value): "Foo",
		}}, output: `Foo`},

		{title: "atom: a", formatter: Formatter{Term: a, Engine: e, Quoted: false}, output: `a`},
		{title: "atom: a with quoted", formatter: Formatter{Term: a, Engine: e, Quoted: true}, output: `a`},
		{title: "atom: escape sequence", formatter: Formatter{Term: escapeSequence, Engine: e, Quoted: false}, output: "\a\b\f\n\r\t\v\x00\\'\"`"},
		{title: "atom: escape sequence with quoted", formatter: Formatter{Term: escapeSequence, Engine: e, Quoted: true}, output: "'\\a\\b\\f\\n\\r\\t\\v\\x0\\\\\\\\'\"`'"},
		{title: "atom: comma", formatter: Formatter{Term: comma, Engine: e, Quoted: false}, output: `,`},
		{title: "atom: comma with quoted", formatter: Formatter{Term: comma, Engine: e, Quoted: true}, output: `','`},
		{title: "atom: empty list", formatter: Formatter{Term: emptyList, Engine: e, Quoted: false}, output: `[]`},
		{title: "atom: empty list with quoted", formatter: Formatter{Term: emptyList, Engine: e, Quoted: true}, output: `[]`},
		{title: "atom: empty block", formatter: Formatter{Term: emptyBlock, Engine: e, Quoted: false}, output: `{}`},
		{title: "atom: empty block with quoted", formatter: Formatter{Term: emptyBlock, Engine: e, Quoted: true}, output: `{}`},
		{title: "atom: minus", formatter: Formatter{Term: minus, Engine: e}, output: `-`},

		{title: "integer: positive", formatter: Formatter{Term: thirtyThree, Engine: e}, output: `33`},
		{title: "integer: negative", formatter: Formatter{Term: minusThirtyThree, Engine: e}, output: `-33`},

		{title: "float: positive", formatter: Formatter{Term: floatThirtyThree, Engine: e, Precision: -1}, output: `33.0`},
		{title: "float: with e", formatter: Formatter{Term: floatWithE, Engine: e, Precision: -1}, output: `3.0e+100`},
		{title: "float: negative", formatter: Formatter{Term: floatMinusThirtyThree, Engine: e, Precision: -1}, output: `-33.0`},

		{title: "compound: list", formatter: Formatter{Term: list, Engine: e}, output: `[a,b,c]`},
		{title: "compound: list-ish", formatter: Formatter{Term: listish, Engine: e}, output: `[a,b|rest]`},
		{title: "compound: circular list", formatter: Formatter{Term: circularList, Engine: e}, output: `[a,b,a|...]`},
		{title: "compound: curly brackets", formatter: Formatter{Term: curlyBrackets, Engine: e}, output: `{foo}`},
		{title: "compound: fx", formatter: Formatter{Term: ifIfFoo, Engine: e, Ops: ops}, output: `:- (:-foo)`},
		{title: "compound: fy", formatter: Formatter{Term: notMinusNotFoo, Engine: e, Ops: ops}, output: `\+ - (\+foo)`},
		{title: "compound: xf", formatter: Formatter{Term: fiFiFoo, Engine: e, Ops: ops}, output: `(foo-:)-:`},
		{title: "compound: yf", formatter: Formatter{Term: tonMinusMinusTonFoo, Engine: e, Ops: ops}, output: `(foo+/)-- +/`},
		{title: "compound: xfx", formatter: Formatter{Term: ifFooIfBarBaz, Engine: e, Ops: ops}, output: `foo:-(bar:-baz)`},
		{title: "compound: yfx", formatter: Formatter{Term: asteriskTwoPlusTwoTwo, Engine: e, Ops: ops}, output: `2*(2+2)`},
		{title: "compound: xfy", formatter: Formatter{Term: commaTwoBarTwoTwo, Engine: e, Ops: ops}, output: `2,(2|2)`},
		{title: "compound: ignore_ops(false)", formatter: Formatter{Term: plusTwoMinusTwo, Engine: e, IgnoreOps: false, Ops: ops}, output: `2+ -2`},
		{title: "compound: ignore_ops(true)", formatter: Formatter{Term: plusTwoMinusTwo, Engine: e, IgnoreOps: true, Ops: ops}, output: `+(2,-2)`},
		{title: "compound: number_vars(false)", formatter: Formatter{Term: fVars, Engine: e, Quoted: true, NumberVars: false, Ops: ops}, output: `f('$VAR'(0),'$VAR'(1),'$VAR'(25),'$VAR'(26),'$VAR'(27))`},
		{title: "compound: number_vars(true)", formatter: Formatter{Term: fVars, Engine: e, Quoted: true, NumberVars: true, Ops: ops}, output: `f(A,B,Z,A1,B1)`},
		{title: "compound: prefix: spacing between operators", formatter: Formatter{Term: asteriskAMinusB, Engine: e, Ops: ops}, output: `a* -b`},
		{title: "compound: postfix: spacing between unary minus and open/close", formatter: Formatter{Term: minusTonA, Engine: e, Ops: ops}, output: `- (a+/)`},
		{title: "compound: infix: spacing between unary minus and open/close", formatter: Formatter{Term: minusAsteriskAB, Engine: e, Ops: ops}, output: `- (a*b)`},
		{title: "compound: recursive", formatter: Formatter{Term: r, Engine: e}, output: `f(...)`},
		{title: "compound: variable following/followed by a letter-digit operator", formatter: Formatter{Term: isXY, Engine: e, Ops: ops}, output: fmt.Sprintf("_%d is _%d", x.value, y.value)},
		{title: "compound: atom minus right after an operator", formatter: Formatter{Term: minusMinus, Engine: e, Ops: ops}, output: `- (-)`},
		{title: "compound: atom minus right before an operator", formatter: Formatter{Term: minusMinusMinus, Engine: e, Ops: ops}, output: `(-)--`},
		{title: "compound: atom X right before/after an operator that requires quotes", formatter: Formatter{Term: FXX, Engine: e, Quoted: true, Ops: ops}, output: `'X' 'F' 'X'`},
		{title: "compound: atom foo right before/after a letter-digit operator", formatter: Formatter{Term: isFooFoo, Engine: e, Ops: ops}, output: `foo is foo`}, // So that it won't be barfoo.
		{title: "compound: positive integer following unary minus", formatter: Formatter{Term: unaryMinusThirtyThree, Engine: e, Ops: ops}, output: `- (33)`},
		{title: "compound: integer ambiguous 0b", formatter: Formatter{Term: b0Zero, Engine: e, Ops: ops}, output: `0 b0`},                   // So that it won't be 0b0.
		{title: "compound: integer ambiguous 0o", formatter: Formatter{Term: o0Zero, Engine: e, Ops: ops}, output: `0 o0`},                   // So that it won't be 0o0.
		{title: "compound: integer ambiguous 0x", formatter: Formatter{Term: x0Zero, Engine: e, Ops: ops}, output: `0 x0`},                   // So that it won't be 0x0.
		{title: "compound: integer ambiguous 0'", formatter: Formatter{Term: FooZero, Engine: e, Quoted: true, Ops: ops}, output: `0 'Foo'`}, // So that it won't be 0'Foo'.
		{title: "float: positive following unary minus", formatter: Formatter{Term: minusFloatThirtyThree, Engine: e, Ops: ops, Precision: -1}, output: `- (33.0)`},
		{title: "float: ambiguous e", formatter: Formatter{Term: eFloatThirtyThree, Engine: e, Ops: ops, Precision: -1}, output: `33.0 e`}, // So that it won't be 33.0e.
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
