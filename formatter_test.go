package prolog

import (
	"bytes"
	"fmt"
	"reflect"
	"testing"
)

func TestFormatter_WriteTo(t *testing.T) {
	h := NewHeap(30 * 1024)

	x, err := NewVariable(h)
	if err != nil {
		t.Fatal(err)
	}

	y, err := NewVariable(h)
	if err != nil {
		t.Fatal(err)
	}

	a, err := NewAtom(h, "a")
	if err != nil {
		t.Fatal(err)
	}

	b, err := NewAtom(h, "b")
	if err != nil {
		t.Fatal(err)
	}

	c, err := NewAtom(h, "c")
	if err != nil {
		t.Fatal(err)
	}

	X, err := NewAtom(h, "X")
	if err != nil {
		t.Fatal(err)
	}

	rest, err := NewAtom(h, "rest")
	if err != nil {
		t.Fatal(err)
	}

	escapeSequence, err := NewAtom(h, "\a\b\f\n\r\t\v\x00\\'\"`")
	if err != nil {
		t.Fatal(err)
	}

	comma, err := NewAtom(h, ",")
	if err != nil {
		t.Fatal(err)
	}

	emptyList, err := NewAtom(h, "[]")
	if err != nil {
		t.Fatal(err)
	}

	emptyBlock, err := NewAtom(h, "{}")
	if err != nil {
		t.Fatal(err)
	}

	minus, err := NewAtom(h, "-")
	if err != nil {
		t.Fatal(err)
	}

	foo, err := NewAtom(h, "foo")
	if err != nil {
		t.Fatal(err)
	}

	bar, err := NewAtom(h, "bar")
	if err != nil {
		t.Fatal(err)
	}

	baz, err := NewAtom(h, "baz")
	if err != nil {
		t.Fatal(err)
	}

	thirtyThree, err := NewInteger(h, 33)
	if err != nil {
		t.Fatal(err)
	}

	minusThirtyThree, err := NewInteger(h, -33)
	if err != nil {
		t.Fatal(err)
	}

	zero, err := NewInteger(h, 0)
	if err != nil {
		t.Fatal(err)
	}

	one, err := NewInteger(h, 1)
	if err != nil {
		t.Fatal(err)
	}

	two, err := NewInteger(h, 2)
	if err != nil {
		t.Fatal(err)
	}

	twentyFive, err := NewInteger(h, 25)
	if err != nil {
		t.Fatal(err)
	}

	twentySix, err := NewInteger(h, 26)
	if err != nil {
		t.Fatal(err)
	}

	twentySeven, err := NewInteger(h, 27)
	if err != nil {
		t.Fatal(err)
	}

	minusTwo, err := NewInteger(h, -2)
	if err != nil {
		t.Fatal(err)
	}

	floatThirtyThree, err := NewFloat(h, 33)
	if err != nil {
		t.Fatal(err)
	}

	floatWithE, err := NewFloat(h, 3.0e+100)
	if err != nil {
		t.Fatal(err)
	}

	floatMinusThirtyThree, err := NewFloat(h, -33)
	if err != nil {
		t.Fatal(err)
	}

	list, err := NewList(h, a, b, c)
	if err != nil {
		t.Fatal(err)
	}

	listish, err := NewPartialList(h, rest, a, b)
	if err != nil {
		t.Fatal(err)
	}

	v, err := NewVariable(h)
	if err != nil {
		t.Fatal(err)
	}
	circularList, err := NewPartialList(h, v, a, b)
	if err != nil {
		t.Fatal(err)
	}
	if err := h.env.Values.Set(Variable(v.payload), circularList); err != nil {
		t.Fatal(err)
	}

	curlyBrackets, err := NewCompound(h, "{}", foo)
	if err != nil {
		t.Fatal(err)
	}

	ifFoo, err := NewCompound(h, ":-", foo)
	if err != nil {
		t.Fatal(err)
	}
	ifIfFoo, err := NewCompound(h, ":-", ifFoo)
	if err != nil {
		t.Fatal(err)
	}

	notFoo, err := NewCompound(h, `\+`, foo)
	if err != nil {
		t.Fatal(err)
	}

	minusNotFoo, err := NewCompound(h, `-`, notFoo)
	if err != nil {
		t.Fatal(err)
	}

	notMinusNotFoo, err := NewCompound(h, `\+`, minusNotFoo)
	if err != nil {
		t.Fatal(err)
	}

	fiFoo, err := NewCompound(h, `-:`, foo)
	if err != nil {
		t.Fatal(err)
	}

	fiFiFoo, err := NewCompound(h, `-:`, fiFoo)
	if err != nil {
		t.Fatal(err)
	}

	tonFoo, err := NewCompound(h, `+/`, foo)
	if err != nil {
		t.Fatal(err)
	}

	minusMinusTonFoo, err := NewCompound(h, `--`, tonFoo)
	if err != nil {
		t.Fatal(err)
	}

	tonMinusMinusTonFoo, err := NewCompound(h, `+/`, minusMinusTonFoo)
	if err != nil {
		t.Fatal(err)
	}

	ifBarBaz, err := NewCompound(h, `:-`, bar, baz)
	if err != nil {
		t.Fatal(err)
	}

	ifFooIfBarBaz, err := NewCompound(h, `:-`, foo, ifBarBaz)
	if err != nil {
		t.Fatal(err)
	}

	plusTwoTwo, err := NewCompound(h, `+`, two, two)
	if err != nil {
		t.Fatal(err)
	}

	asteriskTwoPlusTwoTwo, err := NewCompound(h, `*`, two, plusTwoTwo)
	if err != nil {
		t.Fatal(err)
	}

	barTwoTwo, err := NewCompound(h, `|`, two, two)
	if err != nil {
		t.Fatal(err)
	}

	commaTwoBarTwoTwo, err := NewCompound(h, `,`, two, barTwoTwo)
	if err != nil {
		t.Fatal(err)
	}

	plusTwoMinusTwo, err := NewCompound(h, `+`, two, minusTwo)
	if err != nil {
		t.Fatal(err)
	}

	varZero, err := NewCompound(h, `$VAR`, zero)
	if err != nil {
		t.Fatal(err)
	}

	varOne, err := NewCompound(h, `$VAR`, one)
	if err != nil {
		t.Fatal(err)
	}

	varTwentyFive, err := NewCompound(h, `$VAR`, twentyFive)
	if err != nil {
		t.Fatal(err)
	}

	varTwentySix, err := NewCompound(h, `$VAR`, twentySix)
	if err != nil {
		t.Fatal(err)
	}

	varTwentySeven, err := NewCompound(h, `$VAR`, twentySeven)
	if err != nil {
		t.Fatal(err)
	}

	fVars, err := NewCompound(h, `f`, varZero, varOne, varTwentyFive, varTwentySix, varTwentySeven)
	if err != nil {
		t.Fatal(err)
	}

	minusB, err := NewCompound(h, `-`, b)
	if err != nil {
		t.Fatal(err)
	}

	asteriskAMinusB, err := NewCompound(h, `*`, a, minusB)
	if err != nil {
		t.Fatal(err)
	}

	tonA, err := NewCompound(h, `+/`, a)
	if err != nil {
		t.Fatal(err)
	}

	minusTonA, err := NewCompound(h, `-`, tonA)
	if err != nil {
		t.Fatal(err)
	}

	asteriskAB, err := NewCompound(h, `*`, a, b)
	if err != nil {
		t.Fatal(err)
	}

	minusAsteriskAB, err := NewCompound(h, `-`, asteriskAB)
	if err != nil {
		t.Fatal(err)
	}

	w, err := NewVariable(h)
	if err != nil {
		t.Fatal(err)
	}
	r, err := NewCompound(h, "f", w)
	if err != nil {
		t.Fatal(err)
	}
	if err := h.env.Values.Set(Variable(w.payload), r); err != nil {
		t.Fatal(err)
	}

	isXY, err := NewCompound(h, "is", x, y)
	if err != nil {
		t.Fatal(err)
	}

	minusMinus, err := NewCompound(h, "-", minus)
	if err != nil {
		t.Fatal(err)
	}

	minusMinusMinus, err := NewCompound(h, "--", minus)
	if err != nil {
		t.Fatal(err)
	}

	FXX, err := NewCompound(h, `F`, X, X)
	if err != nil {
		t.Fatal(err)
	}

	isFooFoo, err := NewCompound(h, `is`, foo, foo)
	if err != nil {
		t.Fatal(err)
	}

	unaryMinusThirtyThree, err := NewCompound(h, `-`, thirtyThree)
	if err != nil {
		t.Fatal(err)
	}

	b0Zero, err := NewCompound(h, `b0`, zero)
	if err != nil {
		t.Fatal(err)
	}

	o0Zero, err := NewCompound(h, `o0`, zero)
	if err != nil {
		t.Fatal(err)
	}

	x0Zero, err := NewCompound(h, `x0`, zero)
	if err != nil {
		t.Fatal(err)
	}

	FooZero, err := NewCompound(h, `Foo`, zero)
	if err != nil {
		t.Fatal(err)
	}

	minusFloatThirtyThree, err := NewCompound(h, `-`, floatThirtyThree)
	if err != nil {
		t.Fatal(err)
	}

	eFloatThirtyThree, err := NewCompound(h, `e`, floatThirtyThree)
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
		{title: "variable: unnamed", formatter: Formatter{Term: x, Heap: h}, output: fmt.Sprintf("_%d", x.payload)},
		{title: "variable: variable_names", formatter: Formatter{Term: x, Heap: h, VariableName: map[Variable]string{
			Variable(x.payload): "Foo",
		}}, output: `Foo`},

		{title: "atom: a", formatter: Formatter{Term: a, Heap: h, Quoted: false}, output: `a`},
		{title: "atom: a with quoted", formatter: Formatter{Term: a, Heap: h, Quoted: true}, output: `a`},
		{title: "atom: escape sequence", formatter: Formatter{Term: escapeSequence, Heap: h, Quoted: false}, output: "\a\b\f\n\r\t\v\x00\\'\"`"},
		{title: "atom: escape sequence with quoted", formatter: Formatter{Term: escapeSequence, Heap: h, Quoted: true}, output: "'\\a\\b\\f\\n\\r\\t\\v\\x0\\\\\\\\'\"`'"},
		{title: "atom: comma", formatter: Formatter{Term: comma, Heap: h, Quoted: false}, output: `,`},
		{title: "atom: comma with quoted", formatter: Formatter{Term: comma, Heap: h, Quoted: true}, output: `','`},
		{title: "atom: empty list", formatter: Formatter{Term: emptyList, Heap: h, Quoted: false}, output: `[]`},
		{title: "atom: empty list with quoted", formatter: Formatter{Term: emptyList, Heap: h, Quoted: true}, output: `[]`},
		{title: "atom: empty block", formatter: Formatter{Term: emptyBlock, Heap: h, Quoted: false}, output: `{}`},
		{title: "atom: empty block with quoted", formatter: Formatter{Term: emptyBlock, Heap: h, Quoted: true}, output: `{}`},
		{title: "atom: minus", formatter: Formatter{Term: minus, Heap: h}, output: `-`},

		{title: "integer: positive", formatter: Formatter{Term: thirtyThree, Heap: h}, output: `33`},
		{title: "integer: negative", formatter: Formatter{Term: minusThirtyThree, Heap: h}, output: `-33`},

		{title: "float: positive", formatter: Formatter{Term: floatThirtyThree, Heap: h, Precision: -1}, output: `33.0`},
		{title: "float: with e", formatter: Formatter{Term: floatWithE, Heap: h, Precision: -1}, output: `3.0e+100`},
		{title: "float: negative", formatter: Formatter{Term: floatMinusThirtyThree, Heap: h, Precision: -1}, output: `-33.0`},

		{title: "compound: list", formatter: Formatter{Term: list, Heap: h}, output: `[a,b,c]`},
		{title: "compound: list-ish", formatter: Formatter{Term: listish, Heap: h}, output: `[a,b|rest]`},
		{title: "compound: circular list", formatter: Formatter{Term: circularList, Heap: h}, output: `[a,b,a|...]`},
		{title: "compound: curly brackets", formatter: Formatter{Term: curlyBrackets, Heap: h}, output: `{foo}`},
		{title: "compound: fx", formatter: Formatter{Term: ifIfFoo, Heap: h, Ops: ops}, output: `:- (:-foo)`},
		{title: "compound: fy", formatter: Formatter{Term: notMinusNotFoo, Heap: h, Ops: ops}, output: `\+ - (\+foo)`},
		{title: "compound: xf", formatter: Formatter{Term: fiFiFoo, Heap: h, Ops: ops}, output: `(foo-:)-:`},
		{title: "compound: yf", formatter: Formatter{Term: tonMinusMinusTonFoo, Heap: h, Ops: ops}, output: `(foo+/)-- +/`},
		{title: "compound: xfx", formatter: Formatter{Term: ifFooIfBarBaz, Heap: h, Ops: ops}, output: `foo:-(bar:-baz)`},
		{title: "compound: yfx", formatter: Formatter{Term: asteriskTwoPlusTwoTwo, Heap: h, Ops: ops}, output: `2*(2+2)`},
		{title: "compound: xfy", formatter: Formatter{Term: commaTwoBarTwoTwo, Heap: h, Ops: ops}, output: `2,(2|2)`},
		{title: "compound: ignore_ops(false)", formatter: Formatter{Term: plusTwoMinusTwo, Heap: h, IgnoreOps: false, Ops: ops}, output: `2+ -2`},
		{title: "compound: ignore_ops(true)", formatter: Formatter{Term: plusTwoMinusTwo, Heap: h, IgnoreOps: true, Ops: ops}, output: `+(2,-2)`},
		{title: "compound: number_vars(false)", formatter: Formatter{Term: fVars, Heap: h, Quoted: true, NumberVars: false, Ops: ops}, output: `f('$VAR'(0),'$VAR'(1),'$VAR'(25),'$VAR'(26),'$VAR'(27))`},
		{title: "compound: number_vars(true)", formatter: Formatter{Term: fVars, Heap: h, Quoted: true, NumberVars: true, Ops: ops}, output: `f(A,B,Z,A1,B1)`},
		{title: "compound: prefix: spacing between operators", formatter: Formatter{Term: asteriskAMinusB, Heap: h, Ops: ops}, output: `a* -b`},
		{title: "compound: postfix: spacing between unary minus and open/close", formatter: Formatter{Term: minusTonA, Heap: h, Ops: ops}, output: `- (a+/)`},
		{title: "compound: infix: spacing between unary minus and open/close", formatter: Formatter{Term: minusAsteriskAB, Heap: h, Ops: ops}, output: `- (a*b)`},
		{title: "compound: recursive", formatter: Formatter{Term: r, Heap: h}, output: `f(...)`},
		{title: "compound: variable following/followed by a letter-digit operator", formatter: Formatter{Term: isXY, Heap: h, Ops: ops}, output: fmt.Sprintf("_%d is _%d", x.payload, y.payload)},
		{title: "compound: atom minus right after an operator", formatter: Formatter{Term: minusMinus, Heap: h, Ops: ops}, output: `- (-)`},
		{title: "compound: atom minus right before an operator", formatter: Formatter{Term: minusMinusMinus, Heap: h, Ops: ops}, output: `(-)--`},
		{title: "compound: atom X right before/after an operator that requires quotes", formatter: Formatter{Term: FXX, Heap: h, Quoted: true, Ops: ops}, output: `'X' 'F' 'X'`},
		{title: "compound: atom foo right before/after a letter-digit operator", formatter: Formatter{Term: isFooFoo, Heap: h, Ops: ops}, output: `foo is foo`}, // So that it won't be barfoo.
		{title: "compound: positive integer following unary minus", formatter: Formatter{Term: unaryMinusThirtyThree, Heap: h, Ops: ops}, output: `- (33)`},
		{title: "compound: integer ambiguous 0b", formatter: Formatter{Term: b0Zero, Heap: h, Ops: ops}, output: `0 b0`},                   // So that it won't be 0b0.
		{title: "compound: integer ambiguous 0o", formatter: Formatter{Term: o0Zero, Heap: h, Ops: ops}, output: `0 o0`},                   // So that it won't be 0o0.
		{title: "compound: integer ambiguous 0x", formatter: Formatter{Term: x0Zero, Heap: h, Ops: ops}, output: `0 x0`},                   // So that it won't be 0x0.
		{title: "compound: integer ambiguous 0'", formatter: Formatter{Term: FooZero, Heap: h, Quoted: true, Ops: ops}, output: `0 'Foo'`}, // So that it won't be 0'Foo'.
		{title: "float: positive following unary minus", formatter: Formatter{Term: minusFloatThirtyThree, Heap: h, Ops: ops, Precision: -1}, output: `- (33.0)`},
		{title: "float: ambiguous e", formatter: Formatter{Term: eFloatThirtyThree, Heap: h, Ops: ops, Precision: -1}, output: `33.0 e`}, // So that it won't be 33.0e.
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
