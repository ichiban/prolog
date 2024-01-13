package engine

import (
	"bytes"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestWriteCompound(t *testing.T) {
	f := NewAtom("f")
	v, w := NewVariable(), NewVariable()
	l := PartialList(v, NewAtom("a"), NewAtom("b"))
	r := f.Apply(w)
	env := NewEnv().bind(v, l).bind(w, r)

	ops := operators{}
	ops.define(1200, operatorSpecifierXFX, NewAtom(`:-`), false)
	ops.define(1200, operatorSpecifierFX, NewAtom(`:-`), false)
	ops.define(1200, operatorSpecifierXF, NewAtom(`-:`), false)
	ops.define(1105, operatorSpecifierXFY, NewAtom(`|`), false)
	ops.define(1000, operatorSpecifierXFY, NewAtom(`,`), false)
	ops.define(900, operatorSpecifierFY, atomNegation, false)
	ops.define(900, operatorSpecifierYF, NewAtom(`+/`), false)
	ops.define(500, operatorSpecifierYFX, NewAtom(`+`), false)
	ops.define(400, operatorSpecifierYFX, NewAtom(`*`), false)
	ops.define(200, operatorSpecifierFY, NewAtom(`-`), false)
	ops.define(200, operatorSpecifierYF, NewAtom(`--`), false)

	tests := []struct {
		title  string
		term   Term
		opts   WriteOptions
		output string
	}{
		{title: "list", term: List(NewAtom(`a`), NewAtom(`b`), NewAtom(`c`)), output: `[a,b,c]`},
		{title: "list-ish", term: PartialList(NewAtom(`rest`), NewAtom(`a`), NewAtom(`b`)), output: `[a,b|rest]`},
		{title: "circular list", term: l, output: `[a,b,a|...]`},
		{title: "curly brackets", term: atomEmptyBlock.Apply(NewAtom(`foo`)), output: `{foo}`},
		{title: "fx", term: atomIf.Apply(atomIf.Apply(NewAtom(`foo`))), opts: WriteOptions{ops: ops, priority: 1201}, output: `:- (:-foo)`},
		{title: "fy", term: atomNegation.Apply(atomMinus.Apply(atomNegation.Apply(NewAtom(`foo`)))), opts: WriteOptions{ops: ops, priority: 1201}, output: `\+ - (\+foo)`},
		{title: "xf", term: NewAtom(`-:`).Apply(NewAtom(`-:`).Apply(NewAtom(`foo`))), opts: WriteOptions{ops: ops, priority: 1201}, output: `(foo-:)-:`},
		{title: "yf", term: NewAtom(`+/`).Apply(NewAtom(`--`).Apply(NewAtom(`+/`).Apply(NewAtom(`foo`)))), opts: WriteOptions{ops: ops, priority: 1201}, output: `(foo+/)-- +/`},
		{title: "xfx", term: atomIf.Apply(NewAtom("foo"), atomIf.Apply(NewAtom("bar"), NewAtom("baz"))), opts: WriteOptions{ops: ops, priority: 1201}, output: `foo:-(bar:-baz)`},
		{title: "yfx", term: atomAsterisk.Apply(Integer(2), atomPlus.Apply(Integer(2), Integer(2))), opts: WriteOptions{ops: ops, priority: 1201}, output: `2*(2+2)`},
		{title: "xfy", term: atomComma.Apply(Integer(2), atomBar.Apply(Integer(2), Integer(2))), opts: WriteOptions{ops: ops, priority: 1201}, output: `2,(2|2)`},
		{title: "ignore_ops(false)", term: atomPlus.Apply(Integer(2), Integer(-2)), opts: WriteOptions{ignoreOps: false, ops: ops, priority: 1201}, output: `2+ -2`},
		{title: "ignore_ops(true)", term: atomPlus.Apply(Integer(2), Integer(-2)), opts: WriteOptions{ignoreOps: true, ops: ops, priority: 1201}, output: `+(2,-2)`},
		{title: "number_vars(false)", term: f.Apply(atomVar.Apply(Integer(0)), atomVar.Apply(Integer(1)), atomVar.Apply(Integer(25)), atomVar.Apply(Integer(26)), atomVar.Apply(Integer(27))), opts: WriteOptions{quoted: true, numberVars: false, ops: ops, priority: 1201}, output: `f('$VAR'(0),'$VAR'(1),'$VAR'(25),'$VAR'(26),'$VAR'(27))`},
		{title: "number_vars(true)", term: f.Apply(atomVar.Apply(Integer(0)), atomVar.Apply(Integer(1)), atomVar.Apply(Integer(25)), atomVar.Apply(Integer(26)), atomVar.Apply(Integer(27))), opts: WriteOptions{quoted: true, numberVars: true, ops: ops, priority: 1201}, output: `f(A,B,Z,A1,B1)`},
		{title: "prefix: spacing between operators", term: atomAsterisk.Apply(NewAtom("a"), atomMinus.Apply(NewAtom("b"))), opts: WriteOptions{ops: ops, priority: 1201}, output: `a* -b`},
		{title: "postfix: spacing between unary minus and open/close", term: atomMinus.Apply(NewAtom(`+/`).Apply(NewAtom("a"))), opts: WriteOptions{ops: ops, priority: 1201}, output: `- (a+/)`},
		{title: "infix: spacing between unary minus and open/close", term: atomMinus.Apply(atomAsterisk.Apply(NewAtom("a"), NewAtom("b"))), opts: WriteOptions{ops: ops, priority: 1201}, output: `- (a*b)`},
		{title: "recursive", term: r, output: `f(...)`},
	}

	var buf bytes.Buffer
	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			buf.Reset()
			assert.NoError(t, WriteCompound(&buf, tt.term.(Compound), &tt.opts, env))
			assert.Equal(t, tt.output, buf.String())
		})
	}
}

func TestCompareCompound(t *testing.T) {
	x := NewVariable()

	tests := []struct {
		title string
		x, y  Term
		o     int
	}{
		{title: `f(a) > X`, x: NewAtom("f").Apply(NewAtom("a")), y: x, o: 1},
		{title: `f(a) > 1.0`, x: NewAtom("f").Apply(NewAtom("a")), y: Float(1), o: 1},
		{title: `f(a) > 1`, x: NewAtom("f").Apply(NewAtom("a")), y: Integer(1), o: 1},
		{title: `f(a) > a`, x: NewAtom("f").Apply(NewAtom("a")), y: NewAtom("a"), o: 1}, {title: `f(a) > f('Z')`, x: NewAtom("f").Apply(NewAtom("a")), y: NewAtom("f").Apply(NewAtom("Z")), o: 1},
		{title: `f(a) > e(a)`, x: NewAtom("f").Apply(NewAtom("a")), y: NewAtom("e").Apply(NewAtom("a")), o: 1},
		{title: `f(a, b) > f(a)`, x: NewAtom("f").Apply(NewAtom("a"), NewAtom("b")), y: NewAtom("f").Apply(NewAtom("a")), o: 1},
		{title: `f(a) = f(a)`, x: NewAtom("f").Apply(NewAtom("a")), y: NewAtom("f").Apply(NewAtom("a")), o: 0},
		{title: `f(a) < g(a)`, x: NewAtom("f").Apply(NewAtom("a")), y: NewAtom("g").Apply(NewAtom("a")), o: -1},
		{title: `f(a) < f(a,b)`, x: NewAtom("f").Apply(NewAtom("a")), y: NewAtom("f").Apply(NewAtom("a"), NewAtom("b")), o: -1},
		{title: `f(a) < f(b)`, x: NewAtom("f").Apply(NewAtom("a")), y: NewAtom("f").Apply(NewAtom("b")), o: -1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			assert.Equal(t, tt.o, CompareCompound(tt.x.(Compound), tt.y, nil))
		})
	}
}

func TestList(t *testing.T) {
	tests := []struct {
		title string
		elems []Term
		list  Term
	}{
		{title: "empty", elems: nil, list: atomEmptyList},
		{title: "non-empty", elems: []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")}, list: list{NewAtom("a"), NewAtom("b"), NewAtom("c")}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			assert.Equal(t, tt.list, List(tt.elems...))
		})
	}
}

func TestPartialList(t *testing.T) {
	x := Term(NewVariable())

	tests := []struct {
		title string
		rest  Term
		elems []Term
		list  Term
	}{
		{title: "empty", rest: x, elems: nil, list: x},
		{title: "non-empty", rest: x, elems: []Term{NewAtom("a"), NewAtom("b")}, list: &partial{Compound: list{NewAtom("a"), NewAtom("b")}, tail: &x}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			assert.Equal(t, tt.list, PartialList(tt.rest, tt.elems...))
		})
	}
}

func TestEnv_Set(t *testing.T) {
	env := NewEnv()
	assert.Equal(t, List(), env.set())
	assert.Equal(t, List(NewAtom("a")), env.set(NewAtom("a")))
	assert.Equal(t, List(NewAtom("a")), env.set(NewAtom("a"), NewAtom("a"), NewAtom("a")))
	assert.Equal(t, List(NewAtom("a"), NewAtom("b"), NewAtom("c")), env.set(NewAtom("c"), NewAtom("b"), NewAtom("a")))
}

func TestSeq(t *testing.T) {
	assert.Equal(t, NewAtom("a"), seq(atomComma, NewAtom("a")))
	assert.Equal(t, &compound{
		functor: atomComma,
		args: []Term{
			NewAtom("a"),
			NewAtom("b"),
		},
	}, seq(atomComma, NewAtom("a"), NewAtom("b")))
	assert.Equal(t, &compound{
		functor: atomComma,
		args: []Term{
			NewAtom("a"),
			&compound{
				functor: atomComma,
				args: []Term{
					NewAtom("b"),
					NewAtom("c"),
				},
			},
		},
	}, seq(atomComma, NewAtom("a"), NewAtom("b"), NewAtom("c")))
}

func TestCharList(t *testing.T) {
	assert.Equal(t, atomEmptyList, CharList(""))
	assert.Equal(t, charList("abc"), CharList("abc"))
}

func TestCharList_String(t *testing.T) {
	assert.Equal(t, "abc", CharList("abc").(charList).String())
}

func TestCharList_WriteTerm(t *testing.T) {
	var b bytes.Buffer
	assert.NoError(t, CharList("abc").WriteTerm(&b, &WriteOptions{}, nil))
	assert.Equal(t, `[a,b,c]`, b.String())
}

func TestCharList_Compare(t *testing.T) {
	assert.Equal(t, 0, CharList("abc").Compare(List(NewAtom("a"), NewAtom("b"), NewAtom("c")), nil))
}

func TestCodeList(t *testing.T) {
	assert.Equal(t, atomEmptyList, CodeList(""))
	assert.Equal(t, codeList("abc"), CodeList("abc"))
}

func TestCodeList_String(t *testing.T) {
	assert.Equal(t, "abc", CodeList("abc").(codeList).String())
}

func TestCodeList_WriteTerm(t *testing.T) {
	var b bytes.Buffer
	assert.NoError(t, CodeList("abc").WriteTerm(&b, &WriteOptions{}, nil))
	assert.Equal(t, `[97,98,99]`, b.String())
}

func TestCodeList_Compare(t *testing.T) {
	assert.Equal(t, 0, CodeList("abc").Compare(List(Integer('a'), Integer('b'), Integer('c')), nil))
}
