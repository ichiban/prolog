package engine

import (
	"bytes"
	"errors"
	"fmt"
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestErrStringWriter_WriteString(t *testing.T) {
	var failed = errors.New("failed")

	var m mockStringWriter
	m.On("WriteString", "foo").Return(0, failed).Once()
	defer m.AssertExpectations(t)

	ew := errStringWriter{w: &m}
	_, err := ew.WriteString("foo")
	assert.NoError(t, err)
	_, err = ew.WriteString("bar")
	assert.NoError(t, err)
	_, err = ew.WriteString("baz")
	assert.NoError(t, err)
	assert.Equal(t, failed, ew.err)
}

func Test_iteratedGoalTerm(t *testing.T) {
	tests := []struct {
		t, g Term
	}{
		{t: &compound{
			functor: atomCaret,
			args: []Term{
				NewNamedVariable("X"),
				&compound{
					functor: NewAtom("foo"),
					args:    []Term{NewNamedVariable("X")},
				},
			},
		}, g: &compound{
			functor: NewAtom("foo"),
			args:    []Term{NewNamedVariable("X")},
		}},
		{
			t: atomCaret.Apply(NewVariable(), atomCaret.Apply(NewVariable(), atomEqual.Apply(NewNamedVariable("X"), Integer(1)))),
			g: atomEqual.Apply(NewNamedVariable("X"), Integer(1)),
		},
	}

	for _, tt := range tests {
		assert.Equal(t, tt.g, iteratedGoalTerm(tt.t, nil))
	}
}

func Test_variant(t *testing.T) {
	tests := []struct {
		t1, t2 Term
		result bool
	}{
		{
			t1:     &compound{functor: NewAtom("f"), args: []Term{NewNamedVariable("A"), NewNamedVariable("B"), NewNamedVariable("A")}},
			t2:     &compound{functor: NewAtom("f"), args: []Term{NewNamedVariable("X"), NewNamedVariable("Y"), NewNamedVariable("X")}},
			result: true,
		},
		{
			t1:     &compound{functor: NewAtom("g"), args: []Term{NewNamedVariable("A"), NewNamedVariable("B")}},
			t2:     &compound{functor: NewAtom("g"), args: []Term{NewVariable(), NewVariable()}},
			result: true,
		},
		{
			t1:     &compound{functor: atomPlus, args: []Term{NewNamedVariable("P"), NewNamedVariable("Q")}},
			t2:     &compound{functor: atomPlus, args: []Term{NewNamedVariable("P"), NewNamedVariable("Q")}},
			result: true,
		},
		{
			t1:     &compound{functor: NewAtom("f"), args: []Term{NewNamedVariable("A"), NewNamedVariable("A")}},
			t2:     &compound{functor: NewAtom("f"), args: []Term{NewNamedVariable("X"), NewNamedVariable("Y")}},
			result: false,
		},
		{
			t1:     &compound{functor: NewAtom("f"), args: []Term{NewNamedVariable("A"), NewNamedVariable("A")}},
			t2:     &compound{functor: NewAtom("f"), args: []Term{NewNamedVariable("X"), Integer(0)}},
			result: false,
		},
		{
			t1:     &compound{functor: NewAtom("f"), args: []Term{NewNamedVariable("A"), NewNamedVariable("B")}},
			t2:     &compound{functor: NewAtom("g"), args: []Term{NewNamedVariable("X"), NewNamedVariable("Y")}},
			result: false,
		},
		{
			t1:     &compound{functor: NewAtom("f"), args: []Term{NewNamedVariable("A"), NewNamedVariable("B")}},
			t2:     &compound{functor: NewAtom("f"), args: []Term{NewNamedVariable("X"), NewNamedVariable("Y"), NewNamedVariable("Z")}},
			result: false,
		},
		{
			t1:     &compound{functor: NewAtom("f"), args: []Term{NewNamedVariable("A"), NewNamedVariable("B")}},
			t2:     Integer(0),
			result: false,
		},
		{
			t1:     Integer(1),
			t2:     Integer(0),
			result: false,
		},
	}

	for _, tt := range tests {
		assert.Equal(t, tt.result, variant(tt.t1, tt.t2, nil))
	}
}

func Test_writeTerm(t *testing.T) {
	v := NewNamedVariable("L")
	l := ListRest(v, NewAtom("a"), NewAtom("b"))
	w := NewNamedVariable("R")
	r := &compound{functor: NewAtom("f"), args: []Term{w}}
	env := NewEnv().Bind(v, l).Bind(w, r)

	ops := operators{}
	ops.define(1200, operatorSpecifierXFX, NewAtom(`:-`))
	ops.define(1200, operatorSpecifierFX, NewAtom(`:-`))
	ops.define(1200, operatorSpecifierXF, NewAtom(`-:`))
	ops.define(1105, operatorSpecifierXFY, NewAtom(`|`))
	ops.define(1000, operatorSpecifierXFY, NewAtom(`,`))
	ops.define(900, operatorSpecifierFY, atomNegation)
	ops.define(900, operatorSpecifierYF, NewAtom(`+/`))
	ops.define(500, operatorSpecifierYFX, NewAtom(`+`))
	ops.define(400, operatorSpecifierYFX, NewAtom(`*`))
	ops.define(200, operatorSpecifierFY, NewAtom(`-`))
	ops.define(200, operatorSpecifierYF, NewAtom(`--`))

	tests := []struct {
		title  string
		term   Term
		opts   WriteOptions
		output string
	}{
		{title: "named", term: NewNamedVariable(`X`), output: `X`},
		{title: "unnamed", term: NewVariable(), output: fmt.Sprintf("_%d", varCounter)},
		{title: "variable_names", term: NewNamedVariable(`X`), opts: WriteOptions{VariableNames: map[Variable]Atom{NewNamedVariable("X"): NewAtom("Foo")}}, output: `Foo`},

		{term: NewAtom(`a`), opts: WriteOptions{Quoted: false}, output: `a`},
		{term: NewAtom(`a`), opts: WriteOptions{Quoted: true}, output: `a`},
		{term: NewAtom("\a\b\f\n\r\t\v\x00\\'\"`"), opts: WriteOptions{Quoted: false}, output: "\a\b\f\n\r\t\v\x00\\'\"`"},
		{term: NewAtom("\a\b\f\n\r\t\v\x00\\'\"`"), opts: WriteOptions{Quoted: true}, output: "'\\a\\b\\f\\n\\r\\t\\v\\x0\\\\\\\\'\"`'"},
		{term: NewAtom(`,`), opts: WriteOptions{Quoted: false}, output: `,`},
		{term: NewAtom(`,`), opts: WriteOptions{Quoted: true}, output: `','`},
		{term: NewAtom(`[]`), opts: WriteOptions{Quoted: false}, output: `[]`},
		{term: NewAtom(`[]`), opts: WriteOptions{Quoted: true}, output: `[]`},
		{term: NewAtom(`{}`), opts: WriteOptions{Quoted: false}, output: `{}`},
		{term: NewAtom(`{}`), opts: WriteOptions{Quoted: true}, output: `{}`},
		{term: NewAtom(`-`), output: `-`},
		{term: NewAtom(`-`), opts: WriteOptions{ops: operators{atomPlus: {}, atomMinus: {}}, left: operator{specifier: operatorSpecifierFY, name: atomPlus}}, output: ` (-)`},
		{term: NewAtom(`-`), opts: WriteOptions{ops: operators{atomPlus: {}, atomMinus: {}}, right: operator{name: atomPlus}}, output: `(-)`},
		{term: NewAtom(`X`), opts: WriteOptions{Quoted: true, left: operator{name: NewAtom(`F`)}}, output: ` 'X'`},  // So that it won't be 'F''X'.
		{term: NewAtom(`X`), opts: WriteOptions{Quoted: true, right: operator{name: NewAtom(`F`)}}, output: `'X' `}, // So that it won't be 'X''F'.
		{term: NewAtom(`foo`), opts: WriteOptions{left: operator{name: NewAtom(`bar`)}}, output: ` foo`},            // So that it won't be barfoo.
		{term: NewAtom(`foo`), opts: WriteOptions{right: operator{name: NewAtom(`bar`)}}, output: `foo `},           // So that it won't be foobar.},

		{title: "positive", term: Integer(33), output: `33`},
		{title: "positive following unary minus", term: Integer(33), opts: WriteOptions{left: operator{name: atomMinus, specifier: operatorSpecifierFX}}, output: ` (33)`},
		{title: "negative", term: Integer(-33), output: `-33`},
		{title: "ambiguous 0b", term: Integer(0), opts: WriteOptions{right: operator{name: NewAtom(`b0`)}}, output: `0 `},  // So that it won't be 0b0.
		{title: "ambiguous 0o", term: Integer(0), opts: WriteOptions{right: operator{name: NewAtom(`o0`)}}, output: `0 `},  // So that it won't be 0o0.
		{title: "ambiguous 0x", term: Integer(0), opts: WriteOptions{right: operator{name: NewAtom(`x0`)}}, output: `0 `},  // So that it won't be 0x0.
		{title: "ambiguous 0'", term: Integer(0), opts: WriteOptions{right: operator{name: NewAtom(`Foo`)}}, output: `0 `}, // So that it won't be 0'Foo'.

		{title: "positive", term: Float(33.0), output: `33.0`},
		{title: "with e", term: Float(3.0e+100), output: `3.0e+100`},
		{title: "positive following unary minus", term: Float(33.0), opts: WriteOptions{left: operator{specifier: operatorSpecifierFX, name: atomMinus}}, output: ` (33.0)`},
		{title: "negative", term: Float(-33.0), output: `-33.0`},
		{title: "ambiguous e", term: Float(33.0), opts: WriteOptions{right: operator{name: NewAtom(`e`)}}, output: `33.0 `}, // So that it won't be 33.0e.

		{title: "list", term: List(NewAtom(`a`), NewAtom(`b`), NewAtom(`c`)), output: `[a,b,c]`},
		{title: "list-ish", term: ListRest(NewAtom(`rest`), NewAtom(`a`), NewAtom(`b`)), output: `[a,b|rest]`},
		{title: "circular list", term: l, output: `[a,b,a|...]`},
		{title: "curly brackets", term: &compound{functor: NewAtom(`{}`), args: []Term{NewAtom(`foo`)}}, output: `{foo}`},
		{title: "fx", term: &compound{functor: NewAtom(`:-`), args: []Term{&compound{functor: NewAtom(`:-`), args: []Term{NewAtom(`foo`)}}}}, opts: WriteOptions{ops: ops, priority: 1201}, output: `:- (:-foo)`},
		{title: "fy", term: &compound{functor: atomNegation, args: []Term{&compound{functor: NewAtom(`-`), args: []Term{&compound{functor: atomNegation, args: []Term{NewAtom(`foo`)}}}}}}, opts: WriteOptions{ops: ops, priority: 1201}, output: `\+ - (\+foo)`},
		{title: "xf", term: &compound{functor: NewAtom(`-:`), args: []Term{&compound{functor: NewAtom(`-:`), args: []Term{NewAtom(`foo`)}}}}, opts: WriteOptions{ops: ops, priority: 1201}, output: `(foo-:)-:`},
		{title: "yf", term: &compound{functor: NewAtom(`+/`), args: []Term{&compound{functor: NewAtom(`--`), args: []Term{&compound{functor: NewAtom(`+/`), args: []Term{NewAtom(`foo`)}}}}}}, opts: WriteOptions{ops: ops, priority: 1201}, output: `(foo+/)-- +/`},
		{title: "xfx", term: &compound{functor: atomIf, args: []Term{NewAtom("foo"), &compound{functor: atomIf, args: []Term{NewAtom("bar"), NewAtom("baz")}}}}, opts: WriteOptions{ops: ops, priority: 1201}, output: `foo:-(bar:-baz)`},
		{title: "yfx", term: &compound{functor: NewAtom("*"), args: []Term{Integer(2), &compound{functor: atomPlus, args: []Term{Integer(2), Integer(2)}}}}, opts: WriteOptions{ops: ops, priority: 1201}, output: `2*(2+2)`},
		{title: "xfy", term: &compound{functor: atomComma, args: []Term{Integer(2), &compound{functor: atomBar, args: []Term{Integer(2), Integer(2)}}}}, opts: WriteOptions{ops: ops, priority: 1201}, output: `2,(2|2)`},
		{title: "ignore_ops(false)", term: &compound{functor: atomPlus, args: []Term{Integer(2), Integer(-2)}}, opts: WriteOptions{IgnoreOps: false, ops: ops, priority: 1201}, output: `2+ -2`},
		{title: "ignore_ops(true)", term: &compound{functor: atomPlus, args: []Term{Integer(2), Integer(-2)}}, opts: WriteOptions{IgnoreOps: true, ops: ops, priority: 1201}, output: `+(2,-2)`},
		{title: "number_vars(false)", term: &compound{functor: NewAtom("f"), args: []Term{&compound{functor: atomVar, args: []Term{Integer(0)}}, &compound{functor: atomVar, args: []Term{Integer(1)}}, &compound{functor: atomVar, args: []Term{Integer(25)}}, &compound{functor: atomVar, args: []Term{Integer(26)}}, &compound{functor: atomVar, args: []Term{Integer(27)}}}}, opts: WriteOptions{Quoted: true, NumberVars: false, ops: ops, priority: 1201}, output: `f('$VAR'(0),'$VAR'(1),'$VAR'(25),'$VAR'(26),'$VAR'(27))`},
		{title: "number_vars(true)", term: &compound{functor: NewAtom("f"), args: []Term{&compound{functor: atomVar, args: []Term{Integer(0)}}, &compound{functor: atomVar, args: []Term{Integer(1)}}, &compound{functor: atomVar, args: []Term{Integer(25)}}, &compound{functor: atomVar, args: []Term{Integer(26)}}, &compound{functor: atomVar, args: []Term{Integer(27)}}}}, opts: WriteOptions{Quoted: true, NumberVars: true, ops: ops, priority: 1201}, output: `f(A,B,Z,A1,B1)`},
		{title: "prefix: spacing between operators", term: &compound{functor: NewAtom(`*`), args: []Term{NewAtom("a"), &compound{functor: NewAtom(`-`), args: []Term{NewAtom("b")}}}}, opts: WriteOptions{ops: ops, priority: 1201}, output: `a* -b`},
		{title: "postfix: spacing between unary minus and open/close", term: &compound{functor: NewAtom(`-`), args: []Term{&compound{functor: NewAtom(`+/`), args: []Term{NewAtom("a")}}}}, opts: WriteOptions{ops: ops, priority: 1201}, output: `- (a+/)`},
		{title: "infix: spacing between unary minus and open/close", term: &compound{functor: NewAtom(`-`), args: []Term{&compound{functor: NewAtom(`*`), args: []Term{NewAtom("a"), NewAtom("b")}}}}, opts: WriteOptions{ops: ops, priority: 1201}, output: `- (a*b)`},
		{title: "recursive", term: r, output: `f(...)`},

		{title: "stream", term: &Stream{}, output: `<*engine.Stream Value>`},
	}

	var buf bytes.Buffer
	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			buf.Reset()
			assert.NoError(t, writeTerm(&buf, tt.term, &tt.opts, env))
			assert.Equal(t, tt.output, buf.String())
		})
	}
}
