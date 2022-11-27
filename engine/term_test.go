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
	x := NewVariable()

	tests := []struct {
		t, g Term
	}{
		{t: &compound{
			functor: atomCaret,
			args: []Term{
				x,
				&compound{
					functor: NewAtom("foo"),
					args:    []Term{x},
				},
			},
		}, g: &compound{
			functor: NewAtom("foo"),
			args:    []Term{x},
		}},
		{
			t: atomCaret.Apply(NewVariable(), atomCaret.Apply(NewVariable(), atomEqual.Apply(x, Integer(1)))),
			g: atomEqual.Apply(x, Integer(1)),
		},
	}

	for _, tt := range tests {
		assert.Equal(t, tt.g, iteratedGoalTerm(tt.t, nil))
	}
}

func Test_variant(t *testing.T) {
	f, g := NewAtom("f"), NewAtom("g")
	a, b := NewVariable(), NewVariable()
	x, y, z := NewVariable(), NewVariable(), NewVariable()
	p, q := NewVariable(), NewVariable()

	tests := []struct {
		t1, t2 Term
		result bool
	}{
		{
			t1:     f.Apply(a, b, a),
			t2:     f.Apply(x, y, x),
			result: true,
		},
		{
			t1:     g.Apply(a, b),
			t2:     g.Apply(NewVariable(), NewVariable()),
			result: true,
		},
		{
			t1:     atomPlus.Apply(p, q),
			t2:     atomPlus.Apply(p, q),
			result: true,
		},
		{
			t1:     f.Apply(a, a),
			t2:     f.Apply(x, y),
			result: false,
		},
		{
			t1:     f.Apply(a, a),
			t2:     f.Apply(x, Integer(0)),
			result: false,
		},
		{
			t1:     f.Apply(a, b),
			t2:     g.Apply(x, y),
			result: false,
		},
		{
			t1:     f.Apply(a, b),
			t2:     f.Apply(x, y, z),
			result: false,
		},
		{
			t1:     f.Apply(a, b),
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
	f := NewAtom("f")
	v, w := NewVariable(), NewVariable()
	l := PartialList(v, NewAtom("a"), NewAtom("b"))
	r := f.Apply(w)
	env := NewEnv().Bind(v, l).Bind(w, r)

	x := NewVariable()

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
		opts   writeOptions
		output string
	}{
		{title: "unnamed", term: x, output: fmt.Sprintf("_%d", x)},
		{title: "variable_names", term: x, opts: writeOptions{variableNames: map[Variable]Atom{x: NewAtom("Foo")}}, output: `Foo`},

		{term: NewAtom(`a`), opts: writeOptions{quoted: false}, output: `a`},
		{term: NewAtom(`a`), opts: writeOptions{quoted: true}, output: `a`},
		{term: NewAtom("\a\b\f\n\r\t\v\x00\\'\"`"), opts: writeOptions{quoted: false}, output: "\a\b\f\n\r\t\v\x00\\'\"`"},
		{term: NewAtom("\a\b\f\n\r\t\v\x00\\'\"`"), opts: writeOptions{quoted: true}, output: "'\\a\\b\\f\\n\\r\\t\\v\\x0\\\\\\\\'\"`'"},
		{term: NewAtom(`,`), opts: writeOptions{quoted: false}, output: `,`},
		{term: NewAtom(`,`), opts: writeOptions{quoted: true}, output: `','`},
		{term: NewAtom(`[]`), opts: writeOptions{quoted: false}, output: `[]`},
		{term: NewAtom(`[]`), opts: writeOptions{quoted: true}, output: `[]`},
		{term: NewAtom(`{}`), opts: writeOptions{quoted: false}, output: `{}`},
		{term: NewAtom(`{}`), opts: writeOptions{quoted: true}, output: `{}`},
		{term: NewAtom(`-`), output: `-`},
		{term: NewAtom(`-`), opts: writeOptions{ops: operators{atomPlus: {}, atomMinus: {}}, left: operator{specifier: operatorSpecifierFY, name: atomPlus}}, output: ` (-)`},
		{term: NewAtom(`-`), opts: writeOptions{ops: operators{atomPlus: {}, atomMinus: {}}, right: operator{name: atomPlus}}, output: `(-)`},
		{term: NewAtom(`X`), opts: writeOptions{quoted: true, left: operator{name: NewAtom(`F`)}}, output: ` 'X'`},  // So that it won't be 'F''X'.
		{term: NewAtom(`X`), opts: writeOptions{quoted: true, right: operator{name: NewAtom(`F`)}}, output: `'X' `}, // So that it won't be 'X''F'.
		{term: NewAtom(`foo`), opts: writeOptions{left: operator{name: NewAtom(`bar`)}}, output: ` foo`},            // So that it won't be barfoo.
		{term: NewAtom(`foo`), opts: writeOptions{right: operator{name: NewAtom(`bar`)}}, output: `foo `},           // So that it won't be foobar.},

		{title: "positive", term: Integer(33), output: `33`},
		{title: "positive following unary minus", term: Integer(33), opts: writeOptions{left: operator{name: atomMinus, specifier: operatorSpecifierFX}}, output: ` (33)`},
		{title: "negative", term: Integer(-33), output: `-33`},
		{title: "ambiguous 0b", term: Integer(0), opts: writeOptions{right: operator{name: NewAtom(`b0`)}}, output: `0 `},  // So that it won't be 0b0.
		{title: "ambiguous 0o", term: Integer(0), opts: writeOptions{right: operator{name: NewAtom(`o0`)}}, output: `0 `},  // So that it won't be 0o0.
		{title: "ambiguous 0x", term: Integer(0), opts: writeOptions{right: operator{name: NewAtom(`x0`)}}, output: `0 `},  // So that it won't be 0x0.
		{title: "ambiguous 0'", term: Integer(0), opts: writeOptions{right: operator{name: NewAtom(`Foo`)}}, output: `0 `}, // So that it won't be 0'Foo'.

		{title: "positive", term: Float(33.0), output: `33.0`},
		{title: "with e", term: Float(3.0e+100), output: `3.0e+100`},
		{title: "positive following unary minus", term: Float(33.0), opts: writeOptions{left: operator{specifier: operatorSpecifierFX, name: atomMinus}}, output: ` (33.0)`},
		{title: "negative", term: Float(-33.0), output: `-33.0`},
		{title: "ambiguous e", term: Float(33.0), opts: writeOptions{right: operator{name: NewAtom(`e`)}}, output: `33.0 `}, // So that it won't be 33.0e.

		{title: "list", term: List(NewAtom(`a`), NewAtom(`b`), NewAtom(`c`)), output: `[a,b,c]`},
		{title: "list-ish", term: PartialList(NewAtom(`rest`), NewAtom(`a`), NewAtom(`b`)), output: `[a,b|rest]`},
		{title: "circular list", term: l, output: `[a,b,a|...]`},
		{title: "curly brackets", term: atomEmptyBlock.Apply(NewAtom(`foo`)), output: `{foo}`},
		{title: "fx", term: atomIf.Apply(atomIf.Apply(NewAtom(`foo`))), opts: writeOptions{ops: ops, priority: 1201}, output: `:- (:-foo)`},

		{title: "fy", term: atomNegation.Apply(atomMinus.Apply(atomNegation.Apply(NewAtom(`foo`)))), opts: writeOptions{ops: ops, priority: 1201}, output: `\+ - (\+foo)`},
		{title: "xf", term: NewAtom(`-:`).Apply(NewAtom(`-:`).Apply(NewAtom(`foo`))), opts: writeOptions{ops: ops, priority: 1201}, output: `(foo-:)-:`},
		{title: "yf", term: NewAtom(`+/`).Apply(NewAtom(`--`).Apply(NewAtom(`+/`).Apply(NewAtom(`foo`)))), opts: writeOptions{ops: ops, priority: 1201}, output: `(foo+/)-- +/`},
		{title: "xfx", term: atomIf.Apply(NewAtom("foo"), atomIf.Apply(NewAtom("bar"), NewAtom("baz"))), opts: writeOptions{ops: ops, priority: 1201}, output: `foo:-(bar:-baz)`},
		{title: "yfx", term: atomAsterisk.Apply(Integer(2), atomPlus.Apply(Integer(2), Integer(2))), opts: writeOptions{ops: ops, priority: 1201}, output: `2*(2+2)`},
		{title: "xfy", term: atomComma.Apply(Integer(2), atomBar.Apply(Integer(2), Integer(2))), opts: writeOptions{ops: ops, priority: 1201}, output: `2,(2|2)`},
		{title: "ignore_ops(false)", term: atomPlus.Apply(Integer(2), Integer(-2)), opts: writeOptions{ignoreOps: false, ops: ops, priority: 1201}, output: `2+ -2`},
		{title: "ignore_ops(true)", term: atomPlus.Apply(Integer(2), Integer(-2)), opts: writeOptions{ignoreOps: true, ops: ops, priority: 1201}, output: `+(2,-2)`},
		{title: "number_vars(false)", term: f.Apply(atomVar.Apply(Integer(0)), atomVar.Apply(Integer(1)), atomVar.Apply(Integer(25)), atomVar.Apply(Integer(26)), atomVar.Apply(Integer(27))), opts: writeOptions{quoted: true, numberVars: false, ops: ops, priority: 1201}, output: `f('$VAR'(0),'$VAR'(1),'$VAR'(25),'$VAR'(26),'$VAR'(27))`},
		{title: "number_vars(true)", term: f.Apply(atomVar.Apply(Integer(0)), atomVar.Apply(Integer(1)), atomVar.Apply(Integer(25)), atomVar.Apply(Integer(26)), atomVar.Apply(Integer(27))), opts: writeOptions{quoted: true, numberVars: true, ops: ops, priority: 1201}, output: `f(A,B,Z,A1,B1)`},
		{title: "prefix: spacing between operators", term: atomAsterisk.Apply(NewAtom("a"), atomMinus.Apply(NewAtom("b"))), opts: writeOptions{ops: ops, priority: 1201}, output: `a* -b`},
		{title: "postfix: spacing between unary minus and open/close", term: atomMinus.Apply(NewAtom(`+/`).Apply(NewAtom("a"))), opts: writeOptions{ops: ops, priority: 1201}, output: `- (a+/)`},
		{title: "infix: spacing between unary minus and open/close", term: atomMinus.Apply(atomAsterisk.Apply(NewAtom("a"), NewAtom("b"))), opts: writeOptions{ops: ops, priority: 1201}, output: `- (a*b)`},
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
