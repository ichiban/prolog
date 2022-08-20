package engine

import (
	"bytes"
	"errors"
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestContains(t *testing.T) {
	var env *Env
	assert.True(t, Contains(Atom("a"), Atom("a"), env))
	assert.False(t, Contains(NewVariable(), Atom("a"), env))
	v := Variable("V")
	env = env.Bind(v, Atom("a"))
	assert.True(t, Contains(v, Atom("a"), env))
	assert.True(t, Contains(&compound{functor: "a"}, Atom("a"), env))
	assert.True(t, Contains(&compound{functor: "f", args: []Term{Atom("a")}}, Atom("a"), env))
	assert.False(t, Contains(&compound{functor: "f"}, Atom("a"), env))
}

func TestRulify(t *testing.T) {
	assert.Equal(t, &compound{
		functor: ":-",
		args:    []Term{Atom("a"), Atom("true")},
	}, Rulify(Atom("a"), nil))
	v := Variable("V")
	env := NewEnv().
		Bind(v, Atom("a"))
	assert.Equal(t, &compound{
		functor: ":-",
		args:    []Term{Atom("a"), Atom("true")},
	}, Rulify(v, env))
	assert.Equal(t, &compound{
		functor: ":-",
		args:    []Term{Atom("a"), Atom("b")},
	}, Rulify(&compound{
		functor: ":-",
		args:    []Term{Atom("a"), Atom("b")},
	}, nil))
}

func TestErrWriter_Write(t *testing.T) {
	var failed = errors.New("failed")

	var m mockWriter
	m.On("Write", []byte("foo")).Return(0, failed).Once()
	defer m.AssertExpectations(t)

	ew := errWriter{w: &m}
	_, err := ew.Write([]byte("foo"))
	assert.NoError(t, err)
	_, err = ew.Write([]byte("bar"))
	assert.NoError(t, err)
	_, err = ew.Write([]byte("baz"))
	assert.NoError(t, err)
	assert.Equal(t, failed, ew.err)
}

func Test_iteratedGoalTerm(t *testing.T) {
	tests := []struct {
		t, g Term
	}{
		{t: &compound{
			functor: "^",
			args: []Term{
				Variable("X"),
				&compound{
					functor: "foo",
					args:    []Term{Variable("X")},
				},
			},
		}, g: &compound{
			functor: "foo",
			args:    []Term{Variable("X")},
		}},
		{
			t: Atom("^").Apply(NewVariable(), Atom("^").Apply(NewVariable(), Atom("=").Apply(Variable("X"), Integer(1)))),
			g: Atom("=").Apply(Variable("X"), Integer(1)),
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
			t1:     &compound{functor: "f", args: []Term{Variable("A"), Variable("B"), Variable("A")}},
			t2:     &compound{functor: "f", args: []Term{Variable("X"), Variable("Y"), Variable("X")}},
			result: true,
		},
		{
			t1:     &compound{functor: "g", args: []Term{Variable("A"), Variable("B")}},
			t2:     &compound{functor: "g", args: []Term{NewVariable(), NewVariable()}},
			result: true,
		},
		{
			t1:     &compound{functor: "+", args: []Term{Variable("P"), Variable("Q")}},
			t2:     &compound{functor: "+", args: []Term{Variable("P"), Variable("Q")}},
			result: true,
		},
		{
			t1:     &compound{functor: "f", args: []Term{Variable("A"), Variable("A")}},
			t2:     &compound{functor: "f", args: []Term{Variable("X"), Variable("Y")}},
			result: false,
		},
		{
			t1:     &compound{functor: "f", args: []Term{Variable("A"), Variable("A")}},
			t2:     &compound{functor: "f", args: []Term{Variable("X"), Integer(0)}},
			result: false,
		},
		{
			t1:     &compound{functor: "f", args: []Term{Variable("A"), Variable("B")}},
			t2:     &compound{functor: "g", args: []Term{Variable("X"), Variable("Y")}},
			result: false,
		},
		{
			t1:     &compound{functor: "f", args: []Term{Variable("A"), Variable("B")}},
			t2:     &compound{functor: "f", args: []Term{Variable("X"), Variable("Y"), Variable("Z")}},
			result: false,
		},
		{
			t1:     &compound{functor: "f", args: []Term{Variable("A"), Variable("B")}},
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

func TestWriteTerm(t *testing.T) {
	v := Variable("L")
	l := ListRest(v, Atom("a"), Atom("b"))
	w := Variable("R")
	r := &compound{functor: "f", args: []Term{w}}
	env := NewEnv().Bind(v, l).Bind(w, r)

	ops := operators{}
	ops.define(1200, operatorSpecifierXFX, `:-`)
	ops.define(1200, operatorSpecifierFX, `:-`)
	ops.define(1200, operatorSpecifierXF, `-:`)
	ops.define(1105, operatorSpecifierXFY, `|`)
	ops.define(1000, operatorSpecifierXFY, `,`)
	ops.define(900, operatorSpecifierFY, `\+`)
	ops.define(900, operatorSpecifierYF, `+/`)
	ops.define(500, operatorSpecifierYFX, `+`)
	ops.define(400, operatorSpecifierYFX, `*`)
	ops.define(200, operatorSpecifierFY, `-`)
	ops.define(200, operatorSpecifierYF, `--`)

	tests := []struct {
		title  string
		term   Term
		opts   WriteOptions
		output string
	}{
		{title: "named", term: Variable(`X`), output: `X`},
		{title: "unnamed", term: Variable(``) /* NewVariable() */, output: `_1`},
		{title: "variable_names", term: Variable(`X`), opts: WriteOptions{VariableNames: map[Variable]Atom{"X": "Foo"}}, output: `Foo`},

		{term: Atom(`a`), opts: WriteOptions{Quoted: false}, output: `a`},
		{term: Atom(`a`), opts: WriteOptions{Quoted: true}, output: `a`},
		{term: Atom("\a\b\f\n\r\t\v\x00\\'\"`"), opts: WriteOptions{Quoted: false}, output: "\a\b\f\n\r\t\v\x00\\'\"`"},
		{term: Atom("\a\b\f\n\r\t\v\x00\\'\"`"), opts: WriteOptions{Quoted: true}, output: "'\\a\\b\\f\\n\\r\\t\\v\\x0\\\\\\\\'\"`'"},
		{term: Atom(`,`), opts: WriteOptions{Quoted: false}, output: `,`},
		{term: Atom(`,`), opts: WriteOptions{Quoted: true}, output: `','`},
		{term: Atom(`[]`), opts: WriteOptions{Quoted: false}, output: `[]`},
		{term: Atom(`[]`), opts: WriteOptions{Quoted: true}, output: `[]`},
		{term: Atom(`{}`), opts: WriteOptions{Quoted: false}, output: `{}`},
		{term: Atom(`{}`), opts: WriteOptions{Quoted: true}, output: `{}`},
		{term: Atom(`-`), output: `-`},
		{term: Atom(`-`), opts: WriteOptions{ops: operators{"+": {}, "-": {}}, left: operator{specifier: operatorSpecifierFY, name: "+"}}, output: ` (-)`},
		{term: Atom(`-`), opts: WriteOptions{ops: operators{"+": {}, "-": {}}, right: operator{name: "+"}}, output: `(-)`},
		{term: Atom(`X`), opts: WriteOptions{Quoted: true, left: operator{name: `F`}}, output: ` 'X'`},  // So that it won't be 'F''X'.
		{term: Atom(`X`), opts: WriteOptions{Quoted: true, right: operator{name: `F`}}, output: `'X' `}, // So that it won't be 'X''F'.
		{term: Atom(`foo`), opts: WriteOptions{left: operator{name: `bar`}}, output: ` foo`},            // So that it won't be barfoo.
		{term: Atom(`foo`), opts: WriteOptions{right: operator{name: `bar`}}, output: `foo `},           // So that it won't be foobar.},

		{title: "positive", term: Integer(33), output: `33`},
		{title: "positive following unary minus", term: Integer(33), opts: WriteOptions{left: operator{name: "-", specifier: operatorSpecifierFX}}, output: ` (33)`},
		{title: "negative", term: Integer(-33), output: `-33`},
		{title: "ambiguous 0b", term: Integer(0), opts: WriteOptions{right: operator{name: `b0`}}, output: `0 `},  // So that it won't be 0b0.
		{title: "ambiguous 0o", term: Integer(0), opts: WriteOptions{right: operator{name: `o0`}}, output: `0 `},  // So that it won't be 0o0.
		{title: "ambiguous 0x", term: Integer(0), opts: WriteOptions{right: operator{name: `x0`}}, output: `0 `},  // So that it won't be 0x0.
		{title: "ambiguous 0'", term: Integer(0), opts: WriteOptions{right: operator{name: `Foo`}}, output: `0 `}, // So that it won't be 0'Foo'.

		{title: "positive", term: Float(33.0), output: `33.0`},
		{title: "with e", term: Float(3.0e+100), output: `3.0e+100`},
		{title: "positive following unary minus", term: Float(33.0), opts: WriteOptions{left: operator{specifier: operatorSpecifierFX, name: "-"}}, output: ` (33.0)`},
		{title: "negative", term: Float(-33.0), output: `-33.0`},
		{title: "ambiguous e", term: Float(33.0), opts: WriteOptions{right: operator{name: `e`}}, output: `33.0 `}, // So that it won't be 33.0e.

		{title: "list", term: List(Atom(`a`), Atom(`b`), Atom(`c`)), output: `[a,b,c]`},
		{title: "list-ish", term: ListRest(Atom(`rest`), Atom(`a`), Atom(`b`)), output: `[a,b|rest]`},
		{title: "circular list", term: l, output: `[a,b,a|...]`},
		{title: "curly brackets", term: &compound{functor: `{}`, args: []Term{Atom(`foo`)}}, output: `{foo}`},
		{title: "fx", term: &compound{functor: `:-`, args: []Term{&compound{functor: `:-`, args: []Term{Atom(`foo`)}}}}, opts: WriteOptions{ops: ops, priority: 1201}, output: `:- (:-foo)`},
		{title: "fy", term: &compound{functor: `\+`, args: []Term{&compound{functor: `-`, args: []Term{&compound{functor: `\+`, args: []Term{Atom(`foo`)}}}}}}, opts: WriteOptions{ops: ops, priority: 1201}, output: `\+ - (\+foo)`},
		{title: "xf", term: &compound{functor: `-:`, args: []Term{&compound{functor: `-:`, args: []Term{Atom(`foo`)}}}}, opts: WriteOptions{ops: ops, priority: 1201}, output: `(foo-:)-:`},
		{title: "yf", term: &compound{functor: `+/`, args: []Term{&compound{functor: `--`, args: []Term{&compound{functor: `+/`, args: []Term{Atom(`foo`)}}}}}}, opts: WriteOptions{ops: ops, priority: 1201}, output: `(foo+/)-- +/`},
		{title: "xfx", term: &compound{functor: ":-", args: []Term{Atom("foo"), &compound{functor: ":-", args: []Term{Atom("bar"), Atom("baz")}}}}, opts: WriteOptions{ops: ops, priority: 1201}, output: `foo:-(bar:-baz)`},
		{title: "yfx", term: &compound{functor: "*", args: []Term{Integer(2), &compound{functor: "+", args: []Term{Integer(2), Integer(2)}}}}, opts: WriteOptions{ops: ops, priority: 1201}, output: `2*(2+2)`},
		{title: "xfy", term: &compound{functor: ",", args: []Term{Integer(2), &compound{functor: "|", args: []Term{Integer(2), Integer(2)}}}}, opts: WriteOptions{ops: ops, priority: 1201}, output: `2,(2|2)`},
		{title: "ignore_ops(false)", term: &compound{functor: "+", args: []Term{Integer(2), Integer(-2)}}, opts: WriteOptions{IgnoreOps: false, ops: ops, priority: 1201}, output: `2+ -2`},
		{title: "ignore_ops(true)", term: &compound{functor: "+", args: []Term{Integer(2), Integer(-2)}}, opts: WriteOptions{IgnoreOps: true, ops: ops, priority: 1201}, output: `+(2,-2)`},
		{title: "number_vars(false)", term: &compound{functor: "f", args: []Term{&compound{functor: "$VAR", args: []Term{Integer(0)}}, &compound{functor: "$VAR", args: []Term{Integer(1)}}, &compound{functor: "$VAR", args: []Term{Integer(25)}}, &compound{functor: "$VAR", args: []Term{Integer(26)}}, &compound{functor: "$VAR", args: []Term{Integer(27)}}}}, opts: WriteOptions{Quoted: true, NumberVars: false, ops: ops, priority: 1201}, output: `f('$VAR'(0),'$VAR'(1),'$VAR'(25),'$VAR'(26),'$VAR'(27))`},
		{title: "number_vars(true)", term: &compound{functor: "f", args: []Term{&compound{functor: "$VAR", args: []Term{Integer(0)}}, &compound{functor: "$VAR", args: []Term{Integer(1)}}, &compound{functor: "$VAR", args: []Term{Integer(25)}}, &compound{functor: "$VAR", args: []Term{Integer(26)}}, &compound{functor: "$VAR", args: []Term{Integer(27)}}}}, opts: WriteOptions{Quoted: true, NumberVars: true, ops: ops, priority: 1201}, output: `f(A,B,Z,A1,B1)`},
		{title: "prefix: spacing between operators", term: &compound{functor: `*`, args: []Term{Atom("a"), &compound{functor: `-`, args: []Term{Atom("b")}}}}, opts: WriteOptions{ops: ops, priority: 1201}, output: `a* -b`},
		{title: "postfix: spacing between unary minus and open/close", term: &compound{functor: `-`, args: []Term{&compound{functor: `+/`, args: []Term{Atom("a")}}}}, opts: WriteOptions{ops: ops, priority: 1201}, output: `- (a+/)`},
		{title: "infix: spacing between unary minus and open/close", term: &compound{functor: `-`, args: []Term{&compound{functor: `*`, args: []Term{Atom("a"), Atom("b")}}}}, opts: WriteOptions{ops: ops, priority: 1201}, output: `- (a*b)`},
		{title: "recursive", term: r, output: `f(...)`},

		{title: "stream", term: &Stream{}, output: `<*engine.Stream Value>`},
	}

	var buf bytes.Buffer
	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			buf.Reset()
			varCounter = 0
			if tt.term == Variable("") {
				tt.term = NewVariable()
			}
			assert.NoError(t, WriteTerm(&buf, tt.term, &tt.opts, env))
			assert.Equal(t, tt.output, buf.String())
		})
	}
}
