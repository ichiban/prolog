package engine

import (
	"bytes"
	"fmt"
	"io"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestVariable_WriteTerm(t *testing.T) {
	x := NewVariable()

	tests := []struct {
		title  string
		v      Variable
		w      io.StringWriter
		opts   WriteOptions
		output string
	}{
		{title: "unnamed", v: x, output: fmt.Sprintf("_%d", x)},
		{title: "variable_names", v: x, opts: WriteOptions{variableNames: map[Variable]Atom{x: NewAtom("Foo")}}, output: `Foo`},
	}

	var buf bytes.Buffer
	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			buf.Reset()
			assert.NoError(t, tt.v.WriteTerm(&buf, &tt.opts, nil))
			assert.Equal(t, tt.output, buf.String())
		})
	}
}

func TestVariable_Compare(t *testing.T) {
	w, x, y := NewVariable(), NewVariable(), NewVariable()

	tests := []struct {
		title string
		v     Variable
		t     Term
		o     int
	}{
		{title: `X > W`, v: x, t: w, o: 1},
		{title: `X = X`, v: x, t: x, o: 0},
		{title: `X < Y`, v: x, t: y, o: -1},
		{title: `X < 0.0`, v: x, t: NewFloatFromInt64(0), o: -1},
		{title: `X < 0`, v: x, t: Integer(0), o: -1},
		{title: `X < a`, v: x, t: NewAtom("a"), o: -1},
		{title: `X < f(a)`, v: x, t: NewAtom("f").Apply(NewAtom("a")), o: -1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			assert.Equal(t, tt.o, tt.v.Compare(tt.t, nil))
		})
	}
}

func Test_variableSet(t *testing.T) {
	f := NewAtom("f")
	x, y := NewVariable(), NewVariable()

	tests := []struct {
		term Term
		s    variableSet
	}{
		{term: f.Apply(x, y), s: map[Variable]int{
			x: 1,
			y: 1,
		}},
		{term: f.Apply(y, x), s: map[Variable]int{
			x: 1,
			y: 1,
		}},
		{term: atomPlus.Apply(x, y), s: map[Variable]int{
			x: 1,
			y: 1,
		}},
		{term: atomMinus.Apply(y, atomMinus.Apply(x, x)), s: map[Variable]int{
			x: 2,
			y: 1,
		}},
	}

	for _, tt := range tests {
		assert.Equal(t, tt.s, newVariableSet(tt.term, nil))
	}
}

func Test_existentialVariableSet(t *testing.T) {
	f := NewAtom("f")
	x, y, z := NewVariable(), NewVariable(), NewVariable()

	tests := []struct {
		term Term
		ev   variableSet
	}{
		{term: atomCaret.Apply(x, atomCaret.Apply(y, f.Apply(x, y, z))), ev: variableSet{
			x: 1,
			y: 1,
		}},
		{term: atomCaret.Apply(atomComma.Apply(x, y), f.Apply(z, y, x)), ev: variableSet{
			x: 1,
			y: 1,
		}},
		{term: atomCaret.Apply(atomPlus.Apply(x, y), Integer(3)), ev: variableSet{
			x: 1,
			y: 1,
		}},
	}

	for _, tt := range tests {
		assert.Equal(t, tt.ev, newExistentialVariablesSet(tt.term, nil))
	}
}

func Test_freeVariablesSet(t *testing.T) {
	f := NewAtom("f")
	x, y, z := NewVariable(), NewVariable(), NewVariable()
	a := NewVariable()

	tests := []struct {
		t, v Term
		fv   variableSet
	}{
		{t: atomPlus.Apply(x, atomPlus.Apply(y, z)), v: f.Apply(z), fv: variableSet{
			x: 1,
			y: 1,
		}},
		{t: atomCaret.Apply(z, atomPlus.Apply(a, atomPlus.Apply(x, atomPlus.Apply(y, z)))), v: a, fv: variableSet{
			x: 1,
			y: 1,
		}},
	}

	for _, tt := range tests {
		assert.Equal(t, tt.fv, newFreeVariablesSet(tt.t, tt.v, nil))
	}
}
