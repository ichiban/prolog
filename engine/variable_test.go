package engine

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

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
