package engine

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func Test_variableSet(t *testing.T) {
	tests := []struct {
		term Term
		s    variableSet
	}{
		{term: &compound{
			functor: NewAtom("f"),
			args:    []Term{NewNamedVariable("X"), NewNamedVariable("Y")},
		}, s: map[Variable]int{
			NewNamedVariable("X"): 1,
			NewNamedVariable("Y"): 1,
		}},
		{term: &compound{
			functor: NewAtom("f"),
			args:    []Term{NewNamedVariable("Y"), NewNamedVariable("X")},
		}, s: map[Variable]int{
			NewNamedVariable("X"): 1,
			NewNamedVariable("Y"): 1,
		}},
		{term: &compound{
			functor: atomPlus,
			args:    []Term{NewNamedVariable("X"), NewNamedVariable("Y")},
		}, s: map[Variable]int{
			NewNamedVariable("X"): 1,
			NewNamedVariable("Y"): 1,
		}},
		{term: &compound{
			functor: atomMinus,
			args: []Term{
				NewNamedVariable("Y"),
				&compound{
					functor: atomMinus,
					args:    []Term{NewNamedVariable("X"), NewNamedVariable("X")},
				},
			},
		}, s: map[Variable]int{
			NewNamedVariable("X"): 2,
			NewNamedVariable("Y"): 1,
		}},
	}

	for _, tt := range tests {
		assert.Equal(t, tt.s, newVariableSet(tt.term, nil))
	}
}

func Test_existentialVariableSet(t *testing.T) {
	tests := []struct {
		term Term
		ev   variableSet
	}{
		{term: &compound{
			functor: atomCaret,
			args: []Term{
				NewNamedVariable("X"),
				&compound{
					functor: atomCaret,
					args: []Term{
						NewNamedVariable("Y"),
						&compound{
							functor: NewAtom("f"),
							args:    []Term{NewNamedVariable("X"), NewNamedVariable("Y"), NewNamedVariable("Z")},
						},
					},
				},
			},
		}, ev: variableSet{
			NewNamedVariable("X"): 1,
			NewNamedVariable("Y"): 1,
		}},
		{term: &compound{
			functor: atomCaret,
			args: []Term{
				&compound{
					functor: atomComma,
					args:    []Term{NewNamedVariable("X"), NewNamedVariable("Y")},
				},
				&compound{
					functor: NewAtom("f"),
					args:    []Term{NewNamedVariable("Z"), NewNamedVariable("Y"), NewNamedVariable("X")},
				},
			},
		}, ev: variableSet{
			NewNamedVariable("X"): 1,
			NewNamedVariable("Y"): 1,
		}},
		{term: &compound{
			functor: atomCaret,
			args: []Term{
				&compound{
					functor: atomPlus,
					args:    []Term{NewNamedVariable("X"), NewNamedVariable("Y")},
				},
				Integer(3),
			},
		}, ev: variableSet{
			NewNamedVariable("X"): 1,
			NewNamedVariable("Y"): 1,
		}},
	}

	for _, tt := range tests {
		assert.Equal(t, tt.ev, newExistentialVariablesSet(tt.term, nil))
	}
}

func Test_freeVariablesSet(t *testing.T) {
	tests := []struct {
		t, v Term
		fv   variableSet
	}{
		{t: &compound{
			functor: atomPlus,
			args: []Term{
				NewNamedVariable("X"),
				&compound{
					functor: atomPlus,
					args:    []Term{NewNamedVariable("Y"), NewNamedVariable("Z")},
				},
			},
		}, v: &compound{
			functor: NewAtom("f"),
			args:    []Term{NewNamedVariable("Z")},
		}, fv: variableSet{
			NewNamedVariable("X"): 1,
			NewNamedVariable("Y"): 1,
		}},
		{t: &compound{
			functor: atomCaret,
			args: []Term{
				NewNamedVariable("Z"),
				&compound{
					functor: atomPlus,
					args: []Term{
						NewNamedVariable("A"),
						&compound{
							functor: atomPlus,
							args: []Term{
								NewNamedVariable("X"),
								&compound{
									functor: atomPlus,
									args: []Term{
										NewNamedVariable("Y"),
										NewNamedVariable("Z"),
									},
								},
							},
						},
					},
				},
			},
		}, v: NewNamedVariable("A"), fv: variableSet{
			NewNamedVariable("X"): 1,
			NewNamedVariable("Y"): 1,
		}},
	}

	for _, tt := range tests {
		assert.Equal(t, tt.fv, newFreeVariablesSet(tt.t, tt.v, nil))
	}
}
