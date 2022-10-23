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
			functor: "f",
			args:    []Term{NewNamedVariable("X"), NewNamedVariable("Y")},
		}, s: map[Variable]int{
			NewNamedVariable("X"): 1,
			NewNamedVariable("Y"): 1,
		}},
		{term: &compound{
			functor: "f",
			args:    []Term{NewNamedVariable("Y"), NewNamedVariable("X")},
		}, s: map[Variable]int{
			NewNamedVariable("X"): 1,
			NewNamedVariable("Y"): 1,
		}},
		{term: &compound{
			functor: "+",
			args:    []Term{NewNamedVariable("X"), NewNamedVariable("Y")},
		}, s: map[Variable]int{
			NewNamedVariable("X"): 1,
			NewNamedVariable("Y"): 1,
		}},
		{term: &compound{
			functor: "-",
			args: []Term{
				NewNamedVariable("Y"),
				&compound{
					functor: "-",
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
			functor: "^",
			args: []Term{
				NewNamedVariable("X"),
				&compound{
					functor: "^",
					args: []Term{
						NewNamedVariable("Y"),
						&compound{
							functor: "f",
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
			functor: "^",
			args: []Term{
				&compound{
					functor: ",",
					args:    []Term{NewNamedVariable("X"), NewNamedVariable("Y")},
				},
				&compound{
					functor: "f",
					args:    []Term{NewNamedVariable("Z"), NewNamedVariable("Y"), NewNamedVariable("X")},
				},
			},
		}, ev: variableSet{
			NewNamedVariable("X"): 1,
			NewNamedVariable("Y"): 1,
		}},
		{term: &compound{
			functor: "^",
			args: []Term{
				&compound{
					functor: "+",
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
			functor: "+",
			args: []Term{
				NewNamedVariable("X"),
				&compound{
					functor: "+",
					args:    []Term{NewNamedVariable("Y"), NewNamedVariable("Z")},
				},
			},
		}, v: &compound{
			functor: "f",
			args:    []Term{NewNamedVariable("Z")},
		}, fv: variableSet{
			NewNamedVariable("X"): 1,
			NewNamedVariable("Y"): 1,
		}},
		{t: &compound{
			functor: "^",
			args: []Term{
				NewNamedVariable("Z"),
				&compound{
					functor: "+",
					args: []Term{
						NewNamedVariable("A"),
						&compound{
							functor: "+",
							args: []Term{
								NewNamedVariable("X"),
								&compound{
									functor: "+",
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
