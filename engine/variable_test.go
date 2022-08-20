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
			args:    []Term{Variable("X"), Variable("Y")},
		}, s: map[Variable]int{
			"X": 1,
			"Y": 1,
		}},
		{term: &compound{
			functor: "f",
			args:    []Term{Variable("Y"), Variable("X")},
		}, s: map[Variable]int{
			"X": 1,
			"Y": 1,
		}},
		{term: &compound{
			functor: "+",
			args:    []Term{Variable("X"), Variable("Y")},
		}, s: map[Variable]int{
			"X": 1,
			"Y": 1,
		}},
		{term: &compound{
			functor: "-",
			args: []Term{
				Variable("Y"),
				&compound{
					functor: "-",
					args:    []Term{Variable("X"), Variable("X")},
				},
			},
		}, s: map[Variable]int{
			"X": 2,
			"Y": 1,
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
				Variable("X"),
				&compound{
					functor: "^",
					args: []Term{
						Variable("Y"),
						&compound{
							functor: "f",
							args:    []Term{Variable("X"), Variable("Y"), Variable("Z")},
						},
					},
				},
			},
		}, ev: variableSet{
			"X": 1,
			"Y": 1,
		}},
		{term: &compound{
			functor: "^",
			args: []Term{
				&compound{
					functor: ",",
					args:    []Term{Variable("X"), Variable("Y")},
				},
				&compound{
					functor: "f",
					args:    []Term{Variable("Z"), Variable("Y"), Variable("X")},
				},
			},
		}, ev: variableSet{
			"X": 1,
			"Y": 1,
		}},
		{term: &compound{
			functor: "^",
			args: []Term{
				&compound{
					functor: "+",
					args:    []Term{Variable("X"), Variable("Y")},
				},
				Integer(3),
			},
		}, ev: variableSet{
			"X": 1,
			"Y": 1,
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
				Variable("X"),
				&compound{
					functor: "+",
					args:    []Term{Variable("Y"), Variable("Z")},
				},
			},
		}, v: &compound{
			functor: "f",
			args:    []Term{Variable("Z")},
		}, fv: variableSet{
			"X": 1,
			"Y": 1,
		}},
		{t: &compound{
			functor: "^",
			args: []Term{
				Variable("Z"),
				&compound{
					functor: "+",
					args: []Term{
						Variable("A"),
						&compound{
							functor: "+",
							args: []Term{
								Variable("X"),
								&compound{
									functor: "+",
									args: []Term{
										Variable("Y"),
										Variable("Z"),
									},
								},
							},
						},
					},
				},
			},
		}, v: Variable("A"), fv: variableSet{
			"X": 1,
			"Y": 1,
		}},
	}

	for _, tt := range tests {
		assert.Equal(t, tt.fv, newFreeVariablesSet(tt.t, tt.v, nil))
	}
}
