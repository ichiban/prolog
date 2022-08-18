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
		{term: &Compound{
			Functor: "f",
			Args:    []Term{Variable("X"), Variable("Y")},
		}, s: map[Variable]int{
			"X": 1,
			"Y": 1,
		}},
		{term: &Compound{
			Functor: "f",
			Args:    []Term{Variable("Y"), Variable("X")},
		}, s: map[Variable]int{
			"X": 1,
			"Y": 1,
		}},
		{term: &Compound{
			Functor: "+",
			Args:    []Term{Variable("X"), Variable("Y")},
		}, s: map[Variable]int{
			"X": 1,
			"Y": 1,
		}},
		{term: &Compound{
			Functor: "-",
			Args: []Term{
				Variable("Y"),
				&Compound{
					Functor: "-",
					Args:    []Term{Variable("X"), Variable("X")},
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
		{term: &Compound{
			Functor: "^",
			Args: []Term{
				Variable("X"),
				&Compound{
					Functor: "^",
					Args: []Term{
						Variable("Y"),
						&Compound{
							Functor: "f",
							Args:    []Term{Variable("X"), Variable("Y"), Variable("Z")},
						},
					},
				},
			},
		}, ev: variableSet{
			"X": 1,
			"Y": 1,
		}},
		{term: &Compound{
			Functor: "^",
			Args: []Term{
				&Compound{
					Functor: ",",
					Args:    []Term{Variable("X"), Variable("Y")},
				},
				&Compound{
					Functor: "f",
					Args:    []Term{Variable("Z"), Variable("Y"), Variable("X")},
				},
			},
		}, ev: variableSet{
			"X": 1,
			"Y": 1,
		}},
		{term: &Compound{
			Functor: "^",
			Args: []Term{
				&Compound{
					Functor: "+",
					Args:    []Term{Variable("X"), Variable("Y")},
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
		{t: &Compound{
			Functor: "+",
			Args: []Term{
				Variable("X"),
				&Compound{
					Functor: "+",
					Args:    []Term{Variable("Y"), Variable("Z")},
				},
			},
		}, v: &Compound{
			Functor: "f",
			Args:    []Term{Variable("Z")},
		}, fv: variableSet{
			"X": 1,
			"Y": 1,
		}},
		{t: &Compound{
			Functor: "^",
			Args: []Term{
				Variable("Z"),
				&Compound{
					Functor: "+",
					Args: []Term{
						Variable("A"),
						&Compound{
							Functor: "+",
							Args: []Term{
								Variable("X"),
								&Compound{
									Functor: "+",
									Args: []Term{
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
