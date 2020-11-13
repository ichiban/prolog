package prolog

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestParser_Program(t *testing.T) {
	p := NewParser(`
append(nil,L,L).
append(cons(X,L1),L2,cons(X,L3)) :- append(L1,L2,L3).
`, DefaultOperators)
	cs, err := p.Program()
	if err != nil {
		t.Error(err)
	}

	assert.Equal(t, []Term{
		&Compound{
			Functor: "append",
			Args: []Term{
				Atom("nil"),
				&Variable{Name: "L"},
				&Variable{Name: "L"},
			},
		},
		&Compound{
			Functor: ":-",
			Args: []Term{
				&Compound{
					Functor: "append",
					Args: []Term{
						&Compound{
							Functor: "cons",
							Args: []Term{
								&Variable{Name: "X"},
								&Variable{Name: "L1"},
							},
						},
						&Variable{Name: "L2"},
						&Compound{
							Functor: "cons",
							Args: []Term{
								&Variable{Name: "X"},
								&Variable{Name: "L3"},
							},
						},
					},
				},
				&Compound{
					Functor: "append",
					Args: []Term{
						&Variable{Name: "L1"},
						&Variable{Name: "L2"},
						&Variable{Name: "L3"},
					},
				},
			},
		},
	}, cs)
}

func TestParser_Term(t *testing.T) {
	p := NewParser(`a + b * c`, Operators{
		{Precedence: 500, Type: YFX, Name: `+`},
		{Precedence: 400, Type: YFX, Name: `*`},
	})
	term, err := p.Term()
	assert.NoError(t, err)
	assert.Equal(t, term, &Compound{
		Functor: "+",
		Args: []Term{
			Atom("a"),
			&Compound{
				Functor: "*",
				Args: []Term{
					Atom("b"),
					Atom("c"),
				},
			},
		},
	})
}
