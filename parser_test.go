package prolog

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestParser_Program(t *testing.T) {
	t.Run("empty", func(t *testing.T) {
		p := NewParser(``, nil)
		cs, err := p.Program()
		assert.NoError(t, err)
		assert.Empty(t, cs)
	})

	t.Run("non empty", func(t *testing.T) {
		p := NewParser(`
append(nil,L,L).
append(cons(X,L1),L2,cons(X,L3)) :- append(L1,L2,L3).
`, operators{
			{Precedence: 1200, Type: `xfx`, Name: `:-`},
		})
		cs, err := p.Program()
		assert.NoError(t, err)

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
	})
}

func TestParser_Term(t *testing.T) {
	t.Run("expression", func(t *testing.T) {
		p := NewParser(`a + b * c * d + e`, operators{
			{Precedence: 500, Type: `yfx`, Name: `+`},
			{Precedence: 400, Type: `yfx`, Name: `*`},
		})
		term, err := p.Term()
		assert.NoError(t, err)
		assert.Equal(t, term, &Compound{
			Functor: "+",
			Args: []Term{
				&Compound{
					Functor: "+",
					Args: []Term{
						Atom("a"),
						&Compound{
							Functor: "*",
							Args: []Term{
								&Compound{
									Functor: "*",
									Args:    []Term{Atom("b"), Atom("c")},
								},
								Atom("d"),
							},
						},
					},
				},
				Atom("e"),
			},
		})
	})

	t.Run("Clause", func(t *testing.T) {
		p := NewParser(`append(cons(X,L1),L2,cons(X,L3)) :- append(L1,L2,L3)`, operators{
			{Precedence: 1200, Type: `xfx`, Name: `:-`},
		})
		term, err := p.Term()
		assert.NoError(t, err)
		assert.Equal(t, term, &Compound{
			Functor: ":-",
			Args: []Term{
				&Compound{
					Functor: "append",
					Args: []Term{
						&Compound{
							Functor: "cons",
							Args:    []Term{&Variable{Name: "X"}, &Variable{Name: "L1"}},
						},
						&Variable{Name: "L2"},
						&Compound{
							Functor: "cons",
							Args:    []Term{&Variable{Name: "X"}, &Variable{Name: "L3"}},
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
		})
	})

	t.Run("conjunction", func(t *testing.T) {
		p := NewParser(`p, q :- p, q.`, operators{
			{Precedence: 1200, Type: `xfx`, Name: `:-`},
			{Precedence: 1000, Type: `xfy`, Name: `,`},
		})

		term, err := p.Term()
		assert.NoError(t, err)
		assert.Equal(t, &Compound{
			Functor: ":-",
			Args: []Term{
				&Compound{
					Functor: ",",
					Args: []Term{
						Atom("p"),
						Atom("q"),
					},
				},
				&Compound{
					Functor: ",",
					Args: []Term{
						Atom("p"),
						Atom("q"),
					},
				},
			},
		}, term)
	})
}
