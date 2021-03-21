package prolog

import (
	"bufio"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestParser_Clause(t *testing.T) {
	t.Run("fact", func(t *testing.T) {
		p := NewParser(bufio.NewReader(strings.NewReader(`
append(nil,L,L).
`)), &Operators{}, map[rune]rune{})
		c, err := p.Clause()
		assert.NoError(t, err)

		assert.Equal(t, &Compound{
			Functor: "append",
			Args: []Term{
				Atom("nil"),
				&Variable{Name: "L"},
				&Variable{Name: "L"},
			},
		}, c)

	})

	t.Run("rule", func(t *testing.T) {
		p := NewParser(bufio.NewReader(strings.NewReader(`
append(cons(X,L1),L2,cons(X,L3)) :- append(L1,L2,L3).
`)), &Operators{
			{Priority: 1200, Specifier: `xfx`, Name: `:-`},
		}, map[rune]rune{})
		c, err := p.Clause()
		assert.NoError(t, err)

		assert.Equal(t, &Compound{
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
		}, c)
	})

	t.Run("conjunction", func(t *testing.T) {
		p := NewParser(bufio.NewReader(strings.NewReader(`P, Q :- P, Q.`)), &Operators{
			{Priority: 1200, Specifier: `xfx`, Name: `:-`},
			{Priority: 1000, Specifier: `xfy`, Name: `,`},
		}, map[rune]rune{})

		c, err := p.Clause()
		assert.NoError(t, err)
		assert.Equal(t, &Compound{
			Functor: ":-",
			Args: []Term{
				&Compound{
					Functor: ",",
					Args: []Term{
						&Variable{Name: "P"},
						&Variable{Name: "Q"},
					},
				},
				&Compound{
					Functor: ",",
					Args: []Term{
						&Variable{Name: "P"},
						&Variable{Name: "Q"},
					},
				},
			},
		}, c)
	})

	t.Run("qualifier", func(t *testing.T) {
		p := NewParser(bufio.NewReader(strings.NewReader(`bagof(C, A^foo(A, B, C), Cs).`)), &Operators{
			{Priority: 1000, Specifier: `xfy`, Name: `,`},
			{Priority: 200, Specifier: `xfy`, Name: `^`},
		}, map[rune]rune{})

		c, err := p.Clause()
		assert.NoError(t, err)
		assert.Equal(t, &Compound{
			Functor: "bagof",
			Args: []Term{
				&Variable{Name: "C"},
				&Compound{
					Functor: "^",
					Args: []Term{
						&Variable{Name: "A"},
						&Compound{
							Functor: "foo",
							Args: []Term{
								&Variable{Name: "A"},
								&Variable{Name: "B"},
								&Variable{Name: "C"},
							},
						},
					},
				},
				&Variable{Name: "Cs"},
			},
		}, c)
	})

	t.Run("multiple qualifiers", func(t *testing.T) {
		p := NewParser(bufio.NewReader(strings.NewReader(`bagof(C, (A, B)^foo(A, B, C), Cs).`)), &Operators{
			{Priority: 1000, Specifier: `xfy`, Name: `,`},
			{Priority: 200, Specifier: `xfy`, Name: `^`},
		}, map[rune]rune{})

		c, err := p.Clause()
		assert.NoError(t, err)
		assert.Equal(t, &Compound{
			Functor: "bagof",
			Args: []Term{
				&Variable{Name: "C"},
				&Compound{
					Functor: "^",
					Args: []Term{
						&Compound{
							Functor: ",",
							Args: []Term{
								&Variable{Name: "A"},
								&Variable{Name: "B"},
							},
						},
						&Compound{
							Functor: "foo",
							Args: []Term{
								&Variable{Name: "A"},
								&Variable{Name: "B"},
								&Variable{Name: "C"},
							},
						},
					},
				},
				&Variable{Name: "Cs"},
			},
		}, c)
	})
}

func TestParser_Term(t *testing.T) {
	t.Run("expression", func(t *testing.T) {
		p := NewParser(bufio.NewReader(strings.NewReader(`a + b * c * d + e`)), &Operators{
			{Priority: 500, Specifier: `yfx`, Name: `+`},
			{Priority: 400, Specifier: `yfx`, Name: `*`},
		}, map[rune]rune{})
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

	t.Run("list", func(t *testing.T) {
		p := NewParser(bufio.NewReader(strings.NewReader(`[a, b, c|X]`)), &Operators{}, map[rune]rune{})
		term, err := p.Term()
		assert.NoError(t, err)
		assert.Equal(t, ListRest(&Variable{Name: "X"}, Atom("a"), Atom("b"), Atom("c")), term)
	})

	t.Run("principal functor", func(t *testing.T) {
		p := NewParser(bufio.NewReader(strings.NewReader(`(==)/2`)), &Operators{
			{Priority: 400, Specifier: "yfx", Name: "/"},
		}, map[rune]rune{})
		term, err := p.Term()
		assert.NoError(t, err)
		assert.Equal(t, &Compound{
			Functor: "/",
			Args: []Term{
				Atom("=="),
				Integer(2),
			},
		}, term)
	})
}
