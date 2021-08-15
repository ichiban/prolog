package engine

import (
	"bufio"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestParser_Term(t *testing.T) {
	t.Run("fact", func(t *testing.T) {
		var vm VM
		p := NewParser(&vm, bufio.NewReader(strings.NewReader(`
append(nil,L,L).
`)))
		c, err := p.Term()
		assert.NoError(t, err)

		assert.Equal(t, &Compound{
			Functor: "append",
			Args: []Term{
				Atom("nil"),
				Variable("L"),
				Variable("L"),
			},
		}, c)

	})

	t.Run("rule", func(t *testing.T) {
		vm := VM{
			operators: Operators{
				{Priority: 1200, Specifier: `xfx`, Name: `:-`},
			},
		}
		p := NewParser(&vm, bufio.NewReader(strings.NewReader(`
append(cons(X,L1),L2,cons(X,L3)) :- append(L1,L2,L3).
`)))
		c, err := p.Term()
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
								Variable("X"),
								Variable("L1"),
							},
						},
						Variable("L2"),
						&Compound{
							Functor: "cons",
							Args: []Term{
								Variable("X"),
								Variable("L3"),
							},
						},
					},
				},
				&Compound{
					Functor: "append",
					Args: []Term{
						Variable("L1"),
						Variable("L2"),
						Variable("L3"),
					},
				},
			},
		}, c)
	})

	t.Run("conjunction", func(t *testing.T) {
		vm := VM{
			operators: Operators{
				{Priority: 1200, Specifier: `xfx`, Name: `:-`},
				{Priority: 1000, Specifier: `xfy`, Name: `,`},
			},
		}
		p := NewParser(&vm, bufio.NewReader(strings.NewReader(`P, Q :- P, Q.`)))
		c, err := p.Term()
		assert.NoError(t, err)
		assert.Equal(t, &Compound{
			Functor: ":-",
			Args: []Term{
				&Compound{
					Functor: ",",
					Args: []Term{
						Variable("P"),
						Variable("Q"),
					},
				},
				&Compound{
					Functor: ",",
					Args: []Term{
						Variable("P"),
						Variable("Q"),
					},
				},
			},
		}, c)
	})

	t.Run("qualifier", func(t *testing.T) {
		vm := VM{
			operators: Operators{
				{Priority: 1000, Specifier: `xfy`, Name: `,`},
				{Priority: 200, Specifier: `xfy`, Name: `^`},
			},
		}
		p := NewParser(&vm, bufio.NewReader(strings.NewReader(`bagof(C, A^foo(A, B, C), Cs).`)))
		c, err := p.Term()
		assert.NoError(t, err)
		assert.Equal(t, &Compound{
			Functor: "bagof",
			Args: []Term{
				Variable("C"),
				&Compound{
					Functor: "^",
					Args: []Term{
						Variable("A"),
						&Compound{
							Functor: "foo",
							Args: []Term{
								Variable("A"),
								Variable("B"),
								Variable("C"),
							},
						},
					},
				},
				Variable("Cs"),
			},
		}, c)
	})

	t.Run("multiple qualifiers", func(t *testing.T) {
		vm := VM{
			operators: Operators{
				{Priority: 1000, Specifier: `xfy`, Name: `,`},
				{Priority: 200, Specifier: `xfy`, Name: `^`},
			},
		}
		p := NewParser(&vm, bufio.NewReader(strings.NewReader(`bagof(C, (A, B)^foo(A, B, C), Cs).`)))
		c, err := p.Term()
		assert.NoError(t, err)
		assert.Equal(t, &Compound{
			Functor: "bagof",
			Args: []Term{
				Variable("C"),
				&Compound{
					Functor: "^",
					Args: []Term{
						&Compound{
							Functor: ",",
							Args: []Term{
								Variable("A"),
								Variable("B"),
							},
						},
						&Compound{
							Functor: "foo",
							Args: []Term{
								Variable("A"),
								Variable("B"),
								Variable("C"),
							},
						},
					},
				},
				Variable("Cs"),
			},
		}, c)
	})

	t.Run("expression", func(t *testing.T) {
		vm := VM{
			operators: Operators{
				{Priority: 500, Specifier: `yfx`, Name: `+`},
				{Priority: 400, Specifier: `yfx`, Name: `*`},
			},
		}
		p := NewParser(&vm, bufio.NewReader(strings.NewReader(`a + b * c * d + e.`)))
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
		var vm VM
		p := NewParser(&vm, bufio.NewReader(strings.NewReader(`[a, b, c|X].`)))
		term, err := p.Term()
		assert.NoError(t, err)
		assert.Equal(t, ListRest(Variable("X"), Atom("a"), Atom("b"), Atom("c")), term)
	})

	t.Run("principal functor", func(t *testing.T) {
		vm := VM{
			operators: Operators{
				{Priority: 400, Specifier: "yfx", Name: "/"},
			},
		}
		p := NewParser(&vm, bufio.NewReader(strings.NewReader(`(==)/2.`)))
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

func TestParser_Replace(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		var vm VM
		p := NewParser(&vm, bufio.NewReader(strings.NewReader(`[?, ?, ?, ?, ?].`)))
		assert.NoError(t, p.Replace("?", 1.0, 2, "foo", []string{"a", "b", "c"}, &Compound{
			Functor: "f",
			Args:    []Term{Atom("x")},
		}))

		list, err := p.Term()
		assert.NoError(t, err)
		assert.Equal(t, List(Float(1.0), Integer(2), Atom("foo"), List(Atom("a"), Atom("b"), Atom("c")), &Compound{
			Functor: "f",
			Args:    []Term{Atom("x")},
		}), list)
	})

	t.Run("invalid argument", func(t *testing.T) {
		var vm VM
		p := NewParser(&vm, bufio.NewReader(strings.NewReader(`[?].`)))
		assert.Error(t, p.Replace("?", []struct{}{{}}))
	})

	t.Run("too few arguments", func(t *testing.T) {
		var vm VM
		p := NewParser(&vm, bufio.NewReader(strings.NewReader(`[?, ?, ?, ?, ?].`)))
		assert.NoError(t, p.Replace("?", 1.0, 2, "foo", []string{"a", "b", "c"}))

		_, err := p.Term()
		assert.Error(t, err)
	})

	t.Run("too many arguments", func(t *testing.T) {
		var vm VM
		p := NewParser(&vm, bufio.NewReader(strings.NewReader(`[?, ?, ?, ?, ?].`)))
		assert.NoError(t, p.Replace("?", 1.0, 2, "foo", []string{"a", "b", "c"}, &Compound{
			Functor: "f",
			Args:    []Term{Atom("x")},
		}, "extra"))

		_, err := p.Term()
		assert.Error(t, err)
	})
}
