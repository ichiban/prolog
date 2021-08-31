package term

import (
	"bufio"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestParser_Term(t *testing.T) {
	t.Run("fact", func(t *testing.T) {
		p := NewParser(bufio.NewReader(strings.NewReader(`
append(nil,L,L).
`)), nil, nil)
		c, err := p.Term()
		assert.NoError(t, err)

		assert.Equal(t, &Compound{
			Functor: "append",
			Args: []Interface{
				Atom("nil"),
				Variable("L"),
				Variable("L"),
			},
		}, c)

	})

	t.Run("rule", func(t *testing.T) {
		ops := Operators{
			{Priority: 1200, Specifier: `xfx`, Name: `:-`},
		}
		p := NewParser(bufio.NewReader(strings.NewReader(`
append(cons(X,L1),L2,cons(X,L3)) :- append(L1,L2,L3).
`)), &ops, nil)
		c, err := p.Term()
		assert.NoError(t, err)

		assert.Equal(t, &Compound{
			Functor: ":-",
			Args: []Interface{
				&Compound{
					Functor: "append",
					Args: []Interface{
						&Compound{
							Functor: "cons",
							Args: []Interface{
								Variable("X"),
								Variable("L1"),
							},
						},
						Variable("L2"),
						&Compound{
							Functor: "cons",
							Args: []Interface{
								Variable("X"),
								Variable("L3"),
							},
						},
					},
				},
				&Compound{
					Functor: "append",
					Args: []Interface{
						Variable("L1"),
						Variable("L2"),
						Variable("L3"),
					},
				},
			},
		}, c)
	})

	t.Run("conjunction", func(t *testing.T) {
		ops := Operators{
			{Priority: 1200, Specifier: `xfx`, Name: `:-`},
			{Priority: 1000, Specifier: `xfy`, Name: `,`},
		}
		p := NewParser(bufio.NewReader(strings.NewReader(`P, Q :- P, Q.`)), &ops, nil)
		c, err := p.Term()
		assert.NoError(t, err)
		assert.Equal(t, &Compound{
			Functor: ":-",
			Args: []Interface{
				&Compound{
					Functor: ",",
					Args: []Interface{
						Variable("P"),
						Variable("Q"),
					},
				},
				&Compound{
					Functor: ",",
					Args: []Interface{
						Variable("P"),
						Variable("Q"),
					},
				},
			},
		}, c)
	})

	t.Run("qualifier", func(t *testing.T) {
		ops := Operators{
			{Priority: 1000, Specifier: `xfy`, Name: `,`},
			{Priority: 200, Specifier: `xfy`, Name: `^`},
		}
		p := NewParser(bufio.NewReader(strings.NewReader(`bagof(C, A^foo(A, B, C), Cs).`)), &ops, nil)
		c, err := p.Term()
		assert.NoError(t, err)
		assert.Equal(t, &Compound{
			Functor: "bagof",
			Args: []Interface{
				Variable("C"),
				&Compound{
					Functor: "^",
					Args: []Interface{
						Variable("A"),
						&Compound{
							Functor: "foo",
							Args: []Interface{
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
		ops := Operators{
			{Priority: 1000, Specifier: `xfy`, Name: `,`},
			{Priority: 200, Specifier: `xfy`, Name: `^`},
		}
		p := NewParser(bufio.NewReader(strings.NewReader(`bagof(C, (A, B)^foo(A, B, C), Cs).`)), &ops, nil)
		c, err := p.Term()
		assert.NoError(t, err)
		assert.Equal(t, &Compound{
			Functor: "bagof",
			Args: []Interface{
				Variable("C"),
				&Compound{
					Functor: "^",
					Args: []Interface{
						&Compound{
							Functor: ",",
							Args: []Interface{
								Variable("A"),
								Variable("B"),
							},
						},
						&Compound{
							Functor: "foo",
							Args: []Interface{
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
		ops := Operators{
			{Priority: 500, Specifier: `yfx`, Name: `+`},
			{Priority: 400, Specifier: `yfx`, Name: `*`},
		}
		p := NewParser(bufio.NewReader(strings.NewReader(`a + b * c * d + e.`)), &ops, nil)
		term, err := p.Term()
		assert.NoError(t, err)
		assert.Equal(t, term, &Compound{
			Functor: "+",
			Args: []Interface{
				&Compound{
					Functor: "+",
					Args: []Interface{
						Atom("a"),
						&Compound{
							Functor: "*",
							Args: []Interface{
								&Compound{
									Functor: "*",
									Args:    []Interface{Atom("b"), Atom("c")},
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
		p := NewParser(bufio.NewReader(strings.NewReader(`[a, b, c|X].`)), nil, nil)
		term, err := p.Term()
		assert.NoError(t, err)
		assert.Equal(t, ListRest(Variable("X"), Atom("a"), Atom("b"), Atom("c")), term)
	})

	t.Run("principal functor", func(t *testing.T) {
		ops := Operators{
			{Priority: 400, Specifier: "yfx", Name: "/"},
		}
		p := NewParser(bufio.NewReader(strings.NewReader(`(==)/2.`)), &ops, nil)
		term, err := p.Term()
		assert.NoError(t, err)
		assert.Equal(t, &Compound{
			Functor: "/",
			Args: []Interface{
				Atom("=="),
				Integer(2),
			},
		}, term)
	})

	t.Run("prefix as an arg", func(t *testing.T) {
		ops := Operators{
			{Priority: 200, Specifier: "fy", Name: "+"},
		}

		t.Run("unary", func(t *testing.T) {
			p := NewParser(bufio.NewReader(strings.NewReader(`p(+).`)), &ops, nil)
			term, err := p.Term()
			assert.NoError(t, err)
			assert.Equal(t, &Compound{
				Functor: "p",
				Args: []Interface{
					Atom("+"),
				},
			}, term)
		})

		t.Run("binary", func(t *testing.T) {
			p := NewParser(bufio.NewReader(strings.NewReader(`p(+, a).`)), &ops, nil)
			term, err := p.Term()
			assert.NoError(t, err)
			assert.Equal(t, &Compound{
				Functor: "p",
				Args: []Interface{
					Atom("+"),
					Atom("a"),
				},
			}, term)
		})
	})

	t.Run("ambiguous sign", func(t *testing.T) {
		ops := Operators{
			{Priority: 700, Specifier: `xfx`, Name: `is`},
			{Priority: 500, Specifier: `yfx`, Name: `+`},
		}
		p := NewParser(bufio.NewReader(strings.NewReader(`X is +1 +1.`)), &ops, nil)
		term, err := p.Term()
		assert.NoError(t, err)
		assert.Equal(t, &Compound{
			Functor: "is",
			Args: []Interface{
				Variable("X"),
				&Compound{
					Functor: "+",
					Args: []Interface{
						Integer(1),
						Integer(1),
					},
				},
			},
		}, term)
	})
}

func TestParser_Replace(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		p := NewParser(bufio.NewReader(strings.NewReader(`[?, ?, ?, ?, ?].`)), nil, nil)
		assert.NoError(t, p.Replace("?", 1.0, 2, "foo", []string{"a", "b", "c"}, &Compound{
			Functor: "f",
			Args:    []Interface{Atom("x")},
		}))

		list, err := p.Term()
		assert.NoError(t, err)
		assert.Equal(t, List(Float(1.0), Integer(2), Atom("foo"), List(Atom("a"), Atom("b"), Atom("c")), &Compound{
			Functor: "f",
			Args:    []Interface{Atom("x")},
		}), list)
	})

	t.Run("invalid argument", func(t *testing.T) {
		p := NewParser(bufio.NewReader(strings.NewReader(`[?].`)), nil, nil)
		assert.Error(t, p.Replace("?", []struct{}{{}}))
	})

	t.Run("too few arguments", func(t *testing.T) {
		p := NewParser(bufio.NewReader(strings.NewReader(`[?, ?, ?, ?, ?].`)), nil, nil)
		assert.NoError(t, p.Replace("?", 1.0, 2, "foo", []string{"a", "b", "c"}))

		_, err := p.Term()
		assert.Error(t, err)
	})

	t.Run("too many arguments", func(t *testing.T) {
		p := NewParser(bufio.NewReader(strings.NewReader(`[?, ?, ?, ?, ?].`)), nil, nil)
		assert.NoError(t, p.Replace("?", 1.0, 2, "foo", []string{"a", "b", "c"}, &Compound{
			Functor: "f",
			Args:    []Interface{Atom("x")},
		}, "extra"))

		_, err := p.Term()
		assert.Error(t, err)
	})
}
