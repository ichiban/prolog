package prolog

import (
	"testing"

	"github.com/sirupsen/logrus"

	"github.com/stretchr/testify/assert"
)

func TestClause_Compile(t *testing.T) {
	t.Run("fact", func(t *testing.T) {
		var c clause
		assert.NoError(t, c.compile(&Compound{
			Functor: "append",
			Args: []Term{
				Atom("nil"),
				&Variable{Name: "L"},
				&Variable{Name: "L"},
			},
		}))
		assert.Equal(t, clause{
			xrTable: []Term{
				Atom("nil"),
			},
			vars: []string{"L"},
			bytecode: []byte{
				opConst, 0, // nil
				opVar, 0, // L
				opVar, 0, // L
				opExit,
			},
		}, c)
	})

	t.Run("rule", func(t *testing.T) {
		var c clause
		assert.NoError(t, c.compile(&Compound{
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
		}))

		assert.Equal(t, clause{
			xrTable: []Term{
				&Compound{
					Functor: "/",
					Args:    []Term{Atom("cons"), Integer(2)},
				},
				&Compound{
					Functor: "/",
					Args:    []Term{Atom("append"), Integer(3)},
				},
			},
			vars: []string{"X", "L1", "L2", "L3"},
			bytecode: []byte{
				opFunctor, 0, opVar, 0, opVar, 1, opPop, // cons(X, L1)
				opVar, 2, // L2
				opFunctor, 0, opVar, 0, opVar, 3, opPop, // cons(X, L3)
				opEnter,
				opVar, 1, opVar, 2, opVar, 3, opCall, 1, // append(L1, L2, L3)
				opExit,
			},
		}, c)
	})

}

func TestEngine_Query(t *testing.T) {
	logrus.SetLevel(logrus.DebugLevel)

	// append(nil, L, L).
	// append(cons(X, L1), L2, cons(X, L3)) :- append(L1, L2, L3).
	e := Engine{
		procedures: map[string]procedure{
			"append/3": &clauses{
				{
					xrTable: []Term{
						Atom("nil"),
					},
					vars: []string{"L"},
					bytecode: []byte{
						opConst, 0, // nil
						opVar, 0, // L
						opVar, 0, // L
						opExit,
					},
				},
				{
					xrTable: []Term{
						&Compound{
							Functor: "/",
							Args:    []Term{Atom("cons"), Integer(2)},
						},
						&Compound{
							Functor: "/",
							Args:    []Term{Atom("append"), Integer(3)},
						},
					},
					vars: []string{"X", "L1", "L2", "L3"},
					bytecode: []byte{
						opFunctor, 0, opVar, 0, opVar, 1, opPop, // cons(X, L1)
						opVar, 2, // L2
						opFunctor, 0, opVar, 0, opVar, 3, opPop, // cons(X, L3)
						opEnter,
						opVar, 1, opVar, 2, opVar, 3, opCall, 1, // append(L1, L2, L3)
						opExit,
					},
				},
			},
		},
	}

	t.Run("fact", func(t *testing.T) {
		ok, err := e.Query(`append(X, Y, Z).`, func(vars []*Variable) bool {
			assert.Len(t, vars, 3)
			assert.Equal(t, &Variable{Name: "X", Ref: Atom("nil")}, vars[0])
			assert.Equal(t, &Variable{Name: "Y", Ref: &Variable{Name: "L"}}, vars[1]) // TODO: it should be Y = Z
			assert.Equal(t, &Variable{Name: "Z", Ref: &Variable{Name: "L"}}, vars[2])
			return true
		})
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("rule", func(t *testing.T) {
		ok, err := e.Query(`append(cons(a, cons(b, nil)), cons(c, nil), X).`, func(vars []*Variable) bool {
			assert.Len(t, vars, 1)
			assert.Equal(t, &Variable{
				Name: "X",
				Ref: &Compound{
					Functor: "cons",
					Args: []Term{
						Atom("a"),
						&Compound{
							Functor: "cons",
							Args: []Term{
								Atom("b"),
								&Compound{
									Functor: "cons",
									Args:    []Term{Atom("c"), Atom("nil")},
								},
							},
						},
					},
				},
			}, vars[0])
			return true
		})
		assert.NoError(t, err)
		assert.True(t, ok)
	})
}
