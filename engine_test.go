package prolog

import (
	"testing"

	"github.com/sirupsen/logrus"

	"github.com/stretchr/testify/assert"
)

func TestEngine_Load(t *testing.T) {
	t.Run("fact", func(t *testing.T) {
		var e Engine
		assert.NoError(t, e.Load(`append(nil, L, L).`))
		assert.Equal(t, clauses{
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
		}, e.procedures["append/3"])
	})

	t.Run("rule", func(t *testing.T) {
		e := Engine{
			operators: operators{
				{Precedence: 1200, Type: `xfx`, Name: `:-`},
				{Precedence: 400, Type: `yfx`, Name: `/`},
			},
		}
		assert.NoError(t, e.Load(`append(cons(X, L1), L2, cons(X, L3)) :- append(L1, L2, L3).`))
		assert.Equal(t, clauses{
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
		}, e.procedures["append/3"])
	})
}

func TestEngine_Query(t *testing.T) {
	logrus.SetLevel(logrus.DebugLevel)

	// append(nil, L, L).
	// append(cons(X, L1), L2, cons(X, L3)) :- append(L1, L2, L3).
	e := Engine{
		operators: operators{
			{Precedence: 1200, Type: `xfx`, Name: `:-`},
			{Precedence: 400, Type: `yfx`, Name: `/`},
		},
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
