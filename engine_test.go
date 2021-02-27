package prolog

import (
	"testing"

	"github.com/sirupsen/logrus"

	"github.com/stretchr/testify/assert"
)

func TestNewEngine(t *testing.T) {
	e, err := NewEngine(nil, nil)
	assert.NoError(t, err)
	assert.NotNil(t, e)
}

func TestEngine_Exec(t *testing.T) {
	t.Run("fact", func(t *testing.T) {
		var e Engine
		assert.NoError(t, e.Exec(`append(nil, L, L).`))
		assert.Equal(t, clauses{
			{
				name: "append/3",
				raw: &Compound{
					Functor: "append",
					Args: []Term{
						Atom("nil"),
						&Variable{},
						&Variable{},
					},
				},
				xrTable: []Term{
					Atom("nil"),
				},
				vars: []*Variable{{}},
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
		e := Engine{EngineState{
			operators: Operators{
				{Precedence: 1200, Type: `xfx`, Name: `:-`},
				{Precedence: 400, Type: `yfx`, Name: `/`},
			},
		}}
		assert.NoError(t, e.Exec(`append(cons(X, L1), L2, cons(X, L3)) :- append(L1, L2, L3).`))
		assert.Equal(t, clauses{
			{
				name: "append/3",
				raw: &Compound{
					Functor: ":-",
					Args: []Term{
						&Compound{
							Functor: "append",
							Args: []Term{
								&Compound{
									Functor: "cons",
									Args: []Term{
										&Variable{},
										&Variable{},
									},
								},
								&Variable{},
								&Compound{
									Functor: "cons",
									Args: []Term{
										&Variable{},
										&Variable{},
									},
								},
							},
						},
						&Compound{
							Functor: "append",
							Args: []Term{
								&Variable{},
								&Variable{},
								&Variable{},
							},
						},
					},
				},
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
				vars: []*Variable{{}, {}, {}, {}},
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
	logrus.SetFormatter(&logrus.TextFormatter{
		ForceColors:      true,
		DisableQuote:     true,
		DisableTimestamp: true,
	})

	// append(nil, L, L).
	// append(cons(X, L1), L2, cons(X, L3)) :- append(L1, L2, L3).
	e := Engine{EngineState{
		operators: Operators{
			{Precedence: 1200, Type: `xfx`, Name: `:-`},
			{Precedence: 400, Type: `yfx`, Name: `/`},
		},
		procedures: map[string]procedure{
			"append/3": clauses{
				{
					xrTable: []Term{
						Atom("nil"),
					},
					vars: []*Variable{{}},
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
					vars: []*Variable{{}, {}, {}, {}},
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
	}}

	t.Run("fact", func(t *testing.T) {
		ok, err := e.Query(`append(X, Y, Z).`, func(vars []*Variable) bool {
			assert.Len(t, vars, 3)
			assert.Equal(t, &Variable{Name: "X", Ref: Atom("nil")}, vars[0])
			assert.Equal(t, &Variable{Name: "Y", Ref: &Variable{Ref: &Variable{}}}, vars[1]) // TODO: it should be Y = Z
			assert.Equal(t, &Variable{Name: "Z", Ref: &Variable{}}, vars[2])
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
				Ref: &Variable{
					Ref: &Compound{
						Functor: "cons",
						Args: []Term{
							&Variable{
								Ref: Atom("a"),
							},
							&Variable{
								Ref: &Variable{
									Ref: &Variable{
										Ref: &Compound{
											Functor: "cons",
											Args: []Term{
												&Variable{
													Ref: Atom("b"),
												},
												&Variable{
													Ref: &Variable{
														Ref: &Compound{
															Functor: "cons",
															Args:    []Term{Atom("c"), Atom("nil")},
														},
													},
												},
											},
										},
									},
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
