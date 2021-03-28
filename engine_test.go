package prolog

import (
	"testing"

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
				pf: procedureIndicator{name: "append", arity: 3},
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
		}, e.procedures[procedureIndicator{name: "append", arity: 3}])
	})

	t.Run("rule", func(t *testing.T) {
		e := Engine{EngineState{
			operators: Operators{
				{Priority: 1200, Specifier: `xfx`, Name: `:-`},
				{Priority: 400, Specifier: `yfx`, Name: `/`},
			},
		}}
		assert.NoError(t, e.Exec(`append(cons(X, L1), L2, cons(X, L3)) :- append(L1, L2, L3).`))
		assert.Equal(t, clauses{
			{
				pf: procedureIndicator{name: "append", arity: 3},
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
					procedureIndicator{name: "cons", arity: 2},
					procedureIndicator{name: "append", arity: 3},
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
		}, e.procedures[procedureIndicator{name: "append", arity: 3}])
	})

	t.Run("bindvars", func(t *testing.T) {
		var e Engine
		assert.NoError(t, e.Exec("foo(?, ?, ?, ?).", "a", 1, 2.0, []string{"abc", "def"}))

		assert.Equal(t, clauses{
			{
				pf: procedureIndicator{name: "foo", arity: 4},
				raw: &Compound{
					Functor: "foo",
					Args:    []Term{Atom("a"), Integer(1), Float(2.0), List(Atom("abc"), Atom("def"))},
				},
				xrTable: []Term{
					Atom("a"),
					Integer(1),
					Float(2.0),
					procedureIndicator{name: ".", arity: 2},
					Atom("abc"),
					Atom("def"),
					Atom("[]"),
				},
				bytecode: []byte{
					opConst, 0,
					opConst, 1,
					opConst, 2,
					opFunctor, 3, // .(
					opConst, 4, // abc
					opFunctor, 3, // .(
					opConst, 5, // def
					opConst, 6, // []
					opPop, // )
					opPop, // )
					opExit,
				},
			},
		}, e.procedures[procedureIndicator{name: "foo", arity: 4}])
	})
}

func TestEngine_Query(t *testing.T) {
	// append(nil, L, L).
	// append(cons(X, L1), L2, cons(X, L3)) :- append(L1, L2, L3).
	e := Engine{EngineState{
		operators: Operators{
			{Priority: 1200, Specifier: `xfx`, Name: `:-`},
			{Priority: 400, Specifier: `yfx`, Name: `/`},
		},
		procedures: map[procedureIndicator]procedure{
			{name: "append", arity: 3}: clauses{
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
						procedureIndicator{name: "cons", arity: 2},
						procedureIndicator{name: "append", arity: 3},
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
		sols, err := e.Query(`append(X, Y, Z).`)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, sols.Close())
		}()

		m := map[string]Term{}

		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(m))
		assert.Equal(t, map[string]Term{
			"X": Atom("nil"),
			"Y": &Variable{},
			"Z": &Variable{},
		}, m)
	})

	t.Run("rule", func(t *testing.T) {
		sols, err := e.Query(`append(cons(a, cons(b, nil)), cons(c, nil), X).`)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, sols.Close())
		}()

		m := map[string]Term{}

		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(m))
		assert.Equal(t, map[string]Term{
			"X": &Compound{
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
		}, m)
	})

	t.Run("bindvars", func(t *testing.T) {
		e := Engine{
			EngineState{
				procedures: map[procedureIndicator]procedure{
					{name: "foo", arity: 4}: clauses{
						{
							pf: procedureIndicator{name: "foo", arity: 4},
							raw: &Compound{
								Functor: "foo",
								Args:    []Term{Atom("a"), Integer(1), Float(2.0), List(Atom("abc"), Atom("def"))},
							},
							xrTable: []Term{
								Atom("a"),
								Integer(1),
								Float(2.0),
								procedureIndicator{name: ".", arity: 2},
								Atom("abc"),
								Atom("def"),
								Atom("[]"),
							},
							bytecode: []byte{
								opConst, 0,
								opConst, 1,
								opConst, 2,
								opFunctor, 3, // .(
								opConst, 4, // abc
								opFunctor, 3, // .(
								opConst, 5, // def
								opConst, 6, // []
								opPop, // )
								opPop, // )
								opExit,
							},
						},
					},
				},
			},
		}
		sols, err := e.Query(`foo(?, ?, ?, ?).`, "a", 1, 2.0, []string{"abc", "def"})
		assert.NoError(t, err)

		m := map[string]interface{}{}

		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(m))
		assert.Equal(t, map[string]interface{}{}, m)
	})

	t.Run("scan to struct", func(t *testing.T) {
		e := Engine{
			EngineState{
				procedures: map[procedureIndicator]procedure{
					{name: "foo", arity: 4}: clauses{
						{
							pf: procedureIndicator{name: "foo", arity: 4},
							raw: &Compound{
								Functor: "foo",
								Args:    []Term{Atom("a"), Integer(1), Float(2.0), List(Atom("abc"), Atom("def"))},
							},
							xrTable: []Term{
								Atom("a"),
								Integer(1),
								Float(2.0),
								procedureIndicator{name: ".", arity: 2},
								Atom("abc"),
								Atom("def"),
								Atom("[]"),
							},
							bytecode: []byte{
								opConst, 0,
								opConst, 1,
								opConst, 2,
								opFunctor, 3, // .(
								opConst, 4, // abc
								opFunctor, 3, // .(
								opConst, 5, // def
								opConst, 6, // []
								opPop, // )
								opPop, // )
								opExit,
							},
						},
					},
				},
			},
		}
		sols, err := e.Query(`foo(A, B, C, D).`)
		assert.NoError(t, err)

		type result struct {
			A    string
			B    int
			C    float64
			List []string `prolog:"D"`
		}

		assert.True(t, sols.Next())

		var r result
		assert.NoError(t, sols.Scan(&r))
		assert.Equal(t, result{
			A:    "a",
			B:    1,
			C:    2.0,
			List: []string{"abc", "def"},
		}, r)
	})
}
