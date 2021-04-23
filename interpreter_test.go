package prolog

import (
	"testing"

	"github.com/ichiban/prolog/engine"

	"github.com/stretchr/testify/assert"
)

func TestNew(t *testing.T) {
	i := New(nil, nil)
	assert.NotNil(t, i)
}

func TestInterpreter_Exec(t *testing.T) {
	t.Run("fact", func(t *testing.T) {
		var i Interpreter
		assert.NoError(t, i.Exec(`append(nil, L, L).`))
	})

	t.Run("rule", func(t *testing.T) {
		var i Interpreter
		i.Register3("op", i.Op)
		assert.NoError(t, i.Exec(":-(op(1200, xfx, :-))."))
		assert.NoError(t, i.Exec(`append(cons(X, L1), L2, cons(X, L3)) :- append(L1, L2, L3).`))
	})

	t.Run("bindvars", func(t *testing.T) {
		var i Interpreter
		assert.NoError(t, i.Exec("foo(?, ?, ?, ?).", "a", 1, 2.0, []string{"abc", "def"}))
	})
}

func TestInterpreter_Query(t *testing.T) {
	var i Interpreter
	i.Register3("op", i.Op)
	assert.NoError(t, i.Exec(":-(op(1200, xfx, :-))."))
	assert.NoError(t, i.Exec("append(nil, L, L)."))
	assert.NoError(t, i.Exec("append(cons(X, L1), L2, cons(X, L3)) :- append(L1, L2, L3)."))

	t.Run("fact", func(t *testing.T) {
		sols, err := i.Query(`append(X, Y, Z).`)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, sols.Close())
		}()

		m := map[string]engine.Term{}

		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(m))
		assert.Equal(t, map[string]engine.Term{
			"X": engine.Atom("nil"),
			"Y": &engine.Variable{},
			"Z": &engine.Variable{},
		}, m)
	})

	t.Run("rule", func(t *testing.T) {
		sols, err := i.Query(`append(cons(a, cons(b, nil)), cons(c, nil), X).`)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, sols.Close())
		}()

		m := map[string]engine.Term{}

		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(m))
		assert.Equal(t, map[string]engine.Term{
			"X": &engine.Compound{
				Functor: "cons",
				Args: []engine.Term{
					&engine.Variable{
						Ref: engine.Atom("a"),
					},
					&engine.Variable{
						Ref: &engine.Variable{
							Ref: &engine.Variable{
								Ref: &engine.Compound{
									Functor: "cons",
									Args: []engine.Term{
										&engine.Variable{
											Ref: engine.Atom("b"),
										},
										&engine.Variable{
											Ref: &engine.Variable{
												Ref: &engine.Compound{
													Functor: "cons",
													Args:    []engine.Term{engine.Atom("c"), engine.Atom("nil")},
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
		var i Interpreter
		assert.NoError(t, i.Exec("foo(a, 1, 2.0, [abc, def])."))

		sols, err := i.Query(`foo(?, ?, ?, ?).`, "a", 1, 2.0, []string{"abc", "def"})
		assert.NoError(t, err)

		m := map[string]interface{}{}

		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(m))
		assert.Equal(t, map[string]interface{}{}, m)
	})

	t.Run("scan to struct", func(t *testing.T) {
		var i Interpreter
		assert.NoError(t, i.Exec("foo(a, 1, 2.0, [abc, def])."))

		sols, err := i.Query(`foo(A, B, C, D).`)
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

func TestMisc(t *testing.T) {
	t.Run("negation", func(t *testing.T) {
		i := New(nil, nil)
		sols, err := i.Query(`\+true.`)
		assert.NoError(t, err)

		assert.False(t, sols.Next())
	})

	// http://www.cse.unsw.edu.au/~billw/dictionaries/prolog/cut.html
	t.Run("cut", func(t *testing.T) {
		i := New(nil, nil)
		i.Exec(`
teaches(dr_fred, history).
teaches(dr_fred, english).
teaches(dr_fred, drama).
teaches(dr_fiona, physics).
studies(alice, english).
studies(angus, english).
studies(amelia, drama).
studies(alex, physics).
`)

		t.Run("without cut", func(t *testing.T) {
			sols, err := i.Query(`teaches(dr_fred, Course), studies(Student, Course).`)
			assert.NoError(t, err)
			defer func() {
				assert.NoError(t, sols.Close())
			}()

			type cs struct {
				Course  string
				Student string
			}
			var s cs

			assert.True(t, sols.Next())
			assert.NoError(t, sols.Scan(&s))
			assert.Equal(t, cs{
				Course:  "english",
				Student: "alice",
			}, s)

			assert.True(t, sols.Next())
			assert.NoError(t, sols.Scan(&s))
			assert.Equal(t, cs{
				Course:  "english",
				Student: "angus",
			}, s)

			assert.True(t, sols.Next())
			assert.NoError(t, sols.Scan(&s))
			assert.Equal(t, cs{
				Course:  "drama",
				Student: "amelia",
			}, s)

			assert.False(t, sols.Next())
		})

		t.Run("with cut in the middle", func(t *testing.T) {
			sols, err := i.Query(`teaches(dr_fred, Course), !, studies(Student, Course).`)
			assert.NoError(t, err)
			defer func() {
				assert.NoError(t, sols.Close())
			}()

			assert.False(t, sols.Next())
		})

		t.Run("with cut at the end", func(t *testing.T) {
			sols, err := i.Query(`teaches(dr_fred, Course), studies(Student, Course), !.`)
			assert.NoError(t, err)
			defer func() {
				assert.NoError(t, sols.Close())
			}()

			type cs struct {
				Course  string
				Student string
			}
			var s cs

			assert.True(t, sols.Next())
			assert.NoError(t, sols.Scan(&s))
			assert.Equal(t, cs{
				Course:  "english",
				Student: "alice",
			}, s)

			assert.False(t, sols.Next())
		})

		t.Run("with cut at the beginning", func(t *testing.T) {
			sols, err := i.Query(`!, teaches(dr_fred, Course), studies(Student, Course).`)
			assert.NoError(t, err)
			defer func() {
				assert.NoError(t, sols.Close())
			}()

			type cs struct {
				Course  string
				Student string
			}
			var s cs

			assert.True(t, sols.Next())
			assert.NoError(t, sols.Scan(&s))
			assert.Equal(t, cs{
				Course:  "english",
				Student: "alice",
			}, s)

			assert.True(t, sols.Next())
			assert.NoError(t, sols.Scan(&s))
			assert.Equal(t, cs{
				Course:  "english",
				Student: "angus",
			}, s)

			assert.True(t, sols.Next())
			assert.NoError(t, sols.Scan(&s))
			assert.Equal(t, cs{
				Course:  "drama",
				Student: "amelia",
			}, s)

			assert.False(t, sols.Next())
		})
	})

	t.Run("inriasuite", func(t *testing.T) {
		i := New(nil, nil)

		i.Exec(inriaSuite)

		t.Run("get_all_subs", func(a *testing.T) {
			sols, err := i.Query(`get_all_subs((X=1, var(X)), Subs).`)
			assert.NoError(t, err)
			assert.True(t, sols.Next())

			var ans struct {
				Subs []string
			}
			assert.NoError(t, sols.Scan(&ans))
			assert.Equal(t, []string{"failure"}, ans.Subs)

			assert.False(t, sols.Next())
		})

		t.Run("vars_in_term", func(t *testing.T) {
			sols, err := i.Query(`vars_in_term((X=1, var(X)), Vars).`)
			assert.NoError(t, err)
			assert.True(t, sols.Next())

			var vars struct {
				Vars []engine.Term
			}
			assert.NoError(t, sols.Scan(&vars))
			assert.Len(t, vars.Vars, 1)
			assert.Equal(t, &engine.Variable{Name: "X"}, vars.Vars[0])

			assert.False(t, sols.Next())
		})
	})
}
