package prolog

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/ichiban/prolog/engine"
)

func TestRegister(t *testing.T) {
	Register("foo", func(*Interpreter) error {
		return nil
	})
	assert.Panics(t, func() {
		Register("foo", func(*Interpreter) error {
			return nil
		})
	})
}

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

	t.Run("consult", func(t *testing.T) {
		i := New(nil, nil)

		t.Run("variable", func(t *testing.T) {
			assert.Error(t, i.Exec(":- consult(X)."))
		})

		t.Run("non-proper list", func(t *testing.T) {
			assert.Error(t, i.Exec(":- consult([foo|_])."))
		})

		t.Run("proper list", func(t *testing.T) {
			assert.NoError(t, i.Exec(":- consult([?]).", "testdata/empty.txt"))
		})

		t.Run("atom", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				assert.NoError(t, i.Exec(":- consult(?).", "testdata/empty.txt"))
			})

			t.Run("ng", func(t *testing.T) {
				assert.Error(t, i.Exec(":- consult(?).", "testdata/abc.txt"))
			})
		})

		t.Run("non-library compound", func(t *testing.T) {
			assert.Error(t, i.Exec(":- consult(foo(a, b, c))."))
		})

		t.Run("library", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				var called bool
				libraries = map[string]func(*Interpreter) error{
					"foo": func(in *Interpreter) error {
						assert.Equal(t, i, in)
						called = true
						return nil
					},
				}

				assert.NoError(t, i.Exec(":- consult(library(foo))."))
				assert.True(t, called)
			})

			t.Run("variable", func(t *testing.T) {
				assert.Error(t, i.Exec(":- consult(library(X))."))
			})

			t.Run("not defined", func(t *testing.T) {
				assert.Error(t, i.Exec(":- consult(library(not_defined))."))
			})
		})

		t.Run("neither atom nor library", func(t *testing.T) {
			assert.Error(t, i.Exec(":- consult(1)."))
		})
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
		assert.Len(t, m, 3)
		assert.Equal(t, engine.Atom("nil"), m["X"])
		assert.Equal(t, engine.Variable("Z"), m["Y"])
		assert.Equal(t, engine.Variable("Z"), m["Z"])
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
					engine.Atom("a"),
					&engine.Compound{
						Functor: "cons",
						Args: []engine.Term{
							engine.Atom("b"),
							&engine.Compound{
								Functor: "cons",
								Args:    []engine.Term{engine.Atom("c"), engine.Atom("nil")},
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

	t.Run("cut", func(t *testing.T) {
		// https://www.cs.uleth.ca/~gaur/post/prolog-cut-negation/
		t.Run("p", func(t *testing.T) {
			i := New(nil, nil)
			assert.NoError(t, i.Exec(`
p(a).
p(b):-!.
p(c).
`))

			t.Run("single", func(t *testing.T) {
				sols, err := i.Query(`p(X).`)
				assert.NoError(t, err)
				defer sols.Close()

				var s struct {
					X string
				}

				assert.True(t, sols.Next())
				assert.NoError(t, sols.Scan(&s))
				assert.Equal(t, "a", s.X)

				assert.True(t, sols.Next())
				assert.NoError(t, sols.Scan(&s))
				assert.Equal(t, "b", s.X)

				assert.False(t, sols.Next())
			})

			t.Run("double", func(t *testing.T) {
				sols, err := i.Query(`p(X), p(Y).`)
				assert.NoError(t, err)
				defer sols.Close()

				var s struct {
					X string
					Y string
				}

				assert.True(t, sols.Next())
				assert.NoError(t, sols.Scan(&s))
				assert.Equal(t, "a", s.X)
				assert.Equal(t, "a", s.Y)

				assert.True(t, sols.Next())
				assert.NoError(t, sols.Scan(&s))
				assert.Equal(t, "a", s.X)
				assert.Equal(t, "b", s.Y)

				assert.True(t, sols.Next())
				assert.NoError(t, sols.Scan(&s))
				assert.Equal(t, "b", s.X)
				assert.Equal(t, "a", s.Y)

				assert.True(t, sols.Next())
				assert.NoError(t, sols.Scan(&s))
				assert.Equal(t, "b", s.X)
				assert.Equal(t, "b", s.Y)

				assert.False(t, sols.Next())
			})
		})

		// http://www.cse.unsw.edu.au/~billw/dictionaries/prolog/cut.html
		t.Run("teaches", func(t *testing.T) {
			i := New(nil, nil)
			assert.NoError(t, i.Exec(`
teaches(dr_fred, history).
teaches(dr_fred, english).
teaches(dr_fred, drama).
teaches(dr_fiona, physics).
studies(alice, english).
studies(angus, english).
studies(amelia, drama).
studies(alex, physics).
`))

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

		t.Run("call/1 makes a difference", func(t *testing.T) {
			t.Run("with", func(t *testing.T) {
				i := New(nil, nil)
				sols, err := i.Query(`call(!), fail; true.`)
				assert.NoError(t, err)
				defer sols.Close()

				assert.True(t, sols.Next())
			})

			t.Run("without", func(t *testing.T) {
				i := New(nil, nil)
				sols, err := i.Query(`!, fail; true.`)
				assert.NoError(t, err)
				defer sols.Close()

				assert.False(t, sols.Next())
			})
		})
	})

	t.Run("repeat", func(t *testing.T) {
		t.Run("cut", func(t *testing.T) {
			i := New(nil, nil)
			sols, err := i.Query("repeat, !, fail.")
			assert.NoError(t, err)
			assert.False(t, sols.Next())
		})

		t.Run("stream", func(t *testing.T) {
			i := New(nil, nil)
			sols, err := i.Query("repeat, (X = a; X = b).")
			assert.NoError(t, err)

			var s struct {
				X string
			}

			assert.True(t, sols.Next())
			assert.NoError(t, sols.Scan(&s))
			assert.Equal(t, "a", s.X)

			assert.True(t, sols.Next())
			assert.NoError(t, sols.Scan(&s))
			assert.Equal(t, "b", s.X)

			assert.True(t, sols.Next())
			assert.NoError(t, sols.Scan(&s))
			assert.Equal(t, "a", s.X)

			assert.True(t, sols.Next())
			assert.NoError(t, sols.Scan(&s))
			assert.Equal(t, "b", s.X)
		})
	})

	t.Run("atom_chars", func(t *testing.T) {
		i := New(nil, nil)
		sols, err := i.Query("atom_chars(f(a), L).")
		assert.NoError(t, err)
		assert.False(t, sols.Next())
	})

	t.Run("term_eq", func(t *testing.T) {
		i := New(nil, nil)
		sols, err := i.Query("f(a) == f(a).")
		assert.NoError(t, err)
		assert.True(t, sols.Next())
	})

	t.Run("call cut", func(t *testing.T) {
		i := New(nil, nil)
		assert.NoError(t, i.Exec("foo :- call(true), !."))
		assert.NoError(t, i.Exec("foo :- throw(unreachable)."))
		sols, err := i.Query("foo.")
		assert.NoError(t, err)
		assert.True(t, sols.Next())
		assert.False(t, sols.Next())
		assert.NoError(t, sols.Err())
	})

	t.Run("catch cut", func(t *testing.T) {
		i := New(nil, nil)
		assert.NoError(t, i.Exec("foo :- catch(true, _, true), !."))
		assert.NoError(t, i.Exec("foo :- throw(unreachable)."))
		sols, err := i.Query("foo.")
		assert.NoError(t, err)
		assert.True(t, sols.Next())
		assert.False(t, sols.Next())
		assert.NoError(t, sols.Err())
	})

	t.Run("counter", func(t *testing.T) {
		i := New(nil, nil)
		assert.NoError(t, i.Exec(":- dynamic(count/1)."))
		assert.NoError(t, i.Exec("count(0)."))
		assert.NoError(t, i.Exec("next(N) :- retract(count(X)), N is X + 1, asserta(count(N))."))

		var s struct {
			X int
		}

		sols, err := i.Query("next(X).")
		assert.NoError(t, err)
		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(&s))
		assert.Equal(t, 1, s.X)
		assert.False(t, sols.Next())
		assert.NoError(t, sols.Err())
		assert.NoError(t, sols.Close())

		sols, err = i.Query("next(X).")
		assert.NoError(t, err)
		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(&s))
		assert.Equal(t, 2, s.X)
		assert.False(t, sols.Next())
		assert.NoError(t, sols.Err())
		assert.NoError(t, sols.Close())

		sols, err = i.Query("next(X).")
		assert.NoError(t, err)
		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(&s))
		assert.Equal(t, 3, s.X)
		assert.False(t, sols.Next())
		assert.NoError(t, sols.Err())
		assert.NoError(t, sols.Close())
	})
}

func TestInterpreter_QuerySolution(t *testing.T) {
	var i Interpreter
	assert.NoError(t, i.Exec(`
foo(a, b).
foo(b, c).
foo(c, d).
`))

	t.Run("ok", func(t *testing.T) {
		t.Run("struct", func(t *testing.T) {
			sol := i.QuerySolution(`foo(X, Y).`)

			var s struct {
				X   string
				Foo string `prolog:"Y"`
			}
			assert.NoError(t, sol.Scan(&s))
			assert.Equal(t, "a", s.X)
			assert.Equal(t, "b", s.Foo)
		})

		t.Run("map", func(t *testing.T) {
			sol := i.QuerySolution(`foo(X, Y).`)

			m := map[string]string{}
			assert.NoError(t, sol.Scan(m))
			assert.Equal(t, []string{"X", "Y"}, sol.Vars())
			assert.Equal(t, "a", m["X"])
			assert.Equal(t, "b", m["Y"])
		})
	})

	t.Run("invalid query", func(t *testing.T) {
		sol := i.QuerySolution(``)
		assert.Error(t, sol.Err())
	})

	t.Run("no solutions", func(t *testing.T) {
		sol := i.QuerySolution(`foo(e, f).`)
		assert.Equal(t, ErrNoSolutions, sol.Err())
		assert.Empty(t, sol.Vars())
	})

	t.Run("runtime error", func(t *testing.T) {
		err := errors.New("something went wrong")

		i.Register0("error", func(k func(*engine.Env) *engine.Promise, env *engine.Env) *engine.Promise {
			return engine.Error(err)
		})
		sol := i.QuerySolution(`error.`)
		assert.Equal(t, err, sol.Err())

		var s struct{}
		assert.Error(t, sol.Scan(&s))
	})
}
