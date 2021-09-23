package prolog

import (
	"testing"

	"github.com/ichiban/prolog/term"

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

		m := map[string]term.Interface{}

		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(m))
		assert.Len(t, m, 3)
		assert.Equal(t, term.Atom("nil"), m["X"])
		assert.Equal(t, term.Variable("Z"), m["Y"])
		assert.Equal(t, term.Variable("Z"), m["Z"])
	})

	t.Run("rule", func(t *testing.T) {
		sols, err := i.Query(`append(cons(a, cons(b, nil)), cons(c, nil), X).`)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, sols.Close())
		}()

		m := map[string]term.Interface{}

		assert.True(t, sols.Next())
		assert.NoError(t, sols.Scan(m))
		assert.Equal(t, map[string]term.Interface{
			"X": &term.Compound{
				Functor: "cons",
				Args: []term.Interface{
					term.Atom("a"),
					&term.Compound{
						Functor: "cons",
						Args: []term.Interface{
							term.Atom("b"),
							&term.Compound{
								Functor: "cons",
								Args:    []term.Interface{term.Atom("c"), term.Atom("nil")},
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
}
