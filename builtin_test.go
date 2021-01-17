package prolog

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestCopyTerm(t *testing.T) {
	in := &Variable{Ref: Atom("a")}
	out := &Variable{}
	k := func() (bool, error) {
		return true, nil
	}
	ok, err := CopyTerm(in, out, k)
	assert.NoError(t, err)
	assert.True(t, ok)
	assert.Equal(t, Atom("a"), out.Ref)
}

func TestRepeat(t *testing.T) {
	c := 3
	ok, err := Repeat(func() (bool, error) {
		c--
		return c == 0, nil
	})
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Repeat(func() (bool, error) {
		return false, errors.New("")
	})
	assert.Error(t, err)
	assert.False(t, ok)

	ok, err = Repeat(func() (bool, error) {
		return true, errCut
	})
	assert.True(t, errors.Is(err, errCut))
	assert.True(t, ok)
}

func TestBagOf(t *testing.T) {
	e, err := NewEngine()
	assert.NoError(t, err)
	assert.NoError(t, e.Load(`
foo(a, b, c).
foo(a, b, d).
foo(b, c, e).
foo(b, c, f).
foo(c, c, g).
`))

	t.Run("without qualifier", func(t *testing.T) {
		var c int
		ok, err := e.Query(`bagof(C, foo(A, B, C), Cs).`, func(vs []*Variable) bool {
			switch c {
			case 0:
				assert.Equal(t, []*Variable{
					{Name: "C", Ref: &Variable{}},
					{Name: "A", Ref: Atom("a")},
					{Name: "B", Ref: Atom("b")},
					{Name: "Cs", Ref: &Variable{
						Ref: List(
							&Variable{Ref: &Variable{Ref: Atom("c")}},
							&Variable{Ref: &Variable{Ref: Atom("d")}},
						),
					}},
				}, vs)
			case 1:
				assert.Equal(t, []*Variable{
					{Name: "C", Ref: &Variable{}},
					{Name: "A", Ref: Atom("b")},
					{Name: "B", Ref: Atom("c")},
					{Name: "Cs", Ref: &Variable{
						Ref: List(
							&Variable{Ref: &Variable{Ref: Atom("e")}},
							&Variable{Ref: &Variable{Ref: Atom("f")}},
						),
					}},
				}, vs)
			case 2:
				assert.Equal(t, []*Variable{
					{Name: "C", Ref: &Variable{}},
					{Name: "A", Ref: Atom("c")},
					{Name: "B", Ref: Atom("c")},
					{Name: "Cs", Ref: &Variable{
						Ref: List(
							&Variable{Ref: &Variable{Ref: Atom("g")}},
						),
					}},
				}, vs)
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return false
		})
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("with qualifier", func(t *testing.T) {
		var c int
		ok, err := e.Query(`bagof(C, A^foo(A, B, C), Cs).`, func(vs []*Variable) bool {
			switch c {
			case 0:
				assert.Equal(t, []*Variable{
					{Name: "C", Ref: &Variable{}},
					{Name: "A", Ref: &Variable{}},
					{Name: "B", Ref: Atom("b")},
					{Name: "Cs", Ref: &Variable{
						Ref: List(
							&Variable{Ref: &Variable{Ref: Atom("c")}},
							&Variable{Ref: &Variable{Ref: Atom("d")}},
						),
					}},
				}, vs)
			case 1:
				assert.Equal(t, []*Variable{
					{Name: "C", Ref: &Variable{}},
					{Name: "A", Ref: &Variable{}},
					{Name: "B", Ref: Atom("c")},
					{Name: "Cs", Ref: &Variable{
						Ref: List(
							&Variable{Ref: &Variable{Ref: Atom("e")}},
							&Variable{Ref: &Variable{Ref: Atom("f")}},
							&Variable{Ref: &Variable{Ref: Atom("g")}},
						),
					}},
				}, vs)
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return false
		})
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("with multiple qualifiers", func(t *testing.T) {
		var c int
		ok, err := e.Query(`bagof(C, (A, B)^foo(A, B, C), Cs).`, func(vs []*Variable) bool {
			switch c {
			case 0:
				assert.Equal(t, []*Variable{
					{Name: "C", Ref: &Variable{}},
					{Name: "A"},
					{Name: "B"},
					{Name: "Cs", Ref: &Variable{
						Ref: List(
							&Variable{Ref: &Variable{Ref: Atom("c")}},
							&Variable{Ref: &Variable{Ref: Atom("d")}},
							&Variable{Ref: &Variable{Ref: Atom("e")}},
							&Variable{Ref: &Variable{Ref: Atom("f")}},
							&Variable{Ref: &Variable{Ref: Atom("g")}},
						),
					}},
				}, vs)
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return false
		})
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestCompare(t *testing.T) {
	var vs [2]Variable
	ok, err := Compare(Atom("<"), &vs[0], &vs[1], done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("="), &vs[0], &vs[0], done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom(">"), &vs[1], &vs[0], done)
	assert.NoError(t, err)
	assert.True(t, ok)

	vs[0].Ref = Atom("b")
	vs[1].Ref = Atom("a")
	ok, err = Compare(Atom(">"), &vs[0], &vs[1], done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("<"), &Variable{}, Integer(0), done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("<"), &Variable{}, Atom(""), done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("<"), &Variable{}, &Compound{}, done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom(">"), Integer(0), &Variable{}, done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("<"), Integer(0), Integer(1), done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("="), Integer(0), Integer(0), done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom(">"), Integer(1), Integer(0), done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("<"), Integer(0), Atom(""), done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("<"), Integer(0), &Compound{}, done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom(">"), Atom(""), &Variable{}, done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom(">"), Atom(""), Integer(0), done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("<"), Atom("a"), Atom("b"), done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("="), Atom("a"), Atom("a"), done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom(">"), Atom("b"), Atom("a"), done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("<"), Atom(""), &Compound{}, done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom(">"), &Compound{}, &Variable{}, done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom(">"), &Compound{}, Integer(0), done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom(">"), &Compound{}, Atom(""), done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("<"), &Compound{Functor: "a"}, &Compound{Functor: "b"}, done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("="), &Compound{Functor: "a"}, &Compound{Functor: "a"}, done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom(">"), &Compound{Functor: "b"}, &Compound{Functor: "a"}, done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom(">"), &Compound{Functor: "f", Args: []Term{Atom("a")}}, &Compound{Functor: "f"}, done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("="), &Compound{Functor: "f", Args: []Term{Atom("a")}}, &Compound{Functor: "f", Args: []Term{Atom("a")}}, done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("<"), &Compound{Functor: "f"}, &Compound{Functor: "f", Args: []Term{Atom("a")}}, done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom(">"), &Compound{Functor: "f", Args: []Term{Atom("b")}}, &Compound{Functor: "f", Args: []Term{Atom("a")}}, done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("<"), &Compound{Functor: "f", Args: []Term{Atom("a")}}, &Compound{Functor: "f", Args: []Term{Atom("b")}}, done)
	assert.NoError(t, err)
	assert.True(t, ok)
}

func TestThrow(t *testing.T) {
	ok, err := Throw(Atom("a"), done)
	assert.Equal(t, &Exception{Term: Atom("a")}, err)
	assert.False(t, ok)
}

func TestEngine_Catch(t *testing.T) {
	e, err := NewEngine()
	assert.NoError(t, err)

	t.Run("match", func(t *testing.T) {
		var v Variable
		ok, err := e.Catch(&Compound{
			Functor: "throw",
			Args:    []Term{Atom("a")},
		}, &v, &Compound{
			Functor: "=",
			Args:    []Term{&v, Atom("a")},
		}, done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not match", func(t *testing.T) {
		ok, err := e.Catch(&Compound{
			Functor: "throw",
			Args:    []Term{Atom("a")},
		}, Atom("b"), Atom("fail"), done)
		assert.Equal(t, &Exception{Term: Atom("a")}, err)
		assert.False(t, ok)
	})

	t.Run("true", func(t *testing.T) {
		ok, err := e.Catch(Atom("true"), Atom("b"), Atom("fail"), done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("false", func(t *testing.T) {
		ok, err := e.Catch(Atom("fail"), Atom("b"), Atom("fail"), done)
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestUnifyWithOccursCheck(t *testing.T) {
	v := Variable{Name: "X"}
	ok, err := UnifyWithOccursCheck(&v, &Compound{
		Functor: "f",
		Args:    []Term{&v},
	}, done)
	assert.NoError(t, err)
	assert.False(t, ok)
}

func TestEngine_CurrentPredicate(t *testing.T) {
	e := Engine{procedures: map[string]procedure{
		"(=)/2": nil,
	}}

	var v Variable
	ok, err := e.CurrentPredicate(&v, done)
	assert.NoError(t, err)
	assert.True(t, ok)
	assert.Equal(t, &Compound{
		Functor: "/",
		Args: []Term{
			Atom("="),
			Integer(2),
		},
	}, v.Ref)

	ok, err = e.CurrentPredicate(&v, func() (bool, error) {
		return false, nil
	})
	assert.NoError(t, err)
	assert.False(t, ok)
}

func TestEngine_Assertz(t *testing.T) {
	var e Engine

	ok, err := e.Assertz(&Compound{
		Functor: "foo",
		Args:    []Term{Atom("a")},
	}, done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = e.Assertz(&Compound{
		Functor: "foo",
		Args:    []Term{Atom("b")},
	}, done)
	assert.NoError(t, err)
	assert.True(t, ok)

	var c int
	ok, err = e.Query("foo(X).", func(vars []*Variable) bool {
		switch c {
		case 0:
			assert.Equal(t, &Variable{Name: "X", Ref: Atom("a")}, vars[0])
		case 1:
			assert.Equal(t, &Variable{Name: "X", Ref: Atom("b")}, vars[0])
		default:
			assert.Fail(t, "unreachable")
		}
		c++
		return false
	})
	assert.NoError(t, err)
	assert.False(t, ok)
}

func TestEngine_Asserta(t *testing.T) {
	var e Engine

	ok, err := e.Asserta(&Compound{
		Functor: "foo",
		Args:    []Term{Atom("a")},
	}, done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = e.Asserta(&Compound{
		Functor: "foo",
		Args:    []Term{Atom("b")},
	}, done)
	assert.NoError(t, err)
	assert.True(t, ok)

	var c int
	ok, err = e.Query("foo(X).", func(vars []*Variable) bool {
		switch c {
		case 0:
			assert.Equal(t, &Variable{Name: "X", Ref: Atom("b")}, vars[0])
		case 1:
			assert.Equal(t, &Variable{Name: "X", Ref: Atom("a")}, vars[0])
		default:
			assert.Fail(t, "unreachable")
		}
		c++
		return false
	})
	assert.NoError(t, err)
	assert.False(t, ok)
}
