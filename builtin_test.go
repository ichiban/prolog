package prolog

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"math"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/stretchr/testify/mock"

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
	e, err := NewEngine(nil, nil)
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

func TestSetOf(t *testing.T) {
	e, err := NewEngine(nil, nil)
	assert.NoError(t, err)
	assert.NoError(t, e.Load(`
foo(a, b, c).
foo(a, b, d).
foo(a, b, c).
foo(b, c, e).
foo(b, c, f).
foo(b, c, e).
foo(c, c, g).
foo(c, c, g).
`))

	t.Run("without qualifier", func(t *testing.T) {
		var c int
		ok, err := e.Query(`setof(C, foo(A, B, C), Cs).`, func(vs []*Variable) bool {
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
		ok, err := e.Query(`setof(C, A^foo(A, B, C), Cs).`, func(vs []*Variable) bool {
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
		ok, err := e.Query(`setof(C, (A, B)^foo(A, B, C), Cs).`, func(vs []*Variable) bool {
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
	ok, err := Compare(Atom("<"), &vs[0], &vs[1], Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("="), &vs[0], &vs[0], Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom(">"), &vs[1], &vs[0], Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	vs[0].Ref = Atom("b")
	vs[1].Ref = Atom("a")
	ok, err = Compare(Atom(">"), &vs[0], &vs[1], Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("<"), &Variable{}, Integer(0), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("<"), &Variable{}, Atom(""), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("<"), &Variable{}, &Compound{}, Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom(">"), Integer(0), &Variable{}, Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("<"), Integer(0), Integer(1), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("="), Integer(0), Integer(0), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom(">"), Integer(1), Integer(0), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("<"), Integer(0), Atom(""), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("<"), Integer(0), &Compound{}, Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom(">"), Atom(""), &Variable{}, Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom(">"), Atom(""), Integer(0), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("<"), Atom("a"), Atom("b"), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("="), Atom("a"), Atom("a"), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom(">"), Atom("b"), Atom("a"), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("<"), Atom(""), &Compound{}, Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom(">"), &Compound{}, &Variable{}, Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom(">"), &Compound{}, Integer(0), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom(">"), &Compound{}, Atom(""), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("<"), &Compound{Functor: "a"}, &Compound{Functor: "b"}, Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("="), &Compound{Functor: "a"}, &Compound{Functor: "a"}, Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom(">"), &Compound{Functor: "b"}, &Compound{Functor: "a"}, Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom(">"), &Compound{Functor: "f", Args: []Term{Atom("a")}}, &Compound{Functor: "f"}, Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("="), &Compound{Functor: "f", Args: []Term{Atom("a")}}, &Compound{Functor: "f", Args: []Term{Atom("a")}}, Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("<"), &Compound{Functor: "f"}, &Compound{Functor: "f", Args: []Term{Atom("a")}}, Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom(">"), &Compound{Functor: "f", Args: []Term{Atom("b")}}, &Compound{Functor: "f", Args: []Term{Atom("a")}}, Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = Compare(Atom("<"), &Compound{Functor: "f", Args: []Term{Atom("a")}}, &Compound{Functor: "f", Args: []Term{Atom("b")}}, Done)
	assert.NoError(t, err)
	assert.True(t, ok)
}

func TestThrow(t *testing.T) {
	ok, err := Throw(Atom("a"), Done)
	assert.Equal(t, &Exception{Term: Atom("a")}, err)
	assert.False(t, ok)
}

func TestEngine_Catch(t *testing.T) {
	e, err := NewEngine(nil, nil)
	assert.NoError(t, err)

	t.Run("match", func(t *testing.T) {
		var v Variable
		ok, err := e.Catch(&Compound{
			Functor: "throw",
			Args:    []Term{Atom("a")},
		}, &v, &Compound{
			Functor: "=",
			Args:    []Term{&v, Atom("a")},
		}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not match", func(t *testing.T) {
		ok, err := e.Catch(&Compound{
			Functor: "throw",
			Args:    []Term{Atom("a")},
		}, Atom("b"), Atom("fail"), Done)
		assert.Equal(t, &Exception{Term: Atom("a")}, err)
		assert.False(t, ok)
	})

	t.Run("true", func(t *testing.T) {
		ok, err := e.Catch(Atom("true"), Atom("b"), Atom("fail"), Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("false", func(t *testing.T) {
		ok, err := e.Catch(Atom("fail"), Atom("b"), Atom("fail"), Done)
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestUnifyWithOccursCheck(t *testing.T) {
	v := Variable{Name: "X"}
	ok, err := UnifyWithOccursCheck(&v, &Compound{
		Functor: "f",
		Args:    []Term{&v},
	}, Done)
	assert.NoError(t, err)
	assert.False(t, ok)
}

func TestEngine_CurrentPredicate(t *testing.T) {
	e := Engine{procedures: map[string]procedure{
		"(=)/2": nil,
	}}

	var v Variable
	ok, err := e.CurrentPredicate(&v, Done)
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
	}, Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = e.Assertz(&Compound{
		Functor: "foo",
		Args:    []Term{Atom("b")},
	}, Done)
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
	}, Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = e.Asserta(&Compound{
		Functor: "foo",
		Args:    []Term{Atom("b")},
	}, Done)
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

func TestEngine_Retract(t *testing.T) {
	t.Run("retract the first one", func(t *testing.T) {
		e, err := NewEngine(nil, nil)
		assert.NoError(t, err)
		assert.NoError(t, e.Load("foo(a)."))
		assert.NoError(t, e.Load("foo(b)."))
		assert.NoError(t, e.Load("foo(c)."))
		ok, err := e.Query("retract(foo(X)).", func([]*Variable) bool {
			return true
		})
		assert.NoError(t, err)
		assert.True(t, ok)

		c := 0
		ok, err = e.Query("foo(X).", func(vars []*Variable) bool {
			switch c {
			case 0:
				assert.Equal(t, []*Variable{{Name: "X", Ref: Atom("b")}}, vars)
			case 1:
				assert.Equal(t, []*Variable{{Name: "X", Ref: Atom("c")}}, vars)
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return false
		})
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("retract the specific one", func(t *testing.T) {
		e, err := NewEngine(nil, nil)
		assert.NoError(t, err)
		assert.NoError(t, e.Load("foo(a)."))
		assert.NoError(t, e.Load("foo(b)."))
		assert.NoError(t, e.Load("foo(c)."))
		ok, err := e.Query("retract(foo(b)).", func([]*Variable) bool {
			return true
		})
		assert.NoError(t, err)
		assert.True(t, ok)

		c := 0
		ok, err = e.Query("foo(X).", func(vars []*Variable) bool {
			switch c {
			case 0:
				assert.Equal(t, []*Variable{{Name: "X", Ref: Atom("a")}}, vars)
			case 1:
				assert.Equal(t, []*Variable{{Name: "X", Ref: Atom("c")}}, vars)
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return false
		})
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("retract all", func(t *testing.T) {
		e, err := NewEngine(nil, nil)
		assert.NoError(t, err)
		assert.NoError(t, e.Load("foo(a)."))
		assert.NoError(t, e.Load("foo(b)."))
		assert.NoError(t, e.Load("foo(c)."))
		ok, err := e.Query("retract(foo(X)).", func([]*Variable) bool {
			return false
		})
		assert.NoError(t, err)
		assert.False(t, ok)

		ok, err = e.Query("foo(X).", func([]*Variable) bool {
			assert.Fail(t, "unreachable")
			return true
		})
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("variable", func(t *testing.T) {
		e, err := NewEngine(nil, nil)
		assert.NoError(t, err)
		assert.NoError(t, e.Load("foo(a)."))
		assert.NoError(t, e.Load("foo(b)."))
		assert.NoError(t, e.Load("foo(c)."))
		_, err = e.Query("retract(X).", func([]*Variable) bool {
			return false
		})
		assert.Error(t, err)
	})

	t.Run("no clause matches", func(t *testing.T) {
		e, err := NewEngine(nil, nil)
		assert.NoError(t, err)
		ok, err := e.Query("retract(foo(X)).", func([]*Variable) bool {
			return true
		})
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("builtin", func(t *testing.T) {
		e, err := NewEngine(nil, nil)
		assert.NoError(t, err)
		_, err = e.Query("retract(call(X)).", func([]*Variable) bool {
			return true
		})
		assert.Error(t, err)
	})

	t.Run("exception in continuation", func(t *testing.T) {
		e, err := NewEngine(nil, nil)
		assert.NoError(t, err)
		assert.NoError(t, e.Load("foo(a)."))
		_, err = e.Query("retract(foo(X)), throw(e).", func([]*Variable) bool {
			return false
		})
		assert.Error(t, err)

		// removed
		ok, err := e.Query("foo(a).", func([]*Variable) bool {
			assert.Fail(t, "unreachable")
			return true
		})
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestEngine_Abolish(t *testing.T) {
	e, err := NewEngine(nil, nil)
	assert.NoError(t, err)
	assert.NoError(t, e.Load("foo(a)."))
	assert.NoError(t, e.Load("foo(b)."))
	assert.NoError(t, e.Load("foo(c)."))

	ok, err := e.Abolish(&Compound{
		Functor: "/",
		Args:    []Term{Atom("foo"), Integer(1)},
	}, Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	_, ok = e.procedures["foo/1"]
	assert.False(t, ok)
}

func TestEngine_CurrentInput(t *testing.T) {
	var buf bytes.Buffer
	e, err := NewEngine(&buf, nil)
	assert.NoError(t, err)
	_, err = e.Query("current_input(X).", func(vars []*Variable) bool {
		assert.Equal(t, &Variable{
			Name: "X",
			Ref:  &Variable{Ref: Stream{ReadWriteCloser: &input{Reader: &buf}}},
		}, vars[0])
		return true
	})
	assert.NoError(t, err)
}

func TestEngine_CurrentOutput(t *testing.T) {
	var buf bytes.Buffer
	e, err := NewEngine(nil, &buf)
	assert.NoError(t, err)
	_, err = e.Query("current_output(X).", func(vars []*Variable) bool {
		assert.Equal(t, &Variable{
			Name: "X",
			Ref:  &Variable{Ref: Stream{ReadWriteCloser: &output{Writer: &buf}}},
		}, vars[0])
		return true
	})
	assert.NoError(t, err)
}

func TestEngine_SetInput(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		var e Engine
		s := Stream{ReadWriteCloser: os.Stdin}
		ok, err := e.SetInput(&Variable{Ref: s}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.Equal(t, s, e.input)
	})

	t.Run("atom defined as a stream global variable", func(t *testing.T) {
		s := Stream{ReadWriteCloser: os.Stdin}
		e := Engine{
			globalVars: map[Atom]Term{
				"x": s,
			},
		}
		ok, err := e.SetInput(&Variable{Ref: Atom("x")}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.Equal(t, s, e.input)
	})

	t.Run("atom defined as a non-stream global variable", func(t *testing.T) {
		e := Engine{
			globalVars: map[Atom]Term{
				"x": Integer(1),
			},
		}
		_, err := e.SetInput(&Variable{Ref: Atom("x")}, Done)
		assert.Error(t, err)
	})

	t.Run("atom not defined as a global variable", func(t *testing.T) {
		var e Engine
		_, err := e.SetInput(&Variable{Ref: Atom("x")}, Done)
		assert.Error(t, err)
	})
}

func TestEngine_SetOutput(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		var e Engine
		s := Stream{ReadWriteCloser: os.Stdout}
		ok, err := e.SetOutput(&Variable{Ref: s}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.Equal(t, s, e.output)
	})

	t.Run("atom defined as a stream global variable", func(t *testing.T) {
		s := Stream{ReadWriteCloser: os.Stdout}
		e := Engine{
			globalVars: map[Atom]Term{
				"x": s,
			},
		}
		ok, err := e.SetOutput(&Variable{Ref: Atom("x")}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.Equal(t, s, e.output)
	})

	t.Run("atom defined as a non-stream global variable", func(t *testing.T) {
		e := Engine{
			globalVars: map[Atom]Term{
				"x": Integer(1),
			},
		}
		_, err := e.SetOutput(&Variable{Ref: Atom("x")}, Done)
		assert.Error(t, err)
	})

	t.Run("atom not defined as a global variable", func(t *testing.T) {
		var e Engine
		_, err := e.SetOutput(&Variable{Ref: Atom("x")}, Done)
		assert.Error(t, err)
	})
}

func TestEngine_Open(t *testing.T) {
	var e Engine

	t.Run("read", func(t *testing.T) {
		f, err := ioutil.TempFile("", "open_test_read")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, os.Remove(f.Name()))
		}()

		_, err = fmt.Fprintf(f, "test\n")
		assert.NoError(t, err)

		assert.NoError(t, f.Close())

		var v Variable
		ok, err := e.Open(Atom(f.Name()), Atom("read"), &v, List(&Compound{
			Functor: "alias",
			Args:    []Term{Atom("input")},
		}), Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		s, ok := v.Ref.(Stream)
		assert.True(t, ok)

		assert.Equal(t, e.globalVars["input"], s)

		b, err := ioutil.ReadAll(s)
		assert.NoError(t, err)
		assert.Equal(t, "test\n", string(b))
	})

	t.Run("write", func(t *testing.T) {
		n := filepath.Join(os.TempDir(), "open_test_write")
		defer func() {
			assert.NoError(t, os.Remove(n))
		}()

		var v Variable
		ok, err := e.Open(Atom(n), Atom("write"), &v, List(&Compound{
			Functor: "alias",
			Args:    []Term{Atom("output")},
		}), Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		s, ok := v.Ref.(Stream)
		assert.True(t, ok)

		assert.Equal(t, e.globalVars["output"], s)

		_, err = fmt.Fprintf(s, "test\n")
		assert.NoError(t, err)

		f, err := os.Open(n)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		b, err := ioutil.ReadAll(f)
		assert.NoError(t, err)
		assert.Equal(t, "test\n", string(b))
	})

	t.Run("append", func(t *testing.T) {
		f, err := ioutil.TempFile("", "open_test_append")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, os.Remove(f.Name()))
		}()

		_, err = fmt.Fprintf(f, "test\n")
		assert.NoError(t, err)

		assert.NoError(t, f.Close())

		var v Variable
		ok, err := e.Open(Atom(f.Name()), Atom("append"), &v, List(&Compound{
			Functor: "alias",
			Args:    []Term{Atom("append")},
		}), Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		s, ok := v.Ref.(Stream)
		assert.True(t, ok)

		assert.Equal(t, e.globalVars["append"], s)

		_, err = fmt.Fprintf(s, "test\n")
		assert.NoError(t, err)

		f, err = os.Open(f.Name())
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		b, err := ioutil.ReadAll(f)
		assert.NoError(t, err)
		assert.Equal(t, "test\ntest\n", string(b))
	})

	t.Run("invalid file name", func(t *testing.T) {
		var v Variable
		_, err := e.Open(&Variable{}, Atom("read"), &v, List(&Compound{
			Functor: "alias",
			Args:    []Term{Atom("input")},
		}), Done)
		assert.Error(t, err)
	})

	t.Run("invalid mode", func(t *testing.T) {
		var v Variable
		_, err := e.Open(Atom("/dev/null"), Atom("invalid"), &v, List(&Compound{
			Functor: "alias",
			Args:    []Term{Atom("input")},
		}), Done)
		assert.Error(t, err)
	})

	t.Run("invalid alias", func(t *testing.T) {
		var v Variable
		_, err := e.Open(Atom("/dev/null"), Atom("read"), &v, List(&Compound{
			Functor: "alias",
			Args:    []Term{&Variable{}},
		}), Done)
		assert.Error(t, err)
	})

	t.Run("unknown option", func(t *testing.T) {
		var v Variable
		_, err := e.Open(Atom("/dev/null"), Atom("read"), &v, List(&Compound{
			Functor: "unknown",
			Args:    []Term{Atom("option")},
		}), Done)
		assert.Error(t, err)
	})
}

func TestEngine_Close(t *testing.T) {
	t.Run("without options", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			var m mockReadWriteCloser
			m.On("Close").Return(nil).Once()
			defer m.AssertExpectations(t)

			var e Engine
			ok, err := e.Close(Stream{ReadWriteCloser: &m}, List(), Done)
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("ng", func(t *testing.T) {
			var m mockReadWriteCloser
			m.On("Close").Return(errors.New("")).Once()
			defer m.AssertExpectations(t)

			var e Engine
			_, err := e.Close(Stream{ReadWriteCloser: &m}, List(), Done)
			assert.Error(t, err)
		})
	})

	t.Run("force false", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			var m mockReadWriteCloser
			m.On("Close").Return(nil).Once()
			defer m.AssertExpectations(t)

			var e Engine
			ok, err := e.Close(Stream{ReadWriteCloser: &m}, List(&Compound{
				Functor: "force",
				Args:    []Term{Atom("false")},
			}), Done)
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("ng", func(t *testing.T) {
			var m mockReadWriteCloser
			m.On("Close").Return(errors.New("")).Once()
			defer m.AssertExpectations(t)

			var e Engine
			_, err := e.Close(Stream{ReadWriteCloser: &m}, List(&Compound{
				Functor: "force",
				Args:    []Term{Atom("false")},
			}), Done)
			assert.Error(t, err)
		})
	})

	t.Run("force true", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			var m mockReadWriteCloser
			m.On("Close").Return(nil).Once()
			defer m.AssertExpectations(t)

			var e Engine
			ok, err := e.Close(Stream{ReadWriteCloser: &m}, List(&Compound{
				Functor: "force",
				Args:    []Term{Atom("true")},
			}), Done)
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("ng", func(t *testing.T) {
			var m mockReadWriteCloser
			m.On("Close").Return(errors.New("")).Once()
			defer m.AssertExpectations(t)

			var e Engine
			ok, err := e.Close(Stream{ReadWriteCloser: &m}, List(&Compound{
				Functor: "force",
				Args:    []Term{Atom("true")},
			}), Done)
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("valid global variable", func(t *testing.T) {
		var m mockReadWriteCloser
		m.On("Close").Return(nil).Once()
		defer m.AssertExpectations(t)

		e := Engine{
			globalVars: map[Atom]Term{
				"foo": Stream{ReadWriteCloser: &m},
			},
		}
		ok, err := e.Close(Atom("foo"), List(), Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("unknown global variable", func(t *testing.T) {
		var e Engine
		_, err := e.Close(Atom("foo"), List(), Done)
		assert.Error(t, err)
	})

	t.Run("non stream", func(t *testing.T) {
		var e Engine
		_, err := e.Close(&Variable{}, List(), Done)
		assert.Error(t, err)
	})

	t.Run("unknown option", func(t *testing.T) {
		var e Engine
		_, err := e.Close(Stream{}, List(&Compound{
			Functor: "unknown",
			Args:    []Term{Atom("option")},
		}), Done)
		assert.Error(t, err)
	})
}

type mockReadWriteCloser struct {
	mock.Mock
}

func (m *mockReadWriteCloser) Read(p []byte) (n int, err error) {
	args := m.Called(p)
	return args.Int(0), args.Error(1)
}

func (m *mockReadWriteCloser) Write(p []byte) (n int, err error) {
	args := m.Called(p)
	return args.Int(0), args.Error(1)
}

func (m *mockReadWriteCloser) Close() error {
	args := m.Called()
	return args.Error(0)
}

func TestEngine_FlushOutput(t *testing.T) {
	t.Run("non flusher", func(t *testing.T) {
		var m mockReadWriteCloser
		defer m.AssertExpectations(t)

		var e Engine
		ok, err := e.FlushOutput(Stream{ReadWriteCloser: &m}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("flusher", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			var m struct {
				mockReadWriteCloser
				mockFlusher
			}
			m.mockFlusher.On("Flush").Return(nil).Once()
			defer m.mockReadWriteCloser.AssertExpectations(t)
			defer m.mockFlusher.AssertExpectations(t)

			var e Engine
			ok, err := e.FlushOutput(Stream{ReadWriteCloser: &m}, Done)
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("ng", func(t *testing.T) {
			var m struct {
				mockReadWriteCloser
				mockFlusher
			}
			m.mockFlusher.On("Flush").Return(errors.New("")).Once()
			defer m.mockReadWriteCloser.AssertExpectations(t)
			defer m.mockFlusher.AssertExpectations(t)

			var e Engine
			_, err := e.FlushOutput(Stream{ReadWriteCloser: &m}, Done)
			assert.Error(t, err)
		})
	})

	t.Run("valid global variable", func(t *testing.T) {
		var m mockReadWriteCloser
		defer m.AssertExpectations(t)

		e := Engine{
			globalVars: map[Atom]Term{
				"foo": Stream{ReadWriteCloser: &m},
			},
		}
		ok, err := e.FlushOutput(Atom("foo"), Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("unknown global variable", func(t *testing.T) {
		var e Engine
		_, err := e.FlushOutput(Atom("foo"), Done)
		assert.Error(t, err)
	})

	t.Run("non stream", func(t *testing.T) {
		var e Engine
		_, err := e.FlushOutput(&Variable{}, Done)
		assert.Error(t, err)
	})
}

type mockFlusher struct {
	mock.Mock
}

func (m *mockFlusher) Flush() error {
	args := m.Called()
	return args.Error(0)
}

func TestEngine_WriteTerm(t *testing.T) {
	var io mockReadWriteCloser
	defer io.AssertExpectations(t)

	s := Stream{ReadWriteCloser: &io}

	ops := operators{
		{Precedence: 500, Type: "yfx", Name: "+"},
		{Precedence: 200, Type: "fy", Name: "-"},
	}

	e := Engine{
		operators: ops,
		globalVars: map[Atom]Term{
			"foo": s,
		},
	}

	t.Run("without options", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			var m mockTerm
			m.On("WriteTerm", s, WriteTermOptions{ops: ops}).Return(nil).Once()
			defer m.AssertExpectations(t)

			ok, err := e.WriteTerm(s, &m, List(), Done)
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("ng", func(t *testing.T) {
			var m mockTerm
			m.On("WriteTerm", s, WriteTermOptions{ops: ops}).Return(errors.New("")).Once()
			defer m.AssertExpectations(t)

			_, err := e.WriteTerm(s, &m, List(), Done)
			assert.Error(t, err)
		})
	})

	t.Run("quoted", func(t *testing.T) {
		t.Run("false", func(t *testing.T) {
			var m mockTerm
			m.On("WriteTerm", s, WriteTermOptions{quoted: false, ops: ops}).Return(nil).Once()
			defer m.AssertExpectations(t)

			ok, err := e.WriteTerm(s, &m, List(&Compound{
				Functor: "quoted",
				Args:    []Term{Atom("false")},
			}), Done)
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("true", func(t *testing.T) {
			var m mockTerm
			m.On("WriteTerm", s, WriteTermOptions{quoted: true, ops: ops}).Return(nil).Once()
			defer m.AssertExpectations(t)

			ok, err := e.WriteTerm(s, &m, List(&Compound{
				Functor: "quoted",
				Args:    []Term{Atom("true")},
			}), Done)
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("ignore_ops", func(t *testing.T) {
		t.Run("false", func(t *testing.T) {
			var m mockTerm
			m.On("WriteTerm", s, WriteTermOptions{ops: ops}).Return(nil).Once()
			defer m.AssertExpectations(t)

			ok, err := e.WriteTerm(s, &m, List(&Compound{
				Functor: "ignore_ops",
				Args:    []Term{Atom("false")},
			}), Done)
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("true", func(t *testing.T) {
			var m mockTerm
			m.On("WriteTerm", s, WriteTermOptions{ops: nil}).Return(nil).Once()
			defer m.AssertExpectations(t)

			ok, err := e.WriteTerm(s, &m, List(&Compound{
				Functor: "ignore_ops",
				Args:    []Term{Atom("true")},
			}), Done)
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("numbervars", func(t *testing.T) {
		t.Run("false", func(t *testing.T) {
			var m mockTerm
			m.On("WriteTerm", s, WriteTermOptions{ops: ops, numberVars: false}).Return(nil).Once()
			defer m.AssertExpectations(t)

			ok, err := e.WriteTerm(s, &m, List(&Compound{
				Functor: "numbervars",
				Args:    []Term{Atom("false")},
			}), Done)
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("true", func(t *testing.T) {
			var m mockTerm
			m.On("WriteTerm", s, WriteTermOptions{ops: ops, numberVars: true}).Return(nil).Once()
			defer m.AssertExpectations(t)

			ok, err := e.WriteTerm(s, &m, List(&Compound{
				Functor: "numbervars",
				Args:    []Term{Atom("true")},
			}), Done)
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("unknown option", func(t *testing.T) {
		var m mockTerm
		defer m.AssertExpectations(t)

		_, err := e.WriteTerm(s, &m, List(&Compound{
			Functor: "unknown",
			Args:    []Term{Atom("option")},
		}), Done)
		assert.Error(t, err)
	})

	t.Run("valid global variable", func(t *testing.T) {
		var m mockTerm
		m.On("WriteTerm", s, WriteTermOptions{ops: ops}).Return(nil).Once()
		defer m.AssertExpectations(t)

		ok, err := e.WriteTerm(Atom("foo"), &m, List(), Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("unknown global variable", func(t *testing.T) {
		var m mockTerm
		defer m.AssertExpectations(t)

		_, err := e.WriteTerm(Atom("bar"), &m, List(), Done)
		assert.Error(t, err)
	})

	t.Run("non stream", func(t *testing.T) {
		var m mockTerm
		defer m.AssertExpectations(t)

		_, err := e.WriteTerm(&Variable{}, &m, List(), Done)
		assert.Error(t, err)
	})
}

type mockTerm struct {
	mock.Mock
}

func (m *mockTerm) String() string {
	args := m.Called()
	return args.String(0)
}

func (m *mockTerm) WriteTerm(w io.Writer, opts WriteTermOptions) error {
	args := m.Called(w, opts)
	return args.Error(0)
}

func (m *mockTerm) Unify(t Term, occursCheck bool) bool {
	args := m.Called(t, occursCheck)
	return args.Bool(0)
}

func (m *mockTerm) Copy() Term {
	args := m.Called()
	return args.Get(0).(Term)
}

func TestCharCode(t *testing.T) {
	t.Run("ascii", func(t *testing.T) {
		ok, err := CharCode(Atom("a"), Integer(97), Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("emoji", func(t *testing.T) {
		ok, err := CharCode(Atom("😀"), Integer(128512), Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("query char", func(t *testing.T) {
		var v Variable
		ok, err := CharCode(&v, Integer(128512), Done)
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.Equal(t, Atom("😀"), v.Ref)
	})

	t.Run("query code", func(t *testing.T) {
		var v Variable
		ok, err := CharCode(Atom("😀"), &v, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.Equal(t, Integer(128512), v.Ref)
	})

	t.Run("not a character", func(t *testing.T) {
		var v Variable
		_, err := CharCode(Atom("abc"), &v, Done)
		assert.Error(t, err)
	})

	t.Run("not a code", func(t *testing.T) {
		var v Variable
		_, err := CharCode(&v, Float(1.0), Done)
		assert.Error(t, err)
	})
}

func TestEngine_PutByte(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		var io mockReadWriteCloser
		io.On("Write", []byte{97}).Return(1, nil).Once()
		defer io.AssertExpectations(t)

		s := Stream{ReadWriteCloser: &io}

		var e Engine
		ok, err := e.PutByte(s, Integer(97), Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("ng", func(t *testing.T) {
		var io mockReadWriteCloser
		io.On("Write", []byte{97}).Return(0, errors.New("")).Once()
		defer io.AssertExpectations(t)

		s := Stream{ReadWriteCloser: &io}

		var e Engine
		_, err := e.PutByte(s, Integer(97), Done)
		assert.Error(t, err)
	})

	t.Run("valid global variable", func(t *testing.T) {
		var io mockReadWriteCloser
		io.On("Write", []byte{97}).Return(1, nil).Once()
		defer io.AssertExpectations(t)

		s := Stream{ReadWriteCloser: &io}

		e := Engine{
			globalVars: map[Atom]Term{
				"foo": s,
			},
		}
		ok, err := e.PutByte(Atom("foo"), Integer(97), Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("unknown global variable", func(t *testing.T) {
		var e Engine
		_, err := e.PutByte(Atom("foo"), Integer(97), Done)
		assert.Error(t, err)
	})

	t.Run("not a stream", func(t *testing.T) {
		var e Engine
		_, err := e.PutByte(&Variable{}, Integer(97), Done)
		assert.Error(t, err)
	})

	t.Run("not a byte", func(t *testing.T) {
		var io mockReadWriteCloser
		defer io.AssertExpectations(t)

		s := Stream{ReadWriteCloser: &io}

		t.Run("not an integer", func(t *testing.T) {
			var e Engine
			_, err := e.PutByte(s, Atom("a"), Done)
			assert.Error(t, err)
		})

		t.Run("negative", func(t *testing.T) {
			var e Engine
			_, err := e.PutByte(s, Integer(-1), Done)
			assert.Error(t, err)
		})

		t.Run("more than 255", func(t *testing.T) {
			var e Engine
			_, err := e.PutByte(s, Integer(256), Done)
			assert.Error(t, err)
		})
	})
}

func TestEngine_ReadTerm(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		var e Engine

		var v Variable
		ok, err := e.ReadTerm(Stream{ReadWriteCloser: stringRWC{strings.NewReader("foo.")}}, &v, List(), Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, Atom("foo"), v.Ref)
	})

	t.Run("valid global variable", func(t *testing.T) {
		s := Stream{ReadWriteCloser: stringRWC{strings.NewReader("foo.")}}

		e := Engine{
			globalVars: map[Atom]Term{
				"foo": s,
			},
		}

		var v Variable
		ok, err := e.ReadTerm(Atom("foo"), &v, List(), Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, Atom("foo"), v.Ref)
	})

	t.Run("unknown global variable", func(t *testing.T) {
		var e Engine

		var v Variable
		_, err := e.ReadTerm(Atom("foo"), &v, List(), Done)
		assert.Error(t, err)
	})

	t.Run("non stream", func(t *testing.T) {
		var e Engine

		var v Variable
		_, err := e.ReadTerm(&Variable{}, &v, List(), Done)
		assert.Error(t, err)
	})

	t.Run("singletons", func(t *testing.T) {
		var e Engine

		var term, singletons Variable
		ok, err := e.ReadTerm(Stream{ReadWriteCloser: stringRWC{strings.NewReader("f(X, X, Y).")}}, &term, List(&Compound{
			Functor: "singletons",
			Args:    []Term{&singletons},
		}), Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, &Compound{
			Functor: "f",
			Args: []Term{
				&Variable{},
				&Variable{},
				&Variable{},
			},
		}, term.Ref)

		assert.Equal(t, &Variable{Ref: List(&Variable{})}, singletons.Ref)
	})

	t.Run("variables", func(t *testing.T) {
		var e Engine

		var term, variables Variable
		ok, err := e.ReadTerm(Stream{ReadWriteCloser: stringRWC{strings.NewReader("f(X, X, Y).")}}, &term, List(&Compound{
			Functor: "variables",
			Args:    []Term{&variables},
		}), Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, &Compound{
			Functor: "f",
			Args: []Term{
				&Variable{},
				&Variable{},
				&Variable{},
			},
		}, term.Ref)

		assert.Equal(t, &Variable{Ref: List(&Variable{}, &Variable{})}, variables.Ref)
	})

	t.Run("variable_names", func(t *testing.T) {
		var e Engine

		var term, variableNames Variable
		ok, err := e.ReadTerm(Stream{ReadWriteCloser: stringRWC{strings.NewReader("f(X, X, Y).")}}, &term, List(&Compound{
			Functor: "variable_names",
			Args:    []Term{&variableNames},
		}), Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, &Compound{
			Functor: "f",
			Args: []Term{
				&Variable{},
				&Variable{},
				&Variable{},
			},
		}, term.Ref)

		assert.Equal(t, &Variable{Ref: List(
			&Compound{
				Functor: "=",
				Args:    []Term{Atom("X"), &Variable{}},
			},
			&Compound{
				Functor: "=",
				Args:    []Term{Atom("Y"), &Variable{}},
			},
		)}, variableNames.Ref)
	})

	t.Run("unknown option", func(t *testing.T) {
		var e Engine

		var v Variable
		_, err := e.ReadTerm(Stream{ReadWriteCloser: stringRWC{strings.NewReader("foo.")}}, &v, List(&Compound{
			Functor: "unknown",
			Args:    []Term{Atom("option")},
		}), Done)
		assert.Error(t, err)
	})
}

type stringRWC struct {
	*strings.Reader
}

func (s stringRWC) Read(p []byte) (n int, err error) {
	return s.Reader.Read(p)
}

func (s stringRWC) Write(p []byte) (n int, err error) {
	return 0, errors.New("")
}

func (s stringRWC) Close() error {
	return errors.New("")
}

func TestEngine_GetByte(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		s := Stream{ReadWriteCloser: stringRWC{Reader: strings.NewReader("a")}}

		var e Engine

		var v Variable
		ok, err := e.GetByte(s, &v, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, Integer(97), v.Ref)
	})

	t.Run("valid global variable", func(t *testing.T) {
		s := Stream{ReadWriteCloser: stringRWC{Reader: strings.NewReader("a")}}

		e := Engine{
			globalVars: map[Atom]Term{
				"foo": s,
			},
		}

		var v Variable
		ok, err := e.GetByte(Atom("foo"), &v, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, Integer(97), v.Ref)
	})

	t.Run("unknown global variable", func(t *testing.T) {
		var e Engine

		var v Variable
		_, err := e.GetByte(Atom("foo"), &v, Done)
		assert.Error(t, err)
	})

	t.Run("non stream", func(t *testing.T) {
		var e Engine

		var v Variable
		_, err := e.GetByte(&Variable{}, &v, Done)
		assert.Error(t, err)
	})

	t.Run("eof", func(t *testing.T) {
		s := Stream{ReadWriteCloser: stringRWC{Reader: strings.NewReader("")}}

		var e Engine

		var v Variable
		ok, err := e.GetByte(s, &v, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, Integer(-1), v.Ref)
	})

	t.Run("error", func(t *testing.T) {
		var m mockReadWriteCloser
		m.On("Read", make([]byte, 1)).Return(0, errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		s := Stream{ReadWriteCloser: &m}

		var e Engine

		var v Variable
		_, err := e.GetByte(s, &v, Done)
		assert.Error(t, err)
	})
}

func TestEngine_Clause(t *testing.T) {
	e, err := NewEngine(nil, nil)
	assert.NoError(t, err)
	assert.NoError(t, e.Load("green(X) :- moldy(X)."))
	assert.NoError(t, e.Load("green(kermit)."))

	var c int

	var what, body Variable
	ok, err := e.Clause(&Compound{
		Functor: "green",
		Args:    []Term{&what},
	}, &body, func() (bool, error) {
		switch c {
		case 0:
			assert.Equal(t, &Variable{}, what.Ref)
			assert.Equal(t, &Compound{
				Functor: "moldy",
				Args:    []Term{&Variable{Ref: &Variable{}}},
			}, body.Ref)
		case 1:
			assert.Equal(t, Atom("kermit"), what.Ref)
			assert.Equal(t, Atom("true"), body.Ref)
		default:
			assert.Fail(t, "unreachable")
		}
		c++
		return false, nil
	})
	assert.NoError(t, err)
	assert.False(t, ok)
}

func TestAtomLength(t *testing.T) {
	ok, err := AtomLength(Atom("abc"), Integer(3), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = AtomLength(Atom("😀"), Integer(1), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	_, err = AtomLength(&Variable{}, Integer(0), Done)
	assert.Error(t, err)
}

func TestAtomConcat(t *testing.T) {
	ok, err := AtomConcat(Atom("foo"), Atom("bar"), Atom("foobar"), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	var c int
	var v1, v2 Variable
	ok, err = AtomConcat(&v1, &v2, Atom("foo"), func() (bool, error) {
		switch c {
		case 0:
			assert.Equal(t, Atom(""), v1.Ref)
			assert.Equal(t, Atom("foo"), v2.Ref)
		case 1:
			assert.Equal(t, Atom("f"), v1.Ref)
			assert.Equal(t, Atom("oo"), v2.Ref)
		case 2:
			assert.Equal(t, Atom("fo"), v1.Ref)
			assert.Equal(t, Atom("o"), v2.Ref)
		case 3:
			assert.Equal(t, Atom("foo"), v1.Ref)
			assert.Equal(t, Atom(""), v2.Ref)
		default:
			assert.Fail(t, "unreachable")
		}
		c++
		return false, nil
	})
	assert.NoError(t, err)
	assert.False(t, ok)
}

func TestSubAtom(t *testing.T) {
	t.Run("multiple solutions", func(t *testing.T) {
		var c int
		var before, length, after Variable
		ok, err := SubAtom(Atom("xATGATGAxATGAxATGAx"), &before, &length, &after, Atom("ATGA"), func() (bool, error) {
			switch c {
			case 0:
				assert.Equal(t, Integer(1), before.Ref)
				assert.Equal(t, Integer(4), length.Ref)
				assert.Equal(t, Integer(14), after.Ref)
			case 1:
				assert.Equal(t, Integer(4), before.Ref)
				assert.Equal(t, Integer(4), length.Ref)
				assert.Equal(t, Integer(11), after.Ref)
			case 2:
				assert.Equal(t, Integer(9), before.Ref)
				assert.Equal(t, Integer(4), length.Ref)
				assert.Equal(t, Integer(6), after.Ref)
			case 3:
				assert.Equal(t, Integer(14), before.Ref)
				assert.Equal(t, Integer(4), length.Ref)
				assert.Equal(t, Integer(1), after.Ref)
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return false, nil
		})
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("get the first char", func(t *testing.T) {
		var char Variable
		ok, err := SubAtom(Atom("a"), Integer(0), Integer(1), &Variable{}, &char, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.Equal(t, Atom("a"), char.Ref)
	})
}

func TestAtomChars(t *testing.T) {
	var chars Variable
	ok, err := AtomChars(Atom("foo"), &chars, Done)
	assert.NoError(t, err)
	assert.True(t, ok)
	assert.Equal(t, List(Atom("f"), Atom("o"), Atom("o")), chars.Ref)

	var atom Variable
	ok, err = AtomChars(&atom, List(Atom("f"), Atom("o"), Atom("o")), Done)
	assert.NoError(t, err)
	assert.True(t, ok)
	assert.Equal(t, Atom("foo"), atom.Ref)

	_, err = AtomChars(&Variable{}, List(Integer(102), Integer(111), Integer(111)), Done)
	assert.Error(t, err)
}

func TestAtomCodes(t *testing.T) {
	var codes Variable
	ok, err := AtomCodes(Atom("foo"), &codes, Done)
	assert.NoError(t, err)
	assert.True(t, ok)
	assert.Equal(t, List(Integer(102), Integer(111), Integer(111)), codes.Ref)

	var atom Variable
	ok, err = AtomCodes(&atom, List(Integer(102), Integer(111), Integer(111)), Done)
	assert.NoError(t, err)
	assert.True(t, ok)
	assert.Equal(t, Atom("foo"), atom.Ref)

	_, err = AtomCodes(&Variable{}, List(Atom("f"), Atom("o"), Atom("o")), Done)
	assert.Error(t, err)
}

func TestNumberChars(t *testing.T) {
	t.Run("number to chars", func(t *testing.T) {
		var chars Variable
		ok, err := NumberChars(Float(23.4), &chars, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.Equal(t, List(Atom("2"), Atom("3"), Atom("."), Atom("4")), chars.Ref)
	})

	t.Run("chars to number", func(t *testing.T) {
		var num Variable
		ok, err := NumberChars(&num, List(Atom("2"), Atom("3"), Atom("."), Atom("4")), Done)
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.Equal(t, Float(23.4), num.Ref)
	})

	t.Run("not chars", func(t *testing.T) {
		_, err := NumberChars(&Variable{}, List(Integer(1), Integer(2), Integer(3)), Done)
		assert.Error(t, err)
	})

	t.Run("not number chars", func(t *testing.T) {
		_, err := NumberChars(&Variable{}, List(Atom("f"), Atom("o"), Atom("o")), Done)
		assert.Error(t, err)
	})

	t.Run("not a number", func(t *testing.T) {
		_, err := NumberChars(Atom("abc"), &Variable{}, Done)
		assert.Error(t, err)
	})
}

func TestNumberCodes(t *testing.T) {
	t.Run("number to codes", func(t *testing.T) {
		var codes Variable
		ok, err := NumberCodes(Float(23.4), &codes, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.Equal(t, List(Integer(50), Integer(51), Integer(46), Integer(52)), codes.Ref)
	})

	t.Run("codes to number", func(t *testing.T) {
		var num Variable
		ok, err := NumberCodes(&num, List(Integer(50), Integer(51), Integer(46), Integer(52)), Done)
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.Equal(t, Float(23.4), num.Ref)
	})

	t.Run("not codes", func(t *testing.T) {
		_, err := NumberCodes(&Variable{}, List(Atom("f"), Atom("o"), Atom("o")), Done)
		assert.Error(t, err)
	})

	t.Run("not number codes", func(t *testing.T) {
		_, err := NumberCodes(&Variable{}, List(Integer(102), Integer(111), Integer(111)), Done)
		assert.Error(t, err)
	})

	t.Run("not a number", func(t *testing.T) {
		_, err := NumberCodes(Atom("abc"), &Variable{}, Done)
		assert.Error(t, err)
	})
}

func TestFunctionSet_Is(t *testing.T) {
	t.Run("addition", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(3), &Compound{Functor: "+", Args: []Term{Integer(1), Integer(2)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(3), &Compound{Functor: "+", Args: []Term{Integer(1), Float(2)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(3), &Compound{Functor: "+", Args: []Term{Float(1), Integer(2)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(3), &Compound{Functor: "+", Args: []Term{Float(1), Float(2)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("subtraction", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(1), &Compound{Functor: "-", Args: []Term{Integer(3), Integer(2)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "-", Args: []Term{Integer(3), Float(2)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "-", Args: []Term{Float(3), Integer(2)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "-", Args: []Term{Float(3), Float(2)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("multiplication", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(6), &Compound{Functor: "*", Args: []Term{Integer(3), Integer(2)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(6), &Compound{Functor: "*", Args: []Term{Integer(3), Float(2)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(6), &Compound{Functor: "*", Args: []Term{Float(3), Integer(2)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(6), &Compound{Functor: "*", Args: []Term{Float(3), Float(2)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("floating-point division", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(2), &Compound{Functor: "/", Args: []Term{Integer(4), Integer(2)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(2), &Compound{Functor: "/", Args: []Term{Integer(4), Float(2)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(2), &Compound{Functor: "/", Args: []Term{Float(4), Integer(2)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(2), &Compound{Functor: "/", Args: []Term{Float(4), Float(2)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("integer division", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(2), &Compound{Functor: "//", Args: []Term{Integer(4), Integer(2)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		_, err = DefaultFunctionSet.Is(&Variable{}, &Compound{Functor: "//", Args: []Term{Integer(4), Float(2)}}, Done)
		assert.Error(t, err)

		_, err = DefaultFunctionSet.Is(&Variable{}, &Compound{Functor: "//", Args: []Term{Float(4), Integer(2)}}, Done)
		assert.Error(t, err)
	})

	t.Run("remainder", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(-1), &Compound{Functor: "rem", Args: []Term{Integer(-21), Integer(4)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		_, err = DefaultFunctionSet.Is(&Variable{}, &Compound{Functor: "rem", Args: []Term{Integer(-21), Float(4)}}, Done)
		assert.Error(t, err)

		_, err = DefaultFunctionSet.Is(&Variable{}, &Compound{Functor: "rem", Args: []Term{Float(-21), Integer(4)}}, Done)
		assert.Error(t, err)
	})

	t.Run("mod", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(3), &Compound{Functor: "mod", Args: []Term{Integer(-21), Integer(4)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		_, err = DefaultFunctionSet.Is(&Variable{}, &Compound{Functor: "mod", Args: []Term{Integer(-21), Float(4)}}, Done)
		assert.Error(t, err)

		_, err = DefaultFunctionSet.Is(&Variable{}, &Compound{Functor: "mod", Args: []Term{Float(-21), Integer(4)}}, Done)
		assert.Error(t, err)
	})

	t.Run("exponential", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(16), &Compound{Functor: "**", Args: []Term{Integer(4), Integer(2)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(16), &Compound{Functor: "**", Args: []Term{Integer(4), Float(2)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(16), &Compound{Functor: "**", Args: []Term{Float(4), Integer(2)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(16), &Compound{Functor: "**", Args: []Term{Float(4), Float(2)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("sign reversal", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(-2), &Compound{Functor: "-", Args: []Term{Integer(2)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(-2), &Compound{Functor: "-", Args: []Term{Float(2)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("absolute value", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(2), &Compound{Functor: "abs", Args: []Term{Integer(-2)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(2), &Compound{Functor: "abs", Args: []Term{Float(-2)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("arctangent", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(0), &Compound{Functor: "atan", Args: []Term{Integer(0)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(0), &Compound{Functor: "atan", Args: []Term{Float(0)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("ceiling", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(1), &Compound{Functor: "ceiling", Args: []Term{Integer(1)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "ceiling", Args: []Term{Float(0.9)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("cosine", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(1.0), &Compound{Functor: "cos", Args: []Term{Integer(0)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1.0), &Compound{Functor: "cos", Args: []Term{Float(0)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("natural antilogarithm", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(1.0), &Compound{Functor: "exp", Args: []Term{Integer(0)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1.0), &Compound{Functor: "exp", Args: []Term{Float(0)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("square root", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(1.0), &Compound{Functor: "sqrt", Args: []Term{Integer(1)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1.0), &Compound{Functor: "sqrt", Args: []Term{Float(1)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("sign", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(1), &Compound{Functor: "sign", Args: []Term{Integer(1)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Integer(1), &Compound{Functor: "sign", Args: []Term{Integer(math.MaxInt64)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Integer(0), &Compound{Functor: "sign", Args: []Term{Integer(0)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Integer(-1), &Compound{Functor: "sign", Args: []Term{Integer(-1)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Integer(-1), &Compound{Functor: "sign", Args: []Term{Integer(math.MinInt64)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "sign", Args: []Term{Float(1)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "sign", Args: []Term{Float(math.MaxFloat64)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(0), &Compound{Functor: "sign", Args: []Term{Float(0)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(-1), &Compound{Functor: "sign", Args: []Term{Float(-1)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(-1), &Compound{Functor: "sign", Args: []Term{Float(-math.MaxFloat64)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		var v Variable
		ok, err = DefaultFunctionSet.Is(&v, &Compound{Functor: "sign", Args: []Term{Float(math.NaN())}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.True(t, math.IsNaN(float64(v.Ref.(Float))))
	})

	t.Run("float", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(1.0), &Compound{Functor: "float", Args: []Term{Integer(1)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1.0), &Compound{Functor: "float", Args: []Term{Float(1)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("floor", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(1), &Compound{Functor: "floor", Args: []Term{Integer(1)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "floor", Args: []Term{Float(1.1)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("natural logarithm", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(0), &Compound{Functor: "log", Args: []Term{Integer(1)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(0), &Compound{Functor: "log", Args: []Term{Float(1)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("sine", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(0), &Compound{Functor: "sin", Args: []Term{Integer(0)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(0), &Compound{Functor: "sin", Args: []Term{Float(0)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("truncate", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(1), &Compound{Functor: "truncate", Args: []Term{Integer(1)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "truncate", Args: []Term{Float(1.1)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("round", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(1), &Compound{Functor: "round", Args: []Term{Integer(1)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "round", Args: []Term{Float(1.1)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("bit-shift right", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(2), &Compound{Functor: ">>", Args: []Term{Integer(4), Integer(1)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		_, err = DefaultFunctionSet.Is(&Variable{}, &Compound{Functor: ">>", Args: []Term{Float(4), Integer(1)}}, Done)
		assert.Error(t, err)

		_, err = DefaultFunctionSet.Is(&Variable{}, &Compound{Functor: ">>", Args: []Term{Integer(4), Float(1)}}, Done)
		assert.Error(t, err)

		_, err = DefaultFunctionSet.Is(&Variable{}, &Compound{Functor: ">>", Args: []Term{Float(4), Float(1)}}, Done)
		assert.Error(t, err)
	})

	t.Run("bit-shift left", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(8), &Compound{Functor: "<<", Args: []Term{Integer(4), Integer(1)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		_, err = DefaultFunctionSet.Is(&Variable{}, &Compound{Functor: "<<", Args: []Term{Float(4), Integer(1)}}, Done)
		assert.Error(t, err)

		_, err = DefaultFunctionSet.Is(&Variable{}, &Compound{Functor: "<<", Args: []Term{Integer(4), Float(1)}}, Done)
		assert.Error(t, err)

		_, err = DefaultFunctionSet.Is(&Variable{}, &Compound{Functor: "<<", Args: []Term{Float(4), Float(1)}}, Done)
		assert.Error(t, err)
	})

	t.Run("bitwise and", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(1), &Compound{Functor: "/\\", Args: []Term{Integer(5), Integer(1)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		_, err = DefaultFunctionSet.Is(&Variable{}, &Compound{Functor: "/\\", Args: []Term{Float(5), Integer(1)}}, Done)
		assert.Error(t, err)

		_, err = DefaultFunctionSet.Is(&Variable{}, &Compound{Functor: "/\\", Args: []Term{Integer(5), Float(1)}}, Done)
		assert.Error(t, err)

		_, err = DefaultFunctionSet.Is(&Variable{}, &Compound{Functor: "/\\", Args: []Term{Float(5), Float(1)}}, Done)
		assert.Error(t, err)
	})

	t.Run("bitwise or", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(5), &Compound{Functor: "\\/", Args: []Term{Integer(4), Integer(1)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		_, err = DefaultFunctionSet.Is(&Variable{}, &Compound{Functor: "\\/", Args: []Term{Float(4), Integer(1)}}, Done)
		assert.Error(t, err)

		_, err = DefaultFunctionSet.Is(&Variable{}, &Compound{Functor: "\\/", Args: []Term{Integer(4), Float(1)}}, Done)
		assert.Error(t, err)

		_, err = DefaultFunctionSet.Is(&Variable{}, &Compound{Functor: "\\/", Args: []Term{Float(4), Float(1)}}, Done)
		assert.Error(t, err)
	})

	t.Run("bitwise complement", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(-1), &Compound{Functor: "\\", Args: []Term{Integer(0)}}, Done)
		assert.NoError(t, err)
		assert.True(t, ok)

		_, err = DefaultFunctionSet.Is(&Variable{}, &Compound{Functor: "\\", Args: []Term{Float(0)}}, Done)
		assert.Error(t, err)
	})
}

func TestFunctionSet_Equal(t *testing.T) {
	ok, err := DefaultFunctionSet.Equal(Integer(1), Integer(1), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.Equal(Float(1), Integer(1), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.Equal(Integer(1), Float(1), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.Equal(Float(1), Float(1), Done)
	assert.NoError(t, err)
	assert.True(t, ok)
}

func TestFunctionSet_LessThan(t *testing.T) {
	ok, err := DefaultFunctionSet.LessThan(Integer(1), Integer(2), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.LessThan(Float(1), Integer(2), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.LessThan(Integer(1), Float(2), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.LessThan(Float(1), Float(2), Done)
	assert.NoError(t, err)
	assert.True(t, ok)
}

func TestFunctionSet_GreaterThan(t *testing.T) {
	ok, err := DefaultFunctionSet.GreaterThan(Integer(2), Integer(1), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.GreaterThan(Float(2), Integer(1), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.GreaterThan(Integer(2), Float(1), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.GreaterThan(Float(2), Float(1), Done)
	assert.NoError(t, err)
	assert.True(t, ok)
}

func TestFunctionSet_LessThanOrEqual(t *testing.T) {
	ok, err := DefaultFunctionSet.LessThanOrEqual(Integer(1), Integer(2), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.LessThanOrEqual(Float(1), Integer(2), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.LessThanOrEqual(Integer(1), Float(2), Done)
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.LessThanOrEqual(Float(1), Float(2), Done)
	assert.NoError(t, err)
	assert.True(t, ok)
}
