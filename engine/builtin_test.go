package engine

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"math"
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
)

func TestState_SetUserInput(t *testing.T) {
	t.Run("file", func(t *testing.T) {
		var state State
		state.SetUserInput(os.Stdin)

		s, ok := state.streams[Atom("user_input")]
		assert.True(t, ok)
		assert.Equal(t, os.Stdin, s.file)
	})

	t.Run("ReadCloser", func(t *testing.T) {
		var r struct {
			mockReader
			mockCloser
		}
		r.mockReader.On("Read", mock.Anything).Return(0, nil).Once()
		defer r.mockReader.AssertExpectations(t)
		r.mockCloser.On("Close").Return(nil).Once()
		defer r.mockCloser.AssertExpectations(t)

		var state State
		state.SetUserInput(&r)

		s, ok := state.streams[Atom("user_input")]
		assert.True(t, ok)

		n, err := s.file.Read(nil)
		assert.NoError(t, err)
		assert.Equal(t, 0, n)

		_, err = s.file.Write(nil)
		assert.Equal(t, errNotSupported, err)

		assert.NoError(t, s.file.Close())
	})

	t.Run("Reader", func(t *testing.T) {
		var r mockReader
		r.On("Read", mock.Anything).Return(0, nil).Once()
		defer r.AssertExpectations(t)

		var state State
		state.SetUserInput(&r)

		s, ok := state.streams[Atom("user_input")]
		assert.True(t, ok)

		n, err := s.file.Read(nil)
		assert.NoError(t, err)
		assert.Equal(t, 0, n)

		_, err = s.file.Write(nil)
		assert.Equal(t, errNotSupported, err)

		assert.Equal(t, errNotSupported, s.file.Close())
	})
}

func TestState_SetUserOutput(t *testing.T) {
	t.Run("file", func(t *testing.T) {
		var state State
		state.SetUserOutput(os.Stdout)

		s, ok := state.streams[Atom("user_output")]
		assert.True(t, ok)
		assert.Equal(t, os.Stdout, s.file)
	})

	t.Run("WriteCloser", func(t *testing.T) {
		var w struct {
			mockWriter
			mockCloser
		}
		w.mockWriter.On("Write", mock.Anything).Return(0, nil).Once()
		defer w.mockWriter.AssertExpectations(t)
		w.mockCloser.On("Close").Return(nil).Once()
		defer w.mockCloser.AssertExpectations(t)

		var state State
		state.SetUserOutput(&w)

		s, ok := state.streams[Atom("user_output")]
		assert.True(t, ok)

		_, err := s.file.Read(nil)
		assert.Equal(t, errNotSupported, err)

		n, err := s.file.Write(nil)
		assert.NoError(t, err)
		assert.Equal(t, 0, n)

		assert.NoError(t, s.file.Close())
	})

	t.Run("Writer", func(t *testing.T) {
		var w mockWriter
		w.On("Write", mock.Anything).Return(0, nil).Once()
		defer w.AssertExpectations(t)

		var state State
		state.SetUserOutput(&w)

		s, ok := state.streams[Atom("user_output")]
		assert.True(t, ok)

		_, err := s.file.Read(nil)
		assert.Equal(t, errNotSupported, err)

		n, err := s.file.Write(nil)
		assert.NoError(t, err)
		assert.Equal(t, 0, n)

		assert.Equal(t, errNotSupported, s.file.Close())
	})
}

type mockReader struct {
	mock.Mock
}

func (m *mockReader) Read(p []byte) (int, error) {
	args := m.Called(p)
	return args.Int(0), args.Error(1)
}

type mockWriter struct {
	mock.Mock
}

func (m *mockWriter) Write(p []byte) (int, error) {
	args := m.Called(p)
	return args.Int(0), args.Error(1)
}

type mockCloser struct {
	mock.Mock
}

func (m *mockCloser) Close() error {
	args := m.Called()
	return args.Error(0)
}

func TestState_Call(t *testing.T) {
	var state State

	t.Run("undefined atom", func(t *testing.T) {
		ok, err := state.Call(Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorProcedure(&Compound{
			Functor: "/",
			Args:    []Term{Atom("foo"), Integer(0)},
		}), err)
		assert.False(t, ok)
	})

	state.procedures = map[ProcedureIndicator]procedure{{Name: "foo", Arity: 0}: clauses{}}

	t.Run("defined atom", func(t *testing.T) {
		ok, err := state.Call(Atom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("undefined compound", func(t *testing.T) {
		ok, err := state.Call(&Compound{Functor: "bar", Args: []Term{NewVariable(), NewVariable()}}, Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorProcedure(&Compound{
			Functor: "/",
			Args:    []Term{Atom("bar"), Integer(2)},
		}), err)
		assert.False(t, ok)
	})

	state.procedures = map[ProcedureIndicator]procedure{{Name: "bar", Arity: 2}: clauses{}}

	t.Run("defined compound", func(t *testing.T) {
		ok, err := state.Call(&Compound{Functor: "bar", Args: []Term{NewVariable(), NewVariable()}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("variable", func(t *testing.T) {
		t.Run("single predicate", func(t *testing.T) {
			x := Variable("X")

			ok, err := state.Call(x, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(x), err)
			assert.False(t, ok)
		})

		t.Run("multiple predicates", func(t *testing.T) {
			x := Variable("X")
			state.Register0("fail", func(f func(*Env) *Promise, env *Env) *Promise {
				return Bool(false)
			})
			ok, err := state.Call(&Compound{
				Functor: ",",
				Args: []Term{
					Atom("fail"),
					x,
				},
			}, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("not callable", func(t *testing.T) {
		t.Run("single predicate", func(t *testing.T) {
			ok, err := state.Call(Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorCallable(Integer(0)), err)
			assert.False(t, ok)
		})

		t.Run("multiple predicates", func(t *testing.T) {
			t.Run("conjunction", func(t *testing.T) {
				ok, err := state.Call(&Compound{
					Functor: ",",
					Args: []Term{
						Atom("true"),
						Integer(0),
					},
				}, Success, nil).Force(context.Background())
				assert.Equal(t, typeErrorCallable(&Compound{
					Functor: ",",
					Args: []Term{
						Atom("true"),
						Integer(0),
					},
				}), err)
				assert.False(t, ok)
			})

			t.Run("disjunction", func(t *testing.T) {
				ok, err := state.Call(&Compound{
					Functor: ";",
					Args: []Term{
						Integer(1),
						Atom("true"),
					},
				}, Success, nil).Force(context.Background())
				assert.Equal(t, typeErrorCallable(&Compound{
					Functor: ";",
					Args: []Term{
						Integer(1),
						Atom("true"),
					},
				}), err)
				assert.False(t, ok)
			})
		})
	})
}

func TestUnify(t *testing.T) {
	t.Run("unifiable", func(t *testing.T) {
		x := Variable("X")
		ok, err := Unify(x, &Compound{
			Functor: "f",
			Args:    []Term{Atom("a")},
		}, func(env *Env) *Promise {
			assert.Equal(t, &Compound{
				Functor: "f",
				Args:    []Term{Atom("a")},
			}, env.Resolve(x))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not unifiable", func(t *testing.T) {
		ok, err := Unify(Atom("a"), &Compound{
			Functor: "f",
			Args:    []Term{Atom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("loop", func(t *testing.T) {
		x := Variable("X")
		ok, err := Unify(x, &Compound{
			Functor: "f",
			Args:    []Term{x},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})
}

func TestUnifyWithOccursCheck(t *testing.T) {
	t.Run("unifiable", func(t *testing.T) {
		x := Variable("X")
		ok, err := UnifyWithOccursCheck(x, &Compound{
			Functor: "f",
			Args:    []Term{Atom("a")},
		}, func(env *Env) *Promise {
			assert.Equal(t, &Compound{
				Functor: "f",
				Args:    []Term{Atom("a")},
			}, env.Resolve(x))

			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not unifiable", func(t *testing.T) {
		ok, err := UnifyWithOccursCheck(Atom("a"), &Compound{
			Functor: "f",
			Args:    []Term{Atom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("loop", func(t *testing.T) {
		x := Variable("X")
		ok, err := UnifyWithOccursCheck(x, &Compound{
			Functor: "f",
			Args:    []Term{x},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeVar(t *testing.T) {
	t.Run("var", func(t *testing.T) {
		ok, err := TypeVar(NewVariable(), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not var", func(t *testing.T) {
		ok, err := TypeVar(Atom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeFloat(t *testing.T) {
	t.Run("float", func(t *testing.T) {
		ok, err := TypeFloat(Float(1.0), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not float", func(t *testing.T) {
		ok, err := TypeFloat(Atom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeInteger(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		ok, err := TypeInteger(Integer(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not integer", func(t *testing.T) {
		ok, err := TypeInteger(Atom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeAtom(t *testing.T) {
	t.Run("atom", func(t *testing.T) {
		ok, err := TypeAtom(Atom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not atom", func(t *testing.T) {
		ok, err := TypeAtom(Integer(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeCompound(t *testing.T) {
	t.Run("compound", func(t *testing.T) {
		ok, err := TypeCompound(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not compound", func(t *testing.T) {
		ok, err := TypeCompound(Atom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestFunctor(t *testing.T) {
	t.Run("term is instantiated", func(t *testing.T) {
		t.Run("float", func(t *testing.T) {
			name, arity := Variable("Name"), Variable("Arity")
			ok, err := Functor(Float(2.0), name, arity, func(env *Env) *Promise {
				assert.Equal(t, Float(2.0), env.Resolve(name))
				assert.Equal(t, Integer(0), env.Resolve(arity))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("integer", func(t *testing.T) {
			name, arity := NewVariable(), NewVariable()
			ok, err := Functor(Integer(2), name, arity, func(env *Env) *Promise {
				assert.Equal(t, Integer(2), env.Resolve(name))
				assert.Equal(t, Integer(0), env.Resolve(arity))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("atom", func(t *testing.T) {
			name, arity := NewVariable(), NewVariable()
			ok, err := Functor(Atom("foo"), name, arity, func(env *Env) *Promise {
				assert.Equal(t, Atom("foo"), env.Resolve(name))
				assert.Equal(t, Integer(0), env.Resolve(arity))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("compound", func(t *testing.T) {
			name, arity := NewVariable(), NewVariable()
			ok, err := Functor(&Compound{
				Functor: "f",
				Args:    []Term{Atom("a"), Atom("b"), Atom("c")},
			}, name, arity, func(env *Env) *Promise {
				assert.Equal(t, Atom("f"), env.Resolve(name))
				assert.Equal(t, Integer(3), env.Resolve(arity))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("term is not instantiated", func(t *testing.T) {
		t.Run("atom", func(t *testing.T) {
			v := NewVariable()
			ok, err := Functor(v, Atom("foo"), Integer(0), func(env *Env) *Promise {
				assert.Equal(t, Atom("foo"), env.Resolve(v))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("compound", func(t *testing.T) {
			v := NewVariable()
			ok, err := Functor(v, Atom("f"), Integer(2), func(env *Env) *Promise {
				c, ok := env.Resolve(v).(*Compound)
				assert.True(t, ok)
				assert.Equal(t, Atom("f"), c.Functor)
				assert.Len(t, c.Args, 2)
				assert.True(t, c.Args[0].(Variable).Generated())
				assert.True(t, c.Args[1].(Variable).Generated())
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("name is a variable", func(t *testing.T) {
			name := NewVariable()
			ok, err := Functor(NewVariable(), name, Integer(2), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(name), err)
			assert.False(t, ok)
		})

		t.Run("name is not atomic", func(t *testing.T) {
			ok, err := Functor(NewVariable(), &Compound{
				Functor: "foo",
				Args: []Term{
					Atom("a"),
				},
			}, Integer(1), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorAtomic(&Compound{
				Functor: "foo",
				Args: []Term{
					Atom("a"),
				},
			}), err)
			assert.False(t, ok)
		})

		t.Run("name is not an atom", func(t *testing.T) {
			ok, err := Functor(NewVariable(), Integer(0), Integer(2), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorAtom(Integer(0)), err)
			assert.False(t, ok)
		})

		t.Run("arity is a variable", func(t *testing.T) {
			arity := NewVariable()
			ok, err := Functor(NewVariable(), Atom("f"), arity, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(arity), err)
			assert.False(t, ok)
		})

		t.Run("arity is not an integer", func(t *testing.T) {
			ok, err := Functor(NewVariable(), Atom("f"), Float(2.0), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorInteger(Float(2.0)), err)
			assert.False(t, ok)
		})

		t.Run("arity is negative", func(t *testing.T) {
			ok, err := Functor(NewVariable(), Atom("f"), Integer(-2), Success, nil).Force(context.Background())
			assert.Equal(t, domainErrorNotLessThanZero(Integer(-2)), err)
			assert.False(t, ok)
		})
	})
}

func TestArg(t *testing.T) {
	t.Run("term is a variable", func(t *testing.T) {
		v := NewVariable()
		ok, err := Arg(NewVariable(), v, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(v), err)
		assert.False(t, ok)
	})

	t.Run("term is not a compound", func(t *testing.T) {
		ok, err := Arg(NewVariable(), Atom("foo"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCompound(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("nth is a variable", func(t *testing.T) {
		nth := NewVariable()
		_, err := Arg(nth, &Compound{
			Functor: "f",
			Args:    []Term{Atom("a"), Atom("b"), Atom("a")},
		}, Atom("a"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nth), err)
	})

	t.Run("nth is an integer", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			ok, err := Arg(Integer(2), &Compound{
				Functor: "f",
				Args:    []Term{Atom("a"), Atom("b"), Atom("c")},
			}, Atom("b"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("ng", func(t *testing.T) {
			ok, err := Arg(Integer(4), &Compound{
				Functor: "f",
				Args:    []Term{Atom("a"), Atom("b"), Atom("c")},
			}, Atom("b"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("negative", func(t *testing.T) {
			ok, err := Arg(Integer(-2), &Compound{
				Functor: "f",
				Args:    []Term{Atom("a"), Atom("b"), Atom("c")},
			}, Atom("b"), Success, nil).Force(context.Background())
			assert.Equal(t, domainErrorNotLessThanZero(Integer(-2)), err)
			assert.False(t, ok)
		})
	})

	t.Run("nth is neither a variable nor an integer", func(t *testing.T) {
		ok, err := Arg(Atom("foo"), &Compound{
			Functor: "f",
			Args:    []Term{Atom("a"), Atom("b"), Atom("c")},
		}, Atom("b"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Atom("foo")), err)
		assert.False(t, ok)
	})
}

func TestUniv(t *testing.T) {
	t.Run("term is a variable", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			v := NewVariable()
			ok, err := Univ(v, List(Atom("f"), Atom("a"), Atom("b")), func(env *Env) *Promise {
				assert.Equal(t, &Compound{
					Functor: "f",
					Args:    []Term{Atom("a"), Atom("b")},
				}, env.Resolve(v))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("list is empty", func(t *testing.T) {
			v := NewVariable()
			ok, err := Univ(v, List(), Success, nil).Force(context.Background())
			assert.Equal(t, domainErrorNotEmptyList(Atom("[]")), err)
			assert.False(t, ok)
		})

		t.Run("list is not a list", func(t *testing.T) {
			v := NewVariable()
			ok, err := Univ(v, Atom("list"), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorList(Atom("list")), err)
			assert.False(t, ok)
		})

		t.Run("list's first element is not an atom", func(t *testing.T) {
			v := NewVariable()
			ok, err := Univ(v, List(Integer(0), Atom("a"), Atom("b")), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorAtom(Integer(0)), err)
			assert.False(t, ok)
		})

		t.Run("list is not fully instantiated", func(t *testing.T) {
			v, rest := NewVariable(), Variable("Rest")
			ok, err := Univ(v, ListRest(rest, Atom("f"), Atom("a"), Atom("b")), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(ListRest(rest, Atom("a"), Atom("b"))), err)
			assert.False(t, ok)
		})
	})

	t.Run("term is a compound", func(t *testing.T) {
		ok, err := Univ(&Compound{
			Functor: "f",
			Args:    []Term{Atom("a"), Atom("b")},
		}, List(Atom("f"), Atom("a"), Atom("b")), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("term is neither a variable nor a compound", func(t *testing.T) {
		ok, err := Univ(Atom("foo"), List(Atom("foo")), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})
}

func TestCopyTerm(t *testing.T) {
	in := Variable("In")
	out := Variable("Out")
	env := NewEnv().
		Bind(in, Atom("a"))
	ok, err := CopyTerm(in, out, func(env *Env) *Promise {
		assert.Equal(t, Atom("a"), env.Resolve(out))
		return Bool(true)
	}, env).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)
}

func TestState_Op(t *testing.T) {
	t.Run("insert", func(t *testing.T) {
		state := State{
			operators: operators{
				{
					priority:  900,
					specifier: operatorSpecifierXFX,
					name:      "+++",
				},
				{
					priority:  1100,
					specifier: operatorSpecifierXFX,
					name:      "+",
				},
			},
		}
		ok, err := state.Op(Integer(1000), Atom("xfx"), Atom("++"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, operators{
			{
				priority:  900,
				specifier: operatorSpecifierXFX,
				name:      "+++",
			},
			{
				priority:  1000,
				specifier: operatorSpecifierXFX,
				name:      "++",
			},
			{
				priority:  1100,
				specifier: operatorSpecifierXFX,
				name:      "+",
			},
		}, state.operators)
	})

	t.Run("remove", func(t *testing.T) {
		state := State{
			operators: operators{
				{
					priority:  900,
					specifier: operatorSpecifierXFX,
					name:      "+++",
				},
				{
					priority:  1000,
					specifier: operatorSpecifierXFX,
					name:      "++",
				},
				{
					priority:  1100,
					specifier: operatorSpecifierXFX,
					name:      "+",
				},
			},
		}
		ok, err := state.Op(Integer(0), Atom("xfx"), Atom("++"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, operators{
			{
				priority:  900,
				specifier: operatorSpecifierXFX,
				name:      "+++",
			},
			{
				priority:  1100,
				specifier: operatorSpecifierXFX,
				name:      "+",
			},
		}, state.operators)
	})

	t.Run("priority is not an integer", func(t *testing.T) {
		var state State
		ok, err := state.Op(Atom("foo"), Atom("xfx"), Atom("+"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("priority is negative", func(t *testing.T) {
		var state State
		ok, err := state.Op(Integer(-1), Atom("xfx"), Atom("+"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorOperatorPriority(Integer(-1)), err)
		assert.False(t, ok)
	})

	t.Run("priority is more than 1200", func(t *testing.T) {
		var state State
		ok, err := state.Op(Integer(1201), Atom("xfx"), Atom("+"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorOperatorPriority(Integer(1201)), err)
		assert.False(t, ok)
	})

	t.Run("specifier is not an atom", func(t *testing.T) {
		var state State
		ok, err := state.Op(Integer(1000), Integer(0), Atom("+"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("specifier is not a valid operator specifier", func(t *testing.T) {
		var state State
		ok, err := state.Op(Integer(1000), Atom("foo"), Atom("+"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorOperatorSpecifier(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("operator is not an atom", func(t *testing.T) {
		var state State
		ok, err := state.Op(Integer(1000), Atom("xfx"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(0)), err)
		assert.False(t, ok)
	})
}

func TestState_CurrentOp(t *testing.T) {
	state := State{
		operators: operators{
			{
				priority:  900,
				specifier: operatorSpecifierXFX,
				name:      "+++",
			},
			{
				priority:  1000,
				specifier: operatorSpecifierXFX,
				name:      "++",
			},
			{
				priority:  1100,
				specifier: operatorSpecifierXFX,
				name:      "+",
			},
		},
	}

	t.Run("single solution", func(t *testing.T) {
		ok, err := state.CurrentOp(Integer(1100), Atom("xfx"), Atom("+"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("multiple solutions", func(t *testing.T) {
		var (
			priority, specifier, operator = Variable("Priority"), Variable("Specifier"), Variable("Operator")
			c                             int
		)
		ok, err := state.CurrentOp(priority, specifier, operator, func(env *Env) *Promise {
			switch c {
			case 0:
				assert.Equal(t, Integer(900), env.Resolve(priority))
				assert.Equal(t, Atom("xfx"), env.Resolve(specifier))
				assert.Equal(t, Atom("+++"), env.Resolve(operator))
			case 1:
				assert.Equal(t, Integer(1000), env.Resolve(priority))
				assert.Equal(t, Atom("xfx"), env.Resolve(specifier))
				assert.Equal(t, Atom("++"), env.Resolve(operator))
			case 2:
				assert.Equal(t, Integer(1100), env.Resolve(priority))
				assert.Equal(t, Atom("xfx"), env.Resolve(specifier))
				assert.Equal(t, Atom("+"), env.Resolve(operator))
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("priority is not an operator priority", func(t *testing.T) {
		t.Run("priority is not an integer", func(t *testing.T) {
			ok, err := state.CurrentOp(Atom("foo"), Atom("xfx"), Atom("+"), Success, nil).Force(context.Background())
			assert.Equal(t, domainErrorOperatorPriority(Atom("foo")), err)
			assert.False(t, ok)
		})

		t.Run("priority is negative", func(t *testing.T) {
			ok, err := state.CurrentOp(Integer(-1), Atom("xfx"), Atom("+"), Success, nil).Force(context.Background())
			assert.Equal(t, domainErrorOperatorPriority(Integer(-1)), err)
			assert.False(t, ok)
		})
	})

	t.Run("specifier is not an operator specifier", func(t *testing.T) {
		t.Run("specifier is not an atom", func(t *testing.T) {
			ok, err := state.CurrentOp(Integer(1100), Integer(0), Atom("+"), Success, nil).Force(context.Background())
			assert.Equal(t, domainErrorOperatorSpecifier(Integer(0)), err)
			assert.False(t, ok)
		})

		t.Run("specifier is a non-specifier atom", func(t *testing.T) {
			ok, err := state.CurrentOp(Integer(1100), Atom("foo"), Atom("+"), Success, nil).Force(context.Background())
			assert.Equal(t, domainErrorOperatorSpecifier(Atom("foo")), err)
			assert.False(t, ok)
		})
	})

	t.Run("operator is not an atom", func(t *testing.T) {
		ok, err := state.CurrentOp(Integer(1100), Atom("xfx"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(0)), err)
		assert.False(t, ok)
	})
}

func TestState_BagOf(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 3}: clauses{
						{xrTable: []Term{Atom("a"), Atom("b"), Atom("c")}, bytecode: bytecode{
							{opcode: opConst, operand: 0},
							{opcode: opConst, operand: 1},
							{opcode: opConst, operand: 2},
							{opcode: opExit},
						}},
						{xrTable: []Term{Atom("a"), Atom("b"), Atom("d")}, bytecode: bytecode{
							{opcode: opConst, operand: 0},
							{opcode: opConst, operand: 1},
							{opcode: opConst, operand: 2},
							{opcode: opExit},
						}},
						{xrTable: []Term{Atom("b"), Atom("c"), Atom("e")}, bytecode: bytecode{
							{opcode: opConst, operand: 0},
							{opcode: opConst, operand: 1},
							{opcode: opConst, operand: 2},
							{opcode: opExit},
						}},
						{xrTable: []Term{Atom("b"), Atom("c"), Atom("f")}, bytecode: bytecode{
							{opcode: opConst, operand: 0},
							{opcode: opConst, operand: 1},
							{opcode: opConst, operand: 2},
							{opcode: opExit},
						}},
						{xrTable: []Term{Atom("c"), Atom("c"), Atom("g")}, bytecode: bytecode{
							{opcode: opConst, operand: 0},
							{opcode: opConst, operand: 1},
							{opcode: opConst, operand: 2},
							{opcode: opExit},
						}},
					},
				},
			},
		}

		t.Run("without qualifier", func(t *testing.T) {
			var (
				count       int
				a, b, c, cs = Variable("A"), Variable("B"), Variable("C"), Variable("Cs")
			)
			ok, err := state.BagOf(c, &Compound{
				Functor: "foo",
				Args:    []Term{a, b, c},
			}, cs, func(env *Env) *Promise {
				switch count {
				case 0:
					assert.Equal(t, Atom("a"), env.Resolve(a))
					assert.Equal(t, Atom("b"), env.Resolve(b))
					assert.Equal(t, List(Atom("c"), Atom("d")), env.Resolve(cs))
				case 1:
					assert.Equal(t, Atom("b"), env.Resolve(a))
					assert.Equal(t, Atom("c"), env.Resolve(b))
					assert.Equal(t, List(Atom("e"), Atom("f")), env.Resolve(cs))
				case 2:
					assert.Equal(t, Atom("c"), env.Resolve(a))
					assert.Equal(t, Atom("c"), env.Resolve(b))
					assert.Equal(t, List(Atom("g")), env.Resolve(cs))
				default:
					assert.Fail(t, "unreachable")
				}
				count++
				return Bool(false)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("with qualifier", func(t *testing.T) {
			var (
				count       int
				a, b, c, cs = Variable("A"), Variable("B"), Variable("C"), Variable("Cs")
			)
			ok, err := state.BagOf(c, &Compound{
				Functor: "^",
				Args: []Term{a, &Compound{
					Functor: "foo",
					Args:    []Term{a, b, c},
				}},
			}, cs, func(env *Env) *Promise {
				switch count {
				case 0:
					assert.Equal(t, Atom("b"), env.Resolve(b))
					assert.Equal(t, List(Atom("c"), Atom("d")), env.Resolve(cs))
				case 1:
					assert.Equal(t, Atom("c"), env.Resolve(b))
					assert.Equal(t, List(Atom("e"), Atom("f"), Atom("g")), env.Resolve(cs))
				default:
					assert.Fail(t, "unreachable")
				}
				count++
				return Bool(false)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("with multiple qualifiers", func(t *testing.T) {
			var (
				count       int
				a, b, c, cs = Variable("A"), Variable("B"), Variable("C"), Variable("Cs")
			)
			ok, err := state.BagOf(c, &Compound{
				Functor: "^",
				Args: []Term{
					&Compound{
						Functor: ",",
						Args:    []Term{a, b},
					},
					&Compound{
						Functor: "foo",
						Args:    []Term{a, b, c},
					},
				},
			}, cs, func(env *Env) *Promise {
				switch count {
				case 0:
					assert.Equal(t, List(Atom("c"), Atom("d"), Atom("e"), Atom("f"), Atom("g")), env.Resolve(cs))
				default:
					assert.Fail(t, "unreachable")
				}
				count++
				return Bool(false)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("goal is a variable", func(t *testing.T) {
		goal := Variable("Goal")

		var state State
		ok, err := state.BagOf(NewVariable(), goal, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(&goal), err)
		assert.False(t, ok)
	})

	t.Run("goal is neither a variable nor a callable term", func(t *testing.T) {
		var state State
		ok, err := state.BagOf(NewVariable(), Integer(0), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("disjunction", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 1}: predicate1(func(t Term, k func(*Env) *Promise, env *Env) *Promise {
						return Delay(func(ctx context.Context) *Promise {
							return Unify(t, Atom("a"), k, env)
						}, func(ctx context.Context) *Promise {
							return Unify(t, Atom("b"), k, env)
						})
					}),
				},
			},
		}

		t.Run("variable", func(t *testing.T) {
			x, xs := Variable("X"), Variable("Xs")
			ok, err := state.BagOf(x, &Compound{
				Functor: "foo",
				Args:    []Term{x},
			}, xs, func(env *Env) *Promise {
				assert.Equal(t, List(Atom("a"), Atom("b")), env.Resolve(xs))
				return Bool(false)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("not variable", func(t *testing.T) {
			count := 0
			x, xs := Variable("X"), Variable("Xs")
			ok, err := state.BagOf(Atom("c"), &Compound{
				Functor: "foo",
				Args:    []Term{x},
			}, xs, func(env *Env) *Promise {
				switch count {
				case 0:
					assert.Equal(t, Atom("a"), env.Resolve(x))
					assert.Equal(t, List(Atom("c")), env.Resolve(xs))
				case 1:
					assert.Equal(t, Atom("b"), env.Resolve(x))
					assert.Equal(t, List(Atom("c")), env.Resolve(xs))
				default:
					assert.Fail(t, "unreachable")
				}
				count++
				return Bool(false)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("complex", func(t *testing.T) {
			// bagof(X, X = Y; X = Z; Y = a, Xs).
			count := 0
			x, y, z, xs := Variable("X"), Variable("Y"), Variable("Z"), Variable("Xs")
			var state State
			state.Register2("=", Unify)
			ok, err := state.BagOf(x, &Compound{
				Functor: ";",
				Args: []Term{
					&Compound{Functor: "=", Args: []Term{x, y}},
					&Compound{
						Functor: ";",
						Args: []Term{
							&Compound{Functor: "=", Args: []Term{x, z}},
							&Compound{Functor: "=", Args: []Term{y, Atom("a")}},
						},
					},
				},
			}, xs, func(env *Env) *Promise {
				switch count {
				case 0:
					assert.Equal(t, List(y, z), env.Resolve(xs))
				case 1:
					assert.Equal(t, Atom("a"), env.Resolve(y))
					v := NewVariable()
					_, ok := List(v).Unify(xs, false, env)
					assert.True(t, ok)
				default:
					assert.Fail(t, "unreachable")
				}
				count++
				return Bool(false)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})
}

func TestState_SetOf(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 3}: clauses{
						{xrTable: []Term{Atom("a"), Atom("b"), Atom("c")}, bytecode: bytecode{
							{opcode: opConst, operand: 0},
							{opcode: opConst, operand: 1},
							{opcode: opConst, operand: 2},
							{opcode: opExit},
						}},
						{xrTable: []Term{Atom("a"), Atom("b"), Atom("d")}, bytecode: bytecode{
							{opcode: opConst, operand: 0},
							{opcode: opConst, operand: 1},
							{opcode: opConst, operand: 2},
							{opcode: opExit},
						}},
						{xrTable: []Term{Atom("a"), Atom("b"), Atom("c")}, bytecode: bytecode{
							{opcode: opConst, operand: 0},
							{opcode: opConst, operand: 1},
							{opcode: opConst, operand: 2},
							{opcode: opExit},
						}},
						{xrTable: []Term{Atom("b"), Atom("c"), Atom("e")}, bytecode: bytecode{
							{opcode: opConst, operand: 0},
							{opcode: opConst, operand: 1},
							{opcode: opConst, operand: 2},
							{opcode: opExit},
						}},
						{xrTable: []Term{Atom("b"), Atom("c"), Atom("f")}, bytecode: bytecode{
							{opcode: opConst, operand: 0},
							{opcode: opConst, operand: 1},
							{opcode: opConst, operand: 2},
							{opcode: opExit},
						}},
						{xrTable: []Term{Atom("b"), Atom("c"), Atom("e")}, bytecode: bytecode{
							{opcode: opConst, operand: 0},
							{opcode: opConst, operand: 1},
							{opcode: opConst, operand: 2},
							{opcode: opExit},
						}},
						{xrTable: []Term{Atom("c"), Atom("c"), Atom("g")}, bytecode: bytecode{
							{opcode: opConst, operand: 0},
							{opcode: opConst, operand: 1},
							{opcode: opConst, operand: 2},
							{opcode: opExit},
						}},
						{xrTable: []Term{Atom("c"), Atom("c"), Atom("g")}, bytecode: bytecode{
							{opcode: opConst, operand: 0},
							{opcode: opConst, operand: 1},
							{opcode: opConst, operand: 2},
							{opcode: opExit},
						}},
					},
				},
			},
		}

		t.Run("without qualifier", func(t *testing.T) {
			var (
				count       int
				a, b, c, cs = Variable("A"), Variable("B"), Variable("C"), Variable("Cs")
			)
			ok, err := state.SetOf(c, &Compound{
				Functor: "foo",
				Args:    []Term{a, b, c},
			}, cs, func(env *Env) *Promise {
				switch count {
				case 0:
					assert.Equal(t, Atom("a"), env.Resolve(a))
					assert.Equal(t, Atom("b"), env.Resolve(b))
					assert.Equal(t, List(Atom("c"), Atom("d")), env.Resolve(cs))
				case 1:
					assert.Equal(t, Atom("b"), env.Resolve(a))
					assert.Equal(t, Atom("c"), env.Resolve(b))
					assert.Equal(t, List(Atom("e"), Atom("f")), env.Resolve(cs))
				case 2:
					assert.Equal(t, Atom("c"), env.Resolve(a))
					assert.Equal(t, Atom("c"), env.Resolve(b))
					assert.Equal(t, List(Atom("g")), env.Resolve(cs))
				default:
					assert.Fail(t, "unreachable")
				}
				count++
				return Bool(false)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("with qualifier", func(t *testing.T) {
			var (
				count       int
				a, b, c, cs = Variable("A"), Variable("B"), Variable("C"), Variable("Cs")
			)
			ok, err := state.SetOf(c, &Compound{
				Functor: "^",
				Args: []Term{a, &Compound{
					Functor: "foo",
					Args:    []Term{a, b, c},
				}},
			}, cs, func(env *Env) *Promise {
				switch count {
				case 0:
					assert.Equal(t, Atom("b"), env.Resolve(b))
					assert.Equal(t, List(Atom("c"), Atom("d")), env.Resolve(cs))
				case 1:
					assert.Equal(t, Atom("c"), env.Resolve(b))
					assert.Equal(t, List(Atom("e"), Atom("f"), Atom("g")), env.Resolve(cs))
				default:
					assert.Fail(t, "unreachable")
				}
				count++
				return Bool(false)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("with multiple qualifiers", func(t *testing.T) {
			var (
				count       int
				a, b, c, cs = Variable("A"), Variable("B"), Variable("C"), Variable("Cs")
			)
			ok, err := state.SetOf(c, &Compound{
				Functor: "^",
				Args: []Term{
					&Compound{
						Functor: ",",
						Args:    []Term{a, b},
					},
					&Compound{
						Functor: "foo",
						Args:    []Term{a, b, c},
					},
				},
			}, cs, func(env *Env) *Promise {
				switch count {
				case 0:
					assert.Equal(t, List(Atom("c"), Atom("d"), Atom("e"), Atom("f"), Atom("g")), env.Resolve(cs))
				default:
					assert.Fail(t, "unreachable")
				}
				count++
				return Bool(false)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("goal is a variable", func(t *testing.T) {
		goal := Variable("Goal")

		var state State
		ok, err := state.SetOf(NewVariable(), goal, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(goal), err)
		assert.False(t, ok)
	})

	t.Run("goal is neither a variable nor a callable term", func(t *testing.T) {
		var state State
		ok, err := state.SetOf(NewVariable(), Integer(0), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})
}

func TestState_FindAll(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 3}: clauses{
						{xrTable: []Term{Atom("a"), Atom("b"), Atom("c")}, bytecode: bytecode{
							{opcode: opConst, operand: 0},
							{opcode: opConst, operand: 1},
							{opcode: opConst, operand: 2},
							{opcode: opExit},
						}},
						{xrTable: []Term{Atom("a"), Atom("b"), Atom("d")}, bytecode: bytecode{
							{opcode: opConst, operand: 0},
							{opcode: opConst, operand: 1},
							{opcode: opConst, operand: 2},
							{opcode: opExit},
						}},
						{xrTable: []Term{Atom("b"), Atom("c"), Atom("e")}, bytecode: bytecode{
							{opcode: opConst, operand: 0},
							{opcode: opConst, operand: 1},
							{opcode: opConst, operand: 2},
							{opcode: opExit},
						}},
						{xrTable: []Term{Atom("b"), Atom("c"), Atom("f")}, bytecode: bytecode{
							{opcode: opConst, operand: 0},
							{opcode: opConst, operand: 1},
							{opcode: opConst, operand: 2},
							{opcode: opExit},
						}},
						{xrTable: []Term{Atom("c"), Atom("c"), Atom("g")}, bytecode: bytecode{
							{opcode: opConst, operand: 0},
							{opcode: opConst, operand: 1},
							{opcode: opConst, operand: 2},
							{opcode: opExit},
						}},
					},
				},
			},
		}

		var (
			count       int
			a, b, c, cs = Variable("A"), Variable("B"), Variable("C"), Variable("Cs")
		)
		ok, err := state.FindAll(c, &Compound{
			Functor: "foo",
			Args:    []Term{a, b, c},
		}, cs, func(env *Env) *Promise {
			switch count {
			case 0:
				assert.Equal(t, List(Atom("c"), Atom("d"), Atom("e"), Atom("f"), Atom("g")), env.Resolve(cs))
			default:
				assert.Fail(t, "unreachable")
			}
			count++
			return Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("goal is a variable", func(t *testing.T) {
		goal := Variable("Goal")

		var state State
		ok, err := state.FindAll(NewVariable(), goal, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(&goal), err)
		assert.False(t, ok)
	})

	t.Run("goal is neither a variable nor a callable term", func(t *testing.T) {
		var state State
		ok, err := state.FindAll(NewVariable(), Integer(0), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("goal fails", func(t *testing.T) {
		instances := Variable("instances")

		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "fail", Arity: 0}: predicate0(func(f func(*Env) *Promise, env *Env) *Promise {
						return Bool(false)
					}),
				},
			},
		}
		ok, err := state.FindAll(NewVariable(), Atom("fail"), instances, func(env *Env) *Promise {
			assert.Equal(t, List(), env.Resolve(instances))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})
}

func TestCompare(t *testing.T) {
	t.Run("less than", func(t *testing.T) {
		var x, y mockTerm
		x.On("Compare", &y, (*Env)(nil)).Return(int64(-1))
		defer x.AssertExpectations(t)
		defer y.AssertExpectations(t)

		ok, err := Compare(Atom("<"), &x, &y, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("greater than", func(t *testing.T) {
		var x, y mockTerm
		x.On("Compare", &y, (*Env)(nil)).Return(int64(1))
		defer x.AssertExpectations(t)
		defer y.AssertExpectations(t)

		ok, err := Compare(Atom(">"), &x, &y, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("equals to", func(t *testing.T) {
		var x, y mockTerm
		x.On("Compare", &y, (*Env)(nil)).Return(int64(0))
		defer x.AssertExpectations(t)
		defer y.AssertExpectations(t)

		ok, err := Compare(Atom("="), &x, &y, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("detect order", func(t *testing.T) {
		order := Variable("Order")
		ok, err := Compare(order, Atom("a"), Atom("b"), func(env *Env) *Promise {
			assert.Equal(t, Atom("<"), env.Resolve(order))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("order is neither a variable nor an atom", func(t *testing.T) {
		ok, err := Compare(Integer(0), NewVariable(), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("order is an atom but not <, =, or >", func(t *testing.T) {
		ok, err := Compare(Atom("foo"), NewVariable(), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorOrder(Atom("foo")), err)
		assert.False(t, ok)
	})
}

func TestThrow(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		ok, err := Throw(Atom("a"), Success, nil).Force(context.Background())
		assert.Equal(t, &Exception{Term: Atom("a")}, err)
		assert.False(t, ok)
	})

	t.Run("ball is a variable", func(t *testing.T) {
		ball := Variable("Ball")

		ok, err := Throw(ball, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(&ball), err)
		assert.False(t, ok)
	})
}

func TestState_Catch(t *testing.T) {
	var state State
	state.Register2("=", Unify)
	state.Register1("throw", Throw)
	state.Register0("true", func(k func(*Env) *Promise, env *Env) *Promise {
		return k(env)
	})
	state.Register0("fail", func(_ func(*Env) *Promise, _ *Env) *Promise {
		return Bool(false)
	})

	t.Run("match", func(t *testing.T) {
		v := NewVariable()
		ok, err := state.Catch(&Compound{
			Functor: "throw",
			Args:    []Term{Atom("a")},
		}, v, &Compound{
			Functor: "=",
			Args:    []Term{v, Atom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not match", func(t *testing.T) {
		ok, err := state.Catch(&Compound{
			Functor: "throw",
			Args:    []Term{Atom("a")},
		}, Atom("b"), Atom("fail"), Success, nil).Force(context.Background())
		assert.False(t, ok)
		ex, ok := err.(*Exception)
		assert.True(t, ok)
		assert.Equal(t, Atom("a"), ex.Term)
	})

	t.Run("true", func(t *testing.T) {
		ok, err := state.Catch(Atom("true"), Atom("b"), Atom("fail"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("false", func(t *testing.T) {
		ok, err := state.Catch(Atom("fail"), Atom("b"), Atom("fail"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("non-exception error", func(t *testing.T) {
		ok, err := state.Catch(Atom("true"), NewVariable(), Atom("true"), func(env *Env) *Promise {
			return Error(errors.New("failed"))
		}, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestState_CurrentPredicate(t *testing.T) {
	t.Run("user defined predicate", func(t *testing.T) {
		state := State{VM: VM{procedures: map[ProcedureIndicator]procedure{
			{Name: "foo", Arity: 1}: clauses{},
		}}}
		ok, err := state.CurrentPredicate(&Compound{
			Functor: "/",
			Args: []Term{
				Atom("foo"),
				Integer(1),
			},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("variable", func(t *testing.T) {
		var foo, bar, baz bool

		v := Variable("V")

		state := State{VM: VM{procedures: map[ProcedureIndicator]procedure{
			{Name: "foo", Arity: 1}: clauses{},
			{Name: "bar", Arity: 1}: clauses{},
			{Name: "baz", Arity: 1}: clauses{},
		}}}
		ok, err := state.CurrentPredicate(v, func(env *Env) *Promise {
			c, ok := env.Resolve(v).(*Compound)
			assert.True(t, ok)
			assert.Equal(t, Atom("/"), c.Functor)
			assert.Len(t, c.Args, 2)
			assert.Equal(t, Integer(1), c.Args[1])
			switch c.Args[0] {
			case Atom("foo"):
				foo = true
			case Atom("bar"):
				bar = true
			case Atom("baz"):
				baz = true
			default:
				assert.Fail(t, "unreachable")
			}
			return Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)

		assert.True(t, foo)
		assert.True(t, bar)
		assert.True(t, baz)
	})

	t.Run("builtin predicate", func(t *testing.T) {
		state := State{VM: VM{procedures: map[ProcedureIndicator]procedure{
			{Name: "=", Arity: 2}: predicate2(Unify),
		}}}
		ok, err := state.CurrentPredicate(&Compound{
			Functor: "/",
			Args: []Term{
				Atom("="),
				Integer(2),
			},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("pi is neither a variable nor a predicate indicator", func(t *testing.T) {
		t.Run("atom", func(t *testing.T) {
			var state State
			ok, err := state.CurrentPredicate(Atom("foo"), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorPredicateIndicator(Atom("foo")), err)
			assert.False(t, ok)
		})

		t.Run("compound", func(t *testing.T) {
			t.Run("non slash", func(t *testing.T) {
				var state State
				ok, err := state.CurrentPredicate(&Compound{
					Functor: "f",
					Args:    []Term{Atom("a")},
				}, Success, nil).Force(context.Background())
				assert.Equal(t, typeErrorPredicateIndicator(&Compound{
					Functor: "f",
					Args:    []Term{Atom("a")},
				}), err)
				assert.False(t, ok)
			})

			t.Run("slash but number", func(t *testing.T) {
				var state State
				ok, err := state.CurrentPredicate(&Compound{
					Functor: "/",
					Args:    []Term{Integer(0), Integer(0)},
				}, Success, nil).Force(context.Background())
				assert.Equal(t, typeErrorPredicateIndicator(&Compound{
					Functor: "/",
					Args:    []Term{Integer(0), Integer(0)},
				}), err)
				assert.False(t, ok)
			})

			t.Run("slash but path", func(t *testing.T) {
				var state State
				ok, err := state.CurrentPredicate(&Compound{
					Functor: "/",
					Args:    []Term{Atom("foo"), Atom("bar")},
				}, Success, nil).Force(context.Background())
				assert.Equal(t, typeErrorPredicateIndicator(&Compound{
					Functor: "/",
					Args:    []Term{Atom("foo"), Atom("bar")},
				}), err)
				assert.False(t, ok)
			})
		})
	})
}

func TestState_Assertz(t *testing.T) {
	t.Run("append", func(t *testing.T) {
		var state State

		ok, err := state.Assertz(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.Assertz(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("b")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, clauses{
			{
				pi: ProcedureIndicator{
					Name:  "foo",
					Arity: 1,
				},
				raw: &Compound{
					Functor: "foo",
					Args:    []Term{Atom("a")},
				},
				xrTable: []Term{Atom("a")},
				bytecode: bytecode{
					{opcode: opConst, operand: 0},
					{opcode: opExit},
				},
			},
			{
				pi: ProcedureIndicator{
					Name:  "foo",
					Arity: 1,
				},
				raw: &Compound{
					Functor: "foo",
					Args:    []Term{Atom("b")},
				},
				xrTable: []Term{Atom("b")},
				bytecode: bytecode{
					{opcode: opConst, operand: 0},
					{opcode: opExit},
				},
			},
		}, state.procedures[ProcedureIndicator{
			Name:  "foo",
			Arity: 1,
		}])
	})

	t.Run("directive", func(t *testing.T) {
		var called bool
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "directive", Arity: 0}: predicate0(func(k func(*Env) *Promise, env *Env) *Promise {
						called = true
						return k(env)
					}),
				},
			},
		}

		ok, err := state.Assertz(&Compound{
			Functor: ":-",
			Args:    []Term{Atom("directive")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.True(t, called)
	})

	t.Run("clause is a variable", func(t *testing.T) {
		clause := Variable("Term")

		var state State
		ok, err := state.Assertz(clause, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(&clause), err)
		assert.False(t, ok)
	})

	t.Run("clause is neither a variable, nor callable", func(t *testing.T) {
		var state State
		ok, err := state.Assertz(Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("head is a variable", func(t *testing.T) {
		head := Variable("Head")

		var state State
		ok, err := state.Assertz(&Compound{
			Functor: ":-",
			Args:    []Term{head, Atom("true")},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(&head), err)
		assert.False(t, ok)
	})

	t.Run("head is neither a variable, nor callable", func(t *testing.T) {
		var state State
		ok, err := state.Assertz(&Compound{
			Functor: ":-",
			Args:    []Term{Integer(0), Atom("true")},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("directive is a variable", func(t *testing.T) {
		directive := Variable("Directive")

		var state State
		ok, err := state.Assertz(&Compound{
			Functor: ":-",
			Args:    []Term{directive},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(directive), err)
		assert.False(t, ok)
	})

	t.Run("directive is neither a variable, nor callable", func(t *testing.T) {
		var state State
		ok, err := state.Assertz(&Compound{
			Functor: ":-",
			Args:    []Term{Integer(0)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("body contains a term which is not callable", func(t *testing.T) {
		var state State
		ok, err := state.Assertz(&Compound{
			Functor: ":-",
			Args: []Term{
				Atom("foo"),
				&Compound{
					Functor: ",",
					Args: []Term{
						Atom("true"),
						Integer(0),
					},
				},
			},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(&Compound{
			Functor: ",",
			Args: []Term{
				Atom("true"),
				Integer(0),
			},
		}), err)
		assert.False(t, ok)
	})

	t.Run("static", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 0}: static{},
				},
			},
		}

		ok, err := state.Assertz(Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, permissionErrorModifyStaticProcedure(&Compound{
			Functor: "/",
			Args: []Term{
				Atom("foo"),
				Integer(0),
			},
		}), err)
		assert.False(t, ok)
	})

	t.Run("built-in", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 0}: builtin{},
				},
			},
		}

		ok, err := state.Assertz(Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, permissionErrorModifyStaticProcedure(&Compound{
			Functor: "/",
			Args: []Term{
				Atom("foo"),
				Integer(0),
			},
		}), err)
		assert.False(t, ok)
	})
}

func TestState_Asserta(t *testing.T) {
	t.Run("fact", func(t *testing.T) {
		var state State
		ok, err := state.Asserta(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.Asserta(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("b")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, clauses{
			{
				pi: ProcedureIndicator{Name: "foo", Arity: 1},
				raw: &Compound{
					Functor: "foo",
					Args:    []Term{Atom("b")},
				},
				xrTable: []Term{Atom("b")},
				bytecode: bytecode{
					{opcode: opConst, operand: 0},
					{opcode: opExit},
				},
			},
			{
				pi: ProcedureIndicator{Name: "foo", Arity: 1},
				raw: &Compound{
					Functor: "foo",
					Args:    []Term{Atom("a")},
				},
				xrTable: []Term{Atom("a")},
				bytecode: bytecode{
					{opcode: opConst, operand: 0},
					{opcode: opExit},
				},
			},
		}, state.procedures[ProcedureIndicator{Name: "foo", Arity: 1}])
	})

	t.Run("rule", func(t *testing.T) {
		var state State
		ok, err := state.Asserta(&Compound{
			Functor: ":-",
			Args: []Term{
				Atom("foo"),
				&Compound{
					Functor: "p",
					Args:    []Term{Atom("b")},
				},
			},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.Asserta(&Compound{
			Functor: ":-",
			Args: []Term{
				Atom("foo"),
				&Compound{
					Functor: ",",
					Args: []Term{
						&Compound{
							Functor: "p",
							Args:    []Term{Atom("a")},
						},
						Atom("!"),
					},
				},
			},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, clauses{
			{
				pi: ProcedureIndicator{Name: "foo", Arity: 0},
				raw: &Compound{
					Functor: ":-",
					Args: []Term{
						Atom("foo"),
						&Compound{
							Functor: ",",
							Args: []Term{
								&Compound{
									Functor: "p",
									Args:    []Term{Atom("a")},
								},
								Atom("!"),
							},
						},
					},
				},
				xrTable: []Term{Atom("a")},
				piTable: []ProcedureIndicator{{Name: "p", Arity: 1}},
				bytecode: bytecode{
					{opcode: opEnter},
					{opcode: opConst, operand: 0},
					{opcode: opCall, operand: 0},
					{opcode: opCut},
					{opcode: opExit},
				},
			},
			{
				pi: ProcedureIndicator{Name: "foo", Arity: 0},
				raw: &Compound{
					Functor: ":-",
					Args: []Term{
						Atom("foo"),
						&Compound{
							Functor: "p",
							Args:    []Term{Atom("b")},
						},
					},
				},
				xrTable: []Term{Atom("b")},
				piTable: []ProcedureIndicator{{Name: "p", Arity: 1}},
				bytecode: bytecode{
					{opcode: opEnter},
					{opcode: opConst, operand: 0},
					{opcode: opCall, operand: 0},
					{opcode: opExit},
				},
			},
		}, state.procedures[ProcedureIndicator{Name: "foo", Arity: 0}])
	})

	t.Run("directive", func(t *testing.T) {
		var called bool
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "directive", Arity: 0}: predicate0(func(k func(*Env) *Promise, env *Env) *Promise {
						called = true
						return k(env)
					}),
				},
			},
		}

		ok, err := state.Asserta(&Compound{
			Functor: ":-",
			Args:    []Term{Atom("directive")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.True(t, called)
	})

	t.Run("clause is a variable", func(t *testing.T) {
		clause := Variable("Term")

		var state State
		ok, err := state.Asserta(clause, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(&clause), err)
		assert.False(t, ok)
	})

	t.Run("clause is neither a variable, nor callable", func(t *testing.T) {
		var state State
		ok, err := state.Asserta(Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("head is a variable", func(t *testing.T) {
		head := Variable("Head")

		var state State
		ok, err := state.Asserta(&Compound{
			Functor: ":-",
			Args:    []Term{head, Atom("true")},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(head), err)
		assert.False(t, ok)
	})

	t.Run("head is neither a variable, nor callable", func(t *testing.T) {
		var state State
		ok, err := state.Asserta(&Compound{
			Functor: ":-",
			Args:    []Term{Integer(0), Atom("true")},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("body is not callable", func(t *testing.T) {
		var state State
		ok, err := state.Asserta(&Compound{
			Functor: ":-",
			Args:    []Term{Atom("foo"), Integer(0)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("directive is a variable", func(t *testing.T) {
		directive := Variable("Directive")

		var state State
		ok, err := state.Asserta(&Compound{
			Functor: ":-",
			Args:    []Term{directive},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(directive), err)
		assert.False(t, ok)
	})

	t.Run("directive is neither a variable, nor callable", func(t *testing.T) {
		var state State
		ok, err := state.Asserta(&Compound{
			Functor: ":-",
			Args:    []Term{Integer(0)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("body contains a term which is not callable", func(t *testing.T) {
		var state State
		ok, err := state.Asserta(&Compound{
			Functor: ":-",
			Args: []Term{
				Atom("foo"),
				&Compound{
					Functor: ",",
					Args: []Term{
						Atom("true"),
						Integer(0)},
				},
			},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(&Compound{
			Functor: ",",
			Args: []Term{
				Atom("true"),
				Integer(0)},
		}), err)
		assert.False(t, ok)
	})

	t.Run("static", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 0}: static{},
				},
			},
		}

		ok, err := state.Asserta(Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, permissionErrorModifyStaticProcedure(&Compound{
			Functor: "/",
			Args: []Term{
				Atom("foo"),
				Integer(0),
			},
		}), err)
		assert.False(t, ok)
	})

	t.Run("built-in", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 0}: builtin{},
				},
			},
		}

		ok, err := state.Asserta(Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, permissionErrorModifyStaticProcedure(&Compound{
			Functor: "/",
			Args: []Term{
				Atom("foo"),
				Integer(0),
			},
		}), err)
		assert.False(t, ok)
	})

	t.Run("cut", func(t *testing.T) {
		var state State
		ok, err := state.Asserta(&Compound{
			Functor: ":-",
			Args: []Term{
				Atom("foo"),
				Atom("!"),
			},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})
}

func TestState_AssertStatic(t *testing.T) {
	t.Run("append", func(t *testing.T) {
		var state State

		ok, err := state.AssertStatic(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.AssertStatic(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("b")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, static{clauses{
			{
				pi: ProcedureIndicator{
					Name:  "foo",
					Arity: 1,
				},
				raw: &Compound{
					Functor: "foo",
					Args:    []Term{Atom("a")},
				},
				xrTable: []Term{Atom("a")},
				bytecode: bytecode{
					{opcode: opConst, operand: 0},
					{opcode: opExit},
				},
			},
			{
				pi: ProcedureIndicator{
					Name:  "foo",
					Arity: 1,
				},
				raw: &Compound{
					Functor: "foo",
					Args:    []Term{Atom("b")},
				},
				xrTable: []Term{Atom("b")},
				bytecode: bytecode{
					{opcode: opConst, operand: 0},
					{opcode: opExit},
				},
			},
		}}, state.procedures[ProcedureIndicator{
			Name:  "foo",
			Arity: 1,
		}])
	})

	t.Run("directive", func(t *testing.T) {
		var called bool
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "directive", Arity: 0}: predicate0(func(k func(*Env) *Promise, env *Env) *Promise {
						called = true
						return k(env)
					}),
				},
			},
		}

		ok, err := state.AssertStatic(&Compound{
			Functor: ":-",
			Args:    []Term{Atom("directive")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.True(t, called)
	})

	t.Run("clause is a variable", func(t *testing.T) {
		clause := Variable("Term")

		var state State
		ok, err := state.AssertStatic(clause, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(&clause), err)
		assert.False(t, ok)
	})

	t.Run("clause is neither a variable, nor callable", func(t *testing.T) {
		var state State
		ok, err := state.AssertStatic(Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("head is a variable", func(t *testing.T) {
		head := Variable("Head")

		var state State
		ok, err := state.AssertStatic(&Compound{
			Functor: ":-",
			Args:    []Term{head, Atom("true")},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(&head), err)
		assert.False(t, ok)
	})

	t.Run("head is neither a variable, nor callable", func(t *testing.T) {
		var state State
		ok, err := state.AssertStatic(&Compound{
			Functor: ":-",
			Args:    []Term{Integer(0), Atom("true")},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("directive is a variable", func(t *testing.T) {
		directive := Variable("Directive")

		var state State
		ok, err := state.AssertStatic(&Compound{
			Functor: ":-",
			Args:    []Term{directive},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(directive), err)
		assert.False(t, ok)
	})

	t.Run("directive is neither a variable, nor callable", func(t *testing.T) {
		var state State
		ok, err := state.AssertStatic(&Compound{
			Functor: ":-",
			Args:    []Term{Integer(0)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("body contains a term which is not callable", func(t *testing.T) {
		var state State
		ok, err := state.AssertStatic(&Compound{
			Functor: ":-",
			Args: []Term{
				Atom("foo"),
				&Compound{
					Functor: ",",
					Args: []Term{
						Atom("true"),
						Integer(0),
					},
				},
			},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(&Compound{
			Functor: ",",
			Args: []Term{
				Atom("true"),
				Integer(0),
			},
		}), err)
		assert.False(t, ok)
	})

	t.Run("static", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 0}: static{},
				},
			},
		}

		ok, err := state.AssertStatic(Atom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Len(t, state.procedures[ProcedureIndicator{Name: "foo", Arity: 0}].(static).clauses, 1)
	})

	t.Run("dynamic", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 0}: clauses{},
				},
			},
		}

		ok, err := state.AssertStatic(Atom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Len(t, state.procedures[ProcedureIndicator{Name: "foo", Arity: 0}].(clauses), 1)
	})

	t.Run("builtin", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 0}: builtin{},
				},
			},
		}

		ok, err := state.AssertStatic(Atom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Len(t, state.procedures[ProcedureIndicator{Name: "foo", Arity: 0}].(builtin).clauses, 1)
	})

	t.Run("foreign", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 0}: predicate0(nil),
				},
			},
		}

		ok, err := state.AssertStatic(Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, permissionErrorModifyStaticProcedure(&Compound{
			Functor: "/",
			Args: []Term{
				Atom("foo"),
				Integer(0),
			},
		}), err)
		assert.False(t, ok)
	})
}

func TestState_Retract(t *testing.T) {
	t.Run("retract the first one", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 1}: clauses{
						{raw: &Compound{Functor: "foo", Args: []Term{Atom("a")}}},
						{raw: &Compound{Functor: "foo", Args: []Term{Atom("b")}}},
						{raw: &Compound{Functor: "foo", Args: []Term{Atom("c")}}},
					},
				},
			},
		}

		ok, err := state.Retract(&Compound{
			Functor: "foo",
			Args:    []Term{Variable("X")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, clauses{
			{raw: &Compound{Functor: "foo", Args: []Term{Atom("b")}}},
			{raw: &Compound{Functor: "foo", Args: []Term{Atom("c")}}},
		}, state.procedures[ProcedureIndicator{Name: "foo", Arity: 1}])
	})

	t.Run("retract the specific one", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 1}: clauses{
						{raw: &Compound{Functor: "foo", Args: []Term{Atom("a")}}},
						{raw: &Compound{Functor: "foo", Args: []Term{Atom("b")}}},
						{raw: &Compound{Functor: "foo", Args: []Term{Atom("c")}}},
					},
				},
			},
		}

		ok, err := state.Retract(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("b")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, clauses{
			{raw: &Compound{Functor: "foo", Args: []Term{Atom("a")}}},
			{raw: &Compound{Functor: "foo", Args: []Term{Atom("c")}}},
		}, state.procedures[ProcedureIndicator{Name: "foo", Arity: 1}])
	})

	t.Run("retract all", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 1}: clauses{
						{raw: &Compound{Functor: "foo", Args: []Term{Atom("a")}}},
						{raw: &Compound{Functor: "foo", Args: []Term{Atom("b")}}},
						{raw: &Compound{Functor: "foo", Args: []Term{Atom("c")}}},
					},
				},
			},
		}

		ok, err := state.Retract(&Compound{
			Functor: "foo",
			Args:    []Term{Variable("X")},
		}, Failure, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
		assert.Empty(t, state.procedures[ProcedureIndicator{Name: "foo", Arity: 1}])
	})

	t.Run("variable", func(t *testing.T) {
		x := Variable("X")

		var state State
		ok, err := state.Retract(x, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(x), err)
		assert.False(t, ok)
	})

	t.Run("not callable", func(t *testing.T) {
		var state State
		ok, err := state.Retract(Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("no clause matches", func(t *testing.T) {
		var state State

		ok, err := state.Retract(&Compound{
			Functor: "foo",
			Args:    []Term{Variable("X")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("static", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 0}: static{},
				},
			},
		}

		ok, err := state.Retract(Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, permissionErrorModifyStaticProcedure(&Compound{
			Functor: "/",
			Args:    []Term{Atom("foo"), Integer(0)},
		}), err)
		assert.False(t, ok)
	})

	t.Run("exception in continuation", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 1}: clauses{
						{raw: &Compound{Functor: "foo", Args: []Term{Atom("a")}}},
					},
				},
			},
		}

		ok, err := state.Retract(&Compound{
			Functor: "foo",
			Args:    []Term{Variable("X")},
		}, func(_ *Env) *Promise {
			return Error(errors.New("failed"))
		}, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)

		// removed
		assert.Empty(t, state.procedures[ProcedureIndicator{Name: "foo", Arity: 1}])
	})
}

func TestState_Abolish(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 1}: clauses{
						{raw: &Compound{Functor: "foo", Args: []Term{Atom("a")}}},
						{raw: &Compound{Functor: "foo", Args: []Term{Atom("b")}}},
						{raw: &Compound{Functor: "foo", Args: []Term{Atom("c")}}},
					},
				},
			},
		}

		ok, err := state.Abolish(&Compound{
			Functor: "/",
			Args:    []Term{Atom("foo"), Integer(1)},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		_, ok = state.procedures[ProcedureIndicator{Name: "foo", Arity: 1}]
		assert.False(t, ok)
	})

	t.Run("pi is a variable", func(t *testing.T) {
		pi := Variable("PI")

		var state State
		ok, err := state.Abolish(pi, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(&pi), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and either Name or Arity is a variable", func(t *testing.T) {
		t.Run("Name is a variable", func(t *testing.T) {
			name := Variable("Name")

			var state State
			ok, err := state.Abolish(&Compound{
				Functor: "/",
				Args:    []Term{name, Integer(2)},
			}, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(name), err)
			assert.False(t, ok)
		})

		t.Run("Arity is a variable", func(t *testing.T) {
			arity := Variable("Arity")

			var state State
			ok, err := state.Abolish(&Compound{
				Functor: "/",
				Args:    []Term{Atom("foo"), arity},
			}, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(arity), err)
			assert.False(t, ok)
		})
	})

	t.Run("pi is neither a variable nor a predicate indicator", func(t *testing.T) {
		var state State
		ok, err := state.Abolish(Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorPredicateIndicator(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and Name is neither a variable nor an atom", func(t *testing.T) {
		var state State
		ok, err := state.Abolish(&Compound{
			Functor: "/",
			Args:    []Term{Integer(0), Integer(2)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and Arity is neither a variable nor an integer", func(t *testing.T) {
		var state State
		ok, err := state.Abolish(&Compound{
			Functor: "/",
			Args:    []Term{Atom("foo"), Atom("bar")},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Atom("bar")), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and Arity is an integer less than zero", func(t *testing.T) {
		var state State
		ok, err := state.Abolish(&Compound{
			Functor: "/",
			Args:    []Term{Atom("foo"), Integer(-2)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorNotLessThanZero(Integer(-2)), err)
		assert.False(t, ok)
	})

	t.Run("The predicate indicator pi is that of a static procedure", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 0}: static{},
				},
			},
		}
		ok, err := state.Abolish(&Compound{
			Functor: "/",
			Args:    []Term{Atom("foo"), Integer(0)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, permissionErrorModifyStaticProcedure(&Compound{
			Functor: "/",
			Args:    []Term{Atom("foo"), Integer(0)},
		}), err)
		assert.False(t, ok)
	})
}

func TestState_CurrentInput(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		var s Stream
		state := State{
			input: &s,
		}

		ok, err := state.CurrentInput(&s, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("stream is neither a variable nor a stream", func(t *testing.T) {
		var state State
		ok, err := state.CurrentInput(Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStream(Integer(0)), err)
		assert.False(t, ok)
	})
}

func TestState_CurrentOutput(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		var s Stream
		state := State{
			output: &s,
		}

		ok, err := state.CurrentOutput(&s, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("stream is neither a variable nor a stream", func(t *testing.T) {
		var state State
		ok, err := state.CurrentOutput(Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStream(Integer(0)), err)
		assert.False(t, ok)
	})
}

func TestState_SetInput(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		v := Variable("Stream")
		s := NewStream(os.Stdin, StreamModeRead)
		env := NewEnv().
			Bind(v, s)
		var state State
		ok, err := state.SetInput(v, Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.Equal(t, s, state.input)
	})

	t.Run("alias", func(t *testing.T) {
		v := Variable("Stream")
		s := NewStream(os.Stdin, StreamModeRead)
		env := NewEnv().
			Bind(v, s)
		state := State{
			streams: map[Term]*Stream{
				Atom("x"): s,
			},
		}
		ok, err := state.SetInput(v, Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.Equal(t, s, state.input)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")

		var state State
		ok, err := state.SetInput(streamOrAlias, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable, nor a stream term or alias", func(t *testing.T) {
		var state State
		ok, err := state.SetInput(Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var state State
		ok, err := state.SetInput(Atom("x"), Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorStream(Atom("x")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		v := Variable("Stream")
		env := NewEnv().
			Bind(v, NewStream(os.Stdout, StreamModeWrite))
		var state State
		ok, err := state.SetInput(v, Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputStream(v), err)
		assert.False(t, ok)
	})
}

func TestState_SetOutput(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		v := Variable("Stream")
		s := NewStream(os.Stdout, StreamModeWrite)
		env := NewEnv().
			Bind(v, s)
		var state State
		ok, err := state.SetOutput(v, Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.Equal(t, s, state.output)
	})

	t.Run("alias", func(t *testing.T) {
		s := NewStream(os.Stdout, StreamModeWrite)
		state := State{
			streams: map[Term]*Stream{
				Atom("x"): s,
			},
		}
		ok, err := state.SetOutput(Atom("x"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.Equal(t, s, state.output)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")

		var state State
		ok, err := state.SetOutput(streamOrAlias, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable, nor a stream term or alias", func(t *testing.T) {
		var state State
		ok, err := state.SetOutput(Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var state State
		ok, err := state.SetOutput(Atom("x"), Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorStream(Atom("x")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an input stream", func(t *testing.T) {
		s := Variable("Stream")
		env := NewEnv().
			Bind(s, NewStream(os.Stdin, StreamModeRead))

		var state State
		ok, err := state.SetOutput(s, Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorOutputStream(s), err)
		assert.False(t, ok)
	})
}

func TestState_Open(t *testing.T) {
	var state State

	t.Run("read", func(t *testing.T) {
		f, err := ioutil.TempFile("", "open_test_read")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, os.Remove(f.Name()))
		}()

		_, err = fmt.Fprintf(f, "test\n")
		assert.NoError(t, err)

		assert.NoError(t, f.Close())

		t.Run("alias", func(t *testing.T) {
			v := Variable("Stream")
			ok, err := state.Open(Atom(f.Name()), Atom("read"), v, List(&Compound{
				Functor: "alias",
				Args:    []Term{Atom("input")},
			}), func(env *Env) *Promise {
				ref, ok := env.Lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*Stream)
				assert.True(t, ok)

				assert.Equal(t, state.streams[Atom("input")], s)

				b, err := ioutil.ReadAll(s.buf)
				assert.NoError(t, err)
				assert.Equal(t, "test\n", string(b))

				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("type text", func(t *testing.T) {
			v := Variable("Stream")
			ok, err := state.Open(Atom(f.Name()), Atom("read"), v, List(&Compound{
				Functor: "type",
				Args:    []Term{Atom("text")},
			}), func(env *Env) *Promise {
				ref, ok := env.Lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*Stream)
				assert.True(t, ok)
				assert.Equal(t, StreamTypeText, s.streamType)
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("type binary", func(t *testing.T) {
			v := Variable("Stream")
			ok, err := state.Open(Atom(f.Name()), Atom("read"), v, List(&Compound{
				Functor: "type",
				Args:    []Term{Atom("binary")},
			}), func(env *Env) *Promise {
				ref, ok := env.Lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*Stream)
				assert.True(t, ok)
				assert.Equal(t, StreamTypeBinary, s.streamType)
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("reposition true", func(t *testing.T) {
			v := Variable("Stream")
			ok, err := state.Open(Atom(f.Name()), Atom("read"), v, List(&Compound{
				Functor: "reposition",
				Args:    []Term{Atom("true")},
			}), func(env *Env) *Promise {
				ref, ok := env.Lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*Stream)
				assert.True(t, ok)
				assert.True(t, s.reposition)
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("reposition true", func(t *testing.T) {
			v := Variable("Stream")
			ok, err := state.Open(Atom(f.Name()), Atom("read"), v, List(&Compound{
				Functor: "reposition",
				Args:    []Term{Atom("false")},
			}), func(env *Env) *Promise {
				ref, ok := env.Lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*Stream)
				assert.True(t, ok)
				assert.False(t, s.reposition)
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("eof_action error", func(t *testing.T) {
			v := Variable("Stream")
			ok, err := state.Open(Atom(f.Name()), Atom("read"), v, List(&Compound{
				Functor: "eof_action",
				Args:    []Term{Atom("error")},
			}), func(env *Env) *Promise {
				ref, ok := env.Lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*Stream)
				assert.True(t, ok)
				assert.Equal(t, EOFActionError, s.eofAction)
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("eof_action eof_code", func(t *testing.T) {
			v := Variable("Stream")
			ok, err := state.Open(Atom(f.Name()), Atom("read"), v, List(&Compound{
				Functor: "eof_action",
				Args:    []Term{Atom("eof_code")},
			}), func(env *Env) *Promise {
				ref, ok := env.Lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*Stream)
				assert.True(t, ok)
				assert.Equal(t, EOFActionEOFCode, s.eofAction)
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("eof_action reset", func(t *testing.T) {
			v := Variable("Stream")
			ok, err := state.Open(Atom(f.Name()), Atom("read"), v, List(&Compound{
				Functor: "eof_action",
				Args:    []Term{Atom("reset")},
			}), func(env *Env) *Promise {
				ref, ok := env.Lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*Stream)
				assert.True(t, ok)
				assert.Equal(t, EOFActionReset, s.eofAction)
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("unknown option", func(t *testing.T) {
			v := Variable("Stream")
			ok, err := state.Open(Atom(f.Name()), Atom("read"), v, List(&Compound{
				Functor: "unknown",
				Args:    []Term{Atom("option")},
			}), func(env *Env) *Promise {
				assert.Fail(t, "unreachable")
				return Bool(true)
			}, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})

		t.Run("wrong arity", func(t *testing.T) {
			v := Variable("Stream")
			ok, err := state.Open(Atom(f.Name()), Atom("read"), v, List(&Compound{
				Functor: "type",
				Args:    []Term{Atom("a"), Atom("b")},
			}), func(env *Env) *Promise {
				assert.Fail(t, "unreachable")
				return Bool(true)
			}, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})

		t.Run("variable arg", func(t *testing.T) {
			v := Variable("Stream")
			ok, err := state.Open(Atom(f.Name()), Atom("read"), v, List(&Compound{
				Functor: "type",
				Args:    []Term{NewVariable()},
			}), func(env *Env) *Promise {
				assert.Fail(t, "unreachable")
				return Bool(true)
			}, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})

		t.Run("non-atom arg", func(t *testing.T) {
			v := Variable("Stream")
			ok, err := state.Open(Atom(f.Name()), Atom("read"), v, List(&Compound{
				Functor: "type",
				Args:    []Term{Integer(0)},
			}), func(env *Env) *Promise {
				assert.Fail(t, "unreachable")
				return Bool(true)
			}, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("write", func(t *testing.T) {
		n := filepath.Join(os.TempDir(), "open_test_write")
		defer func() {
			assert.NoError(t, os.Remove(n))
		}()

		v := Variable("Stream")

		ok, err := state.Open(Atom(n), Atom("write"), v, List(&Compound{
			Functor: "alias",
			Args:    []Term{Atom("output")},
		}), func(env *Env) *Promise {
			ref, ok := env.Lookup(v)
			assert.True(t, ok)
			s, ok := ref.(*Stream)
			assert.True(t, ok)

			assert.Equal(t, state.streams[Atom("output")], s)

			_, err := fmt.Fprintf(s.file, "test\n")
			assert.NoError(t, err)

			f, err := os.Open(n)
			assert.NoError(t, err)
			defer func() {
				assert.NoError(t, f.Close())
			}()

			b, err := ioutil.ReadAll(f)
			assert.NoError(t, err)
			assert.Equal(t, "test\n", string(b))

			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
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

		v := Variable("Stream")

		ok, err := state.Open(Atom(f.Name()), Atom("append"), v, List(&Compound{
			Functor: "alias",
			Args:    []Term{Atom("append")},
		}), func(env *Env) *Promise {
			ref, ok := env.Lookup(v)
			assert.True(t, ok)
			s, ok := ref.(*Stream)
			assert.True(t, ok)

			assert.Equal(t, state.streams[Atom("append")], s)

			_, err = fmt.Fprintf(s.file, "test\n")
			assert.NoError(t, err)

			f, err = os.Open(f.Name())
			assert.NoError(t, err)
			defer func() {
				assert.NoError(t, f.Close())
			}()

			b, err := ioutil.ReadAll(f)
			assert.NoError(t, err)
			assert.Equal(t, "test\ntest\n", string(b))

			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("sourceSink is a variable", func(t *testing.T) {
		sourceSink := Variable("Source_Sink")

		var state State
		ok, err := state.Open(sourceSink, Atom("read"), Variable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(sourceSink), err)
		assert.False(t, ok)
	})

	t.Run("mode is a variable", func(t *testing.T) {
		mode := Variable("Mode")

		var state State
		ok, err := state.Open(Atom("/dev/null"), mode, Variable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(mode), err)
		assert.False(t, ok)
	})

	t.Run("options is a partial list or a list with an element E which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			options := ListRest(Variable("Rest"),
				&Compound{Functor: "type", Args: []Term{Atom("text")}},
				&Compound{Functor: "alias", Args: []Term{Atom("foo")}},
			)

			var state State
			ok, err := state.Open(Atom("/dev/null"), Atom("read"), Variable("Stream"), options, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(options), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			option := Variable("Option")

			var state State
			ok, err := state.Open(Atom("/dev/null"), Atom("read"), Variable("Stream"), List(
				option,
				&Compound{Functor: "type", Args: []Term{Atom("text")}},
				&Compound{Functor: "alias", Args: []Term{Atom("foo")}},
			), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(option), err)
			assert.False(t, ok)
		})
	})

	t.Run("mode is neither a variable nor an atom", func(t *testing.T) {
		var state State
		ok, err := state.Open(Atom("/dev/null"), Integer(0), Variable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		var state State
		ok, err := state.Open(Atom("/dev/null"), Atom("read"), Variable("Stream"), Atom("list"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorList(Atom("list")), err)
		assert.False(t, ok)
	})

	t.Run("stream is not a variable", func(t *testing.T) {
		var state State
		ok, err := state.Open(Atom("/dev/null"), Atom("read"), Atom("stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorVariable(Atom("stream")), err)
		assert.False(t, ok)
	})

	t.Run("sourceSink is neither a variable nor a source/sink", func(t *testing.T) {
		var state State
		ok, err := state.Open(Integer(0), Atom("read"), Variable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorSourceSink(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("mode is an atom but not an input/output mode", func(t *testing.T) {
		var state State
		ok, err := state.Open(Atom("/dev/null"), Atom("foo"), Variable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorIOMode(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the options list is neither a variable nor a stream-option", func(t *testing.T) {
		var state State
		ok, err := state.Open(Atom("/dev/null"), Atom("read"), Variable("Stream"), List(Atom("foo")), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOption(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("the source/sink specified by sourceSink does not exist", func(t *testing.T) {
		f, err := ioutil.TempFile("", "open_test_existence")
		assert.NoError(t, err)
		assert.NoError(t, os.Remove(f.Name()))

		var state State
		ok, err := state.Open(Atom(f.Name()), Atom("read"), Variable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorSourceSink(Atom(f.Name())), err)
		assert.False(t, ok)
	})

	t.Run("the source/sink specified by sourceSink cannot be opened", func(t *testing.T) {
		f, err := ioutil.TempFile("", "open_test_permission")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, os.Remove(f.Name()))
		}()

		assert.NoError(t, f.Chmod(0200))

		var state State
		ok, err := state.Open(Atom(f.Name()), Atom("read"), Variable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError("open", "source_sink", Atom(f.Name()), "'%s' cannot be opened.", f.Name()), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the options list is alias and A is already associated with an open stream", func(t *testing.T) {
		f, err := ioutil.TempFile("", "open_test_dup_alias")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, os.Remove(f.Name()))
		}()

		state := State{
			streams: map[Term]*Stream{
				Atom("foo"): nil,
			},
		}
		ok, err := state.Open(Atom(f.Name()), Atom("read"), Variable("Stream"), List(&Compound{
			Functor: "alias",
			Args:    []Term{Atom("foo")},
		}), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError("open", "source_sink", &Compound{
			Functor: "alias",
			Args:    []Term{Atom("foo")},
		}, "foo is already defined as an alias."), err)
		assert.False(t, ok)
	})
}

func TestState_Close(t *testing.T) {
	f, err := ioutil.TempFile("", "")
	assert.NoError(t, err)
	defer func() {
		_ = os.Remove(f.Name())
	}()

	t.Run("without options", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			var state State
			ok, err := state.Close(NewStream(f, StreamModeRead), List(), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("ng", func(t *testing.T) {
			closeFile = func(f io.Closer) error {
				return errors.New("ng")
			}
			defer func() {
				closeFile = io.Closer.Close
			}()

			s, err := Open(Atom(f.Name()), StreamModeRead)
			assert.NoError(t, err)
			defer func() {
				assert.NoError(t, s.file.Close())
			}()

			var state State
			_, err = state.Close(s, List(), Success, nil).Force(context.Background())
			assert.Error(t, err)
		})
	})

	t.Run("force false", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			s, err := Open(Atom(f.Name()), StreamModeRead)
			assert.NoError(t, err)

			var state State
			ok, err := state.Close(s, List(&Compound{
				Functor: "force",
				Args:    []Term{Atom("false")},
			}), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("ng", func(t *testing.T) {
			closeFile = func(f io.Closer) error {
				return errors.New("ng")
			}
			defer func() {
				closeFile = io.Closer.Close
			}()

			s, err := Open(Atom(f.Name()), StreamModeRead)
			assert.NoError(t, err)
			defer func() {
				assert.NoError(t, s.file.Close())
			}()

			var state State
			ok, err := state.Close(s, List(&Compound{
				Functor: "force",
				Args:    []Term{Atom("false")},
			}), Success, nil).Force(context.Background())
			assert.Equal(t, resourceError(s, Atom("ng")), err)
			assert.False(t, ok)
		})
	})

	t.Run("force true", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			s, err := Open(Atom(f.Name()), StreamModeRead)
			assert.NoError(t, err)

			var state State
			ok, err := state.Close(s, List(&Compound{
				Functor: "force",
				Args:    []Term{Atom("true")},
			}), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("ng", func(t *testing.T) {
			closeFile = func(f io.Closer) error {
				return errors.New("ng")
			}
			defer func() {
				closeFile = io.Closer.Close
			}()

			s, err := Open(Atom(f.Name()), StreamModeRead)
			assert.NoError(t, err)
			defer func() {
				assert.NoError(t, s.file.Close())
			}()

			var state State
			ok, err := state.Close(s, List(&Compound{
				Functor: "force",
				Args:    []Term{Atom("true")},
			}), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("valid stream alias", func(t *testing.T) {
		s, err := Open(Atom(f.Name()), StreamModeRead)
		assert.NoError(t, err)

		state := State{
			streams: map[Term]*Stream{
				Atom("foo"): s,
			},
		}
		ok, err := state.Close(Atom("foo"), List(), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias ia a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")

		var state State
		ok, err := state.Close(streamOrAlias, List(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("options is a partial list or a list with an element E which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			options := ListRest(Variable("Rest"),
				&Compound{Functor: "force", Args: []Term{Atom("true")}},
			)

			var state State
			ok, err := state.Close(&Stream{}, options, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(options), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			option := Variable("Option")

			var state State
			ok, err := state.Close(&Stream{}, List(option, &Compound{Functor: "force", Args: []Term{Atom("true")}}), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(option), err)
			assert.False(t, ok)
		})
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		var state State
		ok, err := state.Close(&Stream{}, Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorList(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream-term or alias", func(t *testing.T) {
		var state State
		ok, err := state.Close(Integer(0), List(), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the Options list is neither a variable nor a stream-option", func(t *testing.T) {
		var state State
		ok, err := state.Close(&Stream{}, List(Atom("foo")), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOption(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var state State
		ok, err := state.Close(Atom("foo"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorStream(Atom("foo")), err)
		assert.False(t, ok)
	})
}

func TestState_FlushOutput(t *testing.T) {
	f, err := ioutil.TempFile("", "")
	assert.NoError(t, err)
	defer func() {
		assert.NoError(t, os.Remove(f.Name()))
	}()

	s, err := Open(Atom(f.Name()), StreamModeWrite)
	assert.NoError(t, err)
	defer func() {
		assert.NoError(t, s.Close())
	}()

	t.Run("ok", func(t *testing.T) {
		var state State
		ok, err := state.FlushOutput(s, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("ng", func(t *testing.T) {
		sync = func(f *os.File) error {
			return errors.New("ng")
		}
		defer func() {
			sync = (*os.File).Sync
		}()

		var state State
		_, err := state.FlushOutput(s, Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		state := State{
			streams: map[Term]*Stream{
				Atom("foo"): s,
			},
		}
		ok, err := state.FlushOutput(Atom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")

		var state State
		ok, err := state.FlushOutput(streamOrAlias, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream-term or alias", func(t *testing.T) {
		var state State
		ok, err := state.FlushOutput(Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var state State
		ok, err := state.FlushOutput(Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorStream(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("SorA is an input stream", func(t *testing.T) {
		s := NewStream(os.Stdin, StreamModeRead)

		var state State
		ok, err := state.FlushOutput(s, Success, nil).Force(context.Background())
		assert.Equal(t, permissionErrorOutputStream(s), err)
		assert.False(t, ok)
	})
}

func TestState_WriteTerm(t *testing.T) {
	s := NewStream(os.Stdout, StreamModeWrite)

	ops := operators{
		{priority: 500, specifier: operatorSpecifierYFX, name: "+"},
		{priority: 200, specifier: operatorSpecifierFY, name: "-"},
	}

	state := State{
		operators: ops,
		streams: map[Term]*Stream{
			Atom("foo"): s,
		},
	}

	t.Run("without options", func(t *testing.T) {
		var m mockTerm
		m.On("Unparse", mock.Anything, (*Env)(nil), mock.Anything).Once()
		defer m.AssertExpectations(t)

		ok, err := state.WriteTerm(s, &m, List(), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, state.operators, m.ops)
		assert.Equal(t, 1200, m.priority)
	})

	t.Run("quoted", func(t *testing.T) {
		t.Run("false", func(t *testing.T) {
			var m mockTerm
			m.On("Unparse", mock.Anything, (*Env)(nil), mock.Anything).Once()
			defer m.AssertExpectations(t)

			ok, err := state.WriteTerm(s, &m, List(&Compound{
				Functor: "quoted",
				Args:    []Term{Atom("false")},
			}), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)

			assert.False(t, m.quoted)
			assert.Equal(t, ops, m.ops)
			assert.Equal(t, 1200, m.priority)
		})

		t.Run("true", func(t *testing.T) {
			var m mockTerm
			m.On("Unparse", mock.Anything, (*Env)(nil), mock.Anything).Once()
			defer m.AssertExpectations(t)

			ok, err := state.WriteTerm(s, &m, List(&Compound{
				Functor: "quoted",
				Args:    []Term{Atom("true")},
			}), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)

			assert.True(t, m.quoted)
			assert.Equal(t, ops, m.ops)
			assert.Equal(t, 1200, m.priority)
		})
	})

	t.Run("ignore_ops", func(t *testing.T) {
		t.Run("false", func(t *testing.T) {
			var m mockTerm
			m.On("Unparse", mock.Anything, (*Env)(nil), mock.Anything).Once()
			defer m.AssertExpectations(t)

			ok, err := state.WriteTerm(s, &m, List(&Compound{
				Functor: "ignore_ops",
				Args:    []Term{Atom("false")},
			}), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)

			assert.Equal(t, ops, m.ops)
			assert.Equal(t, 1200, m.priority)
		})

		t.Run("true", func(t *testing.T) {
			var m mockTerm
			m.On("Unparse", mock.Anything, (*Env)(nil), mock.Anything).Once()
			defer m.AssertExpectations(t)

			ok, err := state.WriteTerm(s, &m, List(&Compound{
				Functor: "ignore_ops",
				Args:    []Term{Atom("true")},
			}), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)

			assert.Nil(t, m.ops)
			assert.Equal(t, 1200, m.priority)

		})
	})

	t.Run("numbervars", func(t *testing.T) {
		t.Run("false", func(t *testing.T) {
			var m mockTerm
			m.On("Unparse", mock.Anything, (*Env)(nil), mock.Anything).Once()
			defer m.AssertExpectations(t)

			ok, err := state.WriteTerm(s, &m, List(&Compound{
				Functor: "numbervars",
				Args:    []Term{Atom("false")},
			}), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)

			assert.Equal(t, ops, m.ops)
			assert.False(t, m.numberVars)
			assert.Equal(t, 1200, m.priority)
		})

		t.Run("true", func(t *testing.T) {
			var m mockTerm
			m.On("Unparse", mock.Anything, (*Env)(nil), mock.Anything).Once()
			defer m.AssertExpectations(t)

			ok, err := state.WriteTerm(s, &m, List(&Compound{
				Functor: "numbervars",
				Args:    []Term{Atom("true")},
			}), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)

			assert.Equal(t, ops, m.ops)
			assert.True(t, m.numberVars)
			assert.Equal(t, 1200, m.priority)
		})
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")

		var state State
		ok, err := state.WriteTerm(streamOrAlias, Atom("foo"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("options is a partial list or a list with an element which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			options := ListRest(Variable("Rest"),
				&Compound{Functor: "quoted", Args: []Term{Atom("true")}},
			)

			var state State
			ok, err := state.WriteTerm(s, Atom("foo"), options, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(options), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			option := Variable("Option")

			var state State
			ok, err := state.WriteTerm(s, Atom("foo"), List(option, &Compound{Functor: "quoted", Args: []Term{Atom("true")}}), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(option), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var state State
		ok, err := state.WriteTerm(Integer(0), Atom("foo"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		var state State
		ok, err := state.WriteTerm(s, Atom("foo"), Atom("options"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorList(Atom("options")), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the Options list is neither a variable nor a valid write-option", func(t *testing.T) {
		var state State
		ok, err := state.WriteTerm(s, Atom("foo"), List(&Compound{
			Functor: "unknown",
			Args:    []Term{Atom("option")},
		}), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorWriteOption(&Compound{
			Functor: "unknown",
			Args:    []Term{Atom("option")},
		}), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var state State
		ok, err := state.WriteTerm(Atom("stream"), Atom("foo"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorStream(Atom("stream")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an input stream", func(t *testing.T) {
		s := NewStream(os.Stdin, StreamModeRead)

		var state State
		ok, err := state.WriteTerm(s, Atom("foo"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, permissionErrorOutputStream(s), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		s := NewStream(os.Stdout, StreamModeWrite)
		s.streamType = StreamTypeBinary

		var state State
		ok, err := state.WriteTerm(s, Atom("foo"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, permissionErrorOutputBinaryStream(s), err)
		assert.False(t, ok)
	})
}

type mockTerm struct {
	mock.Mock
	writeTermOptions
}

func (m *mockTerm) String() string {
	args := m.Called()
	return args.String(0)
}

func (m *mockTerm) Unify(t Term, occursCheck bool, env *Env) (*Env, bool) {
	args := m.Called(t, occursCheck, env)
	return args.Get(0).(*Env), args.Bool(1)
}

func (m *mockTerm) Unparse(emit func(Token), env *Env, opts ...WriteOption) {
	_ = m.Called(emit, env, opts)
	for _, o := range opts {
		o(&m.writeTermOptions)
	}
}

func (m *mockTerm) Compare(t Term, env *Env) int64 {
	args := m.Called(t, env)
	return args.Get(0).(int64)
}

func TestCharCode(t *testing.T) {
	t.Run("ascii", func(t *testing.T) {
		ok, err := CharCode(Atom("a"), Integer(97), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("emoji", func(t *testing.T) {
		ok, err := CharCode(Atom("😀"), Integer(128512), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("query char", func(t *testing.T) {
		v := Variable("Char")

		ok, err := CharCode(v, Integer(128512), func(env *Env) *Promise {
			assert.Equal(t, Atom("😀"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("query code", func(t *testing.T) {
		v := Variable("Code")
		ok, err := CharCode(Atom("😀"), v, func(env *Env) *Promise {
			assert.Equal(t, Integer(128512), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("char and code are variables", func(t *testing.T) {
		char, code := Variable("Char"), Variable("Code")

		ok, err := CharCode(char, code, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(&Compound{
			Functor: ",",
			Args:    []Term{char, code},
		}), err)
		assert.False(t, ok)
	})

	t.Run("char is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("atom", func(t *testing.T) {
			ok, err := CharCode(Atom("foo"), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorCharacter(Atom("foo")), err)
			assert.False(t, ok)
		})

		t.Run("non-atom", func(t *testing.T) {
			ok, err := CharCode(Integer(0), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorCharacter(Integer(0)), err)
			assert.False(t, ok)
		})
	})

	t.Run("code is neither a variable nor an integer", func(t *testing.T) {
		t.Run("char is variable", func(t *testing.T) {
			ok, err := CharCode(NewVariable(), Atom("foo"), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorInteger(Atom("foo")), err)
			assert.False(t, ok)
		})

		t.Run("char is a character", func(t *testing.T) {
			ok, err := CharCode(Atom("a"), Atom("x"), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorInteger(Atom("x")), err)
			assert.False(t, ok)
		})
	})

	t.Run("code is neither a variable nor a character-code", func(t *testing.T) {
		ok, err := CharCode(NewVariable(), Integer(-1), Success, nil).Force(context.Background())
		assert.Equal(t, representationError(Atom("character_code"), Atom("-1 is not a valid unicode code point.")), err)
		assert.False(t, ok)
	})
}

func TestState_PutByte(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		write = func(f io.Writer, b []byte) (int, error) {
			assert.Equal(t, []byte{97}, b)
			return 1, nil
		}
		defer func() {
			write = io.Writer.Write
		}()

		s := NewStream(os.Stdout, StreamModeWrite)
		s.streamType = StreamTypeBinary

		var state State
		ok, err := state.PutByte(s, Integer(97), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("ng", func(t *testing.T) {
		write = func(f io.Writer, b []byte) (int, error) {
			assert.Equal(t, []byte{97}, b)
			return 0, errors.New("")
		}
		defer func() {
			write = io.Writer.Write
		}()

		s := NewStream(os.Stdout, StreamModeWrite)
		s.streamType = StreamTypeBinary

		var state State
		_, err := state.PutByte(s, Integer(97), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		write = func(f io.Writer, b []byte) (int, error) {
			assert.Equal(t, []byte{97}, b)
			return 1, nil
		}
		defer func() {
			write = io.Writer.Write
		}()

		s := NewStream(os.Stdout, StreamModeWrite)
		s.streamType = StreamTypeBinary

		state := State{
			streams: map[Term]*Stream{
				Atom("foo"): s,
			},
		}
		ok, err := state.PutByte(Atom("foo"), Integer(97), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")

		var state State
		ok, err := state.PutByte(streamOrAlias, Integer(97), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("byt is a variable", func(t *testing.T) {
		s := NewStream(os.Stdout, StreamModeWrite)
		s.streamType = StreamTypeBinary

		byt := Variable("Byte")

		var state State
		ok, err := state.PutByte(s, byt, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(byt), err)
		assert.False(t, ok)
	})

	t.Run("byt is neither a variable nor an byte", func(t *testing.T) {
		s := NewStream(os.Stdout, StreamModeWrite)
		s.streamType = StreamTypeBinary

		var state State
		ok, err := state.PutByte(s, Atom("byte"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorByte(Atom("byte")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var state State
		ok, err := state.PutByte(Integer(0), Integer(97), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an input stream", func(t *testing.T) {
		s := Variable("Stream")
		env := NewEnv().
			Bind(s, NewStream(os.Stdin, StreamModeRead))

		var state State
		ok, err := state.PutByte(s, Integer(97), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorOutputStream(s), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a text stream", func(t *testing.T) {
		s := Variable("Stream")
		env := NewEnv().
			Bind(s, NewStream(os.Stdout, StreamModeWrite))

		var state State
		ok, err := state.PutByte(s, Integer(97), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorOutputTextStream(s), err)
		assert.False(t, ok)
	})
}

func TestState_PutCode(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		write = func(f io.Writer, b []byte) (int, error) {
			assert.Equal(t, []byte{0xf0, 0x9f, 0x98, 0x80}, b)
			return 1, nil
		}
		defer func() {
			write = io.Writer.Write
		}()

		s := NewStream(os.Stdout, StreamModeWrite)

		var state State
		ok, err := state.PutCode(s, Integer('😀'), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("ng", func(t *testing.T) {
		write = func(f io.Writer, b []byte) (int, error) {
			assert.Equal(t, []byte{0xf0, 0x9f, 0x98, 0x80}, b)
			return 0, errors.New("")
		}
		defer func() {
			write = io.Writer.Write
		}()

		s := NewStream(os.Stdout, StreamModeWrite)

		var state State
		_, err := state.PutCode(s, Integer('😀'), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		write = func(f io.Writer, b []byte) (int, error) {
			assert.Equal(t, []byte{0xf0, 0x9f, 0x98, 0x80}, b)
			return 1, nil
		}
		defer func() {
			write = io.Writer.Write
		}()

		s := NewStream(os.Stdout, StreamModeWrite)

		state := State{
			streams: map[Term]*Stream{
				Atom("foo"): s,
			},
		}
		ok, err := state.PutCode(Atom("foo"), Integer('😀'), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")

		var state State
		ok, err := state.PutCode(streamOrAlias, Integer(97), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("code is a variable", func(t *testing.T) {
		code := Variable("Code")

		var state State
		ok, err := state.PutCode(NewStream(os.Stdout, StreamModeWrite), code, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(code), err)
		assert.False(t, ok)
	})

	t.Run("code is neither a variable nor an integer", func(t *testing.T) {
		var state State
		ok, err := state.PutCode(NewStream(os.Stdout, StreamModeWrite), Atom("code"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Atom("code")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var state State
		ok, err := state.PutCode(Integer(0), Integer(97), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var state State
		ok, err := state.PutCode(Atom("foo"), Integer(97), Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorStream(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an input stream", func(t *testing.T) {
		s := Variable("Stream")
		env := NewEnv().
			Bind(s, NewStream(os.Stdin, StreamModeRead))

		var state State
		ok, err := state.PutCode(s, Integer(97), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorOutputStream(s), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		stream := NewStream(os.Stdout, StreamModeWrite)
		stream.streamType = StreamTypeBinary

		s := Variable("Stream")
		env := NewEnv().
			Bind(s, stream)

		var state State
		ok, err := state.PutCode(s, Integer(97), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorOutputBinaryStream(s), err)
		assert.False(t, ok)
	})

	t.Run("code is an integer but not an character code", func(t *testing.T) {
		var state State
		ok, err := state.PutCode(NewStream(os.Stdout, StreamModeWrite), Integer(-1), Success, nil).Force(context.Background())
		assert.Equal(t, representationError(Atom("character_code"), Atom("-1 is not a valid unicode code point.")), err)
		assert.False(t, ok)
	})

	t.Run("unknown stream alias", func(t *testing.T) {
		var state State
		_, err := state.PutCode(Atom("foo"), Integer('😀'), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("not a stream", func(t *testing.T) {
		var state State
		_, err := state.PutCode(NewVariable(), Integer('😀'), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("not a code", func(t *testing.T) {
		s := NewStream(os.Stdout, StreamModeWrite)

		t.Run("not an integer", func(t *testing.T) {
			var state State
			_, err := state.PutCode(s, Atom("a"), Success, nil).Force(context.Background())
			assert.Error(t, err)
		})
	})
}

func TestState_ReadTerm(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		s, err := Open("testdata/foo.txt", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		v := Variable("Term")

		var state State
		ok, err := state.ReadTerm(s, v, List(), func(env *Env) *Promise {
			assert.Equal(t, Atom("foo"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		s, err := Open("testdata/foo.txt", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		v := Variable("Term")

		state := State{
			streams: map[Term]*Stream{
				Atom("foo"): s,
			},
		}
		ok, err := state.ReadTerm(Atom("foo"), v, List(), func(env *Env) *Promise {
			assert.Equal(t, Atom("foo"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("singletons", func(t *testing.T) {
		s, err := Open("testdata/vars.txt", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		v, singletons := Variable("Term"), Variable("Singletons")

		var state State
		ok, err := state.ReadTerm(s, v, List(&Compound{
			Functor: "singletons",
			Args:    []Term{singletons},
		}), func(env *Env) *Promise {
			c, ok := env.Resolve(v).(*Compound)
			assert.True(t, ok)
			assert.Equal(t, Atom("f"), c.Functor)
			assert.Len(t, c.Args, 3)

			x, ok := c.Args[0].(Variable)
			assert.True(t, ok)
			assert.Equal(t, x, c.Args[1])

			y, ok := c.Args[2].(Variable)
			assert.True(t, ok)

			assert.Equal(t, List(y), env.Resolve(singletons))

			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("variables", func(t *testing.T) {
		s, err := Open("testdata/vars.txt", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		v, variables := Variable("Term"), Variable("Variables")

		var state State
		ok, err := state.ReadTerm(s, v, List(&Compound{
			Functor: "variables",
			Args:    []Term{variables},
		}), func(env *Env) *Promise {
			c, ok := env.Resolve(v).(*Compound)
			assert.True(t, ok)
			assert.Equal(t, Atom("f"), c.Functor)
			assert.Len(t, c.Args, 3)

			x, ok := c.Args[0].(Variable)
			assert.True(t, ok)
			assert.Equal(t, x, c.Args[1])

			y, ok := c.Args[2].(Variable)
			assert.True(t, ok)

			assert.Equal(t, List(x, y), env.Resolve(variables))

			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("variable_names", func(t *testing.T) {
		s, err := Open("testdata/vars.txt", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		v, variableNames := Variable("Term"), Variable("VariableNames")

		var state State
		ok, err := state.ReadTerm(s, v, List(&Compound{
			Functor: "variable_names",
			Args:    []Term{variableNames},
		}), func(env *Env) *Promise {
			c, ok := env.Resolve(v).(*Compound)
			assert.True(t, ok)
			assert.Equal(t, Atom("f"), c.Functor)
			assert.Len(t, c.Args, 3)

			x, ok := c.Args[0].(Variable)
			assert.True(t, ok)
			assert.Equal(t, x, c.Args[1])

			y, ok := c.Args[2].(Variable)
			assert.True(t, ok)

			assert.Equal(t, List(
				&Compound{
					Functor: "=",
					Args:    []Term{Atom("X"), x},
				},
				&Compound{
					Functor: "=",
					Args:    []Term{Atom("Y"), y},
				},
			), env.Resolve(variableNames))

			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("multiple reads", func(t *testing.T) {
		s, err := Open("testdata/multi.txt", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		v := Variable("Term")

		var state State

		ok, err := state.ReadTerm(s, v, List(), func(env *Env) *Promise {
			assert.Equal(t, &Compound{Functor: "foo", Args: []Term{Atom("a")}}, env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.ReadTerm(s, v, List(), func(env *Env) *Promise {
			assert.Equal(t, &Compound{Functor: "foo", Args: []Term{Atom("b")}}, env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.ReadTerm(s, &v, List(), func(env *Env) *Promise {
			assert.Equal(t, &Compound{Functor: "foo", Args: []Term{Atom("c")}}, env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")

		var state State
		ok, err := state.ReadTerm(streamOrAlias, NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("options is a partial list or a list with an element which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			options := ListRest(Variable("Rest"),
				&Compound{Functor: "variables", Args: []Term{Variable("VL")}},
			)

			var state State
			ok, err := state.ReadTerm(NewStream(os.Stdin, StreamModeRead), NewVariable(), options, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(options), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			option := Variable("Option")

			var state State
			ok, err := state.ReadTerm(NewStream(os.Stdin, StreamModeRead), NewVariable(), List(option, &Compound{Functor: "variables", Args: []Term{Variable("VL")}}), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(option), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var state State
		ok, err := state.ReadTerm(Integer(0), NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		var state State
		ok, err := state.ReadTerm(NewStream(os.Stdin, StreamModeRead), NewVariable(), Atom("options"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorList(Atom("options")), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the Options list is neither a variable nor a valid read-option", func(t *testing.T) {
		var state State
		ok, err := state.ReadTerm(NewStream(os.Stdin, StreamModeRead), NewVariable(), List(&Compound{
			Functor: "unknown",
			Args:    []Term{Atom("option")},
		}), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorReadOption(&Compound{
			Functor: "unknown",
			Args:    []Term{Atom("option")},
		}), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var state State
		ok, err := state.ReadTerm(Atom("foo"), NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorStream(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		s := Variable("Stream")
		env := NewEnv().
			Bind(s, NewStream(os.Stdout, StreamModeWrite))

		var state State
		ok, err := state.ReadTerm(s, NewVariable(), List(), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputStream(s), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		stream := NewStream(os.Stdin, StreamModeRead)
		stream.streamType = StreamTypeBinary

		s := Variable("Stream")
		env := NewEnv().
			Bind(s, stream)

		var state State
		ok, err := state.ReadTerm(s, NewVariable(), List(), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputBinaryStream(s), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		stream, err := Open("testdata/empty.txt", StreamModeRead,
			WithEOFAction(EOFActionError),
		)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, stream.Close())
		}()

		s := Variable("Stream")
		env := NewEnv().
			Bind(s, stream)

		var state State
		ok, err := state.ReadTerm(s, NewVariable(), List(), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputPastEndOfStream(s), err)
		assert.False(t, ok)
	})

	t.Run("one or more characters were input, but they cannot be parsed as a sequence of tokens", func(t *testing.T) {
		s, err := Open("testdata/unexpected_token.txt", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		var state State
		ok, err := state.ReadTerm(s, NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, syntaxErrorUnexpectedToken(Atom("unexpected token: <ident bar>")), err)
		assert.False(t, ok)
	})

	t.Run("the sequence of tokens cannot be parsed as a term using the current set of operator definitions", func(t *testing.T) {
		s, err := Open("testdata/unexpected_op.txt", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		var state State
		ok, err := state.ReadTerm(s, NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, syntaxErrorUnexpectedToken(Atom("unexpected token: <graphical =>")), err)
		assert.False(t, ok)
	})
}

func TestState_GetByte(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		s, err := Open("testdata/a.txt", StreamModeRead)
		assert.NoError(t, err)
		s.streamType = StreamTypeBinary
		defer func() {
			assert.NoError(t, s.Close())
		}()

		v := Variable("Byte")

		var state State
		ok, err := state.GetByte(s, v, func(env *Env) *Promise {
			assert.Equal(t, Integer(97), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		s, err := Open("testdata/a.txt", StreamModeRead)
		assert.NoError(t, err)
		s.streamType = StreamTypeBinary
		defer func() {
			assert.NoError(t, s.Close())
		}()

		v := Variable("Byte")

		state := State{
			streams: map[Term]*Stream{
				Atom("foo"): s,
			},
		}
		ok, err := state.GetByte(Atom("foo"), v, func(env *Env) *Promise {
			assert.Equal(t, Integer(97), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("eof", func(t *testing.T) {
		s, err := Open("testdata/empty.txt", StreamModeRead)
		assert.NoError(t, err)
		s.streamType = StreamTypeBinary
		defer func() {
			assert.NoError(t, s.Close())
		}()

		v := Variable("Byte")

		var state State
		ok, err := state.GetByte(s, v, func(env *Env) *Promise {
			assert.Equal(t, Integer(-1), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("error", func(t *testing.T) {
		readByte = func(r *bufio.Reader) (byte, error) {
			return 0, errors.New("failed")
		}
		defer func() {
			readByte = (*bufio.Reader).ReadByte
		}()

		s := NewStream(os.Stdin, StreamModeRead)
		s.streamType = StreamTypeBinary

		var state State

		v := Variable("V")
		_, err := state.GetByte(s, v, Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")
		var state State
		ok, err := state.GetByte(streamOrAlias, Variable("InByte"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("inByte is neither a variable nor an in-byte", func(t *testing.T) {
		s := NewStream(os.Stdin, StreamModeRead)
		s.streamType = StreamTypeBinary

		var state State
		ok, err := state.GetByte(s, Atom("inByte"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInByte(Atom("inByte")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream-term or alias", func(t *testing.T) {
		var state State
		ok, err := state.GetByte(Integer(0), Variable("InByte"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var state State
		ok, err := state.GetByte(Atom("foo"), Variable("InByte"), Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorStream(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, NewStream(os.Stdout, StreamModeWrite))

		var state State
		ok, err := state.GetByte(streamOrAlias, Variable("InByte"), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a text stream", func(t *testing.T) {
		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, NewStream(os.Stdin, StreamModeRead))

		var state State
		ok, err := state.GetByte(streamOrAlias, Variable("InByte"), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputTextStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		s, err := Open("testdata/empty.txt", StreamModeRead,
			WithStreamType(StreamTypeBinary),
			WithEOFAction(EOFActionError),
		)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, s)

		var state State
		ok, err := state.GetByte(streamOrAlias, Variable("InByte"), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputPastEndOfStream(streamOrAlias), err)
		assert.False(t, ok)
	})
}

func TestState_GetChar(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		s, err := Open("testdata/smile.txt", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		v := Variable("Char")

		var state State
		ok, err := state.GetChar(s, v, func(env *Env) *Promise {
			assert.Equal(t, Atom("😀"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		s, err := Open("testdata/smile.txt", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		v := Variable("Char")

		state := State{
			streams: map[Term]*Stream{
				Atom("foo"): s,
			},
		}
		ok, err := state.GetChar(Atom("foo"), v, func(env *Env) *Promise {
			assert.Equal(t, Atom("😀"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("eof", func(t *testing.T) {
		s, err := Open("testdata/empty.txt", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		v := Variable("Char")

		var state State
		ok, err := state.GetChar(s, v, func(env *Env) *Promise {
			assert.Equal(t, Atom("end_of_file"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("error", func(t *testing.T) {
		readRune = func(r *bufio.Reader) (rune, int, error) {
			return 0, 0, errors.New("failed")
		}
		defer func() {
			readRune = (*bufio.Reader).ReadRune
		}()

		v := Variable("V")

		var state State
		ok, err := state.GetChar(NewStream(os.Stdin, StreamModeRead), v, Success, nil).Force(context.Background())
		assert.Equal(t, SystemError(errors.New("failed")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")

		var state State
		ok, err := state.GetChar(streamOrAlias, Variable("Char"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("char is neither a variable nor an in-character", func(t *testing.T) {
		var state State
		ok, err := state.GetChar(NewStream(os.Stdin, StreamModeRead), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInCharacter(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var state State
		ok, err := state.GetChar(Integer(0), Variable("Char"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, NewStream(os.Stdout, StreamModeWrite))

		var state State
		ok, err := state.GetChar(streamOrAlias, Variable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		s := NewStream(os.Stdin, StreamModeRead)
		s.streamType = StreamTypeBinary

		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, s)

		var state State
		ok, err := state.GetChar(streamOrAlias, Variable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputBinaryStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		s, err := Open("testdata/empty.txt", StreamModeRead,
			WithEOFAction(EOFActionError),
		)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, s)

		var state State
		ok, err := state.GetChar(streamOrAlias, Variable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputPastEndOfStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("the entity input from the stream is not a character", func(t *testing.T) {
		s, err := Open("testdata/replacement.txt", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, s)

		var state State
		ok, err := state.GetChar(streamOrAlias, Variable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, representationError(Atom("character"), Atom("invalid character.")), err)
		assert.False(t, ok)
	})
}

func TestState_PeekByte(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		s, err := Open("testdata/abc.txt", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		s.streamType = StreamTypeBinary

		v := Variable("Byte")

		var state State
		ok, err := state.PeekByte(s, v, func(env *Env) *Promise {
			assert.Equal(t, Integer(97), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.PeekByte(s, v, Success, nil).Force(context.Background()) // 'a' again
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		s, err := Open("testdata/abc.txt", StreamModeRead)
		assert.NoError(t, err)
		s.streamType = StreamTypeBinary
		defer func() {
			assert.NoError(t, s.Close())
		}()

		v := Variable("Byte")

		state := State{
			streams: map[Term]*Stream{
				Atom("foo"): s,
			},
		}
		ok, err := state.PeekByte(Atom("foo"), v, func(env *Env) *Promise {
			assert.Equal(t, Integer(97), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("eof", func(t *testing.T) {
		s, err := Open("testdata/empty.txt", StreamModeRead)
		assert.NoError(t, err)
		s.streamType = StreamTypeBinary
		defer func() {
			assert.NoError(t, s.Close())
		}()

		v := Variable("Byte")

		var state State
		ok, err := state.PeekByte(s, v, func(env *Env) *Promise {
			assert.Equal(t, Integer(-1), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("error", func(t *testing.T) {
		peek = func(r *bufio.Reader, n int) ([]byte, error) {
			return nil, errors.New("failed")
		}
		defer func() {
			peek = (*bufio.Reader).Peek
		}()

		s := NewStream(os.Stdin, StreamModeRead)
		s.streamType = StreamTypeBinary

		v := Variable("V")

		var state State
		ok, err := state.PeekByte(s, v, Success, nil).Force(context.Background())
		assert.Equal(t, SystemError(errors.New("failed")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")

		var state State
		ok, err := state.PeekByte(streamOrAlias, Variable("Byte"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("inByte is neither a variable nor an in-byte", func(t *testing.T) {
		s := NewStream(os.Stdin, StreamModeRead)
		s.streamType = StreamTypeBinary

		var state State
		ok, err := state.PeekByte(s, Atom("byte"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInByte(Atom("byte")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var state State
		ok, err := state.PeekByte(Integer(0), Variable("Byte"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, NewStream(os.Stdout, StreamModeWrite))

		var state State
		ok, err := state.PeekByte(streamOrAlias, Variable("Byte"), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a text stream", func(t *testing.T) {
		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, NewStream(os.Stdin, StreamModeRead))

		var state State
		ok, err := state.PeekByte(streamOrAlias, Variable("Byte"), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputTextStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		s, err := Open("testdata/empty.txt", StreamModeRead,
			WithStreamType(StreamTypeBinary),
			WithEOFAction(EOFActionError),
		)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, s)

		var state State
		ok, err := state.PeekByte(streamOrAlias, Variable("Byte"), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputPastEndOfStream(streamOrAlias), err)
		assert.False(t, ok)
	})
}

func TestState_PeekChar(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		s, err := Open("testdata/smile.txt", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		v := Variable("Char")

		var state State
		ok, err := state.PeekChar(s, v, func(env *Env) *Promise {
			assert.Equal(t, Atom("😀"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.PeekChar(s, v, func(env *Env) *Promise {
			assert.Equal(t, Atom("😀"), env.Resolve(v)) // '😀' again
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		s, err := Open("testdata/smile.txt", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		v := Variable("Char")

		state := State{
			streams: map[Term]*Stream{
				Atom("foo"): s,
			},
		}
		ok, err := state.PeekChar(Atom("foo"), v, func(env *Env) *Promise {
			assert.Equal(t, Atom("😀"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("eof", func(t *testing.T) {
		s, err := Open("testdata/empty.txt", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		v := Variable("Char")

		var state State
		ok, err := state.PeekChar(s, v, func(env *Env) *Promise {
			assert.Equal(t, Atom("end_of_file"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("error", func(t *testing.T) {
		t.Run("error on read rune", func(t *testing.T) {
			readRune = func(r *bufio.Reader) (rune, int, error) {
				return 0, 0, errors.New("failed")
			}
			defer func() {
				readRune = (*bufio.Reader).ReadRune
			}()

			v := Variable("V")

			var state State
			ok, err := state.PeekChar(NewStream(os.Stdin, StreamModeRead), v, Success, nil).Force(context.Background())
			assert.Equal(t, SystemError(errors.New("failed")), err)
			assert.False(t, ok)
		})

		t.Run("error on unread rune", func(t *testing.T) {
			unreadRune = func(r *bufio.Reader) error {
				return errors.New("failed")
			}
			defer func() {
				unreadRune = (*bufio.Reader).UnreadRune
			}()

			s, err := Open("testdata/a.txt", StreamModeRead)
			assert.NoError(t, err)
			defer func() {
				assert.NoError(t, s.Close())
			}()

			v := Variable("V")

			var state State
			ok, err := state.PeekChar(s, v, Success, nil).Force(context.Background())
			assert.Equal(t, SystemError(errors.New("failed")), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")

		var state State
		ok, err := state.PeekChar(streamOrAlias, Variable("Char"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("char is neither a variable nor an in-character", func(t *testing.T) {
		var state State
		ok, err := state.PeekChar(NewStream(os.Stdin, StreamModeRead), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInCharacter(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var state State
		ok, err := state.PeekChar(Integer(0), Variable("Char"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, NewStream(os.Stdout, StreamModeWrite))

		var state State
		ok, err := state.PeekChar(streamOrAlias, Variable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		s := NewStream(os.Stdin, StreamModeRead)
		s.streamType = StreamTypeBinary

		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, s)

		var state State
		ok, err := state.PeekChar(streamOrAlias, Variable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputBinaryStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		s, err := Open("testdata/empty.txt", StreamModeRead,
			WithEOFAction(EOFActionError),
		)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, s)

		var state State
		ok, err := state.PeekChar(streamOrAlias, Variable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, permissionErrorInputPastEndOfStream(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("the entity input from the stream is not a character", func(t *testing.T) {
		s, err := Open("testdata/replacement.txt", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		streamOrAlias := Variable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, s)

		var state State
		ok, err := state.PeekChar(streamOrAlias, Variable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, representationError(Atom("character"), Atom("invalid character.")), err)
		assert.False(t, ok)
	})
}

func Test_Halt(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		var exitCalled bool
		osExit = func(code int) {
			assert.Equal(t, 2, code)
			exitCalled = true
		}
		defer func() {
			osExit = os.Exit
		}()

		ok, err := Halt(Integer(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.True(t, exitCalled)
	})

	t.Run("n is a variable", func(t *testing.T) {
		n := Variable("N")

		ok, err := Halt(n, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(n), err)
		assert.False(t, ok)
	})

	t.Run("n is neither a variable nor an integer", func(t *testing.T) {
		ok, err := Halt(Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Atom("foo")), err)
		assert.False(t, ok)
	})
}

func TestState_Clause(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		x := Variable("X")
		what, body := Variable("What"), Variable("Body")

		var c int

		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "green", Arity: 1}: clauses{
						{raw: &Compound{
							Functor: ":-", Args: []Term{
								&Compound{Functor: "green", Args: []Term{x}},
								&Compound{Functor: "moldy", Args: []Term{x}},
							},
						}},
						{raw: &Compound{Functor: "green", Args: []Term{Atom("kermit")}}},
					},
				},
			},
		}
		ok, err := state.Clause(&Compound{
			Functor: "green",
			Args:    []Term{what},
		}, body, func(env *Env) *Promise {
			switch c {
			case 0:
				assert.True(t, env.Resolve(what).(Variable).Generated())
				b, ok := env.Resolve(body).(*Compound)
				assert.True(t, ok)
				assert.Equal(t, Atom("moldy"), b.Functor)
				assert.Len(t, b.Args, 1)
				assert.True(t, b.Args[0].(Variable).Generated())
			case 1:
				assert.Equal(t, Atom("kermit"), env.Resolve(what))
				assert.Equal(t, Atom("true"), env.Resolve(body))
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("head is a variable", func(t *testing.T) {
		head := Variable("Head")

		var state State
		ok, err := state.Clause(head, Atom("true"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(head), err)
		assert.False(t, ok)
	})

	t.Run("head is neither a variable nor a predication", func(t *testing.T) {
		var state State
		ok, err := state.Clause(Integer(0), Atom("true"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("the predicate indicator Pred of Head is that of a private (ie. Not public) procedure", func(t *testing.T) {
		what, body := Variable("What"), Variable("Body")

		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "green", Arity: 1}: predicate1(func(t Term, f func(*Env) *Promise, env *Env) *Promise {
						return Bool(true)
					}),
				},
			},
		}
		ok, err := state.Clause(&Compound{
			Functor: "green",
			Args:    []Term{what},
		}, body, Success, nil).Force(context.Background())
		assert.Equal(t, permissionErrorAccessPrivateProcedure(&Compound{
			Functor: "/",
			Args:    []Term{Atom("green"), Integer(1)},
		}), err)
		assert.False(t, ok)
	})

	t.Run("body is neither a variable nor a callable term", func(t *testing.T) {
		var state State
		ok, err := state.Clause(Atom("foo"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCallable(Integer(0)), err)
		assert.False(t, ok)
	})
}

func TestAtomLength(t *testing.T) {
	t.Run("ascii", func(t *testing.T) {
		ok, err := AtomLength(Atom("abc"), Integer(3), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("emoji", func(t *testing.T) {
		ok, err := AtomLength(Atom("😀"), Integer(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("atom is a variable", func(t *testing.T) {
		atom := Variable("Atom")
		ok, err := AtomLength(atom, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(atom), err)
		assert.False(t, ok)
	})

	t.Run("atom is neither a variable nor an atom", func(t *testing.T) {
		ok, err := AtomLength(Integer(2), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(2)), err)
		assert.False(t, ok)
	})

	t.Run("length is neither a variable nor an integer", func(t *testing.T) {
		ok, err := AtomLength(Atom("😀"), Atom("1"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Atom("1")), err)
		assert.False(t, ok)
	})

	t.Run("length is an integer less than zero", func(t *testing.T) {
		ok, err := AtomLength(Atom("😀"), Integer(-1), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorNotLessThanZero(Integer(-1)), err)
		assert.False(t, ok)
	})
}

func TestAtomConcat(t *testing.T) {
	t.Run("atom3 is a variable", func(t *testing.T) {
		atom3 := Variable("Atom3")

		ok, err := AtomConcat(Atom("foo"), Atom("bar"), atom3, func(env *Env) *Promise {
			assert.Equal(t, Atom("foobar"), env.Resolve(atom3))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("atom3 is an atom", func(t *testing.T) {
		var c int
		v1, v2 := Variable("V1"), Variable("V2")
		ok, err := AtomConcat(v1, v2, Atom("foo"), func(env *Env) *Promise {
			switch c {
			case 0:
				assert.Equal(t, Atom(""), env.Resolve(v1))
				assert.Equal(t, Atom("foo"), env.Resolve(v2))
			case 1:
				assert.Equal(t, Atom("f"), env.Resolve(v1))
				assert.Equal(t, Atom("oo"), env.Resolve(v2))
			case 2:
				assert.Equal(t, Atom("fo"), env.Resolve(v1))
				assert.Equal(t, Atom("o"), env.Resolve(v2))
			case 3:
				assert.Equal(t, Atom("foo"), env.Resolve(v1))
				assert.Equal(t, Atom(""), env.Resolve(v2))
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("atom1 and atom3 are variables", func(t *testing.T) {
		atom1, atom3 := Variable("Atom1"), Variable("Atom3")

		ok, err := AtomConcat(atom1, Atom("bar"), atom3, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(&Compound{
			Functor: ",",
			Args:    []Term{atom1, atom3},
		}), err)
		assert.False(t, ok)
	})

	t.Run("atom2 and atom3 are variables", func(t *testing.T) {
		atom2, atom3 := Variable("Atom2"), Variable("Atom3")

		ok, err := AtomConcat(Atom("foo"), atom2, atom3, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(&Compound{
			Functor: ",",
			Args:    []Term{atom2, atom3},
		}), err)
		assert.False(t, ok)
	})

	t.Run("atom1 is neither a variable nor an atom", func(t *testing.T) {
		t.Run("atom3 is a variable", func(t *testing.T) {
			ok, err := AtomConcat(Integer(1), Atom("bar"), Variable("Atom3"), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorAtom(Integer(1)), err)
			assert.False(t, ok)
		})

		t.Run("atom3 is an atom", func(t *testing.T) {
			ok, err := AtomConcat(Integer(1), Atom("bar"), Atom("foobar"), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorAtom(Integer(1)), err)
			assert.False(t, ok)
		})
	})

	t.Run("atom2 is neither a variable nor an atom", func(t *testing.T) {
		t.Run("atom3 is a variable", func(t *testing.T) {
			ok, err := AtomConcat(Atom("foo"), Integer(2), Variable("Atom3"), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorAtom(Integer(2)), err)
			assert.False(t, ok)
		})

		t.Run("atom3 is an atom", func(t *testing.T) {
			ok, err := AtomConcat(Atom("foo"), Integer(2), Atom("foobar"), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorAtom(Integer(2)), err)
			assert.False(t, ok)
		})
	})

	t.Run("atom3 is neither a variable nor an atom", func(t *testing.T) {
		ok, err := AtomConcat(Atom("foo"), Atom("bar"), Integer(3), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(3)), err)
		assert.False(t, ok)
	})
}

func TestSubAtom(t *testing.T) {
	t.Run("multiple solutions", func(t *testing.T) {
		before, length, after := Variable("Before"), Variable("Length"), Variable("After")
		var c int
		ok, err := SubAtom(Atom("xATGATGAxATGAxATGAx"), before, length, after, Atom("ATGA"), func(env *Env) *Promise {
			switch c {
			case 0:
				assert.Equal(t, Integer(1), env.Resolve(before))
				assert.Equal(t, Integer(4), env.Resolve(length))
				assert.Equal(t, Integer(14), env.Resolve(after))
			case 1:
				assert.Equal(t, Integer(4), env.Resolve(before))
				assert.Equal(t, Integer(4), env.Resolve(length))
				assert.Equal(t, Integer(11), env.Resolve(after))
			case 2:
				assert.Equal(t, Integer(9), env.Resolve(before))
				assert.Equal(t, Integer(4), env.Resolve(length))
				assert.Equal(t, Integer(6), env.Resolve(after))
			case 3:
				assert.Equal(t, Integer(14), env.Resolve(before))
				assert.Equal(t, Integer(4), env.Resolve(length))
				assert.Equal(t, Integer(1), env.Resolve(after))
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("get the first char", func(t *testing.T) {
		char := Variable("Char")
		ok, err := SubAtom(Atom("a"), Integer(0), Integer(1), Integer(0), char, func(env *Env) *Promise {
			assert.Equal(t, Atom("a"), env.Resolve(char))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("atom is a variable", func(t *testing.T) {
		atom := Variable("Atom")
		ok, err := SubAtom(atom, Variable("Before"), Variable("Length"), Variable("After"), Variable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(atom), err)
		assert.False(t, ok)
	})

	t.Run("atom is neither a variable nor an atom", func(t *testing.T) {
		ok, err := SubAtom(Integer(0), Variable("Before"), Variable("Length"), Variable("After"), Variable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("subAtom is neither a variable nor an atom", func(t *testing.T) {
		ok, err := SubAtom(Atom("foo"), Variable("Before"), Variable("Length"), Variable("After"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("before is neither a variable nor an integer", func(t *testing.T) {
		ok, err := SubAtom(Atom("foo"), Atom("before"), Variable("Length"), Variable("After"), Variable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Atom("before")), err)
		assert.False(t, ok)
	})

	t.Run("length is neither a variable nor an integer", func(t *testing.T) {
		ok, err := SubAtom(Atom("foo"), Variable("Before"), Atom("length"), Variable("After"), Variable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Atom("length")), err)
		assert.False(t, ok)
	})

	t.Run("after is neither a variable nor an integer", func(t *testing.T) {
		ok, err := SubAtom(Atom("foo"), Variable("Before"), Variable("Length"), Atom("after"), Variable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Atom("after")), err)
		assert.False(t, ok)
	})

	t.Run("before is an integer less than zero", func(t *testing.T) {
		ok, err := SubAtom(Atom("foo"), Integer(-1), Variable("Length"), Variable("After"), Variable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorNotLessThanZero(Integer(-1)), err)
		assert.False(t, ok)
	})

	t.Run("length is an integer less than zero", func(t *testing.T) {
		ok, err := SubAtom(Atom("foo"), Variable("Before"), Integer(-1), Variable("After"), Variable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorNotLessThanZero(Integer(-1)), err)
		assert.False(t, ok)
	})

	t.Run("after is an integer less than zero", func(t *testing.T) {
		ok, err := SubAtom(Atom("foo"), Variable("Before"), Variable("Length"), Integer(-1), Variable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorNotLessThanZero(Integer(-1)), err)
		assert.False(t, ok)
	})
}

func TestAtomChars(t *testing.T) {
	t.Run("break down", func(t *testing.T) {
		chars := Variable("Char")

		ok, err := AtomChars(Atom("foo"), chars, func(env *Env) *Promise {
			assert.Equal(t, List(Atom("f"), Atom("o"), Atom("o")), env.Resolve(chars))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("construct", func(t *testing.T) {
		atom := Variable("Atom")

		ok, err := AtomChars(atom, List(Atom("f"), Atom("o"), Atom("o")), func(env *Env) *Promise {
			assert.Equal(t, Atom("foo"), env.Resolve(atom))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		_, err = AtomChars(NewVariable(), List(Integer(102), Integer(111), Integer(111)), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("atom is a variable and List is a partial list or list with an element which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			chars := ListRest(Variable("Rest"),
				Atom("0"),
				Atom("0"),
			)

			ok, err := AtomChars(NewVariable(), chars, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(chars), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			char := Variable("Char")
			ok, err := AtomChars(NewVariable(), List(char, Atom("o"), Atom("o")), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(char), err)
			assert.False(t, ok)
		})
	})

	t.Run("atom is neither a variable nor an atom", func(t *testing.T) {
		ok, err := AtomChars(Integer(0), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("atom is a variable and List is neither a list nor a partial list", func(t *testing.T) {
		ok, err := AtomChars(NewVariable(), Atom("chars"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorList(Atom("chars")), err)
		assert.False(t, ok)
	})

	t.Run("atom is a variable and an element E of the list List is neither a variable nor a one-character atom", func(t *testing.T) {
		t.Run("not a one-character atom", func(t *testing.T) {
			ok, err := AtomChars(NewVariable(), List(Atom("chars")), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorCharacter(Atom("chars")), err)
			assert.False(t, ok)
		})

		t.Run("not an atom", func(t *testing.T) {
			ok, err := AtomChars(NewVariable(), List(Integer(0)), Success, nil).Force(context.Background())
			assert.Equal(t, typeErrorCharacter(Integer(0)), err)
			assert.False(t, ok)
		})
	})
}

func TestAtomCodes(t *testing.T) {
	t.Run("break up", func(t *testing.T) {
		codes := Variable("Codes")

		ok, err := AtomCodes(Atom("foo"), codes, func(env *Env) *Promise {
			assert.Equal(t, List(Integer(102), Integer(111), Integer(111)), env.Resolve(codes))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("construct", func(t *testing.T) {
		atom := Variable("Atom")

		ok, err := AtomCodes(atom, List(Integer(102), Integer(111), Integer(111)), func(env *Env) *Promise {
			assert.Equal(t, Atom("foo"), env.Resolve(atom))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("atom is a variable and List is a partial list or list with an element which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			codes := ListRest(Variable("Rest"),
				Integer(111),
				Integer(111),
			)
			ok, err := AtomCodes(NewVariable(), codes, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(codes), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			code := Variable("Code")

			ok, err := AtomCodes(NewVariable(), List(code, Integer(111), Integer(111)), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(code), err)
			assert.False(t, ok)
		})
	})

	t.Run("atom is neither a variable nor an atom", func(t *testing.T) {
		ok, err := AtomCodes(Integer(0), List(Integer(102), Integer(111), Integer(111)), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("atom is a variable and List is neither a list nor a partial list", func(t *testing.T) {
		ok, err := AtomCodes(NewVariable(), Atom("codes"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorList(Atom("codes")), err)
		assert.False(t, ok)
	})

	t.Run("atom is a variable and an element E of the list List is neither a variable nor a character-code", func(t *testing.T) {
		ok, err := AtomCodes(NewVariable(), List(Atom("f"), Integer(111), Integer(111)), Success, nil).Force(context.Background())
		assert.Equal(t, representationError(Atom("character_code"), Atom("invalid character code.")), err)
		assert.False(t, ok)
	})
}

func TestNumberChars(t *testing.T) {
	t.Run("number to chars", func(t *testing.T) {
		chars := Variable("Chars")

		ok, err := NumberChars(Float(23.4), chars, func(env *Env) *Promise {
			assert.Equal(t, List(Atom("2"), Atom("3"), Atom("."), Atom("4")), env.Resolve(chars))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("chars to number", func(t *testing.T) {
		num := Variable("Num")

		ok, err := NumberChars(num, List(Atom("2"), Atom("3"), Atom("."), Atom("4")), func(env *Env) *Promise {
			assert.Equal(t, Float(23.4), env.Resolve(num))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("both provided", func(t *testing.T) {
		t.Run("3.3", func(t *testing.T) {
			ok, err := NumberChars(Float(3.3), List(Atom("3"), Atom("."), Atom("3")), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("3.3E+0", func(t *testing.T) {
			ok, err := NumberChars(Float(3.3), List(Atom("3"), Atom("."), Atom("3"), Atom("E"), Atom("+"), Atom("0")), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("num is a variable and chars is a partial list or list with an element which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			codes := ListRest(Variable("Rest"),
				Atom("2"), Atom("3"), Atom("."), Atom("4"),
			)

			ok, err := NumberChars(NewVariable(), codes, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(codes), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			code := Variable("Code")

			ok, err := NumberChars(NewVariable(), List(code, Atom("3"), Atom("."), Atom("4")), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(code), err)
			assert.False(t, ok)
		})
	})

	t.Run("num is neither a variable nor a number", func(t *testing.T) {
		ok, err := NumberChars(Atom("23.4"), List(Atom("2"), Atom("3"), Atom("."), Atom("4")), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorNumber(Atom("23.4")), err)
		assert.False(t, ok)
	})

	t.Run("num is a variable and chars is neither a list nor partial list", func(t *testing.T) {
		ok, err := NumberChars(NewVariable(), Atom("23.4"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorList(Atom("23.4")), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the list chars is neither a variable nor a one-character atom", func(t *testing.T) {
		ok, err := NumberChars(NewVariable(), List(Integer(2), Atom("3"), Atom("."), Atom("4")), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorCharacter(Integer(2)), err)
		assert.False(t, ok)
	})

	t.Run("chars is a list of one-char atoms but is not parsable as a number", func(t *testing.T) {
		ok, err := NumberChars(NewVariable(), List(Atom("f"), Atom("o"), Atom("o")), Success, nil).Force(context.Background())
		assert.Equal(t, syntaxErrorNotANumber(), err)
		assert.False(t, ok)
	})
}

func TestNumberCodes(t *testing.T) {
	t.Run("number to codes", func(t *testing.T) {
		codes := Variable("Codes")

		ok, err := NumberCodes(Float(23.4), codes, func(env *Env) *Promise {
			assert.Equal(t, List(Integer(50), Integer(51), Integer(46), Integer(52)), env.Resolve(codes))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("codes to number", func(t *testing.T) {
		num := Variable("Num")

		ok, err := NumberCodes(num, List(Integer(50), Integer(51), Integer(46), Integer(52)), func(env *Env) *Promise {
			assert.Equal(t, Float(23.4), env.Resolve(num))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("both provided", func(t *testing.T) {
		t.Run("33.0", func(t *testing.T) {
			ok, err := NumberCodes(Float(33.0), List(Integer(51), Integer(51), Integer(46), Integer(48)), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("33", func(t *testing.T) {
			ok, err := NumberCodes(Float(33.0), List(Integer(51), Integer(51)), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("3.3E+01", func(t *testing.T) {
			ok, err := NumberCodes(Float(33.0), List(Integer(51), Integer(46), Integer(51), Integer(69), Integer(43), Integer(48), Integer(49)), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("num is a variable and codes is a partial list or list with an element which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			codes := ListRest(Variable("Rest"),
				Integer(50), Integer(51), Integer(46), Integer(52),
			)

			ok, err := NumberCodes(NewVariable(), codes, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(codes), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			code := Variable("Code")

			ok, err := NumberCodes(NewVariable(), List(code, Integer(50), Integer(51), Integer(46), Integer(52)), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(code), err)
			assert.False(t, ok)
		})
	})

	t.Run("num is neither a variable nor a number", func(t *testing.T) {
		ok, err := NumberCodes(Atom("23.4"), List(Integer(50), Integer(51), Integer(46), Integer(52)), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorNumber(Atom("23.4")), err)
		assert.False(t, ok)
	})

	t.Run("num is a variable and codes is neither a list nor partial list", func(t *testing.T) {
		ok, err := NumberCodes(NewVariable(), Atom("23.4"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorList(Atom("23.4")), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the list codes is neither a variable nor a one-character atom", func(t *testing.T) {
		ok, err := NumberCodes(NewVariable(), List(Atom("2"), Integer(51), Integer(46), Integer(52)), Success, nil).Force(context.Background())
		assert.Equal(t, representationError(Atom("character_code"), Atom("'2' is not a valid character code.")), err)
		assert.False(t, ok)
	})

	t.Run("codes is a list of one-char atoms but is not parsable as a number", func(t *testing.T) {
		ok, err := NumberCodes(NewVariable(), List(Integer(102), Integer(111), Integer(111)), Success, nil).Force(context.Background())
		assert.Equal(t, syntaxErrorNotANumber(), err)
		assert.False(t, ok)
	})
}

func TestFunctionSet_Is(t *testing.T) {
	t.Run("addition", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(3), &Compound{Functor: "+", Args: []Term{Integer(1), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(3), &Compound{Functor: "+", Args: []Term{Integer(1), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(3), &Compound{Functor: "+", Args: []Term{Float(1), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(3), &Compound{Functor: "+", Args: []Term{Float(1), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("subtraction", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(1), &Compound{Functor: "-", Args: []Term{Integer(3), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "-", Args: []Term{Integer(3), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "-", Args: []Term{Float(3), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "-", Args: []Term{Float(3), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("multiplication", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(6), &Compound{Functor: "*", Args: []Term{Integer(3), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(6), &Compound{Functor: "*", Args: []Term{Integer(3), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(6), &Compound{Functor: "*", Args: []Term{Float(3), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(6), &Compound{Functor: "*", Args: []Term{Float(3), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("floating-point division", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(2), &Compound{Functor: "/", Args: []Term{Integer(4), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(2), &Compound{Functor: "/", Args: []Term{Integer(4), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(2), &Compound{Functor: "/", Args: []Term{Float(4), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(2), &Compound{Functor: "/", Args: []Term{Float(4), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("integer division", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(2), &Compound{Functor: "//", Args: []Term{Integer(4), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "//", Args: []Term{Integer(4), Float(2)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(2)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "//", Args: []Term{Float(4), Integer(2)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "//", Args: []Term{Integer(4), Integer(0)}}, Success, nil).Force(context.Background())
		assert.Equal(t, evaluationErrorZeroDivisor(), err)
		assert.False(t, ok)
	})

	t.Run("remainder", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(-1), &Compound{Functor: "rem", Args: []Term{Integer(-21), Integer(4)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "rem", Args: []Term{Integer(-21), Float(4)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "rem", Args: []Term{Float(-21), Integer(4)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(-21)), err)
		assert.False(t, ok)
	})

	t.Run("mod", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(3), &Compound{Functor: "mod", Args: []Term{Integer(-21), Integer(4)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "mod", Args: []Term{Integer(-21), Float(4)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "mod", Args: []Term{Float(-21), Integer(4)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(-21)), err)
		assert.False(t, ok)
	})

	t.Run("exponential", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(16), &Compound{Functor: "**", Args: []Term{Integer(4), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(16), &Compound{Functor: "**", Args: []Term{Integer(4), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(16), &Compound{Functor: "**", Args: []Term{Float(4), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(16), &Compound{Functor: "**", Args: []Term{Float(4), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("sign reversal", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(-2), &Compound{Functor: "-", Args: []Term{Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(-2), &Compound{Functor: "-", Args: []Term{Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("absolute value", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(2), &Compound{Functor: "abs", Args: []Term{Integer(-2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(2), &Compound{Functor: "abs", Args: []Term{Float(-2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("arctangent", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(0), &Compound{Functor: "atan", Args: []Term{Integer(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(0), &Compound{Functor: "atan", Args: []Term{Float(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("ceiling", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(1), &Compound{Functor: "ceiling", Args: []Term{Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "ceiling", Args: []Term{Float(0.9)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("cosine", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(1.0), &Compound{Functor: "cos", Args: []Term{Integer(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1.0), &Compound{Functor: "cos", Args: []Term{Float(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("natural antilogarithm", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(1.0), &Compound{Functor: "exp", Args: []Term{Integer(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1.0), &Compound{Functor: "exp", Args: []Term{Float(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("square root", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(1.0), &Compound{Functor: "sqrt", Args: []Term{Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1.0), &Compound{Functor: "sqrt", Args: []Term{Float(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("sign", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(1), &Compound{Functor: "sign", Args: []Term{Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Integer(1), &Compound{Functor: "sign", Args: []Term{Integer(math.MaxInt64)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Integer(0), &Compound{Functor: "sign", Args: []Term{Integer(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Integer(-1), &Compound{Functor: "sign", Args: []Term{Integer(-1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Integer(-1), &Compound{Functor: "sign", Args: []Term{Integer(math.MinInt64)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "sign", Args: []Term{Float(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "sign", Args: []Term{Float(math.MaxFloat64)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(0), &Compound{Functor: "sign", Args: []Term{Float(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(-1), &Compound{Functor: "sign", Args: []Term{Float(-1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(-1), &Compound{Functor: "sign", Args: []Term{Float(-math.MaxFloat64)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		v := Variable("N")
		ok, err = DefaultFunctionSet.Is(v, &Compound{Functor: "sign", Args: []Term{Float(math.NaN())}}, func(env *Env) *Promise {
			assert.True(t, math.IsNaN(float64(env.Resolve(v).(Float))))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("float", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(1.0), &Compound{Functor: "float", Args: []Term{Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1.0), &Compound{Functor: "float", Args: []Term{Float(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("floor", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(1), &Compound{Functor: "floor", Args: []Term{Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "floor", Args: []Term{Float(1.1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("natural logarithm", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(0), &Compound{Functor: "log", Args: []Term{Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(0), &Compound{Functor: "log", Args: []Term{Float(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("sine", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(0), &Compound{Functor: "sin", Args: []Term{Integer(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(0), &Compound{Functor: "sin", Args: []Term{Float(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("truncate", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(1), &Compound{Functor: "truncate", Args: []Term{Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "truncate", Args: []Term{Float(1.1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("round", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Float(1), &Compound{Functor: "round", Args: []Term{Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(Float(1), &Compound{Functor: "round", Args: []Term{Float(1.1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("bit-shift right", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(2), &Compound{Functor: ">>", Args: []Term{Integer(4), Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: ">>", Args: []Term{Float(4), Integer(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: ">>", Args: []Term{Integer(4), Float(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(1)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: ">>", Args: []Term{Float(4), Float(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)
	})

	t.Run("bit-shift left", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(8), &Compound{Functor: "<<", Args: []Term{Integer(4), Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "<<", Args: []Term{Float(4), Integer(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "<<", Args: []Term{Integer(4), Float(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(1)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "<<", Args: []Term{Float(4), Float(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)
	})

	t.Run("bitwise and", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(1), &Compound{Functor: "/\\", Args: []Term{Integer(5), Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "/\\", Args: []Term{Float(5), Integer(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(5)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "/\\", Args: []Term{Integer(5), Float(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(1)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "/\\", Args: []Term{Float(5), Float(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(5)), err)
		assert.False(t, ok)
	})

	t.Run("bitwise or", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(5), &Compound{Functor: "\\/", Args: []Term{Integer(4), Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "\\/", Args: []Term{Float(4), Integer(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "\\/", Args: []Term{Integer(4), Float(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(1)), err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "\\/", Args: []Term{Float(4), Float(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)
	})

	t.Run("bitwise complement", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(-1), &Compound{Functor: "\\", Args: []Term{Integer(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Is(NewVariable(), &Compound{Functor: "\\", Args: []Term{Float(0)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(0)), err)
		assert.False(t, ok)
	})

	t.Run("expression is a variable", func(t *testing.T) {
		expression := Variable("Exp")

		ok, err := DefaultFunctionSet.Is(Integer(0), expression, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(expression), err)
		assert.False(t, ok)
	})

	t.Run("unknown constant", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Is(Integer(0), Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorEvaluable(&Compound{
			Functor: "/",
			Args:    []Term{Atom("foo"), Integer(0)},
		}), err)
		assert.False(t, ok)
	})
}

func TestFunctionSet_Equal(t *testing.T) {
	t.Run("same", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Equal(Integer(1), Integer(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Equal(Float(1), Integer(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Equal(Integer(1), Float(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.Equal(Float(1), Float(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("different", func(t *testing.T) {
		ok, err := DefaultFunctionSet.Equal(Integer(1), Integer(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Equal(Float(1), Integer(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Equal(Integer(1), Float(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.Equal(Float(1), Float(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("lhs is a variable", func(t *testing.T) {
		lhs := Variable("LHS")

		ok, err := DefaultFunctionSet.Equal(lhs, Integer(1), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(lhs), err)
		assert.False(t, ok)
	})

	t.Run("rhs is a variable", func(t *testing.T) {
		rhs := Variable("RHS")

		ok, err := DefaultFunctionSet.Equal(Integer(1), rhs, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(rhs), err)
		assert.False(t, ok)
	})
}

func TestFunctionSet_NotEqual(t *testing.T) {
	t.Run("same", func(t *testing.T) {
		ok, err := DefaultFunctionSet.NotEqual(Integer(1), Integer(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.NotEqual(Float(1), Integer(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.NotEqual(Integer(1), Float(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)

		ok, err = DefaultFunctionSet.NotEqual(Float(1), Float(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("different", func(t *testing.T) {
		ok, err := DefaultFunctionSet.NotEqual(Integer(1), Integer(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.NotEqual(Float(1), Integer(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.NotEqual(Integer(1), Float(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultFunctionSet.NotEqual(Float(1), Float(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("lhs is a variable", func(t *testing.T) {
		lhs := Variable("LHS")

		ok, err := DefaultFunctionSet.NotEqual(lhs, Integer(1), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(lhs), err)
		assert.False(t, ok)
	})

	t.Run("rhs is a variable", func(t *testing.T) {
		rhs := Variable("RHS")

		ok, err := DefaultFunctionSet.NotEqual(Integer(1), rhs, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(rhs), err)
		assert.False(t, ok)
	})
}

func TestFunctionSet_LessThan(t *testing.T) {
	ok, err := DefaultFunctionSet.LessThan(Integer(1), Integer(2), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.LessThan(Float(1), Integer(2), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.LessThan(Integer(1), Float(2), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.LessThan(Float(1), Float(2), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)
}

func TestFunctionSet_GreaterThan(t *testing.T) {
	ok, err := DefaultFunctionSet.GreaterThan(Integer(2), Integer(1), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.GreaterThan(Float(2), Integer(1), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.GreaterThan(Integer(2), Float(1), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.GreaterThan(Float(2), Float(1), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)
}

func TestFunctionSet_LessThanOrEqual(t *testing.T) {
	ok, err := DefaultFunctionSet.LessThanOrEqual(Integer(1), Integer(2), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.LessThanOrEqual(Float(1), Integer(2), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.LessThanOrEqual(Integer(1), Float(2), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.LessThanOrEqual(Float(1), Float(2), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)
}

func TestFunctionSet_GreaterThanOrEqual(t *testing.T) {
	ok, err := DefaultFunctionSet.GreaterThanOrEqual(Integer(2), Integer(1), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.GreaterThanOrEqual(Float(2), Integer(1), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.GreaterThanOrEqual(Integer(2), Float(1), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultFunctionSet.GreaterThanOrEqual(Float(2), Float(1), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)
}

func TestState_StreamProperty(t *testing.T) {
	f, err := ioutil.TempFile("", "")
	assert.NoError(t, err)

	defer func() {
		assert.NoError(t, os.Remove(f.Name()))
	}()

	t.Run("stream", func(t *testing.T) {
		expected := []Term{
			&Compound{Functor: "mode", Args: []Term{Atom("read")}},
			Atom("input"),
			&Compound{Functor: "alias", Args: []Term{Atom("null")}},
			&Compound{Functor: "eof_action", Args: []Term{Atom("eof_code")}},
			&Compound{Functor: "file_name", Args: []Term{Atom(f.Name())}},
			&Compound{Functor: "position", Args: []Term{Integer(0)}},
			&Compound{Functor: "end_of_stream", Args: []Term{Atom("at")}},
			&Compound{Functor: "reposition", Args: []Term{Atom("true")}},
			&Compound{Functor: "type", Args: []Term{Atom("text")}},
		}

		s, err := Open(Atom(f.Name()), StreamModeRead)
		assert.NoError(t, err)
		s.alias = "null"
		defer func() {
			assert.NoError(t, s.Close())
		}()

		v := Variable("V")
		c := 0
		var state State
		ok, err := state.StreamProperty(s, v, func(env *Env) *Promise {
			assert.Equal(t, expected[c], env.Resolve(v))
			c++
			return Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("reposition false", func(t *testing.T) {
		expected := []Term{
			&Compound{Functor: "mode", Args: []Term{Atom("read")}},
			Atom("input"),
			&Compound{Functor: "alias", Args: []Term{Atom("null")}},
			&Compound{Functor: "eof_action", Args: []Term{Atom("eof_code")}},
			&Compound{Functor: "file_name", Args: []Term{Atom(f.Name())}},
			&Compound{Functor: "position", Args: []Term{Integer(0)}},
			&Compound{Functor: "end_of_stream", Args: []Term{Atom("at")}},
			&Compound{Functor: "reposition", Args: []Term{Atom("false")}},
			&Compound{Functor: "type", Args: []Term{Atom("text")}},
		}

		s, err := Open(Atom(f.Name()), StreamModeRead)
		s.reposition = false
		assert.NoError(t, err)
		s.alias = "null"
		defer func() {
			assert.NoError(t, s.Close())
		}()

		v := Variable("V")
		c := 0
		var state State
		ok, err := state.StreamProperty(s, v, func(env *Env) *Promise {
			assert.Equal(t, expected[c], env.Resolve(v))
			c++
			return Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("stream alias", func(t *testing.T) {
		expected := []Term{
			&Compound{Functor: "mode", Args: []Term{Atom("write")}},
			Atom("output"),
			&Compound{Functor: "alias", Args: []Term{Atom("null")}},
			&Compound{Functor: "eof_action", Args: []Term{Atom("eof_code")}},
			&Compound{Functor: "file_name", Args: []Term{Atom(f.Name())}},
			&Compound{Functor: "position", Args: []Term{Integer(0)}},
			&Compound{Functor: "end_of_stream", Args: []Term{Atom("at")}},
			&Compound{Functor: "reposition", Args: []Term{Atom("true")}},
			&Compound{Functor: "type", Args: []Term{Atom("text")}},
		}

		s, err := Open(Atom(f.Name()), StreamModeWrite)
		assert.NoError(t, err)
		s.alias = "null"
		defer func() {
			assert.NoError(t, s.Close())
		}()

		state := State{
			streams: map[Term]*Stream{
				Atom("null"): s,
			},
		}
		v := Variable("V")
		c := 0
		ok, err := state.StreamProperty(Atom("null"), v, func(env *Env) *Promise {
			assert.Equal(t, expected[c], env.Resolve(v))
			c++
			return Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("correct property value", func(t *testing.T) {
		t.Run("input", func(t *testing.T) {
			s, err := Open(Atom(f.Name()), StreamModeRead)
			assert.NoError(t, err)
			defer func() {
				assert.NoError(t, s.Close())
			}()

			var state State
			ok, err := state.StreamProperty(s, Atom("input"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("mode", func(t *testing.T) {
			s, err := Open(Atom(f.Name()), StreamModeRead)
			assert.NoError(t, err)
			defer func() {
				assert.NoError(t, s.Close())
			}()

			var state State
			ok, err := state.StreamProperty(s, &Compound{
				Functor: "mode",
				Args:    []Term{Atom("read")},
			}, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("position", func(t *testing.T) {
			s, err := Open(Atom(f.Name()), StreamModeRead)
			assert.NoError(t, err)
			defer func() {
				assert.NoError(t, s.Close())
			}()

			var state State
			ok, err := state.StreamProperty(s, &Compound{
				Functor: "position",
				Args:    []Term{Integer(0)},
			}, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable, a stream-term, nor an alias", func(t *testing.T) {
		var state State
		ok, err := state.StreamProperty(Integer(0), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("property is neither a variable nor a stream property", func(t *testing.T) {
		var state State
		ok, err := state.StreamProperty(NewVariable(), Atom("property"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamProperty(Atom("property")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var state State
		ok, err := state.StreamProperty(Atom("foo"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorStream(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("seek failed", func(t *testing.T) {
		s, err := Open(Atom(f.Name()), StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		seek = func(s io.Seeker, offset int64, whence int) (int64, error) {
			return 0, errors.New("failed")
		}
		defer func() {
			seek = io.Seeker.Seek
		}()

		var state State
		ok, err := state.StreamProperty(s, &Compound{
			Functor: "mode",
			Args:    []Term{Atom("read")},
		}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})

	t.Run("stat failed", func(t *testing.T) {
		s, err := Open(Atom(f.Name()), StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		fileStat = func(f *os.File) (os.FileInfo, error) {
			return nil, errors.New("fialed")
		}
		defer func() {
			fileStat = (*os.File).Stat
		}()

		var state State
		ok, err := state.StreamProperty(s, &Compound{
			Functor: "mode",
			Args:    []Term{Atom("read")},
		}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})

	t.Run("end_of_stream past", func(t *testing.T) {
		s, err := Open(Atom(f.Name()), StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		seek = func(s io.Seeker, offset int64, whence int) (int64, error) {
			return 1000, nil
		}
		defer func() {
			seek = io.Seeker.Seek
		}()

		var state State
		ok, err := state.StreamProperty(s, &Compound{
			Functor: "end_of_stream",
			Args:    []Term{Atom("past")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("unknown atom", func(t *testing.T) {
		s, err := Open(Atom(f.Name()), StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		var state State
		ok, err := state.StreamProperty(s, Atom("foo"), Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})

	t.Run("unknown compound", func(t *testing.T) {
		s, err := Open(Atom(f.Name()), StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		var state State
		ok, err := state.StreamProperty(s, &Compound{Functor: "foo", Args: []Term{NewVariable()}}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})

	t.Run("wrong arity", func(t *testing.T) {
		s, err := Open(Atom(f.Name()), StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		var state State
		ok, err := state.StreamProperty(s, &Compound{Functor: "mode", Args: []Term{NewVariable(), NewVariable()}}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})

	t.Run("integer", func(t *testing.T) {
		s, err := Open(Atom(f.Name()), StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		var state State
		ok, err := state.StreamProperty(s, Integer(0), Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})

	t.Run("non-atom for atom property", func(t *testing.T) {
		s, err := Open(Atom(f.Name()), StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		var state State
		ok, err := state.StreamProperty(s, &Compound{Functor: "mode", Args: []Term{Integer(0)}}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})

	t.Run("non-integer for integer property", func(t *testing.T) {
		s, err := Open(Atom(f.Name()), StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		var state State
		ok, err := state.StreamProperty(s, &Compound{Functor: "position", Args: []Term{Atom("foo")}}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestState_SetStreamPosition(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		s, err := Open("testdata/empty.txt", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		var state State
		ok, err := state.SetStreamPosition(s, Integer(0), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("seek failed", func(t *testing.T) {
		seek = func(f io.Seeker, offset int64, whence int) (int64, error) {
			return 0, errors.New("failed")
		}
		defer func() {
			seek = io.Seeker.Seek
		}()

		s, err := Open("testdata/empty.txt", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		var state State
		ok, err := state.SetStreamPosition(s, Integer(0), Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		streamOrAlias := Variable("Stream")

		var state State
		ok, err := state.SetStreamPosition(streamOrAlias, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("position is a variable", func(t *testing.T) {
		s, err := Open("testdata/empty.txt", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		position := Variable("Pos")

		var state State
		ok, err := state.SetStreamPosition(s, position, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(position), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var state State
		ok, err := state.SetStreamPosition(Integer(2), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorStreamOrAlias(Integer(2)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var state State
		ok, err := state.SetStreamPosition(Atom("foo"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, existenceErrorStream(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream property reposition(false)", func(t *testing.T) {
		stream := NewStream(os.Stdin, StreamModeRead)

		assert.False(t, stream.reposition)

		s := Variable("Stream")
		env := NewEnv().
			Bind(s, stream)

		var state State
		ok, err := state.SetStreamPosition(s, Integer(0), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError("reposition", "stream", s, "Stream is not repositionable."), err)
		assert.False(t, ok)
	})
}

func TestState_CharConversion(t *testing.T) {
	t.Run("register", func(t *testing.T) {
		var state State
		ok, err := state.CharConversion(Atom("a"), Atom("b"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, 'b', state.charConversions['a'])
	})

	t.Run("remove", func(t *testing.T) {
		state := State{
			charConversions: map[rune]rune{
				'a': 'b',
			},
		}
		ok, err := state.CharConversion(Atom("a"), Atom("a"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		_, ok = state.charConversions['a']
		assert.False(t, ok)
	})

	t.Run("inChar is a variable", func(t *testing.T) {
		inChar := Variable("In")

		var state State
		ok, err := state.CharConversion(inChar, Atom("a"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(inChar), err)
		assert.False(t, ok)
	})

	t.Run("outChar is a variable", func(t *testing.T) {
		outChar := Variable("Out")

		var state State
		ok, err := state.CharConversion(Atom("a"), outChar, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(outChar), err)
		assert.False(t, ok)
	})

	t.Run("inChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var state State
			ok, err := state.CharConversion(Integer(0), Atom("a"), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(Atom("character"), Atom("0 is not a character.")), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var state State
			ok, err := state.CharConversion(Atom("foo"), Atom("a"), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(Atom("character"), Atom("foo is not a character.")), err)
			assert.False(t, ok)
		})
	})

	t.Run("outChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var state State
			ok, err := state.CharConversion(Atom("a"), Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(Atom("character"), Atom("0 is not a character.")), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var state State
			ok, err := state.CharConversion(Atom("a"), Atom("foo"), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(Atom("character"), Atom("foo is not a character.")), err)
			assert.False(t, ok)
		})
	})
}

func TestState_CurrentCharConversion(t *testing.T) {
	t.Run("specified", func(t *testing.T) {
		t.Run("as is", func(t *testing.T) {
			var state State
			ok, err := state.CurrentCharConversion(Atom("a"), Atom("a"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("converted", func(t *testing.T) {
			state := State{
				charConversions: map[rune]rune{
					'a': 'b',
				},
			}
			ok, err := state.CurrentCharConversion(Atom("a"), Atom("b"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("not specified", func(t *testing.T) {
		x, y := Variable("X"), Variable("Y")

		var r rune
		var state State
		ok, err := state.CurrentCharConversion(x, y, func(env *Env) *Promise {
			ref, ok := env.Lookup(x)
			assert.True(t, ok)
			x, ok := ref.(Atom)
			assert.True(t, ok)
			assert.Len(t, []rune(x), 1)

			ref, ok = env.Lookup(y)
			assert.True(t, ok)
			y, ok := ref.(Atom)
			assert.True(t, ok)
			assert.Len(t, []rune(y), 1)

			assert.Equal(t, r, []rune(x)[0])
			assert.Equal(t, r, []rune(y)[0])
			r++
			return Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
		assert.Equal(t, rune(256), r)
	})

	t.Run("inChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var state State
			ok, err := state.CurrentCharConversion(Integer(0), Atom("b"), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(Atom("character"), Atom("0 is not a character.")), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var state State
			ok, err := state.CurrentCharConversion(Atom("foo"), Atom("b"), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(Atom("character"), Atom("foo is not a character.")), err)
			assert.False(t, ok)
		})
	})

	t.Run("outChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var state State
			ok, err := state.CurrentCharConversion(Atom("a"), Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(Atom("character"), Atom("0 is not a character.")), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var state State
			ok, err := state.CurrentCharConversion(Atom("a"), Atom("bar"), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(Atom("character"), Atom("bar is not a character.")), err)
			assert.False(t, ok)
		})
	})
}

func TestState_SetPrologFlag(t *testing.T) {
	t.Run("bounded", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(Atom("bounded"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError("modify", "flag", Atom("bounded"), "bounded is not modifiable."), err)
		assert.False(t, ok)
	})

	t.Run("max_integer", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(Atom("max_integer"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError("modify", "flag", Atom("max_integer"), "max_integer is not modifiable."), err)
		assert.False(t, ok)
	})

	t.Run("min_integer", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(Atom("min_integer"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError("modify", "flag", Atom("min_integer"), "min_integer is not modifiable."), err)
		assert.False(t, ok)
	})

	t.Run("integer_rounding_function", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(Atom("integer_rounding_function"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError("modify", "flag", Atom("integer_rounding_function"), "integer_rounding_function is not modifiable."), err)
		assert.False(t, ok)
	})

	t.Run("char_conversion", func(t *testing.T) {
		t.Run("on", func(t *testing.T) {
			var state State
			ok, err := state.SetPrologFlag(Atom("char_conversion"), Atom("on"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.True(t, state.charConvEnabled)
		})

		t.Run("off", func(t *testing.T) {
			state := State{charConvEnabled: true}
			ok, err := state.SetPrologFlag(Atom("char_conversion"), Atom("off"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.False(t, state.charConvEnabled)
		})

		t.Run("unknown", func(t *testing.T) {
			state := State{charConvEnabled: true}
			ok, err := state.SetPrologFlag(Atom("char_conversion"), Atom("foo"), Success, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("debug", func(t *testing.T) {
		t.Run("on", func(t *testing.T) {
			var state State
			ok, err := state.SetPrologFlag(Atom("debug"), Atom("on"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.True(t, state.debug)
		})

		t.Run("off", func(t *testing.T) {
			state := State{debug: true}
			ok, err := state.SetPrologFlag(Atom("debug"), Atom("off"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.False(t, state.debug)
		})

		t.Run("unknown", func(t *testing.T) {
			state := State{debug: true}
			ok, err := state.SetPrologFlag(Atom("debug"), Atom("foo"), Success, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("max_arity", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(Atom("max_arity"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError("modify", "flag", Atom("max_arity"), "max_arity is not modifiable."), err)
		assert.False(t, ok)
	})

	t.Run("unknown", func(t *testing.T) {
		t.Run("error", func(t *testing.T) {
			state := State{VM: VM{unknown: unknownFail}}
			ok, err := state.SetPrologFlag(Atom("unknown"), Atom("error"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, unknownError, state.unknown)
		})

		t.Run("warning", func(t *testing.T) {
			var state State
			ok, err := state.SetPrologFlag(Atom("unknown"), Atom("warning"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, unknownWarning, state.unknown)
		})

		t.Run("fail", func(t *testing.T) {
			var state State
			ok, err := state.SetPrologFlag(Atom("unknown"), Atom("fail"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, unknownFail, state.unknown)
		})

		t.Run("fail", func(t *testing.T) {
			var state State
			ok, err := state.SetPrologFlag(Atom("unknown"), Atom("foo"), Success, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("double_quotes", func(t *testing.T) {
		t.Run("codes", func(t *testing.T) {
			var state State
			ok, err := state.SetPrologFlag(Atom("double_quotes"), Atom("codes"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, doubleQuotesCodes, state.doubleQuotes)
		})

		t.Run("chars", func(t *testing.T) {
			var state State
			ok, err := state.SetPrologFlag(Atom("double_quotes"), Atom("chars"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, doubleQuotesChars, state.doubleQuotes)
		})

		t.Run("atom", func(t *testing.T) {
			var state State
			ok, err := state.SetPrologFlag(Atom("double_quotes"), Atom("atom"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, doubleQuotesAtom, state.doubleQuotes)
		})

		t.Run("unknown", func(t *testing.T) {
			var state State
			ok, err := state.SetPrologFlag(Atom("double_quotes"), Atom("foo"), Success, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("flag is a variable", func(t *testing.T) {
		flag := Variable("Flag")

		var state State
		ok, err := state.SetPrologFlag(flag, Atom("fail"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(flag), err)
		assert.False(t, ok)
	})

	t.Run("value is a variable", func(t *testing.T) {
		value := Variable("Value")

		var state State
		ok, err := state.SetPrologFlag(Atom("unknown"), value, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(value), err)
		assert.False(t, ok)
	})

	t.Run("flag is neither a variable nor an atom", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(Integer(0), Atom("fail"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("flag is an atom but an invalid flag for the processor", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(Atom("foo"), Atom("fail"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorPrologFlag(Atom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("value is inadmissible for flag", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(Atom("unknown"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorFlagValue(&Compound{
			Functor: "+",
			Args:    []Term{Atom("unknown"), Integer(0)},
		}), err)
		assert.False(t, ok)
	})

	t.Run("value is admissible for flag but the flag is not modifiable", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(Atom("bounded"), Atom("true"), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError("modify", "flag", Atom("bounded"), "bounded is not modifiable."), err)
		assert.False(t, ok)
	})
}

func TestState_CurrentPrologFlag(t *testing.T) {
	var state State

	t.Run("specified", func(t *testing.T) {
		ok, err := state.CurrentPrologFlag(Atom("bounded"), Atom("true"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.CurrentPrologFlag(Atom("max_integer"), Integer(math.MaxInt64), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.CurrentPrologFlag(Atom("min_integer"), Integer(math.MinInt64), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.CurrentPrologFlag(Atom("integer_rounding_function"), Atom("toward_zero"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.CurrentPrologFlag(Atom("char_conversion"), Atom("off"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.CurrentPrologFlag(Atom("debug"), Atom("off"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.CurrentPrologFlag(Atom("max_arity"), Atom("unbounded"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.CurrentPrologFlag(Atom("unknown"), Atom("error"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not specified", func(t *testing.T) {
		flag, value := Variable("Flag"), Variable("Value")
		var c int
		ok, err := state.CurrentPrologFlag(flag, value, func(env *Env) *Promise {
			switch c {
			case 0:
				assert.Equal(t, Atom("bounded"), env.Resolve(flag))
				assert.Equal(t, Atom("true"), env.Resolve(value))
			case 1:
				assert.Equal(t, Atom("max_integer"), env.Resolve(flag))
				assert.Equal(t, Integer(math.MaxInt64), env.Resolve(value))
			case 2:
				assert.Equal(t, Atom("min_integer"), env.Resolve(flag))
				assert.Equal(t, Integer(math.MinInt64), env.Resolve(value))
			case 3:
				assert.Equal(t, Atom("integer_rounding_function"), env.Resolve(flag))
				assert.Equal(t, Atom("toward_zero"), env.Resolve(value))
			case 4:
				assert.Equal(t, Atom("char_conversion"), env.Resolve(flag))
				assert.Equal(t, Atom("off"), env.Resolve(value))
			case 5:
				assert.Equal(t, Atom("debug"), env.Resolve(flag))
				assert.Equal(t, Atom("off"), env.Resolve(value))
			case 6:
				assert.Equal(t, Atom("max_arity"), env.Resolve(flag))
				assert.Equal(t, Atom("unbounded"), env.Resolve(value))
			case 7:
				assert.Equal(t, Atom("unknown"), env.Resolve(flag))
				assert.Equal(t, Atom(state.unknown.String()), env.Resolve(value))
			case 8:
				assert.Equal(t, Atom("double_quotes"), env.Resolve(flag))
				assert.Equal(t, Atom(state.doubleQuotes.String()), env.Resolve(value))
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
		assert.Equal(t, 9, c)
	})

	t.Run("flag is neither a variable nor an atom", func(t *testing.T) {
		var state State
		ok, err := state.CurrentPrologFlag(Integer(0), Atom("error"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorAtom(Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("flag is an atom but an invalid flag for the processor", func(t *testing.T) {
		var state State
		ok, err := state.CurrentPrologFlag(Atom("foo"), Atom("error"), Success, nil).Force(context.Background())
		assert.Equal(t, domainErrorPrologFlag(Atom("foo")), err)
		assert.False(t, ok)
	})
}

func TestState_Dynamic(t *testing.T) {
	t.Run("not a procedure indicator", func(t *testing.T) {
		var state State
		ok, err := state.Dynamic(Atom("foo"), Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})

	t.Run("procedure is not defined", func(t *testing.T) {
		var state State
		ok, err := state.Dynamic(&Compound{
			Functor: "/",
			Args: []Term{
				Atom("foo"),
				Integer(1),
			},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.IsType(t, clauses{}, state.procedures[ProcedureIndicator{Name: "foo", Arity: 1}])
	})

	t.Run("procedure is already dynamic", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 1}: clauses{},
				},
			},
		}
		ok, err := state.Dynamic(&Compound{
			Functor: "/",
			Args: []Term{
				Atom("foo"),
				Integer(1),
			},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("procedure is already static", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 1}: static{},
				},
			},
		}
		ok, err := state.Dynamic(&Compound{
			Functor: "/",
			Args: []Term{
				Atom("foo"),
				Integer(1),
			},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, permissionErrorModifyStaticProcedure(&Compound{
			Functor: "/",
			Args: []Term{
				Atom("foo"),
				Integer(1),
			},
		}), err)
		assert.False(t, ok)
	})
}

func TestState_BuiltIn(t *testing.T) {
	t.Run("not a procedure indicator", func(t *testing.T) {
		var state State
		ok, err := state.BuiltIn(Atom("foo"), Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})

	t.Run("procedure is not defined", func(t *testing.T) {
		var state State
		ok, err := state.BuiltIn(&Compound{
			Functor: "/",
			Args: []Term{
				Atom("foo"),
				Integer(1),
			},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.IsType(t, builtin{}, state.procedures[ProcedureIndicator{Name: "foo", Arity: 1}])
	})

	t.Run("procedure is already built-in", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 1}: builtin{},
				},
			},
		}
		ok, err := state.BuiltIn(&Compound{
			Functor: "/",
			Args: []Term{
				Atom("foo"),
				Integer(1),
			},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("procedure is already dynamic", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 1}: clauses{},
				},
			},
		}
		ok, err := state.BuiltIn(&Compound{
			Functor: "/",
			Args: []Term{
				Atom("foo"),
				Integer(1),
			},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, permissionErrorModifyStaticProcedure(&Compound{
			Functor: "/",
			Args: []Term{
				Atom("foo"),
				Integer(1),
			},
		}), err)
		assert.False(t, ok)
	})
}

func TestState_ExpandTerm(t *testing.T) {
	t.Run("term_expansion/2 is undefined", func(t *testing.T) {
		var state State
		ok, err := state.ExpandTerm(&Compound{
			Functor: "f",
			Args:    []Term{Atom("a")},
		}, &Compound{
			Functor: "f",
			Args:    []Term{Atom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("term_expansion/2 is defined", func(t *testing.T) {
		t.Run("not applicable", func(t *testing.T) {
			state := State{
				VM: VM{
					procedures: map[ProcedureIndicator]procedure{
						{Name: "term_expansion", Arity: 2}: predicate2(func(Term, Term, func(*Env) *Promise, *Env) *Promise {
							return Bool(false)
						}),
					},
				},
			}
			ok, err := state.ExpandTerm(&Compound{
				Functor: "f",
				Args:    []Term{Atom("a")},
			}, &Compound{
				Functor: "f",
				Args:    []Term{Atom("a")},
			}, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("applicable", func(t *testing.T) {
			state := State{
				VM: VM{
					procedures: map[ProcedureIndicator]procedure{
						{Name: "term_expansion", Arity: 2}: predicate2(func(t1, t2 Term, k func(*Env) *Promise, env *Env) *Promise {
							return Unify(t2, &Compound{
								Functor: "g",
								Args:    []Term{Atom("b")},
							}, k, env)
						}),
					},
				},
			}
			ok, err := state.ExpandTerm(&Compound{
				Functor: "f",
				Args:    []Term{Atom("a")},
			}, &Compound{
				Functor: "g",
				Args:    []Term{Atom("b")},
			}, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})
}

func TestEnviron(t *testing.T) {
	os.Clearenv()
	assert.NoError(t, os.Setenv("FOO", "foo"))
	assert.NoError(t, os.Setenv("BAR", "bar"))

	var (
		count = 0
		key   = Variable("Key")
		value = Variable("Value")
	)
	ok, err := Environ(key, value, func(env *Env) *Promise {
		count++
		switch count {
		case 1:
			assert.Equal(t, Atom("FOO"), env.Resolve(key))
			assert.Equal(t, Atom("foo"), env.Resolve(value))
		case 2:
			assert.Equal(t, Atom("BAR"), env.Resolve(key))
			assert.Equal(t, Atom("bar"), env.Resolve(value))
		default:
			assert.Fail(t, "unreachable")
		}
		return Bool(true)
	}, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)
}

func TestState_Expand(t *testing.T) {
	var state State

	t.Run("DCG", func(t *testing.T) {
		t.Run("empty terminal-sequence", func(t *testing.T) {
			varCounter = 0
			term, err := state.Expand(&Compound{Functor: "-->", Args: []Term{Atom("s"), List()}}, nil)
			assert.NoError(t, err)
			assert.Equal(t, &Compound{Functor: ":-", Args: []Term{
				&Compound{Functor: "s", Args: []Term{Variable("_1"), Variable("_3")}},
				&Compound{Functor: "=", Args: []Term{Variable("_1"), Variable("_3")}},
			}}, term)
		})

		t.Run("terminal sequence", func(t *testing.T) {
			varCounter = 0
			term, err := state.Expand(&Compound{Functor: "-->", Args: []Term{Atom("s"), List(Atom("a"))}}, nil)
			assert.NoError(t, err)
			assert.Equal(t, &Compound{Functor: ":-", Args: []Term{
				&Compound{Functor: "s", Args: []Term{Variable("_1"), Variable("_3")}},
				&Compound{Functor: "=", Args: []Term{Variable("_1"), ListRest(Variable("_3"), Atom("a"))}},
			}}, term)
		})

		t.Run("concatenation", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				varCounter = 0
				term, err := state.Expand(&Compound{Functor: "-->", Args: []Term{Atom("s"), Seq(",", Atom("a"), Atom("b"))}}, nil)
				assert.NoError(t, err)
				assert.Equal(t, &Compound{Functor: ":-", Args: []Term{
					&Compound{Functor: "s", Args: []Term{Variable("_1"), Variable("_3")}},
					Seq(",",
						&Compound{Functor: "a", Args: []Term{Variable("_1"), Variable("_4")}},
						&Compound{Functor: "b", Args: []Term{Variable("_4"), Variable("_3")}},
					),
				}}, term)
			})

			t.Run("lhs is not callable", func(t *testing.T) {
				varCounter = 0
				_, err := state.Expand(&Compound{Functor: "-->", Args: []Term{Atom("s"), Seq(",", Integer(0), Atom("b"))}}, nil)
				assert.Error(t, err)
			})

			t.Run("rhs is not callable", func(t *testing.T) {
				varCounter = 0
				_, err := state.Expand(&Compound{Functor: "-->", Args: []Term{Atom("s"), Seq(",", Atom("a"), Integer(0))}}, nil)
				assert.Error(t, err)
			})
		})

		t.Run("alternative", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				t.Run("normal", func(t *testing.T) {
					varCounter = 0
					term, err := state.Expand(&Compound{Functor: "-->", Args: []Term{Atom("s"), Seq(";", Atom("a"), Atom("b"))}}, nil)
					assert.NoError(t, err)
					assert.Equal(t, &Compound{Functor: ":-", Args: []Term{
						&Compound{Functor: "s", Args: []Term{Variable("_1"), Variable("_3")}},
						Seq(";",
							&Compound{Functor: "a", Args: []Term{Variable("_1"), Variable("_3")}},
							&Compound{Functor: "b", Args: []Term{Variable("_1"), Variable("_3")}},
						),
					}}, term)
				})

				t.Run("if-then-else", func(t *testing.T) {
					varCounter = 0
					term, err := state.Expand(&Compound{Functor: "-->", Args: []Term{Atom("s"), Seq(";", &Compound{Functor: "->", Args: []Term{Atom("a"), Atom("b")}}, Atom("c"))}}, nil)
					assert.NoError(t, err)
					assert.Equal(t, &Compound{Functor: ":-", Args: []Term{
						&Compound{Functor: "s", Args: []Term{Variable("_1"), Variable("_3")}},
						Seq(";",
							&Compound{Functor: "->", Args: []Term{
								&Compound{Functor: "a", Args: []Term{Variable("_1"), Variable("_4")}},
								&Compound{Functor: "b", Args: []Term{Variable("_4"), Variable("_3")}},
							}},
							&Compound{Functor: "c", Args: []Term{Variable("_1"), Variable("_3")}},
						),
					}}, term)
				})
			})

			t.Run("lhs is not callable", func(t *testing.T) {
				varCounter = 0
				_, err := state.Expand(&Compound{Functor: "-->", Args: []Term{Atom("s"), Seq(";", Integer(0), Atom("b"))}}, nil)
				assert.Error(t, err)
			})

			t.Run("rhs is not callable", func(t *testing.T) {
				varCounter = 0
				_, err := state.Expand(&Compound{Functor: "-->", Args: []Term{Atom("s"), Seq(";", Atom("a"), Integer(0))}}, nil)
				assert.Error(t, err)
			})
		})

		t.Run("second form of alternative", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				varCounter = 0
				term, err := state.Expand(&Compound{Functor: "-->", Args: []Term{Atom("s"), Seq("|", Atom("a"), Atom("b"))}}, nil)
				assert.NoError(t, err)
				assert.Equal(t, &Compound{Functor: ":-", Args: []Term{
					&Compound{Functor: "s", Args: []Term{Variable("_1"), Variable("_3")}},
					Seq(";",
						&Compound{Functor: "a", Args: []Term{Variable("_1"), Variable("_3")}},
						&Compound{Functor: "b", Args: []Term{Variable("_1"), Variable("_3")}},
					),
				}}, term)
			})

			t.Run("lhs is not callable", func(t *testing.T) {
				varCounter = 0
				_, err := state.Expand(&Compound{Functor: "-->", Args: []Term{Atom("s"), Seq("|", Integer(0), Atom("b"))}}, nil)
				assert.Error(t, err)
			})

			t.Run("rhs is not callable", func(t *testing.T) {
				varCounter = 0
				_, err := state.Expand(&Compound{Functor: "-->", Args: []Term{Atom("s"), Seq("|", Atom("a"), Integer(0))}}, nil)
				assert.Error(t, err)
			})
		})

		t.Run("grammar-body-goal", func(t *testing.T) {
			varCounter = 0
			term, err := state.Expand(&Compound{Functor: "-->", Args: []Term{Atom("s"), &Compound{Functor: "{}", Args: []Term{Atom("a")}}}}, nil)
			assert.NoError(t, err)
			assert.Equal(t, &Compound{Functor: ":-", Args: []Term{
				&Compound{Functor: "s", Args: []Term{Variable("_1"), Variable("_3")}},
				Seq(",",
					Atom("a"),
					&Compound{Functor: "=", Args: []Term{Variable("_1"), Variable("_3")}},
				),
			}}, term)
		})

		t.Run("call", func(t *testing.T) {
			varCounter = 0
			term, err := state.Expand(&Compound{Functor: "-->", Args: []Term{Atom("s"), &Compound{Functor: "call", Args: []Term{Atom("a")}}}}, nil)
			assert.NoError(t, err)
			assert.Equal(t, &Compound{Functor: ":-", Args: []Term{
				&Compound{Functor: "s", Args: []Term{Variable("_1"), Variable("_3")}},
				&Compound{Functor: "call", Args: []Term{Atom("a"), Variable("_1"), Variable("_3")}},
			}}, term)
		})

		t.Run("phrase", func(t *testing.T) {
			varCounter = 0
			term, err := state.Expand(&Compound{Functor: "-->", Args: []Term{Atom("s"), &Compound{Functor: "phrase", Args: []Term{Atom("a")}}}}, nil)
			assert.NoError(t, err)
			assert.Equal(t, &Compound{Functor: ":-", Args: []Term{
				&Compound{Functor: "s", Args: []Term{Variable("_1"), Variable("_3")}},
				&Compound{Functor: "phrase", Args: []Term{Atom("a"), Variable("_1"), Variable("_3")}},
			}}, term)
		})

		t.Run("grammar-body-cut", func(t *testing.T) {
			varCounter = 0
			term, err := state.Expand(&Compound{Functor: "-->", Args: []Term{Atom("s"), Atom("!")}}, nil)
			assert.NoError(t, err)
			assert.Equal(t, &Compound{Functor: ":-", Args: []Term{
				&Compound{Functor: "s", Args: []Term{Variable("_1"), Variable("_3")}},
				Seq(",",
					Atom("!"),
					&Compound{Functor: "=", Args: []Term{Variable("_1"), Variable("_3")}},
				),
			}}, term)
		})

		t.Run("grammar-body-not", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				varCounter = 0
				term, err := state.Expand(&Compound{Functor: "-->", Args: []Term{Atom("s"), &Compound{Functor: `\+`, Args: []Term{Atom("a")}}}}, nil)
				assert.NoError(t, err)
				assert.Equal(t, &Compound{Functor: ":-", Args: []Term{
					&Compound{Functor: "s", Args: []Term{Variable("_1"), Variable("_3")}},
					Seq(",",
						&Compound{Functor: `\+`, Args: []Term{&Compound{Functor: "a", Args: []Term{Variable("_1"), Variable("_4")}}}},
						&Compound{Functor: "=", Args: []Term{Variable("_1"), Variable("_3")}},
					),
				}}, term)
			})

			t.Run("goal is not callable", func(t *testing.T) {
				varCounter = 0
				_, err := state.Expand(&Compound{Functor: "-->", Args: []Term{Atom("s"), &Compound{Functor: `\+`, Args: []Term{Integer(0)}}}}, nil)
				assert.Error(t, err)
			})
		})

		t.Run("if-then", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				varCounter = 0
				term, err := state.Expand(&Compound{Functor: "-->", Args: []Term{Atom("s"), &Compound{Functor: "->", Args: []Term{Atom("a"), Atom("b")}}}}, nil)
				assert.NoError(t, err)
				assert.Equal(t, &Compound{Functor: ":-", Args: []Term{
					&Compound{Functor: "s", Args: []Term{Variable("_1"), Variable("_3")}},
					&Compound{
						Functor: "->",
						Args: []Term{
							&Compound{Functor: "a", Args: []Term{Variable("_1"), Variable("_4")}},
							&Compound{Functor: "b", Args: []Term{Variable("_4"), Variable("_3")}},
						},
					},
				}}, term)
			})

			t.Run("lhs is not callable", func(t *testing.T) {
				varCounter = 0
				_, err := state.Expand(&Compound{Functor: "-->", Args: []Term{Atom("s"), &Compound{Functor: "->", Args: []Term{Integer(0), Atom("b")}}}}, nil)
				assert.Error(t, err)
			})

			t.Run("rhs is not callable", func(t *testing.T) {
				varCounter = 0
				_, err := state.Expand(&Compound{Functor: "-->", Args: []Term{Atom("s"), &Compound{Functor: "->", Args: []Term{Atom("a"), Integer(0)}}}}, nil)
				assert.Error(t, err)
			})
		})

		t.Run("with semicontexts", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				varCounter = 0
				term, err := state.Expand(Atom("-->").Apply(Atom(",").Apply(Atom("phrase1"), List(Atom("word"))), Atom(",").Apply(Atom("phrase2"), Atom("phrase3"))), nil)
				assert.NoError(t, err)
				assert.Equal(t, &Compound{
					Functor: ":-",
					Args: []Term{
						&Compound{Functor: "phrase1", Args: []Term{Variable("_1"), Variable("_3")}},
						&Compound{
							Functor: ",",
							Args: []Term{
								&Compound{
									Functor: ",",
									Args: []Term{
										&Compound{Functor: "phrase2", Args: []Term{Variable("_1"), Variable("_4")}},
										&Compound{Functor: "phrase3", Args: []Term{Variable("_4"), Variable("_2")}},
									},
								},
								&Compound{Functor: "=", Args: []Term{Variable("_3"), ListRest(Variable("_2"), Atom("word"))}},
							},
						},
					},
				}, term)
			})

			t.Run("head is not callable", func(t *testing.T) {
				varCounter = 0
				_, err := state.Expand(Atom("-->").Apply(Atom(",").Apply(Integer(0), List(Atom("word"))), Atom(",").Apply(Atom("phrase2"), Atom("phrase3"))), nil)
				assert.Error(t, err)
			})

			t.Run("semicontext is not callable", func(t *testing.T) {
				varCounter = 0
				_, err := state.Expand(Atom("-->").Apply(Atom(",").Apply(Atom("phrase1"), Integer(0)), Atom(",").Apply(Atom("phrase2"), Atom("phrase3"))), nil)
				assert.Error(t, err)
			})

			t.Run("body is not callable", func(t *testing.T) {
				varCounter = 0
				_, err := state.Expand(Atom("-->").Apply(Atom(",").Apply(Atom("phrase1"), List(Atom("word"))), Atom(",").Apply(Integer(0), Atom("phrase3"))), nil)
				assert.Error(t, err)
			})
		})
	})
}
