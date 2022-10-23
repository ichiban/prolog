package engine

import (
	"bufio"
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"math"
	"os"
	"path/filepath"
	"regexp"
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
	state.Register0("fail", func(f func(*Env) *Promise, env *Env) *Promise {
		return Bool(false)
	})
	assert.NoError(t, state.Compile(context.Background(), `
foo.
foo(_, _).
f(g([a, [b|X]])).
`))

	tests := []struct {
		title string
		goal  Term
		ok    bool
		err   error
	}{
		// TODO: redo test cases based on 7.8.3.4 Examples
		{title: `undefined atom`, goal: Atom("bar"), ok: false, err: ExistenceError(ObjectTypeProcedure, Atom("/").Apply(Atom("bar"), Integer(0)), nil)},
		{title: `defined atom`, goal: Atom("foo"), ok: true},
		{title: `undefined compound`, goal: Atom("bar").Apply(NewVariable(), NewVariable()), ok: false, err: ExistenceError(ObjectTypeProcedure, Atom("/").Apply(Atom("bar"), Integer(2)), nil)},
		{title: `defined compound`, goal: Atom("foo").Apply(NewVariable(), NewVariable()), ok: true},
		{title: `variable: single predicate`, goal: NewNamedVariable("X"), ok: false, err: InstantiationError(nil)},
		{title: `variable: multiple predicates`, goal: Atom(",").Apply(Atom("fail"), NewNamedVariable("X")), ok: false},
		{title: `not callable: single predicate`, goal: Integer(0), ok: false, err: TypeError(ValidTypeCallable, Integer(0), nil)},
		{title: `not callable: conjunction`, goal: Atom(",").Apply(Atom("true"), Integer(0)), ok: false, err: TypeError(ValidTypeCallable, Atom(",").Apply(Atom("true"), Integer(0)), nil)},
		{title: `not callable: disjunction`, goal: Atom(";").Apply(Integer(1), Atom("true")), ok: false, err: TypeError(ValidTypeCallable, Atom(";").Apply(Integer(1), Atom("true")), nil)},

		{title: `cover all`, goal: Atom(",").Apply(Atom("!"), Atom("f").Apply(Atom("g").Apply(List(Atom("a"), ListRest(NewNamedVariable("X"), Atom("b")))))), ok: true},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := state.Call(tt.goal, Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestState_Call1(t *testing.T) {
	tests := []struct {
		title      string
		closure    Term
		additional [1]Term
		ok         bool
		err        error
	}{
		{title: "ok", closure: Atom("p").Apply(Atom("a")), additional: [1]Term{Atom("b")}, ok: true},
		{title: "closure is a variable", closure: NewNamedVariable("P"), additional: [1]Term{Atom("b")}, err: InstantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [1]Term{Atom("b")}, err: TypeError(ValidTypeCallable, Integer(3), nil)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			state := State{VM: VM{procedures: map[ProcedureIndicator]procedure{
				{Name: "p", Arity: 2}: predicate2(func(_, _ Term, k func(*Env) *Promise, env *Env) *Promise {
					return k(env)
				}),
			}}}
			ok, err := state.Call1(tt.closure, tt.additional[0], Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestState_Call2(t *testing.T) {
	tests := []struct {
		title      string
		closure    Term
		additional [2]Term
		ok         bool
		err        error
	}{
		{title: "ok", closure: Atom("p").Apply(Atom("a")), additional: [2]Term{Atom("b"), Atom("c")}, ok: true},
		{title: "closure is a variable", closure: NewNamedVariable("P"), additional: [2]Term{Atom("b"), Atom("c")}, err: InstantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [2]Term{Atom("b"), Atom("c")}, err: TypeError(ValidTypeCallable, Integer(3), nil)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			state := State{VM: VM{procedures: map[ProcedureIndicator]procedure{
				{Name: "p", Arity: 3}: predicate3(func(_, _, _ Term, k func(*Env) *Promise, env *Env) *Promise {
					return k(env)
				}),
			}}}
			ok, err := state.Call2(tt.closure, tt.additional[0], tt.additional[1], Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestState_Call3(t *testing.T) {
	tests := []struct {
		title      string
		closure    Term
		additional [3]Term
		ok         bool
		err        error
	}{
		{title: "ok", closure: Atom("p").Apply(Atom("a")), additional: [3]Term{Atom("b"), Atom("c"), Atom("d")}, ok: true},
		{title: "closure is a variable", closure: NewNamedVariable("P"), additional: [3]Term{Atom("b"), Atom("c"), Atom("d")}, err: InstantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [3]Term{Atom("b"), Atom("c"), Atom("d")}, err: TypeError(ValidTypeCallable, Integer(3), nil)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			state := State{VM: VM{procedures: map[ProcedureIndicator]procedure{
				{Name: "p", Arity: 4}: predicate4(func(_, _, _, _ Term, k func(*Env) *Promise, env *Env) *Promise {
					return k(env)
				}),
			}}}
			ok, err := state.Call3(tt.closure, tt.additional[0], tt.additional[1], tt.additional[2], Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestState_Call4(t *testing.T) {
	tests := []struct {
		title      string
		closure    Term
		additional [4]Term
		ok         bool
		err        error
	}{
		{title: "ok", closure: Atom("p").Apply(Atom("a")), additional: [4]Term{Atom("b"), Atom("c"), Atom("d"), Atom("e")}, ok: true},
		{title: "closure is a variable", closure: NewNamedVariable("P"), additional: [4]Term{Atom("b"), Atom("c"), Atom("d"), Atom("e")}, err: InstantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [4]Term{Atom("b"), Atom("c"), Atom("d"), Atom("e")}, err: TypeError(ValidTypeCallable, Integer(3), nil)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			state := State{VM: VM{procedures: map[ProcedureIndicator]procedure{
				{Name: "p", Arity: 5}: predicate5(func(_, _, _, _, _ Term, k func(*Env) *Promise, env *Env) *Promise {
					return k(env)
				}),
			}}}
			ok, err := state.Call4(tt.closure, tt.additional[0], tt.additional[1], tt.additional[2], tt.additional[3], Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestState_Call5(t *testing.T) {
	tests := []struct {
		title      string
		closure    Term
		additional [5]Term
		ok         bool
		err        error
	}{
		{title: "ok", closure: Atom("p").Apply(Atom("a")), additional: [5]Term{Atom("b"), Atom("c"), Atom("d"), Atom("e"), Atom("f")}, ok: true},
		{title: "closure is a variable", closure: NewNamedVariable("P"), additional: [5]Term{Atom("b"), Atom("c"), Atom("d"), Atom("e"), Atom("f")}, err: InstantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [5]Term{Atom("b"), Atom("c"), Atom("d"), Atom("e"), Atom("f")}, err: TypeError(ValidTypeCallable, Integer(3), nil)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			state := State{VM: VM{procedures: map[ProcedureIndicator]procedure{
				{Name: "p", Arity: 6}: predicate6(func(_, _, _, _, _, _ Term, k func(*Env) *Promise, env *Env) *Promise {
					return k(env)
				}),
			}}}
			ok, err := state.Call5(tt.closure, tt.additional[0], tt.additional[1], tt.additional[2], tt.additional[3], tt.additional[4], Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestState_Call6(t *testing.T) {
	tests := []struct {
		title      string
		closure    Term
		additional [6]Term
		ok         bool
		err        error
	}{
		{title: "ok", closure: Atom("p").Apply(Atom("a")), additional: [6]Term{Atom("b"), Atom("c"), Atom("d"), Atom("e"), Atom("f"), Atom("g")}, ok: true},
		{title: "closure is a variable", closure: NewNamedVariable("P"), additional: [6]Term{Atom("b"), Atom("c"), Atom("d"), Atom("e"), Atom("f"), Atom("g")}, err: InstantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [6]Term{Atom("b"), Atom("c"), Atom("d"), Atom("e"), Atom("f"), Atom("g")}, err: TypeError(ValidTypeCallable, Integer(3), nil)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			state := State{VM: VM{procedures: map[ProcedureIndicator]procedure{
				{Name: "p", Arity: 7}: predicate7(func(_, _, _, _, _, _, _ Term, k func(*Env) *Promise, env *Env) *Promise {
					return k(env)
				}),
			}}}
			ok, err := state.Call6(tt.closure, tt.additional[0], tt.additional[1], tt.additional[2], tt.additional[3], tt.additional[4], tt.additional[5], Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestState_Call7(t *testing.T) {
	tests := []struct {
		title      string
		closure    Term
		additional [7]Term
		ok         bool
		err        error
	}{
		{title: "ok", closure: Atom("p").Apply(Atom("a")), additional: [7]Term{Atom("b"), Atom("c"), Atom("d"), Atom("e"), Atom("f"), Atom("g"), Atom("h")}, ok: true},
		{title: "closure is a variable", closure: NewNamedVariable("P"), additional: [7]Term{Atom("b"), Atom("c"), Atom("d"), Atom("e"), Atom("f"), Atom("g"), Atom("h")}, err: InstantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [7]Term{Atom("b"), Atom("c"), Atom("d"), Atom("e"), Atom("f"), Atom("g"), Atom("h")}, err: TypeError(ValidTypeCallable, Integer(3), nil)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			state := State{VM: VM{procedures: map[ProcedureIndicator]procedure{
				{Name: "p", Arity: 8}: predicate8(func(_, _, _, _, _, _, _, _ Term, k func(*Env) *Promise, env *Env) *Promise {
					return k(env)
				}),
			}}}
			ok, err := state.Call7(tt.closure, tt.additional[0], tt.additional[1], tt.additional[2], tt.additional[3], tt.additional[4], tt.additional[5], tt.additional[6], Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestState_CallNth(t *testing.T) {
	state := State{
		VM: VM{
			procedures: map[ProcedureIndicator]procedure{
				{Name: "foo", Arity: 0}: predicate0(func(k func(*Env) *Promise, env *Env) *Promise {
					return Delay(func(context.Context) *Promise {
						return k(env)
					}, func(context.Context) *Promise {
						return k(env)
					}, func(context.Context) *Promise {
						return Error(errors.New("three"))
					})
				}),
			},
		},
	}

	t.Run("ok", func(t *testing.T) {
		t.Run("nth is a variable", func(t *testing.T) {
			var ns []Integer
			ok, err := state.CallNth(Atom("foo"), NewNamedVariable("Nth"), func(env *Env) *Promise {
				n, ok := env.Resolve(NewNamedVariable("Nth")).(Integer)
				assert.True(t, ok)
				ns = append(ns, n)
				switch n {
				case Integer(1):
					return Bool(false)
				case Integer(2):
					return Bool(true)
				default:
					return Error(errors.New("unreachable"))
				}
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)

			assert.Equal(t, []Integer{1, 2}, ns)
		})

		t.Run("nth is an integer", func(t *testing.T) {
			ok, err := state.CallNth(Atom("foo"), Integer(2), Failure, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("nth is 0", func(t *testing.T) {
		ok, err := state.CallNth(Atom("foo"), Integer(0), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("goal is a variable and nth is not zero", func(t *testing.T) {
		_, err := state.CallNth(NewNamedVariable("Goal"), Integer(3), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
	})

	t.Run("goal is neither a variable nor a callable term", func(t *testing.T) {
		_, err := state.CallNth(Integer(0), Integer(3), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeCallable, Integer(0), nil), err)
	})

	t.Run("nth is neither a variable nor an integer", func(t *testing.T) {
		_, err := state.CallNth(Atom("foo"), Atom("bar"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, Atom("bar"), nil), err)
	})

	t.Run("nth is an integer which is less than zero", func(t *testing.T) {
		_, err := state.CallNth(Atom("foo"), Integer(-1), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainNotLessThanZero, Integer(-1), nil), err)
	})

	t.Run("n+1 is larger than max_integer", func(t *testing.T) {
		maxInt = 0
		defer func() {
			maxInt = math.MaxInt64
		}()
		_, err := state.CallNth(Atom("foo"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, RepresentationError(FlagMaxInteger, nil), err)
	})
}

func TestUnify(t *testing.T) {
	x, y := NewNamedVariable("X"), NewNamedVariable("Y")
	tests := []struct {
		title   string
		premise *Env
		x, y    Term
		ok      bool
		err     error
		env     map[Variable]Term
	}{
		// 8.2.1.4 Examples
		{title: `'='(1, 1).`, x: Integer(1), y: Integer(1), ok: true},
		{title: `'='(X, 1).`, x: NewNamedVariable("X"), y: Integer(1), ok: true, env: map[Variable]Term{
			x: Integer(1),
		}},
		{title: `'='(X, Y).`, x: NewNamedVariable("X"), y: NewNamedVariable("Y"), ok: true, env: map[Variable]Term{
			x: NewNamedVariable("Y"),
		}},
		{title: `'='(_, _).`, x: NewVariable(), y: NewVariable(), ok: true},
		{title: `'='(X, Y), '='(X, abc).`, premise: NewEnv().Bind(x, NewNamedVariable("Y")), x: NewNamedVariable("X"), y: Atom("abc"), ok: true, env: map[Variable]Term{
			x: Atom("abc"),
			y: Atom("abc"),
		}},
		{title: `'='(f(X, def), f(def, Y)).`, x: Atom("f").Apply(NewNamedVariable("X"), Atom("def")), y: Atom("f").Apply(Atom("def"), NewNamedVariable("Y")), ok: true, env: map[Variable]Term{
			x: Atom("def"),
			y: Atom("def"),
		}},
		{title: `'='(1, 2).`, x: Integer(1), y: Integer(2), ok: false},
		{title: `'='(1, 1.0).`, x: Integer(1), y: Float(1), ok: false},
		{title: `'='(g(X), f(f(X))).`, x: Atom("g").Apply(NewNamedVariable("X")), y: Atom("f").Apply(Atom("f").Apply(NewNamedVariable("X"))), ok: false},
		{title: `'='(f(X, 1), f(a(X))).`, x: Atom("f").Apply(NewNamedVariable("X"), Integer(1)), y: Atom("f").Apply(Atom("a").Apply(NewNamedVariable("X"))), ok: false},
		{title: `'='(f(X, Y, X), f(a(X), a(Y), Y, 2)).`, x: Atom("f").Apply(NewNamedVariable("X"), NewNamedVariable("Y"), NewNamedVariable("X")), y: Atom("f").Apply(Atom("a").Apply(NewNamedVariable("X")), Atom("a").Apply(NewNamedVariable("Y")), NewNamedVariable("Y"), Integer(2)), ok: false},
		{title: `'='(X, a(X)).`, x: NewNamedVariable("X"), y: Atom("a").Apply(NewNamedVariable("X")), ok: true},
		{title: `'='(f(X, 1), f(a(X), 2)).`, x: Atom("f").Apply(NewNamedVariable("X"), Integer(1)), y: Atom("f").Apply(Atom("a").Apply(NewNamedVariable("X")), Integer(2)), ok: false},
		{title: `'='(f(1, X, 1), f(2, a(X), 2)).`, x: Atom("f").Apply(Integer(1), NewNamedVariable("X"), Integer(1)), y: Atom("f").Apply(Integer(2), Atom("a").Apply(NewNamedVariable("X")), Integer(2)), ok: false},
		{title: `'='(f(1, X), f(2, a(X))).`, x: Atom("f").Apply(Integer(1), NewNamedVariable("X")), y: Atom("f").Apply(Integer(2), Atom("a").Apply(NewNamedVariable("X"))), ok: false},
		// {title: `'='(f(X, Y, X, 1), f(a(X), a(Y), Y, 2)).`, x: Atom("f").Apply(NewNamedVariable("X"), NewNamedVariable("Y"), NewNamedVariable("X"), Integer(1)), y: Atom("f").Apply(Atom("a").Apply(NewNamedVariable("X")), Atom("a").Apply(NewNamedVariable("Y")), NewNamedVariable("Y"), Integer(2)), ok: false},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := Unify(tt.x, tt.y, func(env *Env) *Promise {
				for k, v := range tt.env {
					_, ok := env.Unify(k, v, false)
					assert.True(t, ok)
				}
				return Bool(true)
			}, tt.premise).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestUnifyWithOccursCheck(t *testing.T) {
	x, y := NewNamedVariable("X"), NewNamedVariable("Y")
	tests := []struct {
		title   string
		premise *Env
		x, y    Term
		ok      bool
		err     error
		env     map[Variable]Term
	}{
		// 8.2.2.4 Examples
		{title: `unify_with_occurs_check(1, 1).`, x: Integer(1), y: Integer(1), ok: true},
		{title: `unify_with_occurs_check(X, 1).`, x: NewNamedVariable("X"), y: Integer(1), ok: true, env: map[Variable]Term{
			x: Integer(1),
		}},
		{title: `unify_with_occurs_check(X, Y).`, x: NewNamedVariable("X"), y: NewNamedVariable("Y"), ok: true, env: map[Variable]Term{
			x: NewNamedVariable("Y"),
		}},
		{title: `unify_with_occurs_check(_, _).`, x: NewVariable(), y: NewVariable(), ok: true},
		{title: `unify_with_occurs_check(X, Y), unify_with_occurs_check(X, abc).`, premise: NewEnv().Bind(x, NewNamedVariable("Y")), x: NewNamedVariable("X"), y: Atom("abc"), ok: true, env: map[Variable]Term{
			x: Atom("abc"),
			y: Atom("abc"),
		}},
		{title: `unify_with_occurs_check(f(X, def), f(def, Y)).`, x: Atom("f").Apply(NewNamedVariable("X"), Atom("def")), y: Atom("f").Apply(Atom("def"), NewNamedVariable("Y")), ok: true, env: map[Variable]Term{
			x: Atom("def"),
			y: Atom("def"),
		}},
		{title: `unify_with_occurs_check(1, 2).`, x: Integer(1), y: Integer(2), ok: false},
		{title: `unify_with_occurs_check(1, 1.0).`, x: Integer(1), y: Float(1), ok: false},
		{title: `unify_with_occurs_check(g(X), f(f(X))).`, x: Atom("g").Apply(NewNamedVariable("X")), y: Atom("f").Apply(Atom("f").Apply(NewNamedVariable("X"))), ok: false},
		{title: `unify_with_occurs_check(f(X, 1), f(a(X))).`, x: Atom("f").Apply(NewNamedVariable("X"), Integer(1)), y: Atom("f").Apply(Atom("a").Apply(NewNamedVariable("X"))), ok: false},
		{title: `unify_with_occurs_check(f(X, Y, X), f(a(X), a(Y), Y, 2)).`, x: Atom("f").Apply(NewNamedVariable("X"), NewNamedVariable("Y"), NewNamedVariable("X")), y: Atom("f").Apply(Atom("a").Apply(NewNamedVariable("X")), Atom("a").Apply(NewNamedVariable("Y")), NewNamedVariable("Y"), Integer(2)), ok: false},
		{title: `unify_with_occurs_check(X, a(X)).`, x: NewNamedVariable("X"), y: Atom("a").Apply(NewNamedVariable("X")), ok: false},
		{title: `unify_with_occurs_check(f(X, 1), f(a(X), 2)).`, x: Atom("f").Apply(NewNamedVariable("X"), Integer(1)), y: Atom("f").Apply(Atom("a").Apply(NewNamedVariable("X")), Integer(2)), ok: false},
		{title: `unify_with_occurs_check(f(1, X, 1), f(2, a(X), 2)).`, x: Atom("f").Apply(Integer(1), NewNamedVariable("X"), Integer(1)), y: Atom("f").Apply(Integer(2), Atom("a").Apply(NewNamedVariable("X")), Integer(2)), ok: false},
		{title: `unify_with_occurs_check(f(1, X), f(2, a(X))).`, x: Atom("f").Apply(Integer(1), NewNamedVariable("X")), y: Atom("f").Apply(Integer(2), Atom("a").Apply(NewNamedVariable("X"))), ok: false},
		{title: `unify_with_occurs_check(f(X, Y, X, 1), f(a(X), a(Y), Y, 2)).`, x: Atom("f").Apply(NewNamedVariable("X"), NewNamedVariable("Y"), NewNamedVariable("X"), Integer(1)), y: Atom("f").Apply(Atom("a").Apply(NewNamedVariable("X")), Atom("a").Apply(NewNamedVariable("Y")), NewNamedVariable("Y"), Integer(2)), ok: false},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := UnifyWithOccursCheck(tt.x, tt.y, func(env *Env) *Promise {
				for k, v := range tt.env {
					_, ok := env.Unify(k, v, false)
					assert.True(t, ok)
				}
				return Bool(true)
			}, tt.premise).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestSubsumesTerm(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		ok, err := SubsumesTerm(NewNamedVariable("X"), Atom("a"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not unifiable", func(t *testing.T) {
		ok, err := SubsumesTerm(Atom("a"), Atom("b"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("specific-general", func(t *testing.T) {
		ok, err := SubsumesTerm(Atom("a"), NewNamedVariable("X"), Success, nil).Force(context.Background())
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
		ok, err := TypeCompound(&compound{
			functor: "foo",
			args:    []Term{Atom("a")},
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

func TestAcyclicTerm(t *testing.T) {
	t.Run("atomic", func(t *testing.T) {
		ok, err := AcyclicTerm(Atom("a"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("compound", func(t *testing.T) {
		t.Run("cyclic", func(t *testing.T) {
			var c = compound{
				functor: "f",
				args: []Term{
					Atom("a"),
					nil, // placeholder
				},
			}
			c.args[1] = &c

			ok, err := AcyclicTerm(&c, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})
}

func TestFunctor(t *testing.T) {
	x, y := NewNamedVariable("X"), NewNamedVariable("Y")
	a, b := NewNamedVariable("A"), NewNamedVariable("B")
	tests := []struct {
		title             string
		term, name, arity Term
		ok                bool
		err               error
		env               map[Variable]Term
	}{
		// 8.5.1.4 Examples
		{title: `functor(foo(a, b, c), foo, 3).`, term: Atom("foo").Apply(Atom("a"), Atom("b"), Atom("c")), name: Atom("foo"), arity: Integer(3), ok: true},
		{title: `functor(foo(a, b, c), X, Y).`, term: Atom("foo").Apply(Atom("a"), Atom("b"), Atom("c")), name: NewNamedVariable("X"), arity: NewNamedVariable("Y"), ok: true, env: map[Variable]Term{
			x: Atom("foo"),
			y: Integer(3),
		}},
		{title: `functor(X, foo, 3).`, term: NewNamedVariable("X"), name: Atom("foo"), arity: Integer(3), ok: true, env: map[Variable]Term{
			x: Atom("foo").Apply(NewVariable(), NewVariable(), NewVariable()),
		}},
		{title: `functor(X, foo, 0).`, term: NewNamedVariable("X"), name: Atom("foo"), arity: Integer(0), ok: true, env: map[Variable]Term{
			x: Atom("foo"),
		}},
		{title: `functor(mats(A, B), A, B).`, term: Atom("mats").Apply(NewNamedVariable("A"), NewNamedVariable("B")), name: NewNamedVariable("A"), arity: NewNamedVariable("B"), ok: true, env: map[Variable]Term{
			a: Atom("mats"),
			b: Integer(2),
		}},
		{title: `functor(foo(a), foo, 2).`, term: Atom("foo").Apply(Atom("a")), name: Atom("foo"), arity: Integer(2), ok: false},
		{title: `functor(foo(a), fo, 1).`, term: Atom("foo").Apply(Atom("a")), name: Atom("fo"), arity: Integer(1), ok: false},
		{title: `functor(1, X, Y).`, term: Integer(1), name: NewNamedVariable("X"), arity: NewNamedVariable("Y"), ok: true, env: map[Variable]Term{
			x: Integer(1),
			y: Integer(0),
		}},
		{title: `functor(X, 1.1, 0).`, term: NewNamedVariable("X"), name: Float(1.1), arity: Integer(0), ok: true, env: map[Variable]Term{
			x: Float(1.1),
		}},
		{title: `functor([_|_], '.', 2).`, term: Cons(NewVariable(), NewVariable()), name: Atom("."), arity: Integer(2), ok: true},
		{title: `functor([], [], 0).`, term: Atom("[]"), name: Atom("[]"), arity: Integer(0), ok: true},
		{title: `functor(X, Y, 3).`, term: NewNamedVariable("X"), name: NewNamedVariable("Y"), arity: Integer(3), err: InstantiationError(nil)},
		{title: `functor(X, foo, N).`, term: NewNamedVariable("X"), name: Atom("foo"), arity: NewNamedVariable("N"), err: InstantiationError(nil)},
		{title: `functor(X, foo, a).`, term: NewNamedVariable("X"), name: Atom("foo"), arity: Atom("a"), err: TypeError(ValidTypeInteger, Atom("a"), nil)},
		{title: `functor(F, 1.5, 1).`, term: NewNamedVariable("F"), name: Float(1.5), arity: Integer(1), err: TypeError(ValidTypeAtom, Float(1.5), nil)},
		{title: `functor(F, foo(a), 1).`, term: NewNamedVariable("F"), name: Atom("foo").Apply(Atom("a")), arity: Integer(1), err: TypeError(ValidTypeAtomic, Atom("foo").Apply(Atom("a")), nil)},
		// {title: `current_prolog_flag(max_arity, A), X is A + 1, functor(T, foo, X).`}
		{title: `Minus_1 is 0 - 1, functor(F, foo, Minus_1).`, term: NewNamedVariable("F"), name: Atom("foo"), arity: Integer(-1), err: DomainError(ValidDomainNotLessThanZero, Integer(-1), nil)},

		// https://github.com/ichiban/prolog/issues/247
		{title: `functor(X, Y, 0).`, term: NewNamedVariable("X"), name: NewNamedVariable("Y"), arity: Integer(0), err: InstantiationError(nil)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := Functor(tt.term, tt.name, tt.arity, func(env *Env) *Promise {
				for k, v := range tt.env {
					_, ok := env.Unify(k, v, false)
					assert.True(t, ok)
				}
				return Bool(true)
			}, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestArg(t *testing.T) {
	t.Run("term is a variable", func(t *testing.T) {
		v := NewVariable()
		ok, err := Arg(NewVariable(), v, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("term is not a compound", func(t *testing.T) {
		ok, err := Arg(NewVariable(), Atom("foo"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeCompound, Atom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("nth is a variable", func(t *testing.T) {
		nth := NewVariable()
		_, err := Arg(nth, &compound{
			functor: "f",
			args:    []Term{Atom("a"), Atom("b"), Atom("a")},
		}, Atom("a"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
	})

	t.Run("nth is an integer", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			ok, err := Arg(Integer(1), &compound{
				functor: "f",
				args:    []Term{Atom("a"), Atom("b"), Atom("c")},
			}, Atom("a"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)

			ok, err = Arg(Integer(2), &compound{
				functor: "f",
				args:    []Term{Atom("a"), Atom("b"), Atom("c")},
			}, Atom("b"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)

			ok, err = Arg(Integer(3), &compound{
				functor: "f",
				args:    []Term{Atom("a"), Atom("b"), Atom("c")},
			}, Atom("c"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("ng", func(t *testing.T) {
			ok, err := Arg(Integer(0), &compound{
				functor: "f",
				args:    []Term{Atom("a"), Atom("b"), Atom("c")},
			}, NewVariable(), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)

			ok, err = Arg(Integer(4), &compound{
				functor: "f",
				args:    []Term{Atom("a"), Atom("b"), Atom("c")},
			}, NewVariable(), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("negative", func(t *testing.T) {
			ok, err := Arg(Integer(-2), &compound{
				functor: "f",
				args:    []Term{Atom("a"), Atom("b"), Atom("c")},
			}, Atom("b"), Success, nil).Force(context.Background())
			assert.Equal(t, DomainError(ValidDomainNotLessThanZero, Integer(-2), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("nth is neither a variable nor an integer", func(t *testing.T) {
		ok, err := Arg(Atom("foo"), &compound{
			functor: "f",
			args:    []Term{Atom("a"), Atom("b"), Atom("c")},
		}, Atom("b"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, Atom("foo"), nil), err)
		assert.False(t, ok)
	})
}

func TestUniv(t *testing.T) {
	x, y := NewNamedVariable("X"), NewNamedVariable("Y")
	l := NewNamedVariable("L")
	a, as := NewNamedVariable("A"), NewNamedVariable("As")
	tests := []struct {
		title      string
		term, list Term
		ok         bool
		err        error
		env        map[Variable]Term
	}{
		// 8.5.3.4 Examples
		{title: "1", term: Atom("foo").Apply(Atom("a"), Atom("b")), list: List(Atom("foo"), Atom("a"), Atom("b")), ok: true},
		{title: "2", term: NewNamedVariable("X"), list: List(Atom("foo"), Atom("a"), Atom("b")), ok: true, env: map[Variable]Term{
			x: Atom("foo").Apply(Atom("a"), Atom("b")),
		}},
		{title: "3", term: Atom("foo").Apply(Atom("a"), Atom("b")), list: NewNamedVariable("L"), ok: true, env: map[Variable]Term{
			l: List(Atom("foo"), Atom("a"), Atom("b")),
		}},
		{title: "4", term: Atom("foo").Apply(NewNamedVariable("X"), Atom("b")), list: List(Atom("foo"), Atom("a"), NewNamedVariable("Y")), ok: true, env: map[Variable]Term{
			x: Atom("a"),
			y: Atom("b"),
		}},
		{title: "5", term: Integer(1), list: List(Integer(1)), ok: true},
		{title: "6", term: Atom("foo").Apply(Atom("a"), Atom("b")), list: List(Atom("foo"), Atom("b"), Atom("a")), ok: false},
		{title: "7", term: NewNamedVariable("X"), list: NewNamedVariable("Y"), err: InstantiationError(nil)},
		{title: "8", term: NewNamedVariable("X"), list: ListRest(NewNamedVariable("Y"), Atom("foo"), Atom("a")), err: InstantiationError(nil)},
		{title: "9", term: NewNamedVariable("X"), list: ListRest(Atom("bar"), Atom("foo")), err: TypeError(ValidTypeList, ListRest(Atom("bar"), Atom("foo")), nil)},
		{title: "10", term: NewNamedVariable("X"), list: List(NewNamedVariable("Foo"), Atom("bar")), err: InstantiationError(nil)},
		{title: "11", term: NewNamedVariable("X"), list: List(Integer(3), Integer(1)), err: TypeError(ValidTypeAtom, Integer(3), nil)},
		{title: "12", term: NewNamedVariable("X"), list: List(Float(1.1), Atom("foo")), err: TypeError(ValidTypeAtom, Float(1.1), nil)},
		{title: "13", term: NewNamedVariable("X"), list: List(Atom("a").Apply(Atom("b")), Integer(1)), err: TypeError(ValidTypeAtom, Atom("a").Apply(Atom("b")), nil)},
		{title: "14", term: NewNamedVariable("X"), list: Integer(4), err: TypeError(ValidTypeList, Integer(4), nil)},
		{title: "15", term: Atom("f").Apply(NewNamedVariable("X")), list: List(Atom("f"), Atom("u").Apply(NewNamedVariable("X"))), ok: true, env: map[Variable]Term{
			x: Atom("u").Apply(NewNamedVariable("X")),
		}},

		// 8.5.3.3 Errors
		{title: "b: term is a compound", term: Atom("f").Apply(Atom("a")), list: ListRest(Atom("a"), Atom("f")), err: TypeError(ValidTypeList, ListRest(Atom("a"), Atom("f")), nil)},
		{title: "b: term is an atomic", term: Integer(1), list: ListRest(Atom("a"), Atom("f")), err: TypeError(ValidTypeList, ListRest(Atom("a"), Atom("f")), nil)},
		{title: "c", term: NewNamedVariable("X"), list: List(NewNamedVariable("Y")), err: InstantiationError(nil)},
		{title: "e", term: NewNamedVariable("X"), list: List(Atom("f").Apply(Atom("a"))), err: TypeError(ValidTypeAtomic, Atom("f").Apply(Atom("a")), nil)},
		{title: "f", term: NewNamedVariable("X"), list: List(), err: DomainError(ValidDomainNonEmptyList, List(), nil)},

		{title: "term is a variable, list has exactly one member which is an atomic", term: NewNamedVariable("X"), list: List(Integer(1)), ok: true, env: map[Variable]Term{
			x: Integer(1),
		}},
		{title: "term is an atomic, the length of list is not 1", term: Integer(1), list: List(), ok: false},

		// https://github.com/ichiban/prolog/issues/244
		{title: "term is atomic", term: Atom("c"), list: ListRest(NewNamedVariable("As"), NewNamedVariable("A")), ok: true, env: map[Variable]Term{
			a:  Atom("c"),
			as: List(),
		}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := Univ(tt.term, tt.list, func(env *Env) *Promise {
				for k, v := range tt.env {
					assert.Equal(t, v, env.Resolve(k))
				}
				return Bool(true)
			}, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestCopyTerm(t *testing.T) {
	in := NewNamedVariable("In")
	out := NewNamedVariable("Out")
	env := NewEnv().
		Bind(in, Atom("a"))
	ok, err := CopyTerm(in, out, func(env *Env) *Promise {
		assert.Equal(t, Atom("a"), env.Resolve(out))
		return Bool(true)
	}, env).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)
}

func TestTermVariables(t *testing.T) {
	vars := NewNamedVariable("Vars")
	vs, vt := NewNamedVariable("S"), NewNamedVariable("T")
	b := NewNamedVariable("B")
	tests := []struct {
		title      string
		term, vars Term
		ok         bool
		err        error
		env        map[Variable]Term
	}{
		// 8.5.5.4 Examples
		{title: "1", term: Atom("t"), vars: NewNamedVariable("Vars"), ok: true, env: map[Variable]Term{
			vars: List(),
		}},
		{title: "2", term: Atom("-").Apply(
			Atom("+").Apply(
				NewNamedVariable("A"),
				Atom("/").Apply(
					Atom("*").Apply(
						NewNamedVariable("B"),
						NewNamedVariable("C"),
					),
					NewNamedVariable("B"),
				),
			),
			NewNamedVariable("D"),
		), vars: NewNamedVariable("Vars"), ok: true, env: map[Variable]Term{
			vars: List(NewNamedVariable("A"), NewNamedVariable("B"), NewNamedVariable("C"), NewNamedVariable("D")),
		}},
		{title: "3", term: Atom("t"), vars: ListRest(Atom("a"), Atom("x"), Atom("y")), err: TypeError(ValidTypeList, ListRest(Atom("a"), Atom("x"), Atom("y")), nil)},
		{title: "4, 5", term: NewNamedVariable("S"), vars: NewNamedVariable("Vars"), ok: true, env: map[Variable]Term{
			vars: List(NewNamedVariable("B"), NewNamedVariable("A")),
			vs:   Atom("+").Apply(NewNamedVariable("B"), NewNamedVariable("T")),
			vt:   Atom("*").Apply(NewNamedVariable("A"), NewNamedVariable("B")),
		}},
		{title: "6", term: Atom("+").Apply(Atom("+").Apply(NewNamedVariable("A"), NewNamedVariable("B")), NewNamedVariable("B")), vars: ListRest(NewNamedVariable("Vars"), NewNamedVariable("B")), ok: true, env: map[Variable]Term{
			b:    NewNamedVariable("A"),
			vars: List(NewNamedVariable("B")),
		}},
	}

	env := NewEnv().
		Bind(vs, Atom("+").Apply(NewNamedVariable("B"), NewNamedVariable("T"))).
		Bind(vt, Atom("*").Apply(NewNamedVariable("A"), NewNamedVariable("B")))
	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := TermVariables(tt.term, tt.vars, func(env *Env) *Promise {
				for k, v := range tt.env {
					assert.Equal(t, v, env.Resolve(k))
				}
				return Bool(true)
			}, env).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestState_Op(t *testing.T) {
	t.Run("insert", func(t *testing.T) {
		t.Run("atom", func(t *testing.T) {
			state := State{operators: operators{}}
			state.operators.define(900, operatorSpecifierXFX, `+++`)
			state.operators.define(1100, operatorSpecifierXFX, `+`)

			ok, err := state.Op(Integer(1000), Atom("xfx"), Atom("++"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)

			assert.Equal(t, operators{
				`+++`: {
					operatorClassInfix: {
						priority:  900,
						specifier: operatorSpecifierXFX,
						name:      "+++",
					},
				},
				`++`: {
					operatorClassInfix: {
						priority:  1000,
						specifier: operatorSpecifierXFX,
						name:      "++",
					},
				},
				`+`: {
					operatorClassInfix: {
						priority:  1100,
						specifier: operatorSpecifierXFX,
						name:      "+",
					},
				},
			}, state.operators)
		})

		t.Run("list", func(t *testing.T) {
			state := State{
				operators: operators{
					`+++`: {
						operatorClassInfix: {
							priority:  900,
							specifier: operatorSpecifierXFX,
							name:      "+++",
						},
					},
					`+`: {
						operatorClassInfix: {
							priority:  1100,
							specifier: operatorSpecifierXFX,
							name:      "+",
						},
					},
				},
			}
			ok, err := state.Op(Integer(1000), Atom("xfx"), List(Atom("++"), Atom("++")), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)

			assert.Equal(t, operators{
				`+++`: {
					operatorClassInfix: {
						priority:  900,
						specifier: operatorSpecifierXFX,
						name:      "+++",
					},
				},
				`++`: {
					operatorClassInfix: {
						priority:  1000,
						specifier: operatorSpecifierXFX,
						name:      "++",
					},
				},
				`+`: {
					operatorClassInfix: {
						priority:  1100,
						specifier: operatorSpecifierXFX,
						name:      "+",
					},
				},
			}, state.operators)
		})
	})

	t.Run("remove", func(t *testing.T) {
		state := State{
			operators: operators{
				`+++`: {
					operatorClassInfix: {
						priority:  900,
						specifier: operatorSpecifierXFX,
						name:      "+++",
					},
				},
				`++`: {
					operatorClassInfix: {
						priority:  1000,
						specifier: operatorSpecifierXFX,
						name:      "++",
					},
				},
				`+`: {
					operatorClassInfix: {
						priority:  1100,
						specifier: operatorSpecifierXFX,
						name:      "+",
					},
				},
			},
		}
		ok, err := state.Op(Integer(0), Atom("xfx"), Atom("++"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, operators{
			`+++`: {
				operatorClassInfix: {
					priority:  900,
					specifier: operatorSpecifierXFX,
					name:      "+++",
				},
			},
			`+`: {
				operatorClassInfix: {
					priority:  1100,
					specifier: operatorSpecifierXFX,
					name:      "+",
				},
			},
		}, state.operators)
	})

	t.Run("priority is a variable", func(t *testing.T) {
		var state State
		ok, err := state.Op(NewNamedVariable("X"), Atom("xfx"), Atom("+"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("specifier is a variable", func(t *testing.T) {
		var state State
		ok, err := state.Op(Integer(1000), NewNamedVariable("X"), Atom("+"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("priority is neither a variable nor an integer", func(t *testing.T) {
		var state State
		ok, err := state.Op(Atom("foo"), Atom("xfx"), Atom("+"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, Atom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("specifier is neither a variable nor an atom", func(t *testing.T) {
		var state State
		ok, err := state.Op(Integer(1000), Integer(0), Atom("+"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("operator is neither a partial list nor a list nor an atom", func(t *testing.T) {
		var state State
		ok, err := state.Op(Integer(1000), Atom("xfx"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeList, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the operator list is neither a variable nor an atom", func(t *testing.T) {
		var state State
		ok, err := state.Op(Integer(1000), Atom("xfx"), List(Integer(0)), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("priority is not between 0 and 1200 inclusive", func(t *testing.T) {
		t.Run("priority is negative", func(t *testing.T) {
			var state State
			ok, err := state.Op(Integer(-1), Atom("xfx"), Atom("+"), Success, nil).Force(context.Background())
			assert.Equal(t, DomainError(ValidDomainOperatorPriority, Integer(-1), nil), err)
			assert.False(t, ok)
		})

		t.Run("priority is more than 1200", func(t *testing.T) {
			var state State
			ok, err := state.Op(Integer(1201), Atom("xfx"), Atom("+"), Success, nil).Force(context.Background())
			assert.Equal(t, DomainError(ValidDomainOperatorPriority, Integer(1201), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("specifier is not a valid operator specifier", func(t *testing.T) {
		var state State
		ok, err := state.Op(Integer(1000), Atom("foo"), Atom("+"), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainOperatorSpecifier, Atom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("operator is ','", func(t *testing.T) {
		state := State{operators: operators{}}
		state.operators.define(1000, operatorSpecifierXFY, `,`)
		ok, err := state.Op(Integer(1000), Atom("xfy"), Atom(","), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationModify, PermissionTypeOperator, Atom(","), nil), err)
		assert.False(t, ok)
	})

	t.Run("an element of the operator list is ','", func(t *testing.T) {
		state := State{operators: operators{}}
		state.operators.define(1000, operatorSpecifierXFY, `,`)
		ok, err := state.Op(Integer(1000), Atom("xfy"), List(Atom(",")), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationModify, PermissionTypeOperator, Atom(","), nil), err)
		assert.False(t, ok)
	})

	t.Run("operator is an atom, priority is a priority, and specifier is a specifier such that operator would have an invalid set of priorities and specifiers", func(t *testing.T) {
		t.Run("empty list", func(t *testing.T) {
			var state State
			ok, err := state.Op(Integer(1000), Atom("xfy"), Atom("[]"), Success, nil).Force(context.Background())
			assert.Equal(t, PermissionError(OperationCreate, PermissionTypeOperator, Atom("[]"), nil), err)
			assert.False(t, ok)
		})

		t.Run("empty curly brackets", func(t *testing.T) {
			var state State
			ok, err := state.Op(Integer(1000), Atom("xfy"), Atom("{}"), Success, nil).Force(context.Background())
			assert.Equal(t, PermissionError(OperationCreate, PermissionTypeOperator, Atom("{}"), nil), err)
			assert.False(t, ok)
		})

		t.Run("bar", func(t *testing.T) {
			t.Run("create", func(t *testing.T) {
				var state State
				ok, err := state.Op(Integer(1000), Atom("xfy"), Atom("|"), Success, nil).Force(context.Background())
				assert.Equal(t, PermissionError(OperationCreate, PermissionTypeOperator, Atom("|"), nil), err)
				assert.False(t, ok)
			})

			t.Run("modify", func(t *testing.T) {
				state := State{operators: operators{}}
				state.operators.define(1001, operatorSpecifierXFY, `|`)
				ok, err := state.Op(Integer(1000), Atom("xfy"), Atom("|"), Success, nil).Force(context.Background())
				assert.Equal(t, PermissionError(OperationModify, PermissionTypeOperator, Atom("|"), nil), err)
				assert.False(t, ok)
			})
		})
	})

	t.Run("operator is a list, priority is a priority, and specifier is a specifier such that an element op of the list operator would have an invalid set of priorities and specifiers", func(t *testing.T) {
		t.Run("empty list", func(t *testing.T) {
			var state State
			ok, err := state.Op(Integer(1000), Atom("xfy"), List(Atom("[]")), Success, nil).Force(context.Background())
			assert.Equal(t, PermissionError(OperationCreate, PermissionTypeOperator, Atom("[]"), nil), err)
			assert.False(t, ok)
		})

		t.Run("empty curly brackets", func(t *testing.T) {
			var state State
			ok, err := state.Op(Integer(1000), Atom("xfy"), List(Atom("{}")), Success, nil).Force(context.Background())
			assert.Equal(t, PermissionError(OperationCreate, PermissionTypeOperator, Atom("{}"), nil), err)
			assert.False(t, ok)
		})

		t.Run("bar", func(t *testing.T) {
			t.Run("create", func(t *testing.T) {
				var state State
				ok, err := state.Op(Integer(1000), Atom("xfy"), List(Atom("|")), Success, nil).Force(context.Background())
				assert.Equal(t, PermissionError(OperationCreate, PermissionTypeOperator, Atom("|"), nil), err)
				assert.False(t, ok)
			})

			t.Run("modify", func(t *testing.T) {
				state := State{operators: operators{}}
				state.operators.define(101, operatorSpecifierXFY, `|`)
				ok, err := state.Op(Integer(1000), Atom("xfy"), List(Atom("|")), Success, nil).Force(context.Background())
				assert.Equal(t, PermissionError(OperationModify, PermissionTypeOperator, Atom("|"), nil), err)
				assert.False(t, ok)
			})
		})
	})

	t.Run("There shall not be an infix and a postfix operator with the same name.", func(t *testing.T) {
		t.Run("infix", func(t *testing.T) {
			state := State{operators: operators{}}
			state.operators.define(200, operatorSpecifierYF, `+`)
			ok, err := state.Op(Integer(500), Atom("yfx"), List(Atom("+")), Success, nil).Force(context.Background())
			assert.Equal(t, PermissionError(OperationCreate, PermissionTypeOperator, Atom("+"), nil), err)
			assert.False(t, ok)
		})

		t.Run("postfix", func(t *testing.T) {
			state := State{operators: operators{}}
			state.operators.define(500, operatorSpecifierYFX, `+`)
			ok, err := state.Op(Integer(200), Atom("yf"), List(Atom("+")), Success, nil).Force(context.Background())
			assert.Equal(t, PermissionError(OperationCreate, PermissionTypeOperator, Atom("+"), nil), err)
			assert.False(t, ok)
		})
	})
}

func TestState_CurrentOp(t *testing.T) {
	state := State{operators: operators{}}
	state.operators.define(900, operatorSpecifierXFX, `+++`)
	state.operators.define(1000, operatorSpecifierXFX, `++`)
	state.operators.define(1100, operatorSpecifierXFX, `+`)

	t.Run("single solution", func(t *testing.T) {
		ok, err := state.CurrentOp(Integer(1100), Atom("xfx"), Atom("+"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("multiple solutions", func(t *testing.T) {
		priority, specifier, operator := NewNamedVariable("Priority"), NewNamedVariable("Specifier"), NewNamedVariable("Operator")
		ok, err := state.CurrentOp(priority, specifier, operator, func(env *Env) *Promise {
			switch env.Resolve(operator) {
			case Atom("+++"):
				assert.Equal(t, Integer(900), env.Resolve(priority))
				assert.Equal(t, Atom("xfx"), env.Resolve(specifier))
			case Atom("++"):
				assert.Equal(t, Integer(1000), env.Resolve(priority))
				assert.Equal(t, Atom("xfx"), env.Resolve(specifier))
			case Atom("+"):
				assert.Equal(t, Integer(1100), env.Resolve(priority))
				assert.Equal(t, Atom("xfx"), env.Resolve(specifier))
			default:
				assert.Fail(t, "unreachable")
			}
			return Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("priority is not an operator priority", func(t *testing.T) {
		t.Run("priority is not an integer", func(t *testing.T) {
			ok, err := state.CurrentOp(Atom("foo"), Atom("xfx"), Atom("+"), Success, nil).Force(context.Background())
			assert.Equal(t, DomainError(ValidDomainOperatorPriority, Atom("foo"), nil), err)
			assert.False(t, ok)
		})

		t.Run("priority is negative", func(t *testing.T) {
			ok, err := state.CurrentOp(Integer(-1), Atom("xfx"), Atom("+"), Success, nil).Force(context.Background())
			assert.Equal(t, DomainError(ValidDomainOperatorPriority, Integer(-1), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("specifier is not an operator specifier", func(t *testing.T) {
		t.Run("specifier is not an atom", func(t *testing.T) {
			ok, err := state.CurrentOp(Integer(1100), Integer(0), Atom("+"), Success, nil).Force(context.Background())
			assert.Equal(t, DomainError(ValidDomainOperatorSpecifier, Integer(0), nil), err)
			assert.False(t, ok)
		})

		t.Run("specifier is a non-specifier atom", func(t *testing.T) {
			ok, err := state.CurrentOp(Integer(1100), Atom("foo"), Atom("+"), Success, nil).Force(context.Background())
			assert.Equal(t, DomainError(ValidDomainOperatorSpecifier, Atom("foo"), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("operator is not an atom", func(t *testing.T) {
		ok, err := state.CurrentOp(Integer(1100), Atom("xfx"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})
}

func TestState_BagOf(t *testing.T) {
	s := NewNamedVariable("S")
	x, y := NewNamedVariable("X"), NewNamedVariable("Y")
	l := NewNamedVariable("L")
	tests := []struct {
		title                     string
		template, goal, instances Term
		err                       error
		env                       []map[Variable]Term
		warning                   bool
	}{
		// 8.10.2.4 Examples
		{
			title:     "bagof(X, (X=1 ; X=2), S).",
			template:  NewNamedVariable("X"),
			goal:      Atom(";").Apply(Atom("=").Apply(NewNamedVariable("X"), Integer(1)), Atom("=").Apply(NewNamedVariable("X"), Integer(2))),
			instances: NewNamedVariable("S"),
			env: []map[Variable]Term{
				{s: List(Integer(1), Integer(2))},
			},
		},
		{
			title:     "bagof(X, (X=1 ; X=2), X).",
			template:  NewNamedVariable("X"),
			goal:      Atom(";").Apply(Atom("=").Apply(NewNamedVariable("X"), Integer(1)), Atom("=").Apply(NewNamedVariable("X"), Integer(2))),
			instances: NewNamedVariable("X"),
			env: []map[Variable]Term{
				{x: List(Integer(1), Integer(2))},
			},
		},
		{
			title:     "bagof(X, (X=Y ; X=Z), S).",
			template:  NewNamedVariable("X"),
			goal:      Atom(";").Apply(Atom("=").Apply(NewNamedVariable("X"), NewNamedVariable("Y")), Atom("=").Apply(NewNamedVariable("X"), NewNamedVariable("Z"))),
			instances: NewNamedVariable("S"),
			env: []map[Variable]Term{
				{s: List(NewNamedVariable("Y"), NewNamedVariable("Z"))},
			},
		},
		{
			title:     "bagof(X, fail, S).",
			template:  NewNamedVariable("X"),
			goal:      Atom("fail"),
			instances: NewNamedVariable("S"),
			env:       nil,
		},
		{
			title:     "bagof(1, (Y=1 ; Y=2), L).",
			template:  Integer(1),
			goal:      Atom(";").Apply(Atom("=").Apply(NewNamedVariable("Y"), Integer(1)), Atom("=").Apply(NewNamedVariable("Y"), Integer(2))),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{l: List(Integer(1)), y: Integer(1)},
				{l: List(Integer(1)), y: Integer(2)},
			},
		},
		{
			title:     "bagof(f(X, Y), (X=a ; Y=b), L).",
			template:  Atom("f").Apply(NewNamedVariable("X"), NewNamedVariable("Y")),
			goal:      Atom(";").Apply(Atom("=").Apply(NewNamedVariable("X"), Atom("a")), Atom("=").Apply(NewNamedVariable("Y"), Atom("b"))),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{l: List(Atom("f").Apply(Atom("a"), NewVariable()), Atom("f").Apply(NewVariable(), Atom("b")))},
			},
		},
		{
			title:    "bagof(X, Y^((X=1, Y=1) ; (X=2, Y=2)), S).",
			template: NewNamedVariable("X"),
			goal: Atom("^").Apply(NewNamedVariable("Y"), Atom(";").Apply(
				Atom(",").Apply(Atom("=").Apply(NewNamedVariable("X"), Integer(1)), Atom("=").Apply(NewNamedVariable("Y"), Integer(1))),
				Atom(",").Apply(Atom("=").Apply(NewNamedVariable("X"), Integer(2)), Atom("=").Apply(NewNamedVariable("Y"), Integer(2))),
			)),
			instances: NewNamedVariable("S"),
			env: []map[Variable]Term{
				{s: List(Integer(1), Integer(2))},
			},
		},
		{
			title:    "bagof(X, Y^((X=1 ; Y=1) ; (X=2, Y=2)), S).",
			template: NewNamedVariable("X"),
			goal: Atom("^").Apply(NewNamedVariable("Y"), Atom(";").Apply(
				Atom(";").Apply(Atom("=").Apply(NewNamedVariable("X"), Integer(1)), Atom("=").Apply(NewNamedVariable("Y"), Integer(1))),
				Atom(",").Apply(Atom("=").Apply(NewNamedVariable("X"), Integer(2)), Atom("=").Apply(NewNamedVariable("Y"), Integer(2))),
			)),
			instances: NewNamedVariable("S"),
			env: []map[Variable]Term{
				{s: List(Integer(1), NewVariable(), Integer(2))},
			},
		},
		{
			title:    "bagof(X, (Y^(X=1 ; Y=2) ; X=3), S).",
			template: NewNamedVariable("X"),
			goal: Atom(";").Apply(
				Atom("^").Apply(
					NewNamedVariable("Y"),
					Atom(";").Apply(
						Atom("=").Apply(NewNamedVariable("X"), Integer(1)),
						Atom("=").Apply(NewNamedVariable("Y"), Integer(2)),
					),
				),
				Atom("=").Apply(NewNamedVariable("X"), Integer(3)),
			),
			instances: NewNamedVariable("S"),
			env: []map[Variable]Term{
				{s: List(Integer(3)), y: NewVariable()},
			},
			warning: true,
		},
		{
			title:    "bagof(X, (X=Y ; X=Z ; Y=1), S).",
			template: NewNamedVariable("X"),
			goal: Atom(";").Apply(
				Atom("=").Apply(NewNamedVariable("X"), NewNamedVariable("Y")),
				Atom(";").Apply(
					Atom("=").Apply(NewNamedVariable("X"), NewNamedVariable("Z")),
					Atom("=").Apply(NewNamedVariable("Y"), Integer(1)),
				),
			),
			instances: NewNamedVariable("S"),
			env: []map[Variable]Term{
				{s: List(NewNamedVariable("Y"), NewNamedVariable("Z"))},
				{s: List(NewVariable())},
			},
		},
		{
			title:     "bagof(X, a(X, Y), L).",
			template:  NewNamedVariable("X"),
			goal:      Atom("a").Apply(NewNamedVariable("X"), NewNamedVariable("Y")),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{l: List(Integer(1), Integer(2)), y: Atom("f").Apply(NewVariable())},
			},
		},
		{
			title:     "bagof(X, b(X, Y), L).",
			template:  NewNamedVariable("X"),
			goal:      Atom("b").Apply(NewNamedVariable("X"), NewNamedVariable("Y")),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{l: List(Integer(1), Integer(1), Integer(2)), y: Integer(1)},
				{l: List(Integer(1), Integer(2), Integer(2)), y: Integer(2)},
			},
		},
		{
			title:     "bagof(X, Y^Z, L).",
			template:  NewNamedVariable("X"),
			goal:      Atom("^").Apply(NewNamedVariable("X"), NewNamedVariable("Z")),
			instances: NewNamedVariable("L"),
			err:       InstantiationError(nil),
		},
		{
			title:     "bagof(X, 1, L).",
			template:  NewNamedVariable("X"),
			goal:      Integer(1),
			instances: NewNamedVariable("L"),
			err:       TypeError(ValidTypeCallable, Integer(1), nil),
		},

		// 8.10.2.3 Errors
		{
			title:     "c",
			template:  Atom("t"),
			goal:      Atom("true"),
			instances: ListRest(Integer(1), Atom("t")),
			err:       TypeError(ValidTypeList, ListRest(Integer(1), Atom("t")), nil),
		},
	}

	state := State{
		VM: VM{
			unknown: unknownWarning,
		},
	}
	state.Register2("=", Unify)
	state.Register2(",", func(g1, g2 Term, k func(*Env) *Promise, env *Env) *Promise {
		return state.Call(g1, func(env *Env) *Promise {
			return state.Call(g2, k, env)
		}, env)
	})
	state.Register2(";", func(g1, g2 Term, k func(*Env) *Promise, env *Env) *Promise {
		return Delay(func(context.Context) *Promise {
			return state.Call(g1, k, env)
		}, func(context.Context) *Promise {
			return state.Call(g2, k, env)
		})
	})
	state.Register0("true", func(k func(*Env) *Promise, env *Env) *Promise {
		return k(env)
	})
	state.Register0("fail", func(func(*Env) *Promise, *Env) *Promise {
		return Bool(false)
	})
	state.Register2("a", func(x, y Term, k func(*Env) *Promise, env *Env) *Promise {
		const a, f = Atom("$a"), Atom("f")
		return Delay(func(context.Context) *Promise {
			return Unify(a.Apply(x, y), a.Apply(Integer(1), f.Apply(NewVariable())), k, env)
		}, func(context.Context) *Promise {
			return Unify(a.Apply(x, y), a.Apply(Integer(2), f.Apply(NewVariable())), k, env)
		})
	})
	state.Register2("b", func(x, y Term, k func(*Env) *Promise, env *Env) *Promise {
		const b = Atom("$b")
		return Delay(func(context.Context) *Promise {
			return Unify(b.Apply(x, y), b.Apply(Integer(1), Integer(1)), k, env)
		}, func(context.Context) *Promise {
			return Unify(b.Apply(x, y), b.Apply(Integer(1), Integer(1)), k, env)
		}, func(context.Context) *Promise {
			return Unify(b.Apply(x, y), b.Apply(Integer(1), Integer(2)), k, env)
		}, func(context.Context) *Promise {
			return Unify(b.Apply(x, y), b.Apply(Integer(2), Integer(1)), k, env)
		}, func(context.Context) *Promise {
			return Unify(b.Apply(x, y), b.Apply(Integer(2), Integer(2)), k, env)
		}, func(context.Context) *Promise {
			return Unify(b.Apply(x, y), b.Apply(Integer(2), Integer(2)), k, env)
		})
	})

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			state.OnUnknown = func(ProcedureIndicator, []Term, *Env) {
				assert.True(t, tt.warning)
			}
			_, err := state.BagOf(tt.template, tt.goal, tt.instances, func(env *Env) *Promise {
				for k, v := range tt.env[0] {
					_, ok := env.Unify(v, k, false)
					assert.True(t, ok)
				}
				tt.env = tt.env[1:]
				return Bool(false)
			}, nil).Force(context.Background())
			assert.Equal(t, tt.err, err)
			assert.Empty(t, tt.env)
		})
	}
}

func TestState_SetOf(t *testing.T) {
	s := NewNamedVariable("S")
	x, y := NewNamedVariable("X"), NewNamedVariable("Y")
	l := NewNamedVariable("L")
	u, v := NewNamedVariable("U"), NewNamedVariable("V")
	tests := []struct {
		title                     string
		template, goal, instances Term
		err                       error
		env                       []map[Variable]Term
		warning                   bool
	}{
		// 8.10.3.4 Examples
		{
			title:     "setof(X, (X=1; X=2), S).",
			template:  NewNamedVariable("X"),
			goal:      Atom(";").Apply(Atom("=").Apply(NewNamedVariable("X"), Integer(1)), Atom("=").Apply(NewNamedVariable("X"), Integer(2))),
			instances: NewNamedVariable("S"),
			env: []map[Variable]Term{
				{s: List(Integer(1), Integer(2))},
			},
		},
		{
			title:     "setof(X, (X=1; X=2), X).",
			template:  NewNamedVariable("X"),
			goal:      Atom(";").Apply(Atom("=").Apply(NewNamedVariable("X"), Integer(1)), Atom("=").Apply(NewNamedVariable("X"), Integer(2))),
			instances: NewNamedVariable("X"),
			env: []map[Variable]Term{
				{x: List(Integer(1), Integer(2))},
			},
		},
		{
			title:     "setof(X, (X=2; X=1), S).",
			template:  NewNamedVariable("X"),
			goal:      Atom(";").Apply(Atom("=").Apply(NewNamedVariable("X"), Integer(2)), Atom("=").Apply(NewNamedVariable("X"), Integer(1))),
			instances: NewNamedVariable("S"),
			env: []map[Variable]Term{
				{s: List(Integer(1), Integer(2))},
			},
		},
		{
			title:     "setof(X, (X=2; X=2), S).",
			template:  NewNamedVariable("X"),
			goal:      Atom(";").Apply(Atom("=").Apply(NewNamedVariable("X"), Integer(2)), Atom("=").Apply(NewNamedVariable("X"), Integer(2))),
			instances: NewNamedVariable("S"),
			env: []map[Variable]Term{
				{s: List(Integer(2))},
			},
		},
		{
			title:     "setof(X, (X=Y ; X=Z), S).",
			template:  NewNamedVariable("X"),
			goal:      Atom(";").Apply(Atom("=").Apply(NewNamedVariable("X"), NewNamedVariable("Y")), Atom("=").Apply(NewNamedVariable("X"), NewNamedVariable("Z"))),
			instances: NewNamedVariable("S"),
			env: []map[Variable]Term{
				{s: List(NewNamedVariable("Y"), NewNamedVariable("Z"))},
			},
		},
		{
			title:     "setof(X, fail, S).",
			template:  NewNamedVariable("X"),
			goal:      Atom("fail"),
			instances: NewNamedVariable("S"),
			env:       nil,
		},
		{
			title:     "setof(1, (Y=2; Y=1), L).",
			template:  Integer(1),
			goal:      Atom(";").Apply(Atom("=").Apply(NewNamedVariable("Y"), Integer(2)), Atom("=").Apply(NewNamedVariable("Y"), Integer(1))),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{l: List(Integer(1)), y: Integer(2)},
				{l: List(Integer(1)), y: Integer(1)},
			},
		},
		{
			title:     "setof(f(X, Y), (X=a ; Y=b), L).",
			template:  Atom("f").Apply(NewNamedVariable("X"), NewNamedVariable("Y")),
			goal:      Atom(";").Apply(Atom("=").Apply(NewNamedVariable("X"), Atom("a")), Atom("=").Apply(NewNamedVariable("Y"), Atom("b"))),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{l: List(Atom("f").Apply(Atom("a"), NewVariable()), Atom("f").Apply(NewVariable(), Atom("b")))},
			},
		},
		{
			title:    "setof(X, Y^((X=1, Y=1) ; (X=2, Y=2)), S).",
			template: NewNamedVariable("X"),
			goal: Atom("^").Apply(NewNamedVariable("Y"), Atom(";").Apply(
				Atom(",").Apply(Atom("=").Apply(NewNamedVariable("X"), Integer(1)), Atom("=").Apply(NewNamedVariable("Y"), Integer(1))),
				Atom(",").Apply(Atom("=").Apply(NewNamedVariable("X"), Integer(2)), Atom("=").Apply(NewNamedVariable("Y"), Integer(2))),
			)),
			instances: NewNamedVariable("S"),
			env: []map[Variable]Term{
				{s: List(Integer(1), Integer(2))},
			},
		},
		{
			title:    "setof(X, Y^((X=1 ; Y=1) ; (X=2, Y=2)), S).",
			template: NewNamedVariable("X"),
			goal: Atom("^").Apply(NewNamedVariable("Y"), Atom(";").Apply(
				Atom(";").Apply(Atom("=").Apply(NewNamedVariable("X"), Integer(1)), Atom("=").Apply(NewNamedVariable("Y"), Integer(1))),
				Atom(",").Apply(Atom("=").Apply(NewNamedVariable("X"), Integer(2)), Atom("=").Apply(NewNamedVariable("Y"), Integer(2))),
			)),
			instances: NewNamedVariable("S"),
			env: []map[Variable]Term{
				{s: List(NewVariable(), Integer(1), Integer(2))},
			},
		},
		{
			title:    "setof(X, (Y^(X=1 ; Y=2) ; X=3), S).",
			template: NewNamedVariable("X"),
			goal: Atom(";").Apply(
				Atom("^").Apply(
					NewNamedVariable("Y"),
					Atom(";").Apply(
						Atom("=").Apply(NewNamedVariable("X"), Integer(1)),
						Atom("=").Apply(NewNamedVariable("Y"), Integer(2)),
					),
				),
				Atom("=").Apply(NewNamedVariable("X"), Integer(3)),
			),
			instances: NewNamedVariable("S"),
			env: []map[Variable]Term{
				{s: List(Integer(3)), y: NewVariable()},
			},
			warning: true,
		},
		{
			title:    "setof(X, (X=Y ; X=Z ; Y=1), S).",
			template: NewNamedVariable("X"),
			goal: Atom(";").Apply(
				Atom("=").Apply(NewNamedVariable("X"), NewNamedVariable("Y")),
				Atom(";").Apply(
					Atom("=").Apply(NewNamedVariable("X"), NewNamedVariable("Z")),
					Atom("=").Apply(NewNamedVariable("Y"), Integer(1)),
				),
			),
			instances: NewNamedVariable("S"),
			env: []map[Variable]Term{
				{s: List(NewNamedVariable("Y"), NewNamedVariable("Z"))},
				{s: List(NewVariable())},
			},
		},
		{
			title:     "setof(X, a(X, Y), L).",
			template:  NewNamedVariable("X"),
			goal:      Atom("a").Apply(NewNamedVariable("X"), NewNamedVariable("Y")),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{l: List(Integer(1), Integer(2)), y: Atom("f").Apply(NewVariable())},
			},
		},
		{
			title:     "setof(X, member(X,[f(U,b),f(V,c)]), L).",
			template:  NewNamedVariable("X"),
			goal:      Atom("member").Apply(NewNamedVariable("X"), List(Atom("f").Apply(NewNamedVariable("U"), Atom("b")), Atom("f").Apply(NewNamedVariable("V"), Atom("c")))),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{l: List(Atom("f").Apply(NewNamedVariable("U"), Atom("b")), Atom("f").Apply(NewNamedVariable("V"), Atom("c")))},
			},
		},
		{
			title:     "setof(X, member(X,[f(U,b),f(V,c)]), [f(a,c),f(a,b)]).",
			template:  NewNamedVariable("X"),
			goal:      Atom("member").Apply(NewNamedVariable("X"), List(Atom("f").Apply(NewNamedVariable("U"), Atom("b")), Atom("f").Apply(NewNamedVariable("V"), Atom("c")))),
			instances: List(Atom("f").Apply(Atom("a"), Atom("c")), Atom("f").Apply(Atom("a"), Atom("b"))),
			env:       nil,
		},
		{
			title:     "setof(X, member(X,[f(b,U),f(c,V)]), [f(b,a),f(c,a)]).",
			template:  NewNamedVariable("X"),
			goal:      Atom("member").Apply(NewNamedVariable("X"), List(Atom("f").Apply(Atom("b"), NewNamedVariable("U")), Atom("f").Apply(Atom("c"), NewNamedVariable("V")))),
			instances: List(Atom("f").Apply(Atom("b"), Atom("a")), Atom("f").Apply(Atom("c"), Atom("a"))),
			env: []map[Variable]Term{
				{u: Atom("a"), v: Atom("a")},
			},
		},
		{
			title:     "setof(X, member(X,[V,U,f(U),f(V)]), L).",
			template:  NewNamedVariable("X"),
			goal:      Atom("member").Apply(NewNamedVariable("X"), List(NewNamedVariable("V"), NewNamedVariable("U"), Atom("f").Apply(NewNamedVariable("U")), Atom("f").Apply(NewNamedVariable("V")))),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{l: List(NewNamedVariable("U"), NewNamedVariable("V"), Atom("f").Apply(NewNamedVariable("U")), Atom("f").Apply(NewNamedVariable("V")))},
			},
		},
		{
			title:     "setof(X, member(X,[V,U,f(U),f(V)]), [a,b,f(a),f(b)]).",
			template:  NewNamedVariable("X"),
			goal:      Atom("member").Apply(NewNamedVariable("X"), List(NewNamedVariable("V"), NewNamedVariable("U"), Atom("f").Apply(NewNamedVariable("U")), Atom("f").Apply(NewNamedVariable("V")))),
			instances: List(Atom("a"), Atom("b"), Atom("f").Apply(Atom("a")), Atom("f").Apply(Atom("b"))),
			env: []map[Variable]Term{
				{u: Atom("a"), v: Atom("b")},
			},
		},
		{
			title:     "setof(X, member(X,[V,U,f(U),f(V)]), [a,b,f(b),f(a)]).",
			template:  NewNamedVariable("X"),
			goal:      Atom("member").Apply(NewNamedVariable("X"), List(NewNamedVariable("V"), NewNamedVariable("U"), Atom("f").Apply(NewNamedVariable("U")), Atom("f").Apply(NewNamedVariable("V")))),
			instances: List(Atom("a"), Atom("b"), Atom("f").Apply(Atom("b")), Atom("f").Apply(Atom("a"))),
			env:       nil,
		},
		{
			title:    "setof(X, (exists(U,V)^member(X,[V,U,f(U),f(V)])), [a,b,f(b),f(a)]).",
			template: NewNamedVariable("X"),
			goal: Atom("^").Apply(
				Atom("exists").Apply(NewNamedVariable("U"), NewNamedVariable("V")),
				Atom("member").Apply(NewNamedVariable("X"), List(NewNamedVariable("V"), NewNamedVariable("U"), Atom("f").Apply(NewNamedVariable("U")), Atom("f").Apply(NewNamedVariable("V")))),
			),
			instances: List(Atom("a"), Atom("b"), Atom("f").Apply(Atom("b")), Atom("f").Apply(Atom("a"))),
			env: []map[Variable]Term{
				{},
			},
		},
		{
			title:     "setof(X, b(X, Y), L).",
			template:  NewNamedVariable("X"),
			goal:      Atom("b").Apply(NewNamedVariable("X"), NewNamedVariable("Y")),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{l: List(Integer(1), Integer(2)), y: Integer(1)},
				{l: List(Integer(1), Integer(2)), y: Integer(2)},
			},
		},
		{
			title:    "setof(X-Xs, Y^setof(Y,b(X, Y),Xs), L).",
			template: Atom("-").Apply(NewNamedVariable("X"), NewNamedVariable("Xs")),
			goal: Atom("^").Apply(
				NewNamedVariable("Y"),
				Atom("setof").Apply(
					NewNamedVariable("Y"),
					Atom("b").Apply(NewNamedVariable("X"), NewNamedVariable("Y")),
					NewNamedVariable("Xs"),
				),
			),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{l: List(
					Atom("-").Apply(Integer(1), List(Integer(1), Integer(2))),
					Atom("-").Apply(Integer(2), List(Integer(1), Integer(2))),
				)},
			},
		},
		{
			title:    "setof(X-Xs, setof(Y,b(X, Y),Xs), L).",
			template: Atom("-").Apply(NewNamedVariable("X"), NewNamedVariable("Xs")),
			goal: Atom("^").Apply(
				NewNamedVariable("Y"),
				Atom("setof").Apply(
					NewNamedVariable("Y"),
					Atom("b").Apply(NewNamedVariable("X"), NewNamedVariable("Y")),
					NewNamedVariable("Xs"),
				),
			),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{
					l: List(
						Atom("-").Apply(Integer(1), List(Integer(1), Integer(2))),
						Atom("-").Apply(Integer(2), List(Integer(1), Integer(2))),
					),
				},
			},
		},
		{
			title:    "setof(X-Xs, bagof(Y,d(X, Y),Xs), L).",
			template: Atom("-").Apply(NewNamedVariable("X"), NewNamedVariable("Xs")),
			goal: Atom("^").Apply(
				NewNamedVariable("Y"),
				Atom("bagof").Apply(
					NewNamedVariable("Y"),
					Atom("d").Apply(NewNamedVariable("X"), NewNamedVariable("Y")),
					NewNamedVariable("Xs"),
				),
			),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{
					l: List(
						Atom("-").Apply(Integer(1), List(Integer(1), Integer(2), Integer(1))),
						Atom("-").Apply(Integer(2), List(Integer(2), Integer(1), Integer(2))),
					),
				},
			},
		},

		// 8.10.3.3 Errors
		{
			title:     "c",
			template:  Atom("t"),
			goal:      Atom("true"),
			instances: ListRest(Integer(1), Atom("t")),
			err:       TypeError(ValidTypeList, ListRest(Integer(1), Atom("t")), nil),
		},
	}

	state := State{
		VM: VM{
			unknown: unknownWarning,
		},
	}
	state.Register2("=", Unify)
	state.Register2(",", func(g1, g2 Term, k func(*Env) *Promise, env *Env) *Promise {
		return state.Call(g1, func(env *Env) *Promise {
			return state.Call(g2, k, env)
		}, env)
	})
	state.Register2(";", func(g1, g2 Term, k func(*Env) *Promise, env *Env) *Promise {
		return Delay(func(context.Context) *Promise {
			return state.Call(g1, k, env)
		}, func(context.Context) *Promise {
			return state.Call(g2, k, env)
		})
	})
	state.Register0("true", func(k func(*Env) *Promise, env *Env) *Promise {
		return k(env)
	})
	state.Register0("fail", func(func(*Env) *Promise, *Env) *Promise {
		return Bool(false)
	})
	state.Register2("a", func(x, y Term, k func(*Env) *Promise, env *Env) *Promise {
		const a, f = Atom("$a"), Atom("f")
		return Delay(func(context.Context) *Promise {
			return Unify(a.Apply(x, y), a.Apply(Integer(1), f.Apply(NewVariable())), k, env)
		}, func(context.Context) *Promise {
			return Unify(a.Apply(x, y), a.Apply(Integer(2), f.Apply(NewVariable())), k, env)
		})
	})
	state.Register2("b", func(x, y Term, k func(*Env) *Promise, env *Env) *Promise {
		const b = Atom("$b")
		return Delay(func(context.Context) *Promise {
			return Unify(b.Apply(x, y), b.Apply(Integer(1), Integer(1)), k, env)
		}, func(context.Context) *Promise {
			return Unify(b.Apply(x, y), b.Apply(Integer(1), Integer(1)), k, env)
		}, func(context.Context) *Promise {
			return Unify(b.Apply(x, y), b.Apply(Integer(1), Integer(2)), k, env)
		}, func(context.Context) *Promise {
			return Unify(b.Apply(x, y), b.Apply(Integer(2), Integer(1)), k, env)
		}, func(context.Context) *Promise {
			return Unify(b.Apply(x, y), b.Apply(Integer(2), Integer(2)), k, env)
		}, func(context.Context) *Promise {
			return Unify(b.Apply(x, y), b.Apply(Integer(2), Integer(2)), k, env)
		})
	})
	state.Register2("d", func(x, y Term, k func(*Env) *Promise, env *Env) *Promise {
		const d = Atom("$d")
		return Delay(func(context.Context) *Promise {
			return Unify(d.Apply(x, y), d.Apply(Integer(1), Integer(1)), k, env)
		}, func(context.Context) *Promise {
			return Unify(d.Apply(x, y), d.Apply(Integer(1), Integer(2)), k, env)
		}, func(context.Context) *Promise {
			return Unify(d.Apply(x, y), d.Apply(Integer(1), Integer(1)), k, env)
		}, func(context.Context) *Promise {
			return Unify(d.Apply(x, y), d.Apply(Integer(2), Integer(2)), k, env)
		}, func(context.Context) *Promise {
			return Unify(d.Apply(x, y), d.Apply(Integer(2), Integer(1)), k, env)
		}, func(context.Context) *Promise {
			return Unify(d.Apply(x, y), d.Apply(Integer(2), Integer(2)), k, env)
		})
	})
	state.Register2("member", func(elem, list Term, k func(*Env) *Promise, env *Env) *Promise {
		var ks []func(context.Context) *Promise
		iter := ListIterator{List: list, Env: env, AllowPartial: true}
		for iter.Next() {
			e := iter.Current()
			ks = append(ks, func(context.Context) *Promise {
				return Unify(elem, e, k, env)
			})
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}
		return Delay(ks...)
	})
	state.Register3("setof", state.SetOf)
	state.Register3("bagof", state.BagOf)

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			state.OnUnknown = func(ProcedureIndicator, []Term, *Env) {
				assert.True(t, tt.warning)
			}
			_, err := state.SetOf(tt.template, tt.goal, tt.instances, func(env *Env) *Promise {
				for k, v := range tt.env[0] {
					_, ok := env.Unify(v, k, false)
					assert.True(t, ok)
				}
				tt.env = tt.env[1:]
				return Bool(false)
			}, nil).Force(context.Background())
			assert.Equal(t, tt.err, err)
			assert.Empty(t, tt.env)
		})
	}
}

func TestState_FindAll(t *testing.T) {
	s := NewNamedVariable("S")
	l := NewNamedVariable("L")
	tests := []struct {
		title                     string
		template, goal, instances Term
		ok                        bool
		err                       error
		env                       map[Variable]Term
	}{
		// 8.10.1.4 Examples
		{title: "1", template: NewNamedVariable("X"), goal: Atom(";").Apply(Atom("=").Apply(NewNamedVariable("X"), Integer(1)), Atom("=").Apply(NewNamedVariable("X"), Integer(2))), instances: NewNamedVariable("S"), ok: true, env: map[Variable]Term{
			s: List(Integer(1), Integer(2)),
		}},
		{title: "2", template: Atom("+").Apply(NewNamedVariable("X"), NewNamedVariable("Y")), goal: Atom("=").Apply(NewNamedVariable("X"), Integer(1)), instances: NewNamedVariable("S"), ok: true, env: map[Variable]Term{
			s: List(Atom("+").Apply(Integer(1), NewVariable())),
		}},
		{title: "3", template: NewNamedVariable("X"), goal: Atom("fail"), instances: NewNamedVariable("L"), ok: true, env: map[Variable]Term{
			l: List(),
		}},
		{title: "4", template: NewNamedVariable("X"), goal: Atom(";").Apply(Atom("=").Apply(NewNamedVariable("X"), Integer(1)), Atom("=").Apply(NewNamedVariable("X"), Integer(1))), instances: NewNamedVariable("S"), ok: true, env: map[Variable]Term{
			s: List(Integer(1), Integer(1)),
		}},
		{title: "5", template: NewNamedVariable("X"), goal: Atom(";").Apply(Atom("=").Apply(NewNamedVariable("X"), Integer(2)), Atom("=").Apply(NewNamedVariable("X"), Integer(1))), instances: List(Integer(1), Integer(2)), ok: false},
		{title: "6", template: NewNamedVariable("X"), goal: NewNamedVariable("Goal"), instances: NewNamedVariable("S"), err: InstantiationError(nil)},
		{title: "7", template: NewNamedVariable("X"), goal: Integer(4), instances: NewNamedVariable("S"), err: TypeError(ValidTypeCallable, Integer(4), nil)},

		// 8.10.1.3 Errors
		{title: "c", template: NewNamedVariable("X"), goal: Atom(";").Apply(Atom("=").Apply(NewNamedVariable("X"), Integer(1)), Atom("=").Apply(NewNamedVariable("X"), Integer(2))), instances: Atom("foo"), err: TypeError(ValidTypeList, Atom("foo"), nil)},
	}

	var state State
	state.Register2("=", Unify)
	state.Register2(";", func(g1, g2 Term, k func(*Env) *Promise, env *Env) *Promise {
		return Delay(func(context.Context) *Promise {
			return state.Call(g1, k, env)
		}, func(context.Context) *Promise {
			return state.Call(g2, k, env)
		})
	})
	state.Register0("fail", func(func(*Env) *Promise, *Env) *Promise {
		return Bool(false)
	})

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := state.FindAll(tt.template, tt.goal, tt.instances, func(env *Env) *Promise {
				for k, v := range tt.env {
					_, ok := env.Unify(v, k, false)
					assert.True(t, ok)
				}
				return Bool(true)
			}, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestCompare(t *testing.T) {
	order := NewNamedVariable("Order")
	tests := []struct {
		title       string
		order, x, y Term
		ok          bool
		err         error
		env         map[Variable]Term
	}{
		// 8.4.2.4 Examples
		{title: `compare(Order, 3, 5).`, order: NewNamedVariable("Order"), x: Integer(3), y: Integer(5), ok: true, env: map[Variable]Term{
			order: Atom("<"),
		}},
		{title: `compare(Order, d, d).`, order: NewNamedVariable("Order"), x: Atom("d"), y: Atom("d"), ok: true, env: map[Variable]Term{
			order: Atom("="),
		}},
		{title: `compare(Order, Order, <).`, order: NewNamedVariable("Order"), x: NewNamedVariable("Order"), y: Atom("<"), ok: true, env: map[Variable]Term{
			order: Atom("<"),
		}},
		{title: `compare(<, <, <).`, order: Atom("<"), x: Atom("<"), y: Atom("<"), ok: false},
		{title: `compare(1+2, 3, 3.0).`, order: Atom("+").Apply(Integer(1), Integer(2)), x: Integer(3), y: Float(3.0), ok: false, err: TypeError(ValidTypeAtom, Atom("+").Apply(Integer(1), Integer(2)), nil)},
		{title: `compare(>=, 3, 3.0).`, order: Atom(">="), x: Integer(3), y: Float(3.0), ok: false, err: DomainError(ValidDomainOrder, Atom(">="), nil)},

		{title: `missing case for >`, order: Atom(">"), x: Integer(2), y: Integer(1), ok: true},
	}

	for _, tt := range tests {
		ok, err := Compare(tt.order, tt.x, tt.y, func(env *Env) *Promise {
			for k, v := range tt.env {
				assert.Equal(t, v, env.Resolve(k))
			}
			return Bool(true)
		}, nil).Force(context.Background())
		assert.Equal(t, tt.ok, ok)
		assert.Equal(t, tt.err, err)
	}
}

func TestBetween(t *testing.T) {
	t.Run("value is an integer", func(t *testing.T) {
		t.Run("between lower and upper", func(t *testing.T) {
			ok, err := Between(Integer(1), Integer(3), Integer(2), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("equal to lower", func(t *testing.T) {
			ok, err := Between(Integer(1), Integer(3), Integer(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("equal to upper", func(t *testing.T) {
			ok, err := Between(Integer(1), Integer(3), Integer(3), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("value, lower, higher are all equal", func(t *testing.T) {
			ok, err := Between(Integer(1), Integer(1), Integer(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("value, lower, higher are all MaxInt64", func(t *testing.T) {
			ok, err := Between(Integer(math.MaxInt64), Integer(math.MaxInt64), Integer(math.MaxInt64), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("equal to lower, but lower > upper", func(t *testing.T) {
			ok, err := Between(Integer(3), Integer(1), Integer(3), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("less than lower", func(t *testing.T) {
			ok, err := Between(Integer(1), Integer(3), Integer(0), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("greater than upper", func(t *testing.T) {
			ok, err := Between(Integer(1), Integer(3), Integer(100), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("value is a variable", func(t *testing.T) {
		t.Run("lower and upper are equal integers", func(t *testing.T) {
			value := NewNamedVariable("Value")
			ok, err := Between(Integer(1), Integer(1), value, func(env *Env) *Promise {
				assert.Equal(t, Integer(1), env.Resolve(value))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("lower and upper are MaxInt64", func(t *testing.T) {
			value := NewNamedVariable("Value")
			ok, err := Between(Integer(math.MaxInt64), Integer(math.MaxInt64), value, func(env *Env) *Promise {
				assert.Equal(t, Integer(math.MaxInt64), env.Resolve(value))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("multiple choice points", func(t *testing.T) {
			var n int
			value := NewNamedVariable("Value")
			ok, err := Between(Integer(0), Integer(3), value, func(env *Env) *Promise {
				assert.Equal(t, Integer(n), env.Resolve(value))
				n++
				return Bool(false)
			}, nil).Force(context.Background())
			assert.Equal(t, n, 4)
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("lower > upper", func(t *testing.T) {
			value := NewNamedVariable("Value")
			ok, err := Between(Integer(3), Integer(0), value, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("lower is uninstantiated", func(t *testing.T) {
		_, err := Between(NewNamedVariable("X"), Integer(2), Integer(1), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
	})

	t.Run("upper is uninstantiated", func(t *testing.T) {
		_, err := Between(Integer(1), NewNamedVariable("X"), Integer(1), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
	})

	t.Run("lower is not an integer", func(t *testing.T) {
		_, err := Between(Atom("inf"), Integer(2), Integer(1), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, Atom("inf"), nil), err)
	})

	t.Run("upper is not an integer", func(t *testing.T) {
		_, err := Between(Integer(1), Atom("inf"), Integer(1), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, Atom("inf"), nil), err)
	})

	t.Run("value is not an integer or variable", func(t *testing.T) {
		_, err := Between(Integer(1), Integer(1), Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, Atom("foo"), nil), err)
	})
}

func TestSort(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		t.Run("variable", func(t *testing.T) {
			sorted := NewNamedVariable("Sorted")
			ok, err := Sort(List(Atom("a"), Atom("c"), Atom("b"), Atom("a")), sorted, func(env *Env) *Promise {
				assert.Equal(t, List(Atom("a"), Atom("b"), Atom("c")), env.Resolve(sorted))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("list", func(t *testing.T) {
			ok, err := Sort(List(Atom("a"), Atom("c"), Atom("b"), Atom("a")), List(Atom("a"), Atom("b"), Atom("c")), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("list is a partial list", func(t *testing.T) {
		_, err := Sort(ListRest(NewNamedVariable("X"), Atom("a"), Atom("b")), NewNamedVariable("Sorted"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
	})

	t.Run("list is neither a partial list nor a list", func(t *testing.T) {
		_, err := Sort(Atom("a"), NewNamedVariable("Sorted"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeList, Atom("a"), nil), err)
	})

	t.Run("sorted is neither a partial list nor a list", func(t *testing.T) {
		t.Run("obviously not a list", func(t *testing.T) {
			_, err := Sort(List(Atom("a")), Atom("a"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeList, Atom("a"), nil), err)
		})

		t.Run("list-ish", func(t *testing.T) {
			_, err := Sort(List(Atom("a")), &compound{functor: ".", args: []Term{Atom("a")}}, Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeList, &compound{functor: ".", args: []Term{Atom("a")}}, nil), err)
		})
	})
}

func TestKeySort(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		t.Run("variable", func(t *testing.T) {
			sorted := NewNamedVariable("Sorted")
			ok, err := KeySort(List(
				Pair(Atom("c"), Atom("4")),
				Pair(Atom("b"), Atom("3")),
				Pair(Atom("a"), Atom("1")),
				Pair(Atom("a"), Atom("2")),
			), sorted, func(env *Env) *Promise {
				assert.Equal(t, List(
					Pair(Atom("a"), Atom("1")),
					Pair(Atom("a"), Atom("2")),
					Pair(Atom("b"), Atom("3")),
					Pair(Atom("c"), Atom("4")),
				), env.Resolve(sorted))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("list", func(t *testing.T) {
			second := NewNamedVariable("Second")
			ok, err := KeySort(List(
				Pair(Atom("c"), Atom("4")),
				Pair(Atom("b"), Atom("3")),
				Pair(Atom("a"), Atom("1")),
				Pair(Atom("a"), Atom("2")),
			), List(
				Pair(Atom("a"), Atom("1")),
				second,
				Pair(Atom("b"), Atom("3")),
				Pair(Atom("c"), Atom("4")),
			), func(env *Env) *Promise {
				assert.Equal(t, Pair(Atom("a"), Atom("2")), env.Resolve(second))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("pairs is a partial list", func(t *testing.T) {
		_, err := KeySort(ListRest(NewNamedVariable("Rest"), Pair(Atom("a"), Integer(1))), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
	})

	t.Run("pairs is neither a partial list nor a list", func(t *testing.T) {
		_, err := KeySort(Atom("a"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeList, Atom("a"), nil), err)
	})

	t.Run("sorted is neither a partial list nor a list", func(t *testing.T) {
		_, err := KeySort(List(), Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeList, Atom("foo"), nil), err)
	})

	t.Run("an element of a list prefix of pairs is a variable", func(t *testing.T) {
		_, err := KeySort(List(NewNamedVariable("X")), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
	})

	t.Run("an element of a list prefix of pairs is neither a variable nor a compound term with principal functor (-)/2", func(t *testing.T) {
		t.Run("atomic", func(t *testing.T) {
			_, err := KeySort(List(Atom("foo")), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypePair, Atom("foo"), nil), err)
		})

		t.Run("compound", func(t *testing.T) {
			_, err := KeySort(List(Atom("f").Apply(Atom("a"))), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypePair, Atom("f").Apply(Atom("a")), nil), err)
		})
	})

	t.Run("an element of a list prefix of sorted is neither a variable nor a compound term with principal functor (-)/2", func(t *testing.T) {
		t.Run("atomic", func(t *testing.T) {
			_, err := KeySort(List(), List(Atom("foo")), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypePair, Atom("foo"), nil), err)
		})

		t.Run("compound", func(t *testing.T) {
			_, err := KeySort(List(), List(Atom("f").Apply(Atom("a"))), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypePair, Atom("f").Apply(Atom("a")), nil), err)
		})
	})
}

func TestThrow(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		ok, err := Throw(Atom("a"), Success, nil).Force(context.Background())
		assert.Equal(t, Exception{term: Atom("a")}, err)
		assert.False(t, ok)
	})

	t.Run("ball is a variable", func(t *testing.T) {
		ok, err := Throw(NewNamedVariable("Ball"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
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
		ok, err := state.Catch(&compound{
			functor: "throw",
			args:    []Term{Atom("a")},
		}, v, &compound{
			functor: "=",
			args:    []Term{v, Atom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not match", func(t *testing.T) {
		ok, err := state.Catch(&compound{
			functor: "throw",
			args:    []Term{Atom("a")},
		}, Atom("b"), Atom("fail"), Success, nil).Force(context.Background())
		assert.False(t, ok)
		ex, ok := err.(Exception)
		assert.True(t, ok)
		assert.Equal(t, Atom("a"), ex.term)
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
			{Name: "foo", Arity: 1}: &userDefined{},
		}}}
		ok, err := state.CurrentPredicate(&compound{
			functor: "/",
			args: []Term{
				Atom("foo"),
				Integer(1),
			},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("variable", func(t *testing.T) {
		var foo, bar, baz bool

		v := NewNamedVariable("V")

		state := State{VM: VM{procedures: map[ProcedureIndicator]procedure{
			{Name: "foo", Arity: 1}: &userDefined{},
			{Name: "bar", Arity: 1}: &userDefined{},
			{Name: "baz", Arity: 1}: &userDefined{},
		}}}
		ok, err := state.CurrentPredicate(v, func(env *Env) *Promise {
			c, ok := env.Resolve(v).(*compound)
			assert.True(t, ok)
			assert.Equal(t, Atom("/"), c.functor)
			assert.Len(t, c.args, 2)
			assert.Equal(t, Integer(1), c.args[1])
			switch c.args[0] {
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
		ok, err := state.CurrentPredicate(&compound{
			functor: "/",
			args: []Term{
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
			assert.Equal(t, TypeError(ValidTypePredicateIndicator, Atom("foo"), nil), err)
			assert.False(t, ok)
		})

		t.Run("compound", func(t *testing.T) {
			t.Run("non slash", func(t *testing.T) {
				var state State
				ok, err := state.CurrentPredicate(&compound{
					functor: "f",
					args:    []Term{Atom("a")},
				}, Success, nil).Force(context.Background())
				assert.Equal(t, TypeError(ValidTypePredicateIndicator, &compound{
					functor: "f",
					args:    []Term{Atom("a")},
				}, nil), err)
				assert.False(t, ok)
			})

			t.Run("slash but number", func(t *testing.T) {
				var state State
				ok, err := state.CurrentPredicate(&compound{
					functor: "/",
					args:    []Term{Integer(0), Integer(0)},
				}, Success, nil).Force(context.Background())
				assert.Equal(t, TypeError(ValidTypePredicateIndicator, &compound{
					functor: "/",
					args:    []Term{Integer(0), Integer(0)},
				}, nil), err)
				assert.False(t, ok)
			})

			t.Run("slash but path", func(t *testing.T) {
				var state State
				ok, err := state.CurrentPredicate(&compound{
					functor: "/",
					args:    []Term{Atom("foo"), Atom("bar")},
				}, Success, nil).Force(context.Background())
				assert.Equal(t, TypeError(ValidTypePredicateIndicator, &compound{
					functor: "/",
					args:    []Term{Atom("foo"), Atom("bar")},
				}, nil), err)
				assert.False(t, ok)
			})
		})
	})
}

func TestState_Assertz(t *testing.T) {
	t.Run("append", func(t *testing.T) {
		var state State

		ok, err := state.Assertz(&compound{
			functor: "foo",
			args:    []Term{Atom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.Assertz(&compound{
			functor: "foo",
			args:    []Term{Atom("b")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, &userDefined{dynamic: true, clauses: []clause{
			{
				pi: ProcedureIndicator{
					Name:  "foo",
					Arity: 1,
				},
				raw: &compound{
					functor: "foo",
					args:    []Term{Atom("a")},
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
				raw: &compound{
					functor: "foo",
					args:    []Term{Atom("b")},
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

	t.Run("clause is a variable", func(t *testing.T) {
		clause := NewNamedVariable("Term")

		var state State
		ok, err := state.Assertz(clause, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("clause is neither a variable, nor callable", func(t *testing.T) {
		var state State
		ok, err := state.Assertz(Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("head is a variable", func(t *testing.T) {
		head := NewNamedVariable("Head")

		var state State
		ok, err := state.Assertz(&compound{
			functor: ":-",
			args:    []Term{head, Atom("true")},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("head is neither a variable, nor callable", func(t *testing.T) {
		var state State
		ok, err := state.Assertz(&compound{
			functor: ":-",
			args:    []Term{Integer(0), Atom("true")},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("body contains a term which is not callable", func(t *testing.T) {
		var state State
		ok, err := state.Assertz(&compound{
			functor: ":-",
			args: []Term{
				Atom("foo"),
				&compound{
					functor: ",",
					args: []Term{
						Atom("true"),
						Integer(0),
					},
				},
			},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeCallable, &compound{
			functor: ",",
			args: []Term{
				Atom("true"),
				Integer(0),
			},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("static", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 0}: &userDefined{dynamic: false},
				},
			},
		}

		ok, err := state.Assertz(Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationModify, PermissionTypeStaticProcedure, &compound{
			functor: "/",
			args: []Term{
				Atom("foo"),
				Integer(0),
			},
		}, nil), err)
		assert.False(t, ok)
	})
}

func TestState_Asserta(t *testing.T) {
	t.Run("fact", func(t *testing.T) {
		var state State
		ok, err := state.Asserta(&compound{
			functor: "foo",
			args:    []Term{Atom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.Asserta(&compound{
			functor: "foo",
			args:    []Term{Atom("b")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, &userDefined{dynamic: true, clauses: []clause{
			{
				pi: ProcedureIndicator{Name: "foo", Arity: 1},
				raw: &compound{
					functor: "foo",
					args:    []Term{Atom("b")},
				},
				xrTable: []Term{Atom("b")},
				bytecode: bytecode{
					{opcode: opConst, operand: 0},
					{opcode: opExit},
				},
			},
			{
				pi: ProcedureIndicator{Name: "foo", Arity: 1},
				raw: &compound{
					functor: "foo",
					args:    []Term{Atom("a")},
				},
				xrTable: []Term{Atom("a")},
				bytecode: bytecode{
					{opcode: opConst, operand: 0},
					{opcode: opExit},
				},
			},
		}}, state.procedures[ProcedureIndicator{Name: "foo", Arity: 1}])
	})

	t.Run("rule", func(t *testing.T) {
		var state State
		ok, err := state.Asserta(&compound{
			functor: ":-",
			args: []Term{
				Atom("foo"),
				&compound{
					functor: "p",
					args:    []Term{Atom("b")},
				},
			},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.Asserta(&compound{
			functor: ":-",
			args: []Term{
				Atom("foo"),
				&compound{
					functor: ",",
					args: []Term{
						&compound{
							functor: "p",
							args:    []Term{Atom("a")},
						},
						Atom("!"),
					},
				},
			},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, &userDefined{dynamic: true, clauses: []clause{
			{
				pi: ProcedureIndicator{Name: "foo", Arity: 0},
				raw: &compound{
					functor: ":-",
					args: []Term{
						Atom("foo"),
						&compound{
							functor: ",",
							args: []Term{
								&compound{
									functor: "p",
									args:    []Term{Atom("a")},
								},
								Atom("!"),
							},
						},
					},
				},
				xrTable: []Term{
					Atom("a"),
					ProcedureIndicator{Name: "p", Arity: 1},
				},
				bytecode: bytecode{
					{opcode: opEnter},
					{opcode: opConst, operand: 0},
					{opcode: opCall, operand: 1},
					{opcode: opCut},
					{opcode: opExit},
				},
			},
			{
				pi: ProcedureIndicator{Name: "foo", Arity: 0},
				raw: &compound{
					functor: ":-",
					args: []Term{
						Atom("foo"),
						&compound{
							functor: "p",
							args:    []Term{Atom("b")},
						},
					},
				},
				xrTable: []Term{
					Atom("b"),
					ProcedureIndicator{Name: "p", Arity: 1},
				},
				bytecode: bytecode{
					{opcode: opEnter},
					{opcode: opConst, operand: 0},
					{opcode: opCall, operand: 1},
					{opcode: opExit},
				},
			},
		}}, state.procedures[ProcedureIndicator{Name: "foo", Arity: 0}])
	})

	t.Run("clause is a variable", func(t *testing.T) {
		clause := NewNamedVariable("Term")

		var state State
		ok, err := state.Asserta(clause, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("clause is neither a variable, nor callable", func(t *testing.T) {
		var state State
		ok, err := state.Asserta(Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("head is a variable", func(t *testing.T) {
		head := NewNamedVariable("Head")

		var state State
		ok, err := state.Asserta(&compound{
			functor: ":-",
			args:    []Term{head, Atom("true")},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("head is neither a variable, nor callable", func(t *testing.T) {
		var state State
		ok, err := state.Asserta(&compound{
			functor: ":-",
			args:    []Term{Integer(0), Atom("true")},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("body is not callable", func(t *testing.T) {
		var state State
		ok, err := state.Asserta(&compound{
			functor: ":-",
			args:    []Term{Atom("foo"), Integer(0)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("body contains a term which is not callable", func(t *testing.T) {
		var state State
		ok, err := state.Asserta(&compound{
			functor: ":-",
			args: []Term{
				Atom("foo"),
				&compound{
					functor: ",",
					args: []Term{
						Atom("true"),
						Integer(0)},
				},
			},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeCallable, &compound{
			functor: ",",
			args: []Term{
				Atom("true"),
				Integer(0)},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("static", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 0}: &userDefined{dynamic: false},
				},
			},
		}

		ok, err := state.Asserta(Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationModify, PermissionTypeStaticProcedure, &compound{
			functor: "/",
			args: []Term{
				Atom("foo"),
				Integer(0),
			},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("cut", func(t *testing.T) {
		var state State
		ok, err := state.Asserta(&compound{
			functor: ":-",
			args: []Term{
				Atom("foo"),
				Atom("!"),
			},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})
}

func TestState_Retract(t *testing.T) {
	t.Run("retract the first one", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 1}: &userDefined{dynamic: true, clauses: []clause{
						{raw: &compound{functor: "foo", args: []Term{Atom("a")}}},
						{raw: &compound{functor: "foo", args: []Term{Atom("b")}}},
						{raw: &compound{functor: "foo", args: []Term{Atom("c")}}},
					}},
				},
			},
		}

		ok, err := state.Retract(&compound{
			functor: "foo",
			args:    []Term{NewNamedVariable("X")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, &userDefined{dynamic: true, clauses: []clause{
			{raw: &compound{functor: "foo", args: []Term{Atom("b")}}},
			{raw: &compound{functor: "foo", args: []Term{Atom("c")}}},
		}}, state.procedures[ProcedureIndicator{Name: "foo", Arity: 1}])
	})

	t.Run("retract the specific one", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 1}: &userDefined{dynamic: true, clauses: []clause{
						{raw: &compound{functor: "foo", args: []Term{Atom("a")}}},
						{raw: &compound{functor: "foo", args: []Term{Atom("b")}}},
						{raw: &compound{functor: "foo", args: []Term{Atom("c")}}},
					}},
				},
			},
		}

		ok, err := state.Retract(&compound{
			functor: "foo",
			args:    []Term{Atom("b")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, &userDefined{dynamic: true, clauses: []clause{
			{raw: &compound{functor: "foo", args: []Term{Atom("a")}}},
			{raw: &compound{functor: "foo", args: []Term{Atom("c")}}},
		}}, state.procedures[ProcedureIndicator{Name: "foo", Arity: 1}])
	})

	t.Run("retract all", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 1}: &userDefined{dynamic: true, clauses: []clause{
						{raw: &compound{functor: "foo", args: []Term{Atom("a")}}},
						{raw: &compound{functor: "foo", args: []Term{Atom("b")}}},
						{raw: &compound{functor: "foo", args: []Term{Atom("c")}}},
					}},
				},
			},
		}

		ok, err := state.Retract(&compound{
			functor: "foo",
			args:    []Term{NewNamedVariable("X")},
		}, Failure, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
		assert.Empty(t, state.procedures[ProcedureIndicator{Name: "foo", Arity: 1}].(*userDefined).clauses)
	})

	t.Run("variable", func(t *testing.T) {
		var state State
		ok, err := state.Retract(NewNamedVariable("X"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("not callable", func(t *testing.T) {
		var state State
		ok, err := state.Retract(Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("no clause matches", func(t *testing.T) {
		var state State

		ok, err := state.Retract(&compound{
			functor: "foo",
			args:    []Term{NewNamedVariable("X")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("static", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 0}: &userDefined{dynamic: false},
				},
			},
		}

		ok, err := state.Retract(Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationModify, PermissionTypeStaticProcedure, &compound{
			functor: "/",
			args:    []Term{Atom("foo"), Integer(0)},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("exception in continuation", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 1}: &userDefined{dynamic: true, clauses: []clause{
						{raw: &compound{functor: "foo", args: []Term{Atom("a")}}},
					}},
				},
			},
		}

		ok, err := state.Retract(&compound{
			functor: "foo",
			args:    []Term{NewNamedVariable("X")},
		}, func(_ *Env) *Promise {
			return Error(errors.New("failed"))
		}, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)

		// removed
		assert.Empty(t, state.procedures[ProcedureIndicator{Name: "foo", Arity: 1}].(*userDefined).clauses)
	})
}

func TestState_Abolish(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 1}: &userDefined{dynamic: true, clauses: []clause{
						{raw: &compound{functor: "foo", args: []Term{Atom("a")}}},
						{raw: &compound{functor: "foo", args: []Term{Atom("b")}}},
						{raw: &compound{functor: "foo", args: []Term{Atom("c")}}},
					}},
				},
			},
		}

		ok, err := state.Abolish(&compound{
			functor: "/",
			args:    []Term{Atom("foo"), Integer(1)},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		_, ok = state.procedures[ProcedureIndicator{Name: "foo", Arity: 1}]
		assert.False(t, ok)
	})

	t.Run("pi is a variable", func(t *testing.T) {
		var state State
		ok, err := state.Abolish(NewNamedVariable("PI"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and either Name or Arity is a variable", func(t *testing.T) {
		t.Run("Name is a variable", func(t *testing.T) {
			var state State
			ok, err := state.Abolish(&compound{
				functor: "/",
				args:    []Term{NewNamedVariable("Name"), Integer(2)},
			}, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		})

		t.Run("Arity is a variable", func(t *testing.T) {
			var state State
			ok, err := state.Abolish(&compound{
				functor: "/",
				args:    []Term{Atom("foo"), NewNamedVariable("Arity")},
			}, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("pi is neither a variable nor a predicate indicator", func(t *testing.T) {
		t.Run("compound", func(t *testing.T) {
			var state State
			ok, err := state.Abolish(Atom("+").Apply(Atom("foo"), Integer(1)), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypePredicateIndicator, Atom("+").Apply(Atom("foo"), Integer(1)), nil), err)
			assert.False(t, ok)
		})

		t.Run("not a comnpound", func(t *testing.T) {
			var state State
			ok, err := state.Abolish(Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypePredicateIndicator, Integer(0), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("pi is a term Name/Arity and Name is neither a variable nor an atom", func(t *testing.T) {
		var state State
		ok, err := state.Abolish(&compound{
			functor: "/",
			args:    []Term{Integer(0), Integer(2)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and Arity is neither a variable nor an integer", func(t *testing.T) {
		var state State
		ok, err := state.Abolish(&compound{
			functor: "/",
			args:    []Term{Atom("foo"), Atom("bar")},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, Atom("bar"), nil), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and Arity is an integer less than zero", func(t *testing.T) {
		var state State
		ok, err := state.Abolish(&compound{
			functor: "/",
			args:    []Term{Atom("foo"), Integer(-2)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainNotLessThanZero, Integer(-2), nil), err)
		assert.False(t, ok)
	})

	t.Run("The predicate indicator pi is that of a static procedure", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "foo", Arity: 0}: &userDefined{dynamic: false},
				},
			},
		}
		ok, err := state.Abolish(&compound{
			functor: "/",
			args:    []Term{Atom("foo"), Integer(0)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationModify, PermissionTypeStaticProcedure, &compound{
			functor: "/",
			args:    []Term{Atom("foo"), Integer(0)},
		}, nil), err)
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
		assert.Equal(t, DomainError(ValidDomainStream, Integer(0), nil), err)
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
		assert.Equal(t, DomainError(ValidDomainStream, Integer(0), nil), err)
		assert.False(t, ok)
	})
}

func TestState_SetInput(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		v := NewNamedVariable("Stream")
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
		v := NewNamedVariable("Stream")
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
		var state State
		ok, err := state.SetInput(NewNamedVariable("Stream"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable, nor a stream term or alias", func(t *testing.T) {
		var state State
		ok, err := state.SetInput(Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var state State
		ok, err := state.SetInput(Atom("x"), Success, nil).Force(context.Background())
		assert.Equal(t, ExistenceError(ObjectTypeStream, Atom("x"), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		v := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(v, NewStream(os.Stdout, StreamModeWrite))
		var state State
		ok, err := state.SetInput(v, Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypeStream, v, env), err)
		assert.False(t, ok)
	})
}

func TestState_SetOutput(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		v := NewNamedVariable("Stream")
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
		var state State
		ok, err := state.SetOutput(NewNamedVariable("Stream"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable, nor a stream term or alias", func(t *testing.T) {
		var state State
		ok, err := state.SetOutput(Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var state State
		ok, err := state.SetOutput(Atom("x"), Success, nil).Force(context.Background())
		assert.Equal(t, ExistenceError(ObjectTypeStream, Atom("x"), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an input stream", func(t *testing.T) {
		s := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(s, NewStream(os.Stdin, StreamModeRead))

		var state State
		ok, err := state.SetOutput(s, Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationOutput, PermissionTypeStream, s, env), err)
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
			v := NewNamedVariable("Stream")
			ok, err := state.Open(Atom(f.Name()), Atom("read"), v, List(&compound{
				functor: "alias",
				args:    []Term{Atom("input")},
			}), func(env *Env) *Promise {
				ref, ok := env.Lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*Stream)
				assert.True(t, ok)

				assert.Equal(t, state.streams[Atom("input")], s)

				b, err := ioutil.ReadAll(bufio.NewReader(s.file))
				assert.NoError(t, err)
				assert.Equal(t, "test\n", string(b))

				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("type text", func(t *testing.T) {
			v := NewNamedVariable("Stream")
			ok, err := state.Open(Atom(f.Name()), Atom("read"), v, List(&compound{
				functor: "type",
				args:    []Term{Atom("text")},
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
			v := NewNamedVariable("Stream")
			ok, err := state.Open(Atom(f.Name()), Atom("read"), v, List(&compound{
				functor: "type",
				args:    []Term{Atom("binary")},
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
			v := NewNamedVariable("Stream")
			ok, err := state.Open(Atom(f.Name()), Atom("read"), v, List(&compound{
				functor: "reposition",
				args:    []Term{Atom("true")},
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
			v := NewNamedVariable("Stream")
			ok, err := state.Open(Atom(f.Name()), Atom("read"), v, List(&compound{
				functor: "reposition",
				args:    []Term{Atom("false")},
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
			v := NewNamedVariable("Stream")
			ok, err := state.Open(Atom(f.Name()), Atom("read"), v, List(&compound{
				functor: "eof_action",
				args:    []Term{Atom("error")},
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
			v := NewNamedVariable("Stream")
			ok, err := state.Open(Atom(f.Name()), Atom("read"), v, List(&compound{
				functor: "eof_action",
				args:    []Term{Atom("eof_code")},
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
			v := NewNamedVariable("Stream")
			ok, err := state.Open(Atom(f.Name()), Atom("read"), v, List(&compound{
				functor: "eof_action",
				args:    []Term{Atom("reset")},
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
			v := NewNamedVariable("Stream")
			ok, err := state.Open(Atom(f.Name()), Atom("read"), v, List(&compound{
				functor: "unknown",
				args:    []Term{Atom("option")},
			}), func(env *Env) *Promise {
				assert.Fail(t, "unreachable")
				return Bool(true)
			}, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})

		t.Run("wrong arity", func(t *testing.T) {
			v := NewNamedVariable("Stream")
			ok, err := state.Open(Atom(f.Name()), Atom("read"), v, List(&compound{
				functor: "type",
				args:    []Term{Atom("a"), Atom("b")},
			}), func(env *Env) *Promise {
				assert.Fail(t, "unreachable")
				return Bool(true)
			}, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})

		t.Run("variable arg", func(t *testing.T) {
			v := NewNamedVariable("Stream")
			ok, err := state.Open(Atom(f.Name()), Atom("read"), v, List(&compound{
				functor: "type",
				args:    []Term{NewVariable()},
			}), func(env *Env) *Promise {
				assert.Fail(t, "unreachable")
				return Bool(true)
			}, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})

		t.Run("non-atom arg", func(t *testing.T) {
			v := NewNamedVariable("Stream")
			ok, err := state.Open(Atom(f.Name()), Atom("read"), v, List(&compound{
				functor: "type",
				args:    []Term{Integer(0)},
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

		v := NewNamedVariable("Stream")

		ok, err := state.Open(Atom(n), Atom("write"), v, List(&compound{
			functor: "alias",
			args:    []Term{Atom("output")},
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

		v := NewNamedVariable("Stream")

		ok, err := state.Open(Atom(f.Name()), Atom("append"), v, List(&compound{
			functor: "alias",
			args:    []Term{Atom("append")},
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
		var state State
		ok, err := state.Open(NewNamedVariable("Source_Sink"), Atom("read"), NewNamedVariable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("mode is a variable", func(t *testing.T) {
		var state State
		ok, err := state.Open(Atom("/dev/null"), NewNamedVariable("Mode"), NewNamedVariable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("options is a partial list or a list with an element E which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			var state State
			ok, err := state.Open(Atom("/dev/null"), Atom("read"), NewNamedVariable("Stream"), ListRest(NewNamedVariable("Rest"),
				&compound{functor: "type", args: []Term{Atom("text")}},
				&compound{functor: "alias", args: []Term{Atom("foo")}},
			), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			var state State
			ok, err := state.Open(Atom("/dev/null"), Atom("read"), NewNamedVariable("Stream"), List(
				NewNamedVariable("Option"),
				&compound{functor: "type", args: []Term{Atom("text")}},
				&compound{functor: "alias", args: []Term{Atom("foo")}},
			), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("mode is neither a variable nor an atom", func(t *testing.T) {
		var state State
		ok, err := state.Open(Atom("/dev/null"), Integer(0), NewNamedVariable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		var state State
		ok, err := state.Open(Atom("/dev/null"), Atom("read"), NewNamedVariable("Stream"), Atom("list"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeList, Atom("list"), nil), err)
		assert.False(t, ok)
	})

	t.Run("stream is not a variable", func(t *testing.T) {
		var state State
		ok, err := state.Open(Atom("/dev/null"), Atom("read"), Atom("stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("sourceSink is neither a variable nor a source/sink", func(t *testing.T) {
		var state State
		ok, err := state.Open(Integer(0), Atom("read"), NewNamedVariable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainSourceSink, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("mode is an atom but not an input/output mode", func(t *testing.T) {
		var state State
		ok, err := state.Open(Atom("/dev/null"), Atom("foo"), NewNamedVariable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainIOMode, Atom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the options list is neither a variable nor a stream-option", func(t *testing.T) {
		var state State
		for _, o := range []Term{
			Atom("foo"),
			&compound{functor: "foo", args: []Term{Atom("bar")}},
			&compound{functor: "alias", args: []Term{Integer(0)}},
			&compound{functor: "type", args: []Term{Integer(0)}},
			&compound{functor: "reposition", args: []Term{Integer(0)}},
			&compound{functor: "eof_action", args: []Term{Integer(0)}},
		} {
			ok, err := state.Open(Atom("/dev/null"), Atom("read"), NewNamedVariable("Stream"), List(o), Success, nil).Force(context.Background())
			assert.Equal(t, DomainError(ValidDomainStreamOption, o, nil), err)
			assert.False(t, ok)
		}
	})

	// Derived from 5.5.12 Options in Cor.3
	t.Run("a component of an element E of the options list is a variable", func(t *testing.T) {
		var state State
		for _, o := range []Term{
			NewNamedVariable("X"),
			&compound{functor: "alias", args: []Term{NewNamedVariable("X")}},
			&compound{functor: "type", args: []Term{NewNamedVariable("X")}},
			&compound{functor: "reposition", args: []Term{NewNamedVariable("X")}},
			&compound{functor: "eof_action", args: []Term{NewNamedVariable("X")}},
		} {
			ok, err := state.Open(Atom("/dev/null"), Atom("read"), NewNamedVariable("Stream"), List(o), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		}
	})

	t.Run("the source/sink specified by sourceSink does not exist", func(t *testing.T) {
		f, err := ioutil.TempFile("", "open_test_existence")
		assert.NoError(t, err)
		assert.NoError(t, os.Remove(f.Name()))

		var state State
		ok, err := state.Open(Atom(f.Name()), Atom("read"), NewNamedVariable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, ExistenceError(ObjectTypeSourceSink, Atom(f.Name()), nil), err)
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
		ok, err := state.Open(Atom(f.Name()), Atom("read"), NewNamedVariable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationOpen, PermissionTypeSourceSink, Atom(f.Name()), nil), err)
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
		ok, err := state.Open(Atom(f.Name()), Atom("read"), NewNamedVariable("Stream"), List(&compound{
			functor: "alias",
			args:    []Term{Atom("foo")},
		}), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationOpen, PermissionTypeSourceSink, &compound{
			functor: "alias",
			args:    []Term{Atom("foo")},
		}, nil), err)
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
			ok, err := state.Close(s, List(&compound{
				functor: "force",
				args:    []Term{Atom("false")},
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
			ok, err := state.Close(s, List(&compound{
				functor: "force",
				args:    []Term{Atom("false")},
			}), Success, nil).Force(context.Background())
			assert.Equal(t, SystemError(errors.New("ng")), err)
			assert.False(t, ok)
		})
	})

	t.Run("force true", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			s, err := Open(Atom(f.Name()), StreamModeRead)
			assert.NoError(t, err)

			var state State
			ok, err := state.Close(s, List(&compound{
				functor: "force",
				args:    []Term{Atom("true")},
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
			ok, err := state.Close(s, List(&compound{
				functor: "force",
				args:    []Term{Atom("true")},
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
		var state State
		ok, err := state.Close(NewNamedVariable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("options is a partial list or a list with an element E which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			var state State
			ok, err := state.Close(&Stream{}, ListRest(NewNamedVariable("Rest"),
				&compound{functor: "force", args: []Term{Atom("true")}},
			), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			var state State
			ok, err := state.Close(&Stream{}, List(NewNamedVariable("Option"), &compound{functor: "force", args: []Term{Atom("true")}}), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		var state State
		ok, err := state.Close(&Stream{}, Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeList, Atom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream-term or alias", func(t *testing.T) {
		var state State
		ok, err := state.Close(Integer(0), List(), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the Options list is neither a variable nor a stream-option", func(t *testing.T) {
		t.Run("not a compound", func(t *testing.T) {
			var state State
			ok, err := state.Close(&Stream{}, List(Atom("foo")), Success, nil).Force(context.Background())
			assert.Equal(t, DomainError(ValidDomainStreamOption, Atom("foo"), nil), err)
			assert.False(t, ok)
		})

		t.Run("compound", func(t *testing.T) {
			t.Run("force but arity is not 1", func(t *testing.T) {
				var state State
				ok, err := state.Close(&Stream{}, List(Atom("force").Apply(Atom("a"), Atom("b"))), Success, nil).Force(context.Background())
				assert.Equal(t, DomainError(ValidDomainStreamOption, Atom("force").Apply(Atom("a"), Atom("b")), nil), err)
				assert.False(t, ok)
			})

			t.Run("force but the argument is a variable", func(t *testing.T) {
				var state State
				_, err := state.Close(&Stream{}, List(Atom("force").Apply(NewNamedVariable("X"))), Success, nil).Force(context.Background())
				_, ok := NewEnv().Unify(DomainError(ValidDomainStreamOption, Atom("force").Apply(NewVariable()), nil).term, err.(Exception).term, false)
				assert.True(t, ok)
			})

			t.Run("force but the argument is neither true nor false", func(t *testing.T) {
				var state State
				ok, err := state.Close(&Stream{}, List(Atom("force").Apply(Atom("meh"))), Success, nil).Force(context.Background())
				assert.Equal(t, DomainError(ValidDomainStreamOption, Atom("force").Apply(Atom("meh")), nil), err)
				assert.False(t, ok)
			})
		})
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var state State
		ok, err := state.Close(Atom("foo"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, ExistenceError(ObjectTypeStream, Atom("foo"), nil), err)
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
		fileSync = func(f *os.File) error {
			return errors.New("ng")
		}
		defer func() {
			fileSync = (*os.File).Sync
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
		var state State
		ok, err := state.FlushOutput(NewNamedVariable("Stream"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream-term or alias", func(t *testing.T) {
		var state State
		ok, err := state.FlushOutput(Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var state State
		ok, err := state.FlushOutput(Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, ExistenceError(ObjectTypeStream, Atom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("SorA is an input stream", func(t *testing.T) {
		s := NewStream(os.Stdin, StreamModeRead)

		var state State
		ok, err := state.FlushOutput(s, Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationOutput, PermissionTypeStream, s, nil), err)
		assert.False(t, ok)
	})
}

func TestState_WriteTerm(t *testing.T) {
	var buf bytes.Buffer
	w := NewStream(&rwc{w: &buf}, StreamModeWrite)
	r := NewStream(&rwc{r: &buf}, StreamModeRead)
	b := NewStream(&rwc{w: &buf}, StreamModeWrite, WithStreamType(StreamTypeBinary))

	err := errors.New("failed")

	var m mockWriter
	m.On("Write", mock.Anything).Return(0, err)

	mw := NewStream(&rwc{w: &m}, StreamModeWrite)

	tests := []struct {
		title               string
		sOrA, term, options Term
		env                 *Env
		ok                  bool
		err                 error
		output              string
		outputPattern       *regexp.Regexp
	}{
		// 8.14.2.4 Examples
		{title: `write_term(S, [1,2,3], []).`, sOrA: w, term: List(Integer(1), Integer(2), Integer(3)), options: List(), ok: true, output: `[1,2,3]`},
		{title: `write_canonical([1,2,3]).`, sOrA: w, term: List(Integer(1), Integer(2), Integer(3)), options: List(Atom("quoted").Apply(Atom("true")), Atom("ignore_ops").Apply(Atom("true"))), ok: true, output: `'.'(1,'.'(2,'.'(3,[])))`},
		{title: `write_term(S, '1<2', []).`, sOrA: w, term: Atom("1<2"), options: List(), ok: true, output: `1<2`},
		{title: `writeq(S, '1<2').`, sOrA: w, term: Atom("1<2"), options: List(Atom("quoted").Apply(Atom("true")), Atom("numbervars").Apply(Atom("true"))), ok: true, output: `'1<2'`},
		{title: `writeq('$VAR'(0)).`, sOrA: w, term: Atom("$VAR").Apply(Integer(0)), options: List(Atom("quoted").Apply(Atom("true")), Atom("numbervars").Apply(Atom("true"))), ok: true, output: `A`},
		{title: `write_term(S, '$VAR'(1), [numbervars(false)]).`, sOrA: w, term: Atom("$VAR").Apply(Integer(1)), options: List(Atom("numbervars").Apply(Atom("false"))), ok: true, output: `$VAR(1)`},
		{title: `write_term(S, '$VAR'(51), [numbervars(true)]).`, sOrA: w, term: Atom("$VAR").Apply(Integer(51)), options: List(Atom("numbervars").Apply(Atom("true"))), ok: true, output: `Z1`},
		{title: `write_term(1, [quoted(non_boolean)]).`, sOrA: w, term: Integer(1), options: List(Atom("quoted").Apply(Atom("non_boolean"))), err: DomainError(ValidDomainWriteOption, Atom("quoted").Apply(Atom("non_boolean")), nil)},
		{title: `write_term(1, [quoted(B)]).`, sOrA: w, term: Integer(1), options: List(Atom("quoted").Apply(NewNamedVariable("B"))), err: InstantiationError(nil)},
		{title: `B = true, write_term(1, [quoted(B)]).`, sOrA: w, env: NewEnv().Bind(NewNamedVariable("B"), Atom("true")), term: Integer(1), options: List(Atom("quoted").Apply(NewNamedVariable("B"))), ok: true, output: `1`},

		// 8.14.2.3 Errors
		{title: `a`, sOrA: NewNamedVariable("S"), term: Atom("foo"), options: List(), err: InstantiationError(nil)},
		{title: `b: partial list`, sOrA: w, term: Atom("foo"), options: ListRest(NewNamedVariable("X"), Atom("quoted").Apply(Atom("true"))), err: InstantiationError(nil)},
		{title: `b: variable element`, sOrA: w, term: Atom("foo"), options: List(NewNamedVariable("X")), err: InstantiationError(nil)},
		{title: `b: variable component`, sOrA: w, term: Atom("foo"), options: List(Atom("quoted").Apply(NewNamedVariable("X"))), err: InstantiationError(nil)},
		{title: `b: variable_names, partial list`, sOrA: w, term: Atom("foo"), options: List(Atom("variable_names").Apply(NewNamedVariable("L"))), err: InstantiationError(nil)},
		{title: `b: variable_names, element`, sOrA: w, term: Atom("foo"), options: List(Atom("variable_names").Apply(List(NewNamedVariable("E")))), err: InstantiationError(nil)},
		{title: `b: variable_names, name`, sOrA: w, term: NewNamedVariable("V"), options: List(Atom("variable_names").Apply(List(Atom("=").Apply(NewNamedVariable("N"), NewNamedVariable("V"))))), err: InstantiationError(nil)},
		{title: `c`, sOrA: w, term: Atom("foo"), options: Atom("options"), err: TypeError(ValidTypeList, Atom("options"), nil)},
		{title: `d`, sOrA: Integer(0), term: Atom("foo"), options: List(), err: DomainError(ValidDomainStreamOrAlias, Integer(0), nil)},
		{title: `e: not a compound`, sOrA: w, term: Atom("foo"), options: List(Atom("bar")), err: DomainError(ValidDomainWriteOption, Atom("bar"), nil)},
		{title: `e: arity is not 1`, sOrA: w, term: Atom("foo"), options: List(Atom("bar").Apply(Atom("a"), Atom("b"))), err: DomainError(ValidDomainWriteOption, Atom("bar").Apply(Atom("a"), Atom("b")), nil)},
		{title: `e: variable_names, not a list, atom`, sOrA: w, term: Atom("foo"), options: List(Atom("variable_names").Apply(Atom("a"))), err: DomainError(ValidDomainWriteOption, Atom("variable_names").Apply(Atom("a")), nil)},
		{title: `e: variable_names, not a list, atomic`, sOrA: w, term: Atom("foo"), options: List(Atom("variable_names").Apply(Integer(0))), err: DomainError(ValidDomainWriteOption, Atom("variable_names").Apply(Integer(0)), nil)},
		{title: `e: variable_names, element is not a pair, atomic`, sOrA: w, term: Atom("foo"), options: List(Atom("variable_names").Apply(List(Atom("a")))), err: DomainError(ValidDomainWriteOption, Atom("variable_names").Apply(List(Atom("a"))), nil)},
		{title: `e: variable_names, element is not a pair, compound`, sOrA: w, term: Atom("foo"), options: List(Atom("variable_names").Apply(List(Atom("f").Apply(Atom("a"))))), err: DomainError(ValidDomainWriteOption, Atom("variable_names").Apply(List(Atom("f").Apply(Atom("a")))), nil)},
		{title: `e: variable_names, name is not an atom`, sOrA: w, term: NewNamedVariable("V"), options: List(Atom("variable_names").Apply(List(Atom("=").Apply(Integer(0), NewNamedVariable("V"))))), err: DomainError(ValidDomainWriteOption, Atom("variable_names").Apply(List(Atom("=").Apply(Integer(0), NewVariable()))), nil)},
		{title: `e: boolean option, not an atom`, sOrA: w, term: Atom("foo"), options: List(Atom("quoted").Apply(Integer(0))), err: DomainError(ValidDomainWriteOption, Atom("quoted").Apply(Integer(0)), nil)},
		{title: `e: unknown functor`, sOrA: w, term: Atom("foo"), options: List(Atom("bar").Apply(Atom("true"))), err: DomainError(ValidDomainWriteOption, Atom("bar").Apply(Atom("true")), nil)},
		{title: `f`, sOrA: Atom("stream"), term: Atom("foo"), options: List(), err: ExistenceError(ObjectTypeStream, Atom("stream"), nil)},
		{title: `g`, sOrA: r, term: Atom("foo"), options: List(), err: PermissionError(OperationOutput, PermissionTypeStream, r, nil)},
		{title: `h`, sOrA: b, term: Atom("foo"), options: List(), err: PermissionError(OperationOutput, PermissionTypeBinaryStream, b, nil)},

		// 7.10.5
		{title: `a`, sOrA: w, term: NewNamedVariable("X"), options: List(), ok: true, outputPattern: regexp.MustCompile(`_\d+`)},

		{title: `variable_names`, sOrA: w, term: NewNamedVariable("V"), options: List(Atom("variable_names").Apply(List(
			Atom("=").Apply(Atom("n"), NewNamedVariable("V")), // left-most is used
			Atom("=").Apply(Atom("m"), NewNamedVariable("V")), // ignored
			Atom("=").Apply(Atom("a"), Atom("b")),             // ignored
		))), ok: true, output: `n`},

		{title: `failure`, sOrA: mw, term: Atom("foo"), options: List(), err: err},
	}

	var state State
	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			buf.Reset()
			ok, err := state.WriteTerm(tt.sOrA, tt.term, tt.options, Success, tt.env).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			if tt.err == nil {
				assert.NoError(t, err)
			} else if te, ok := tt.err.(*Exception); ok {
				_, ok := NewEnv().Unify(te.term, err.(*Exception).term, false)
				assert.True(t, ok)
			}
			if tt.outputPattern == nil {
				assert.Equal(t, tt.output, buf.String())
			} else {
				assert.Regexp(t, tt.outputPattern, buf.String())
			}
		})
	}
}

type mockTerm struct {
	mock.Mock
	WriteOptions
}

func (m *mockTerm) String() string {
	args := m.Called()
	return args.String(0)
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
		v := NewNamedVariable("Char")

		ok, err := CharCode(v, Integer(128512), func(env *Env) *Promise {
			assert.Equal(t, Atom("😀"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("query code", func(t *testing.T) {
		v := NewNamedVariable("Code")
		ok, err := CharCode(Atom("😀"), v, func(env *Env) *Promise {
			assert.Equal(t, Integer(128512), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("char and code are variables", func(t *testing.T) {
		char, code := NewNamedVariable("Char"), NewNamedVariable("Code")

		ok, err := CharCode(char, code, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("char is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("atom", func(t *testing.T) {
			ok, err := CharCode(Atom("foo"), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeCharacter, Atom("foo"), nil), err)
			assert.False(t, ok)
		})

		t.Run("non-atom", func(t *testing.T) {
			ok, err := CharCode(Integer(0), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeCharacter, Integer(0), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("code is neither a variable nor an integer", func(t *testing.T) {
		t.Run("char is variable", func(t *testing.T) {
			ok, err := CharCode(NewVariable(), Atom("foo"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeInteger, Atom("foo"), nil), err)
			assert.False(t, ok)
		})

		t.Run("char is a character", func(t *testing.T) {
			ok, err := CharCode(Atom("a"), Atom("x"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeInteger, Atom("x"), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("code is neither a variable nor a character-code", func(t *testing.T) {
		ok, err := CharCode(NewVariable(), Integer(-1), Success, nil).Force(context.Background())
		assert.Equal(t, RepresentationError(FlagCharacterCode, nil), err)
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
		var state State
		ok, err := state.PutByte(NewNamedVariable("Stream"), Integer(97), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("byt is a variable", func(t *testing.T) {
		s := NewStream(os.Stdout, StreamModeWrite)
		s.streamType = StreamTypeBinary

		var state State
		ok, err := state.PutByte(s, NewNamedVariable("Byte"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("byt is neither a variable nor an byte", func(t *testing.T) {
		s := NewStream(os.Stdout, StreamModeWrite)
		s.streamType = StreamTypeBinary

		t.Run("not even an integer", func(t *testing.T) {
			var state State
			ok, err := state.PutByte(s, Atom("byte"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeByte, Atom("byte"), nil), err)
			assert.False(t, ok)
		})

		t.Run("integer", func(t *testing.T) {
			var state State
			ok, err := state.PutByte(s, Integer(256), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeByte, Integer(256), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var state State
		ok, err := state.PutByte(Integer(0), Integer(97), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an input stream", func(t *testing.T) {
		s := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(s, NewStream(os.Stdin, StreamModeRead))

		var state State
		ok, err := state.PutByte(s, Integer(97), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationOutput, PermissionTypeStream, s, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a text stream", func(t *testing.T) {
		s := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(s, NewStream(os.Stdout, StreamModeWrite))

		var state State
		ok, err := state.PutByte(s, Integer(97), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationOutput, PermissionTypeTextStream, s, env), err)
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
		var state State
		ok, err := state.PutCode(NewNamedVariable("Stream"), Integer(97), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("code is a variable", func(t *testing.T) {
		var state State
		ok, err := state.PutCode(NewStream(os.Stdout, StreamModeWrite), NewNamedVariable("Code"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("code is neither a variable nor an integer", func(t *testing.T) {
		var state State
		ok, err := state.PutCode(NewStream(os.Stdout, StreamModeWrite), Atom("code"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, Atom("code"), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var state State
		ok, err := state.PutCode(Integer(0), Integer(97), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var state State
		ok, err := state.PutCode(Atom("foo"), Integer(97), Success, nil).Force(context.Background())
		assert.Equal(t, ExistenceError(ObjectTypeStream, Atom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an input stream", func(t *testing.T) {
		s := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(s, NewStream(os.Stdin, StreamModeRead))

		var state State
		ok, err := state.PutCode(s, Integer(97), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationOutput, PermissionTypeStream, s, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		stream := NewStream(os.Stdout, StreamModeWrite)
		stream.streamType = StreamTypeBinary

		s := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(s, stream)

		var state State
		ok, err := state.PutCode(s, Integer(97), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationOutput, PermissionTypeBinaryStream, s, env), err)
		assert.False(t, ok)
	})

	t.Run("code is an integer but not an character code", func(t *testing.T) {
		var state State
		ok, err := state.PutCode(NewStream(os.Stdout, StreamModeWrite), Integer(-1), Success, nil).Force(context.Background())
		assert.Equal(t, RepresentationError(FlagCharacterCode, nil), err)
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
		s, err := Open("testdata/foo.pl", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		v := NewNamedVariable("Term")

		var state State
		ok, err := state.ReadTerm(s, v, List(), func(env *Env) *Promise {
			assert.Equal(t, Atom("foo"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		s, err := Open("testdata/foo.pl", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		v := NewNamedVariable("Term")

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

		v, singletons := NewNamedVariable("Term"), NewNamedVariable("Singletons")

		var state State
		ok, err := state.ReadTerm(s, v, List(&compound{
			functor: "singletons",
			args:    []Term{singletons},
		}), func(env *Env) *Promise {
			c, ok := env.Resolve(v).(*compound)
			assert.True(t, ok)
			assert.Equal(t, Atom("f"), c.functor)
			assert.Len(t, c.args, 3)

			x, ok := c.args[0].(Variable)
			assert.True(t, ok)
			assert.Equal(t, x, c.args[1])

			y, ok := c.args[2].(Variable)
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

		v, variables := NewNamedVariable("Term"), NewNamedVariable("Variables")

		var state State
		ok, err := state.ReadTerm(s, v, List(&compound{
			functor: "variables",
			args:    []Term{variables},
		}), func(env *Env) *Promise {
			c, ok := env.Resolve(v).(*compound)
			assert.True(t, ok)
			assert.Equal(t, Atom("f"), c.functor)
			assert.Len(t, c.args, 3)

			x, ok := c.args[0].(Variable)
			assert.True(t, ok)
			assert.Equal(t, x, c.args[1])

			y, ok := c.args[2].(Variable)
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

		v, variableNames := NewNamedVariable("Term"), NewNamedVariable("VariableNames")

		var state State
		ok, err := state.ReadTerm(s, v, List(&compound{
			functor: "variable_names",
			args:    []Term{variableNames},
		}), func(env *Env) *Promise {
			c, ok := env.Resolve(v).(*compound)
			assert.True(t, ok)
			assert.Equal(t, Atom("f"), c.functor)
			assert.Len(t, c.args, 3)

			x, ok := c.args[0].(Variable)
			assert.True(t, ok)
			assert.Equal(t, x, c.args[1])

			y, ok := c.args[2].(Variable)
			assert.True(t, ok)

			assert.Equal(t, List(
				&compound{
					functor: "=",
					args:    []Term{Atom("X"), x},
				},
				&compound{
					functor: "=",
					args:    []Term{Atom("Y"), y},
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

		v := NewNamedVariable("Term")

		var state State

		ok, err := state.ReadTerm(s, v, List(), func(env *Env) *Promise {
			assert.Equal(t, &compound{functor: "foo", args: []Term{Atom("a")}}, env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.ReadTerm(s, v, List(), func(env *Env) *Promise {
			assert.Equal(t, &compound{functor: "foo", args: []Term{Atom("b")}}, env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.ReadTerm(s, v, List(), func(env *Env) *Promise {
			assert.Equal(t, &compound{functor: "foo", args: []Term{Atom("c")}}, env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		var state State
		ok, err := state.ReadTerm(NewNamedVariable("Stream"), NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("options is a partial list or a list with an element which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			var state State
			ok, err := state.ReadTerm(NewStream(os.Stdin, StreamModeRead), NewVariable(), ListRest(NewNamedVariable("Rest"),
				&compound{functor: "variables", args: []Term{NewNamedVariable("VL")}},
			), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			var state State
			ok, err := state.ReadTerm(NewStream(os.Stdin, StreamModeRead), NewVariable(), List(NewNamedVariable("Option"), &compound{functor: "variables", args: []Term{NewNamedVariable("VL")}}), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var state State
		ok, err := state.ReadTerm(Integer(0), NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		var state State
		ok, err := state.ReadTerm(NewStream(os.Stdin, StreamModeRead), NewVariable(), Atom("options"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeList, Atom("options"), nil), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the Options list is neither a variable nor a valid read-option", func(t *testing.T) {
		var state State
		ok, err := state.ReadTerm(NewStream(os.Stdin, StreamModeRead), NewVariable(), List(&compound{
			functor: "unknown",
			args:    []Term{Atom("option")},
		}), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainReadOption, &compound{
			functor: "unknown",
			args:    []Term{Atom("option")},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var state State
		ok, err := state.ReadTerm(Atom("foo"), NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, ExistenceError(ObjectTypeStream, Atom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		s := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(s, NewStream(os.Stdout, StreamModeWrite))

		var state State
		ok, err := state.ReadTerm(s, NewVariable(), List(), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypeStream, s, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		stream := NewStream(os.Stdin, StreamModeRead)
		stream.streamType = StreamTypeBinary

		s := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(s, stream)

		var state State
		ok, err := state.ReadTerm(s, NewVariable(), List(), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypeBinaryStream, s, env), err)
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

		s := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(s, stream)

		var state State
		ok, err := state.ReadTerm(s, NewVariable(), List(), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypePastEndOfStream, s, env), err)
		assert.False(t, ok)
	})

	t.Run("one or more characters were input, but they cannot be parsed as a sequence of tokens", func(t *testing.T) {
		t.Run("unexpected token", func(t *testing.T) {
			s, err := Open("testdata/unexpected_token.txt", StreamModeRead)
			assert.NoError(t, err)
			defer func() {
				assert.NoError(t, s.Close())
			}()

			var state State
			ok, err := state.ReadTerm(s, NewVariable(), List(), Success, nil).Force(context.Background())
			assert.Equal(t, SyntaxError(unexpectedTokenError{actual: Token{Kind: TokenLetterDigit, Val: "bar"}}, nil), err)
			assert.False(t, ok)
		})

		t.Run("insufficient", func(t *testing.T) {
			s, err := Open("testdata/insufficient.txt", StreamModeRead)
			assert.NoError(t, err)
			defer func() {
				assert.NoError(t, s.Close())
			}()

			var state State
			ok, err := state.ReadTerm(s, NewVariable(), List(), Success, nil).Force(context.Background())
			assert.Equal(t, SyntaxError(ErrInsufficient, nil), err)
			assert.False(t, ok)
		})

	})

	t.Run("the sequence of tokens cannot be parsed as a term using the current set of operator definitions", func(t *testing.T) {
		s, err := Open("testdata/unexpected_op.txt", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		var state State
		ok, err := state.ReadTerm(s, NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, SyntaxError(unexpectedTokenError{actual: Token{Kind: TokenGraphic, Val: "="}}, nil), err)
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

		v := NewNamedVariable("Byte")

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

		v := NewNamedVariable("Byte")

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

		v := NewNamedVariable("Byte")

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

		v := NewNamedVariable("V")
		_, err := state.GetByte(s, v, Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		var state State
		ok, err := state.GetByte(NewNamedVariable("Stream"), NewNamedVariable("InByte"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("inByte is neither a variable nor an in-byte", func(t *testing.T) {
		s := NewStream(os.Stdin, StreamModeRead)
		s.streamType = StreamTypeBinary

		t.Run("not even an integer", func(t *testing.T) {
			var state State
			ok, err := state.GetByte(s, Atom("inByte"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeInByte, Atom("inByte"), nil), err)
			assert.False(t, ok)
		})

		t.Run("integer", func(t *testing.T) {
			var state State
			ok, err := state.GetByte(s, Integer(256), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeInByte, Integer(256), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream-term or alias", func(t *testing.T) {
		var state State
		ok, err := state.GetByte(Integer(0), NewNamedVariable("InByte"), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var state State
		ok, err := state.GetByte(Atom("foo"), NewNamedVariable("InByte"), Success, nil).Force(context.Background())
		assert.Equal(t, ExistenceError(ObjectTypeStream, Atom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, NewStream(os.Stdout, StreamModeWrite))

		var state State
		ok, err := state.GetByte(streamOrAlias, NewNamedVariable("InByte"), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypeStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a text stream", func(t *testing.T) {
		streamOrAlias := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, NewStream(os.Stdin, StreamModeRead))

		var state State
		ok, err := state.GetByte(streamOrAlias, NewNamedVariable("InByte"), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypeTextStream, streamOrAlias, env), err)
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

		streamOrAlias := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, s)

		var state State
		ok, err := state.GetByte(streamOrAlias, NewNamedVariable("InByte"), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypePastEndOfStream, streamOrAlias, env), err)
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

		v := NewNamedVariable("Char")

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

		v := NewNamedVariable("Char")

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

		v := NewNamedVariable("Char")

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

		v := NewNamedVariable("V")

		var state State
		ok, err := state.GetChar(NewStream(os.Stdin, StreamModeRead), v, Success, nil).Force(context.Background())
		assert.Equal(t, SystemError(errors.New("failed")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		var state State
		ok, err := state.GetChar(NewNamedVariable("Stream"), NewNamedVariable("Char"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("char is neither a variable nor an in-character", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var state State
			ok, err := state.GetChar(NewStream(os.Stdin, StreamModeRead), Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeInCharacter, Integer(0), nil), err)
			assert.False(t, ok)
		})

		t.Run("atom", func(t *testing.T) {
			var state State
			ok, err := state.GetChar(NewStream(os.Stdin, StreamModeRead), Atom("ab"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeInCharacter, Atom("ab"), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var state State
		ok, err := state.GetChar(Integer(0), NewNamedVariable("Char"), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, NewStream(os.Stdout, StreamModeWrite))

		var state State
		ok, err := state.GetChar(streamOrAlias, NewNamedVariable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypeStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		s := NewStream(os.Stdin, StreamModeRead)
		s.streamType = StreamTypeBinary

		streamOrAlias := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, s)

		var state State
		ok, err := state.GetChar(streamOrAlias, NewNamedVariable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypeBinaryStream, streamOrAlias, env), err)
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

		streamOrAlias := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, s)

		var state State
		ok, err := state.GetChar(streamOrAlias, NewNamedVariable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypePastEndOfStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("the entity input from the stream is not a character", func(t *testing.T) {
		s, err := Open("testdata/replacement.txt", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		streamOrAlias := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, s)

		var state State
		ok, err := state.GetChar(streamOrAlias, NewNamedVariable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, RepresentationError(FlagCharacter, nil), err)
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

		v := NewNamedVariable("Byte")

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

		v := NewNamedVariable("Byte")

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

		v := NewNamedVariable("Byte")

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

		v := NewNamedVariable("V")

		var state State
		ok, err := state.PeekByte(s, v, Success, nil).Force(context.Background())
		assert.Equal(t, SystemError(errors.New("failed")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		var state State
		ok, err := state.PeekByte(NewNamedVariable("Stream"), NewNamedVariable("Byte"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("inByte is neither a variable nor an in-byte", func(t *testing.T) {
		s := NewStream(os.Stdin, StreamModeRead)
		s.streamType = StreamTypeBinary

		t.Run("not even an integer", func(t *testing.T) {
			var state State
			ok, err := state.PeekByte(s, Atom("byte"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeInByte, Atom("byte"), nil), err)
			assert.False(t, ok)
		})

		t.Run("integer", func(t *testing.T) {
			var state State
			ok, err := state.PeekByte(s, Integer(256), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeInByte, Integer(256), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var state State
		ok, err := state.PeekByte(Integer(0), NewNamedVariable("Byte"), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, NewStream(os.Stdout, StreamModeWrite))

		var state State
		ok, err := state.PeekByte(streamOrAlias, NewNamedVariable("Byte"), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypeStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a text stream", func(t *testing.T) {
		streamOrAlias := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, NewStream(os.Stdin, StreamModeRead))

		var state State
		ok, err := state.PeekByte(streamOrAlias, NewNamedVariable("Byte"), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypeTextStream, streamOrAlias, env), err)
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

		streamOrAlias := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, s)

		var state State
		ok, err := state.PeekByte(streamOrAlias, NewNamedVariable("Byte"), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypePastEndOfStream, streamOrAlias, env), err)
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

		v := NewNamedVariable("Char")

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

		v := NewNamedVariable("Char")

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

		v := NewNamedVariable("Char")

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

			v := NewNamedVariable("V")

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

			v := NewNamedVariable("V")

			var state State
			ok, err := state.PeekChar(s, v, Success, nil).Force(context.Background())
			assert.Equal(t, SystemError(errors.New("failed")), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		var state State
		ok, err := state.PeekChar(NewNamedVariable("Stream"), NewNamedVariable("Char"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("char is neither a variable nor an in-character", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var state State
			ok, err := state.PeekChar(NewStream(os.Stdin, StreamModeRead), Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeInCharacter, Integer(0), nil), err)
			assert.False(t, ok)
		})

		t.Run("atom", func(t *testing.T) {
			var state State
			ok, err := state.PeekChar(NewStream(os.Stdin, StreamModeRead), Atom("ab"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeInCharacter, Atom("ab"), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var state State
		ok, err := state.PeekChar(Integer(0), NewNamedVariable("Char"), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, NewStream(os.Stdout, StreamModeWrite))

		var state State
		ok, err := state.PeekChar(streamOrAlias, NewNamedVariable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypeStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		s := NewStream(os.Stdin, StreamModeRead)
		s.streamType = StreamTypeBinary

		streamOrAlias := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, s)

		var state State
		ok, err := state.PeekChar(streamOrAlias, NewNamedVariable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypeBinaryStream, streamOrAlias, env), err)
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

		streamOrAlias := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, s)

		var state State
		ok, err := state.PeekChar(streamOrAlias, NewNamedVariable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypePastEndOfStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("the entity input from the stream is not a character", func(t *testing.T) {
		s, err := Open("testdata/replacement.txt", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		streamOrAlias := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, s)

		var state State
		ok, err := state.PeekChar(streamOrAlias, NewNamedVariable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, RepresentationError(FlagCharacter, nil), err)
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
		n := NewNamedVariable("N")

		ok, err := Halt(n, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("n is neither a variable nor an integer", func(t *testing.T) {
		ok, err := Halt(Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, Atom("foo"), nil), err)
		assert.False(t, ok)
	})
}

func TestState_Clause(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		x := NewNamedVariable("X")
		what, body := NewNamedVariable("What"), NewNamedVariable("Body")

		var c int

		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "green", Arity: 1}: &userDefined{public: true, clauses: []clause{
						{raw: &compound{
							functor: ":-", args: []Term{
								&compound{functor: "green", args: []Term{x}},
								&compound{functor: "moldy", args: []Term{x}},
							},
						}},
						{raw: &compound{functor: "green", args: []Term{Atom("kermit")}}},
					}},
				},
			},
		}
		ok, err := state.Clause(&compound{
			functor: "green",
			args:    []Term{what},
		}, body, func(env *Env) *Promise {
			switch c {
			case 0:
				assert.True(t, env.Resolve(what).(Variable).Anonymous())
				b, ok := env.Resolve(body).(*compound)
				assert.True(t, ok)
				assert.Equal(t, Atom("moldy"), b.functor)
				assert.Len(t, b.args, 1)
				assert.True(t, b.args[0].(Variable).Anonymous())
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

	t.Run("not found", func(t *testing.T) {
		var state State
		ok, err := state.Clause(Atom("foo"), Atom("true"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("head is a variable", func(t *testing.T) {
		var state State
		ok, err := state.Clause(NewNamedVariable("Head"), Atom("true"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("head is neither a variable nor a predication", func(t *testing.T) {
		var state State
		ok, err := state.Clause(Integer(0), Atom("true"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("the predicate indicator Pred of Head is that of a private (ie. Not public) procedure", func(t *testing.T) {
		what, body := NewNamedVariable("What"), NewNamedVariable("Body")

		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: "green", Arity: 1}: predicate1(func(t Term, f func(*Env) *Promise, env *Env) *Promise {
						return Bool(true)
					}),
				},
			},
		}
		ok, err := state.Clause(&compound{
			functor: "green",
			args:    []Term{what},
		}, body, Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationAccess, PermissionTypePrivateProcedure, &compound{
			functor: "/",
			args:    []Term{Atom("green"), Integer(1)},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("body is neither a variable nor a callable term", func(t *testing.T) {
		var state State
		ok, err := state.Clause(Atom("foo"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeCallable, Integer(0), nil), err)
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
		atom := NewNamedVariable("Atom")
		ok, err := AtomLength(atom, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("atom is neither a variable nor an atom", func(t *testing.T) {
		ok, err := AtomLength(Integer(2), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeAtom, Integer(2), nil), err)
		assert.False(t, ok)
	})

	t.Run("length is neither a variable nor an integer", func(t *testing.T) {
		ok, err := AtomLength(Atom("😀"), Atom("1"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, Atom("1"), nil), err)
		assert.False(t, ok)
	})

	t.Run("length is an integer less than zero", func(t *testing.T) {
		ok, err := AtomLength(Atom("😀"), Integer(-1), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainNotLessThanZero, Integer(-1), nil), err)
		assert.False(t, ok)
	})
}

func TestAtomConcat(t *testing.T) {
	t.Run("atom3 is a variable", func(t *testing.T) {
		atom3 := NewNamedVariable("Atom3")

		ok, err := AtomConcat(Atom("foo"), Atom("bar"), atom3, func(env *Env) *Promise {
			assert.Equal(t, Atom("foobar"), env.Resolve(atom3))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("atom3 is an atom", func(t *testing.T) {
		var c int
		v1, v2 := NewNamedVariable("V1"), NewNamedVariable("V2")
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
		atom1, atom3 := NewNamedVariable("Atom1"), NewNamedVariable("Atom3")

		ok, err := AtomConcat(atom1, Atom("bar"), atom3, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("atom2 and atom3 are variables", func(t *testing.T) {
		atom2, atom3 := NewNamedVariable("Atom2"), NewNamedVariable("Atom3")

		ok, err := AtomConcat(Atom("foo"), atom2, atom3, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("atom1 is neither a variable nor an atom", func(t *testing.T) {
		t.Run("atom3 is a variable", func(t *testing.T) {
			ok, err := AtomConcat(Integer(1), Atom("bar"), NewNamedVariable("Atom3"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeAtom, Integer(1), nil), err)
			assert.False(t, ok)
		})

		t.Run("atom3 is an atom", func(t *testing.T) {
			ok, err := AtomConcat(Integer(1), Atom("bar"), Atom("foobar"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeAtom, Integer(1), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("atom2 is neither a variable nor an atom", func(t *testing.T) {
		t.Run("atom3 is a variable", func(t *testing.T) {
			ok, err := AtomConcat(Atom("foo"), Integer(2), NewNamedVariable("Atom3"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeAtom, Integer(2), nil), err)
			assert.False(t, ok)
		})

		t.Run("atom3 is an atom", func(t *testing.T) {
			ok, err := AtomConcat(Atom("foo"), Integer(2), Atom("foobar"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeAtom, Integer(2), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("atom3 is neither a variable nor an atom", func(t *testing.T) {
		ok, err := AtomConcat(Atom("foo"), Atom("bar"), Integer(3), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeAtom, Integer(3), nil), err)
		assert.False(t, ok)
	})
}

func TestSubAtom(t *testing.T) {
	t.Run("multiple solutions", func(t *testing.T) {
		before, length, after := NewNamedVariable("Before"), NewNamedVariable("Length"), NewNamedVariable("After")
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
		char := NewNamedVariable("Char")
		ok, err := SubAtom(Atom("a"), Integer(0), Integer(1), Integer(0), char, func(env *Env) *Promise {
			assert.Equal(t, Atom("a"), env.Resolve(char))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("atom is a variable", func(t *testing.T) {
		ok, err := SubAtom(NewNamedVariable("Atom"), NewNamedVariable("Before"), NewNamedVariable("Length"), NewNamedVariable("After"), NewNamedVariable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("atom is neither a variable nor an atom", func(t *testing.T) {
		ok, err := SubAtom(Integer(0), NewNamedVariable("Before"), NewNamedVariable("Length"), NewNamedVariable("After"), NewNamedVariable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("subAtom is neither a variable nor an atom", func(t *testing.T) {
		ok, err := SubAtom(Atom("foo"), NewNamedVariable("Before"), NewNamedVariable("Length"), NewNamedVariable("After"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("before is neither a variable nor an integer", func(t *testing.T) {
		ok, err := SubAtom(Atom("foo"), Atom("before"), NewNamedVariable("Length"), NewNamedVariable("After"), NewNamedVariable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, Atom("before"), nil), err)
		assert.False(t, ok)
	})

	t.Run("length is neither a variable nor an integer", func(t *testing.T) {
		ok, err := SubAtom(Atom("foo"), NewNamedVariable("Before"), Atom("length"), NewNamedVariable("After"), NewNamedVariable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, Atom("length"), nil), err)
		assert.False(t, ok)
	})

	t.Run("after is neither a variable nor an integer", func(t *testing.T) {
		ok, err := SubAtom(Atom("foo"), NewNamedVariable("Before"), NewNamedVariable("Length"), Atom("after"), NewNamedVariable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, Atom("after"), nil), err)
		assert.False(t, ok)
	})

	t.Run("before is an integer less than zero", func(t *testing.T) {
		ok, err := SubAtom(Atom("foo"), Integer(-1), NewNamedVariable("Length"), NewNamedVariable("After"), NewNamedVariable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainNotLessThanZero, Integer(-1), nil), err)
		assert.False(t, ok)
	})

	t.Run("length is an integer less than zero", func(t *testing.T) {
		ok, err := SubAtom(Atom("foo"), NewNamedVariable("Before"), Integer(-1), NewNamedVariable("After"), NewNamedVariable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainNotLessThanZero, Integer(-1), nil), err)
		assert.False(t, ok)
	})

	t.Run("after is an integer less than zero", func(t *testing.T) {
		ok, err := SubAtom(Atom("foo"), NewNamedVariable("Before"), NewNamedVariable("Length"), Integer(-1), NewNamedVariable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainNotLessThanZero, Integer(-1), nil), err)
		assert.False(t, ok)
	})
}

func TestAtomChars(t *testing.T) {
	l := NewNamedVariable("L")
	str := NewNamedVariable("Str")
	x := NewNamedVariable("X")
	tests := []struct {
		title      string
		atom, list Term
		ok         bool
		err        error
		env        map[Variable]Term
	}{
		// 8.16.4.4 Examples
		{title: "atom_chars('', L).", atom: Atom(""), list: NewNamedVariable("L"), ok: true, env: map[Variable]Term{
			l: List(),
		}},
		{title: "atom_chars([], L).", atom: Atom("[]"), list: NewNamedVariable("L"), ok: true, env: map[Variable]Term{
			l: List(Atom("["), Atom("]")),
		}},
		{title: "atom_chars('''', L).", atom: Atom("'"), list: NewNamedVariable("L"), ok: true, env: map[Variable]Term{
			l: List(Atom("'")),
		}},
		{title: "atom_chars('ant', L).", atom: Atom("ant"), list: NewNamedVariable("L"), ok: true, env: map[Variable]Term{
			l: List(Atom("a"), Atom("n"), Atom("t")),
		}},
		{title: "atom_chars(Str, ['s', 'o', 'p']).", atom: NewNamedVariable("Str"), list: List(Atom("s"), Atom("o"), Atom("p")), ok: true, env: map[Variable]Term{
			str: Atom("sop"),
		}},
		{title: "atom_chars('North', ['N' | X]).", atom: Atom("North"), list: ListRest(NewNamedVariable("X"), Atom("N")), ok: true, env: map[Variable]Term{
			x: List(Atom("o"), Atom("r"), Atom("t"), Atom("h")),
		}},
		{title: "atom_chars('soap', ['s', 'o', 'p']).", atom: Atom("soap"), list: List(Atom("s"), Atom("o"), Atom("p")), ok: false},
		{title: "atom_chars(X, Y).", atom: NewNamedVariable("X"), list: NewNamedVariable("Y"), err: InstantiationError(nil)},

		// 8.16.4.3 Errors
		{title: "a", atom: NewNamedVariable("X"), list: ListRest(NewNamedVariable("Y"), Atom("a")), err: InstantiationError(nil)},
		{title: "b", atom: Integer(0), list: List(Atom("a"), Atom("b"), Atom("c")), err: TypeError(ValidTypeAtom, Integer(0), nil)},
		{title: "c: atom is a variable", atom: NewNamedVariable("X"), list: Integer(0), err: TypeError(ValidTypeList, Integer(0), nil)},
		{title: "c: atom is an atom", atom: Atom("a"), list: Integer(0), err: TypeError(ValidTypeList, Integer(0), nil)},
		{title: "d", atom: NewNamedVariable("X"), list: List(NewNamedVariable("Y"), Atom("a")), err: InstantiationError(nil)},
		{title: "e: atom is a variable, more than one char", atom: NewNamedVariable("X"), list: List(Atom("abc")), err: TypeError(ValidTypeCharacter, Atom("abc"), nil)},
		{title: "e: atom is a variable, not an atom", atom: NewNamedVariable("X"), list: List(Integer(0)), err: TypeError(ValidTypeCharacter, Integer(0), nil)},
		{title: "e: atom is an atom, more than one char", atom: Atom("abc"), list: List(Atom("ab"), Atom("c")), err: TypeError(ValidTypeCharacter, Atom("ab"), nil)},
		{title: "e: atom is an atom, not an atom", atom: Atom("abc"), list: List(Integer('a'), Atom("b"), Atom("c")), err: TypeError(ValidTypeCharacter, Integer('a'), nil)},

		{title: "atom_chars('ant', ['a', X, 't']).", atom: Atom("ant"), list: List(Atom("a"), NewNamedVariable("X"), Atom("t")), ok: true, env: map[Variable]Term{
			x: Atom("n"),
		}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := AtomChars(tt.atom, tt.list, func(env *Env) *Promise {
				for k, v := range tt.env {
					_, ok := env.Unify(k, v, false)
					assert.True(t, ok)
				}
				return Bool(true)
			}, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestAtomCodes(t *testing.T) {
	l := NewNamedVariable("L")
	str := NewNamedVariable("Str")
	x := NewNamedVariable("X")
	tests := []struct {
		title      string
		atom, list Term
		ok         bool
		err        error
		env        map[Variable]Term
	}{
		// 8.16.5.4 Examples
		{title: "atom_codes('', L).", atom: Atom(""), list: NewNamedVariable("L"), ok: true, env: map[Variable]Term{
			l: List(),
		}},
		{title: "atom_codes([], L).", atom: Atom("[]"), list: NewNamedVariable("L"), ok: true, env: map[Variable]Term{
			l: List(Integer('['), Integer(']')),
		}},
		{title: "atom_codes('''', L).", atom: Atom("'"), list: NewNamedVariable("L"), ok: true, env: map[Variable]Term{
			l: List(Integer('\'')),
		}},
		{title: "atom_codes('ant', L).", atom: Atom("ant"), list: NewNamedVariable("L"), ok: true, env: map[Variable]Term{
			l: List(Integer('a'), Integer('n'), Integer('t')),
		}},
		{title: "atom_codes(Str, [0's, 0'o, 0'p]).", atom: NewNamedVariable("Str"), list: List(Integer('s'), Integer('o'), Integer('p')), ok: true, env: map[Variable]Term{
			str: Atom("sop"),
		}},
		{title: "atom_codes('North', [0'N | X]).", atom: Atom("North"), list: ListRest(NewNamedVariable("X"), Integer('N')), ok: true, env: map[Variable]Term{
			x: List(Integer('o'), Integer('r'), Integer('t'), Integer('h')),
		}},
		{title: "atom_codes('soap', [0's, 0'o, 0'p]).", atom: Atom("soap"), list: List(Integer('s'), Integer('o'), Integer('p')), ok: false},
		{title: "atom_codes(X, Y).", atom: NewNamedVariable("X"), list: NewNamedVariable("Y"), err: InstantiationError(nil)},

		// 8.16.5.3 Errors
		{title: "a", atom: NewNamedVariable("X"), list: ListRest(NewNamedVariable("Y"), Integer(0)), err: InstantiationError(nil)},
		{title: "b", atom: Integer(0), list: NewNamedVariable("L"), err: TypeError(ValidTypeAtom, Integer(0), nil)},
		{title: "c: atom is a variable", atom: NewNamedVariable("X"), list: Integer(0), err: TypeError(ValidTypeList, Integer(0), nil)},
		{title: "c: atom is an atom", atom: Atom("abc"), list: Integer(0), err: TypeError(ValidTypeList, Integer(0), nil)},
		{title: "d", atom: NewNamedVariable("X"), list: List(NewNamedVariable("Y"), Integer('b'), Integer('c')), err: InstantiationError(nil)},
		{title: "e: atom is a variable", atom: NewNamedVariable("X"), list: List(Atom("a"), Integer('b'), Integer('c')), err: TypeError(ValidTypeInteger, Atom("a"), nil)},
		{title: "e: atom is an atom", atom: Atom("abc"), list: List(Atom("a"), Integer('b'), Integer('c')), err: TypeError(ValidTypeInteger, Atom("a"), nil)},
		{title: "f: atom is a variable", atom: NewNamedVariable("X"), list: List(Integer(-1), Integer('b'), Integer('c')), err: RepresentationError(FlagCharacterCode, nil)},
		{title: "f: atom is an atom", atom: Atom("abc"), list: List(Integer(-1), Integer('b'), Integer('c')), err: RepresentationError(FlagCharacterCode, nil)},

		{title: "atom_codes('ant', [0'a, X, 0't]).", atom: Atom("ant"), list: List(Integer('a'), NewNamedVariable("X"), Integer('t')), ok: true, env: map[Variable]Term{
			x: Integer('n'),
		}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := AtomCodes(tt.atom, tt.list, func(env *Env) *Promise {
				for k, v := range tt.env {
					_, ok := env.Unify(k, v, false)
					assert.True(t, ok)
				}
				return Bool(true)
			}, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestNumberChars(t *testing.T) {
	t.Run("number to chars", func(t *testing.T) {
		t.Run("chars is a partial list", func(t *testing.T) {
			chars := NewNamedVariable("Chars")

			ok, err := NumberChars(Float(23.4), chars, func(env *Env) *Promise {
				assert.Equal(t, List(Atom("2"), Atom("3"), Atom("."), Atom("4")), env.Resolve(chars))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("chars is a list with variables", func(t *testing.T) {
			char := NewNamedVariable("Char")

			ok, err := NumberChars(Float(23.4), List(char, Atom("3"), Atom("."), Atom("4")), func(env *Env) *Promise {
				assert.Equal(t, Atom("2"), env.Resolve(char))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("chars to number", func(t *testing.T) {
		num := NewNamedVariable("Num")

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

	t.Run("num is a variable and chars is a partial list", func(t *testing.T) {
		chars := ListRest(NewNamedVariable("Rest"),
			Atom("2"), Atom("3"), Atom("."), Atom("4"),
		)

		ok, err := NumberChars(NewVariable(), chars, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("num is neither a variable nor a number", func(t *testing.T) {
		t.Run("chars is a list of one-char atoms", func(t *testing.T) {
			ok, err := NumberChars(Atom("23.4"), List(Atom("2"), Atom("3"), Atom("."), Atom("4")), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeNumber, Atom("23.4"), nil), err)
			assert.False(t, ok)
		})

		t.Run("chars is not a list of one-char atoms", func(t *testing.T) {
			ok, err := NumberChars(Atom("23.4"), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeNumber, Atom("23.4"), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("chars is neither a partial list nor a list", func(t *testing.T) {
		t.Run("not even list-ish", func(t *testing.T) {
			ok, err := NumberChars(NewVariable(), Atom("foo"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeList, Atom("foo"), nil), err)
			assert.False(t, ok)
		})

		t.Run("list-ish", func(t *testing.T) {
			_, err := NumberChars(Integer(0), ListRest(Atom("b"), NewNamedVariable("A")), Success, nil).Force(context.Background())
			_, ok := NewEnv().Unify(err.(Exception).Term(), TypeError(ValidTypeList, ListRest(Atom("b"), NewVariable()), nil).Term(), false)
			assert.True(t, ok)
		})
	})

	t.Run("num is a variable and an element of a list prefix of chars is a variable", func(t *testing.T) {
		ok, err := NumberChars(NewVariable(), List(Atom("1"), NewVariable()), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("chars is a list of one-char atoms but is not parsable as a number", func(t *testing.T) {
		t.Run("not a number", func(t *testing.T) {
			ok, err := NumberChars(NewVariable(), List(Atom("f"), Atom("o"), Atom("o")), Success, nil).Force(context.Background())
			assert.Equal(t, SyntaxError(errNotANumber, nil), err)
			assert.False(t, ok)
		})

		t.Run("unexpected token", func(t *testing.T) {
			ok, err := NumberChars(NewVariable(), List(Atom("1"), Atom(".")), Success, nil).Force(context.Background())
			assert.Equal(t, SyntaxError(errNotANumber, nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("an element E of a list prefix of chars is neither a variable nor a one-char atom", func(t *testing.T) {
		t.Run("chars contains a variable", func(t *testing.T) {
			t.Run("not even an atom", func(t *testing.T) {
				ok, err := NumberChars(Integer(100), List(NewVariable(), Atom("0"), Integer(0)), Success, nil).Force(context.Background())
				assert.Equal(t, TypeError(ValidTypeCharacter, Integer(0), nil), err)
				assert.False(t, ok)
			})

			t.Run("atom", func(t *testing.T) {
				ok, err := NumberChars(Integer(100), List(NewVariable(), Atom("00")), Success, nil).Force(context.Background())
				assert.Equal(t, TypeError(ValidTypeCharacter, Atom("00"), nil), err)
				assert.False(t, ok)
			})
		})

		t.Run("chars does not contain a variable", func(t *testing.T) {
			t.Run("not even an atom", func(t *testing.T) {
				ok, err := NumberChars(Integer(100), List(Atom("1"), Atom("0"), Integer(0)), Success, nil).Force(context.Background())
				assert.Equal(t, TypeError(ValidTypeCharacter, Integer(0), nil), err)
				assert.False(t, ok)
			})

			t.Run("atom", func(t *testing.T) {
				ok, err := NumberChars(Integer(100), List(Atom("1"), Atom("00")), Success, nil).Force(context.Background())
				assert.Equal(t, TypeError(ValidTypeCharacter, Atom("00"), nil), err)
				assert.False(t, ok)
			})
		})
	})
}

func TestNumberCodes(t *testing.T) {
	t.Run("number to codes", func(t *testing.T) {
		codes := NewNamedVariable("Codes")

		ok, err := NumberCodes(Float(23.4), codes, func(env *Env) *Promise {
			assert.Equal(t, List(Integer(50), Integer(51), Integer(46), Integer(52)), env.Resolve(codes))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("codes to number", func(t *testing.T) {
		num := NewNamedVariable("Num")

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
			codes := ListRest(NewNamedVariable("Rest"),
				Integer(50), Integer(51), Integer(46), Integer(52),
			)

			ok, err := NumberCodes(NewVariable(), codes, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			code := NewNamedVariable("Code")

			ok, err := NumberCodes(NewVariable(), List(code, Integer(50), Integer(51), Integer(46), Integer(52)), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("num is neither a variable nor a number", func(t *testing.T) {
		ok, err := NumberCodes(Atom("23.4"), List(Integer(50), Integer(51), Integer(46), Integer(52)), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeNumber, Atom("23.4"), nil), err)
		assert.False(t, ok)
	})

	t.Run("num is a variable and codes is neither a list nor partial list", func(t *testing.T) {
		ok, err := NumberCodes(NewVariable(), Atom("23.4"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeList, Atom("23.4"), nil), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the list codes is neither a variable nor a one-character atom", func(t *testing.T) {
		ok, err := NumberCodes(NewVariable(), List(Atom("2"), Integer(51), Integer(46), Integer(52)), Success, nil).Force(context.Background())
		assert.Equal(t, RepresentationError(FlagCharacterCode, nil), err)
		assert.False(t, ok)
	})

	t.Run("codes is a list of one-char atoms but is not parsable as a number", func(t *testing.T) {
		ok, err := NumberCodes(NewVariable(), List(Integer(102), Integer(111), Integer(111)), Success, nil).Force(context.Background())
		assert.Equal(t, SyntaxError(errNotANumber, nil), err)
		assert.False(t, ok)
	})
}

func TestState_StreamProperty(t *testing.T) {
	f, err := ioutil.TempFile("", "")
	assert.NoError(t, err)

	defer func() {
		assert.NoError(t, os.Remove(f.Name()))
	}()

	t.Run("stream", func(t *testing.T) {
		expected := []Term{
			&compound{functor: "mode", args: []Term{Atom("read")}},
			Atom("input"),
			&compound{functor: "alias", args: []Term{Atom("null")}},
			&compound{functor: "eof_action", args: []Term{Atom("eof_code")}},
			&compound{functor: "file_name", args: []Term{Atom(f.Name())}},
			&compound{functor: "position", args: []Term{Integer(0)}},
			&compound{functor: "end_of_stream", args: []Term{Atom("at")}},
			&compound{functor: "reposition", args: []Term{Atom("true")}},
			&compound{functor: "type", args: []Term{Atom("text")}},
		}

		s, err := Open(Atom(f.Name()), StreamModeRead)
		assert.NoError(t, err)
		s.alias = "null"
		defer func() {
			assert.NoError(t, s.Close())
		}()

		v := NewNamedVariable("V")
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
			&compound{functor: "mode", args: []Term{Atom("read")}},
			Atom("input"),
			&compound{functor: "alias", args: []Term{Atom("null")}},
			&compound{functor: "eof_action", args: []Term{Atom("eof_code")}},
			&compound{functor: "file_name", args: []Term{Atom(f.Name())}},
			&compound{functor: "position", args: []Term{Integer(0)}},
			&compound{functor: "end_of_stream", args: []Term{Atom("at")}},
			&compound{functor: "reposition", args: []Term{Atom("false")}},
			&compound{functor: "type", args: []Term{Atom("text")}},
		}

		s, err := Open(Atom(f.Name()), StreamModeRead)
		s.reposition = false
		assert.NoError(t, err)
		s.alias = "null"
		defer func() {
			assert.NoError(t, s.Close())
		}()

		v := NewNamedVariable("V")
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
			&compound{functor: "mode", args: []Term{Atom("write")}},
			Atom("output"),
			&compound{functor: "alias", args: []Term{Atom("null")}},
			&compound{functor: "eof_action", args: []Term{Atom("eof_code")}},
			&compound{functor: "file_name", args: []Term{Atom(f.Name())}},
			&compound{functor: "position", args: []Term{Integer(0)}},
			&compound{functor: "end_of_stream", args: []Term{Atom("at")}},
			&compound{functor: "reposition", args: []Term{Atom("true")}},
			&compound{functor: "type", args: []Term{Atom("text")}},
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
		v := NewNamedVariable("V")
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
			ok, err := state.StreamProperty(s, &compound{
				functor: "mode",
				args:    []Term{Atom("read")},
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
			ok, err := state.StreamProperty(s, &compound{
				functor: "position",
				args:    []Term{Integer(0)},
			}, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable, a stream-term, nor an alias", func(t *testing.T) {
		var state State
		ok, err := state.StreamProperty(Integer(0), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("property is neither a variable nor a stream property", func(t *testing.T) {
		var state State
		ok, err := state.StreamProperty(NewVariable(), Atom("property"), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainStreamProperty, Atom("property"), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var state State
		ok, err := state.StreamProperty(Atom("foo"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, ExistenceError(ObjectTypeStream, Atom("foo"), nil), err)
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
		ok, err := state.StreamProperty(s, &compound{
			functor: "mode",
			args:    []Term{Atom("read")},
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
		ok, err := state.StreamProperty(s, &compound{
			functor: "mode",
			args:    []Term{Atom("read")},
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
		ok, err := state.StreamProperty(s, &compound{
			functor: "end_of_stream",
			args:    []Term{Atom("past")},
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
		ok, err := state.StreamProperty(s, &compound{functor: "foo", args: []Term{NewVariable()}}, Success, nil).Force(context.Background())
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
		ok, err := state.StreamProperty(s, &compound{functor: "mode", args: []Term{NewVariable(), NewVariable()}}, Success, nil).Force(context.Background())
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
		ok, err := state.StreamProperty(s, &compound{functor: "mode", args: []Term{Integer(0)}}, Success, nil).Force(context.Background())
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
		ok, err := state.StreamProperty(s, &compound{functor: "position", args: []Term{Atom("foo")}}, Success, nil).Force(context.Background())
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
		var state State
		ok, err := state.SetStreamPosition(NewNamedVariable("Stream"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("position is a variable", func(t *testing.T) {
		s, err := Open("testdata/empty.txt", StreamModeRead)
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, s.Close())
		}()

		var state State
		ok, err := state.SetStreamPosition(s, NewNamedVariable("Pos"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var state State
		ok, err := state.SetStreamPosition(Integer(2), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainStreamOrAlias, Integer(2), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var state State
		ok, err := state.SetStreamPosition(Atom("foo"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, ExistenceError(ObjectTypeStream, Atom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream property reposition(false)", func(t *testing.T) {
		stream := NewStream(os.Stdin, StreamModeRead)

		assert.False(t, stream.reposition)

		s := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(s, stream)

		var state State
		ok, err := state.SetStreamPosition(s, Integer(0), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationReposition, PermissionTypeStream, s, env), err)
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
		var state State
		ok, err := state.CharConversion(NewNamedVariable("In"), Atom("a"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("outChar is a variable", func(t *testing.T) {
		var state State
		ok, err := state.CharConversion(Atom("a"), NewNamedVariable("Out"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("inChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var state State
			ok, err := state.CharConversion(Integer(0), Atom("a"), Success, nil).Force(context.Background())
			assert.Equal(t, RepresentationError(FlagCharacter, nil), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var state State
			ok, err := state.CharConversion(Atom("foo"), Atom("a"), Success, nil).Force(context.Background())
			assert.Equal(t, RepresentationError(FlagCharacter, nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("outChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var state State
			ok, err := state.CharConversion(Atom("a"), Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, RepresentationError(FlagCharacter, nil), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var state State
			ok, err := state.CharConversion(Atom("a"), Atom("foo"), Success, nil).Force(context.Background())
			assert.Equal(t, RepresentationError(FlagCharacter, nil), err)
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
		x, y := NewNamedVariable("X"), NewNamedVariable("Y")

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
			assert.Equal(t, RepresentationError(FlagCharacter, nil), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var state State
			ok, err := state.CurrentCharConversion(Atom("foo"), Atom("b"), Success, nil).Force(context.Background())
			assert.Equal(t, RepresentationError(FlagCharacter, nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("outChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var state State
			ok, err := state.CurrentCharConversion(Atom("a"), Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, RepresentationError(FlagCharacter, nil), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var state State
			ok, err := state.CurrentCharConversion(Atom("a"), Atom("bar"), Success, nil).Force(context.Background())
			assert.Equal(t, RepresentationError(FlagCharacter, nil), err)
			assert.False(t, ok)
		})
	})
}

func TestState_SetPrologFlag(t *testing.T) {
	t.Run("bounded", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(Atom("bounded"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationModify, PermissionTypeFlag, Atom("bounded"), nil), err)
		assert.False(t, ok)
	})

	t.Run("max_integer", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(Atom("max_integer"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationModify, PermissionTypeFlag, Atom("max_integer"), nil), err)
		assert.False(t, ok)
	})

	t.Run("min_integer", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(Atom("min_integer"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationModify, PermissionTypeFlag, Atom("min_integer"), nil), err)
		assert.False(t, ok)
	})

	t.Run("integer_rounding_function", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(Atom("integer_rounding_function"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationModify, PermissionTypeFlag, Atom("integer_rounding_function"), nil), err)
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
		assert.Equal(t, PermissionError(OperationModify, PermissionTypeFlag, Atom("max_arity"), nil), err)
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
		var state State
		ok, err := state.SetPrologFlag(NewNamedVariable("Flag"), Atom("fail"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("value is a variable", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(Atom("unknown"), NewNamedVariable("Value"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("flag is neither a variable nor an atom", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(Integer(0), Atom("fail"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("flag is an atom but an invalid flag for the processor", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(Atom("foo"), Atom("fail"), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainPrologFlag, Atom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("value is inadmissible for flag", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(Atom("unknown"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainFlagValue, &compound{
			functor: "+",
			args:    []Term{Atom("unknown"), Integer(0)},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("value is admissible for flag but the flag is not modifiable", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(Atom("bounded"), Atom("true"), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationModify, PermissionTypeFlag, Atom("bounded"), nil), err)
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
		flag, value := NewNamedVariable("Flag"), NewNamedVariable("Value")
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
		assert.Equal(t, TypeError(ValidTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("flag is an atom but an invalid flag for the processor", func(t *testing.T) {
		var state State
		ok, err := state.CurrentPrologFlag(Atom("foo"), Atom("error"), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainPrologFlag, Atom("foo"), nil), err)
		assert.False(t, ok)
	})
}

func TestState_ExpandTerm(t *testing.T) {
	t.Run("term_expansion/2 is undefined", func(t *testing.T) {
		var state State
		ok, err := state.ExpandTerm(&compound{
			functor: "f",
			args:    []Term{Atom("a")},
		}, &compound{
			functor: "f",
			args:    []Term{Atom("a")},
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
			ok, err := state.ExpandTerm(&compound{
				functor: "f",
				args:    []Term{Atom("a")},
			}, &compound{
				functor: "f",
				args:    []Term{Atom("a")},
			}, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("throws an exception", func(t *testing.T) {
			state := State{
				VM: VM{
					procedures: map[ProcedureIndicator]procedure{
						{Name: "term_expansion", Arity: 2}: predicate2(func(Term, Term, func(*Env) *Promise, *Env) *Promise {
							return Error(errors.New("failed"))
						}),
					},
				},
			}
			ok, err := state.ExpandTerm(&compound{
				functor: "f",
				args:    []Term{Atom("a")},
			}, &compound{
				functor: "f",
				args:    []Term{Atom("a")},
			}, Success, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})

		t.Run("applicable", func(t *testing.T) {
			state := State{
				VM: VM{
					procedures: map[ProcedureIndicator]procedure{
						{Name: "term_expansion", Arity: 2}: predicate2(func(t1, t2 Term, k func(*Env) *Promise, env *Env) *Promise {
							return Unify(t2, &compound{
								functor: "g",
								args:    []Term{Atom("b")},
							}, k, env)
						}),
					},
				},
			}
			ok, err := state.ExpandTerm(&compound{
				functor: "f",
				args:    []Term{Atom("a")},
			}, &compound{
				functor: "g",
				args:    []Term{Atom("b")},
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
		key   = NewNamedVariable("Key")
		value = NewNamedVariable("Value")
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
			offset := varCounter
			term, err := state.expand(&compound{functor: "-->", args: []Term{Atom("s"), List()}}, nil)
			assert.NoError(t, err)
			assert.Equal(t, &compound{functor: ":-", args: []Term{
				&compound{functor: "s", args: []Term{Variable(offset + 1), Variable(offset + 3)}},
				&compound{functor: "=", args: []Term{Variable(offset + 1), Variable(offset + 3)}},
			}}, term)
		})

		t.Run("terminal sequence", func(t *testing.T) {
			offset := varCounter
			term, err := state.expand(&compound{functor: "-->", args: []Term{Atom("s"), List(Atom("a"))}}, nil)
			assert.NoError(t, err)
			assert.Equal(t, &compound{functor: ":-", args: []Term{
				&compound{functor: "s", args: []Term{Variable(offset + 1), Variable(offset + 3)}},
				&compound{functor: "=", args: []Term{Variable(offset + 1), ListRest(Variable(offset+3), Atom("a"))}},
			}}, term)
		})

		t.Run("concatenation", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				offset := varCounter
				term, err := state.expand(&compound{functor: "-->", args: []Term{Atom("s"), Seq(",", Atom("a"), Atom("b"))}}, nil)
				assert.NoError(t, err)
				assert.Equal(t, &compound{functor: ":-", args: []Term{
					&compound{functor: "s", args: []Term{Variable(offset + 1), Variable(offset + 3)}},
					Seq(",",
						&compound{functor: "a", args: []Term{Variable(offset + 1), Variable(offset + 4)}},
						&compound{functor: "b", args: []Term{Variable(offset + 4), Variable(offset + 3)}},
					),
				}}, term)
			})

			t.Run("lhs is not callable", func(t *testing.T) {
				_, err := state.expand(&compound{functor: "-->", args: []Term{Atom("s"), Seq(",", Integer(0), Atom("b"))}}, nil)
				assert.Error(t, err)
			})

			t.Run("rhs is not callable", func(t *testing.T) {
				_, err := state.expand(&compound{functor: "-->", args: []Term{Atom("s"), Seq(",", Atom("a"), Integer(0))}}, nil)
				assert.Error(t, err)
			})
		})

		t.Run("alternative", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				t.Run("normal", func(t *testing.T) {
					offset := varCounter
					term, err := state.expand(&compound{functor: "-->", args: []Term{Atom("s"), Seq(";", Atom("a"), Atom("b"))}}, nil)
					assert.NoError(t, err)
					assert.Equal(t, &compound{functor: ":-", args: []Term{
						&compound{functor: "s", args: []Term{Variable(offset + 1), Variable(offset + 3)}},
						Seq(";",
							&compound{functor: "a", args: []Term{Variable(offset + 1), Variable(offset + 3)}},
							&compound{functor: "b", args: []Term{Variable(offset + 1), Variable(offset + 3)}},
						),
					}}, term)
				})

				t.Run("if-then-else", func(t *testing.T) {
					offset := varCounter
					term, err := state.expand(&compound{functor: "-->", args: []Term{Atom("s"), Seq(";", &compound{functor: "->", args: []Term{Atom("a"), Atom("b")}}, Atom("c"))}}, nil)
					assert.NoError(t, err)
					assert.Equal(t, &compound{functor: ":-", args: []Term{
						&compound{functor: "s", args: []Term{Variable(offset + 1), Variable(offset + 3)}},
						Seq(";",
							&compound{functor: "->", args: []Term{
								&compound{functor: "a", args: []Term{Variable(offset + 1), Variable(offset + 4)}},
								&compound{functor: "b", args: []Term{Variable(offset + 4), Variable(offset + 3)}},
							}},
							&compound{functor: "c", args: []Term{Variable(offset + 1), Variable(offset + 3)}},
						),
					}}, term)
				})
			})

			t.Run("lhs is not callable", func(t *testing.T) {
				_, err := state.expand(&compound{functor: "-->", args: []Term{Atom("s"), Seq(";", Integer(0), Atom("b"))}}, nil)
				assert.Error(t, err)
			})

			t.Run("rhs is not callable", func(t *testing.T) {
				_, err := state.expand(&compound{functor: "-->", args: []Term{Atom("s"), Seq(";", Atom("a"), Integer(0))}}, nil)
				assert.Error(t, err)
			})
		})

		t.Run("second form of alternative", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				offset := varCounter
				term, err := state.expand(&compound{functor: "-->", args: []Term{Atom("s"), Seq("|", Atom("a"), Atom("b"))}}, nil)
				assert.NoError(t, err)
				assert.Equal(t, &compound{functor: ":-", args: []Term{
					&compound{functor: "s", args: []Term{Variable(offset + 1), Variable(offset + 3)}},
					Seq(";",
						&compound{functor: "a", args: []Term{Variable(offset + 1), Variable(offset + 3)}},
						&compound{functor: "b", args: []Term{Variable(offset + 1), Variable(offset + 3)}},
					),
				}}, term)
			})

			t.Run("lhs is not callable", func(t *testing.T) {
				_, err := state.expand(&compound{functor: "-->", args: []Term{Atom("s"), Seq("|", Integer(0), Atom("b"))}}, nil)
				assert.Error(t, err)
			})

			t.Run("rhs is not callable", func(t *testing.T) {
				_, err := state.expand(&compound{functor: "-->", args: []Term{Atom("s"), Seq("|", Atom("a"), Integer(0))}}, nil)
				assert.Error(t, err)
			})
		})

		t.Run("grammar-body-goal", func(t *testing.T) {
			offset := varCounter
			term, err := state.expand(&compound{functor: "-->", args: []Term{Atom("s"), &compound{functor: "{}", args: []Term{Atom("a")}}}}, nil)
			assert.NoError(t, err)
			assert.Equal(t, &compound{functor: ":-", args: []Term{
				&compound{functor: "s", args: []Term{Variable(offset + 1), Variable(offset + 3)}},
				Seq(",",
					Atom("a"),
					&compound{functor: "=", args: []Term{Variable(offset + 1), Variable(offset + 3)}},
				),
			}}, term)
		})

		t.Run("call", func(t *testing.T) {
			offset := varCounter
			term, err := state.expand(&compound{functor: "-->", args: []Term{Atom("s"), &compound{functor: "call", args: []Term{Atom("a")}}}}, nil)
			assert.NoError(t, err)
			assert.Equal(t, &compound{functor: ":-", args: []Term{
				&compound{functor: "s", args: []Term{Variable(offset + 1), Variable(offset + 3)}},
				&compound{functor: "call", args: []Term{Atom("a"), Variable(offset + 1), Variable(offset + 3)}},
			}}, term)
		})

		t.Run("phrase", func(t *testing.T) {
			offset := varCounter
			term, err := state.expand(&compound{functor: "-->", args: []Term{Atom("s"), &compound{functor: "phrase", args: []Term{Atom("a")}}}}, nil)
			assert.NoError(t, err)
			assert.Equal(t, &compound{functor: ":-", args: []Term{
				&compound{functor: "s", args: []Term{Variable(offset + 1), Variable(offset + 3)}},
				&compound{functor: "phrase", args: []Term{Atom("a"), Variable(offset + 1), Variable(offset + 3)}},
			}}, term)
		})

		t.Run("grammar-body-cut", func(t *testing.T) {
			offset := varCounter
			term, err := state.expand(&compound{functor: "-->", args: []Term{Atom("s"), Atom("!")}}, nil)
			assert.NoError(t, err)
			assert.Equal(t, &compound{functor: ":-", args: []Term{
				&compound{functor: "s", args: []Term{Variable(offset + 1), Variable(offset + 3)}},
				Seq(",",
					Atom("!"),
					&compound{functor: "=", args: []Term{Variable(offset + 1), Variable(offset + 3)}},
				),
			}}, term)
		})

		t.Run("grammar-body-not", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				offset := varCounter
				term, err := state.expand(&compound{functor: "-->", args: []Term{Atom("s"), &compound{functor: `\+`, args: []Term{Atom("a")}}}}, nil)
				assert.NoError(t, err)
				assert.Equal(t, &compound{functor: ":-", args: []Term{
					&compound{functor: "s", args: []Term{Variable(offset + 1), Variable(offset + 3)}},
					Seq(",",
						&compound{functor: `\+`, args: []Term{&compound{functor: "a", args: []Term{Variable(offset + 1), Variable(offset + 4)}}}},
						&compound{functor: "=", args: []Term{Variable(offset + 1), Variable(offset + 3)}},
					),
				}}, term)
			})

			t.Run("goal is not callable", func(t *testing.T) {
				_, err := state.expand(&compound{functor: "-->", args: []Term{Atom("s"), &compound{functor: `\+`, args: []Term{Integer(0)}}}}, nil)
				assert.Error(t, err)
			})
		})

		t.Run("if-then", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				offset := varCounter
				term, err := state.expand(&compound{functor: "-->", args: []Term{Atom("s"), &compound{functor: "->", args: []Term{Atom("a"), Atom("b")}}}}, nil)
				assert.NoError(t, err)
				assert.Equal(t, &compound{functor: ":-", args: []Term{
					&compound{functor: "s", args: []Term{Variable(offset + 1), Variable(offset + 3)}},
					&compound{
						functor: "->",
						args: []Term{
							&compound{functor: "a", args: []Term{Variable(offset + 1), Variable(offset + 4)}},
							&compound{functor: "b", args: []Term{Variable(offset + 4), Variable(offset + 3)}},
						},
					},
				}}, term)
			})

			t.Run("lhs is not callable", func(t *testing.T) {
				_, err := state.expand(&compound{functor: "-->", args: []Term{Atom("s"), &compound{functor: "->", args: []Term{Integer(0), Atom("b")}}}}, nil)
				assert.Error(t, err)
			})

			t.Run("rhs is not callable", func(t *testing.T) {
				_, err := state.expand(&compound{functor: "-->", args: []Term{Atom("s"), &compound{functor: "->", args: []Term{Atom("a"), Integer(0)}}}}, nil)
				assert.Error(t, err)
			})
		})

		t.Run("with semicontexts", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				offset := varCounter
				term, err := state.expand(Atom("-->").Apply(Atom(",").Apply(Atom("phrase1"), List(Atom("word"))), Atom(",").Apply(Atom("phrase2"), Atom("phrase3"))), nil)
				assert.NoError(t, err)
				assert.Equal(t, &compound{
					functor: ":-",
					args: []Term{
						&compound{functor: "phrase1", args: []Term{Variable(offset + 1), Variable(offset + 3)}},
						&compound{
							functor: ",",
							args: []Term{
								&compound{
									functor: ",",
									args: []Term{
										&compound{functor: "phrase2", args: []Term{Variable(offset + 1), Variable(offset + 4)}},
										&compound{functor: "phrase3", args: []Term{Variable(offset + 4), Variable(offset + 2)}},
									},
								},
								&compound{functor: "=", args: []Term{Variable(offset + 3), ListRest(Variable(offset+2), Atom("word"))}},
							},
						},
					},
				}, term)
			})

			t.Run("head is not callable", func(t *testing.T) {
				_, err := state.expand(Atom("-->").Apply(Atom(",").Apply(Integer(0), List(Atom("word"))), Atom(",").Apply(Atom("phrase2"), Atom("phrase3"))), nil)
				assert.Error(t, err)
			})

			t.Run("semicontext is not callable", func(t *testing.T) {
				_, err := state.expand(Atom("-->").Apply(Atom(",").Apply(Atom("phrase1"), Integer(0)), Atom(",").Apply(Atom("phrase2"), Atom("phrase3"))), nil)
				assert.Error(t, err)
			})

			t.Run("body is not callable", func(t *testing.T) {
				_, err := state.expand(Atom("-->").Apply(Atom(",").Apply(Atom("phrase1"), List(Atom("word"))), Atom(",").Apply(Integer(0), Atom("phrase3"))), nil)
				assert.Error(t, err)
			})
		})
	})
}

func TestNth0(t *testing.T) {
	t.Run("n is a variable", func(t *testing.T) {
		t.Run("list is a proper list", func(t *testing.T) {
			const pair = Atom("-")
			var (
				n       = NewNamedVariable("N")
				elem    = NewNamedVariable("Elem")
				results []Term
			)
			ok, err := Nth0(n, List(Atom("a"), Atom("b"), Atom("c")), elem, func(env *Env) *Promise {
				results = append(results, pair.Apply(env.Resolve(n), env.Resolve(elem)))
				return Bool(false)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)

			assert.Equal(t, []Term{
				pair.Apply(Integer(0), Atom("a")),
				pair.Apply(Integer(1), Atom("b")),
				pair.Apply(Integer(2), Atom("c")),
			}, results)
		})

		t.Run("list is an improper list", func(t *testing.T) {
			_, err := Nth0(NewNamedVariable("N"), ListRest(NewNamedVariable("X"), Atom("a")), NewNamedVariable("Elem"), Failure, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
		})
	})

	t.Run("n is an integer", func(t *testing.T) {
		t.Run("list is a proper list", func(t *testing.T) {
			t.Run("n is a valid index", func(t *testing.T) {
				ok, err := Nth0(Integer(1), List(Atom("a"), Atom("b"), Atom("c")), Atom("b"), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("n is too small for an index", func(t *testing.T) {
				ok, err := Nth0(Integer(-1), List(Atom("a"), Atom("b"), Atom("c")), NewVariable(), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.False(t, ok)
			})

			t.Run("n is too big for an index", func(t *testing.T) {
				ok, err := Nth0(Integer(3), List(Atom("a"), Atom("b"), Atom("c")), NewVariable(), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.False(t, ok)
			})
		})

		t.Run("list is an improper list", func(t *testing.T) {
			_, err := Nth0(Integer(1), ListRest(NewVariable(), Atom("a")), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
		})
	})

	t.Run("n is neither a variable nor an integer", func(t *testing.T) {
		_, err := Nth0(Atom("foo"), List(Atom("a"), Atom("b"), Atom("c")), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, Atom("foo"), nil), err)
	})
}

func TestNth1(t *testing.T) {
	t.Run("n is a variable", func(t *testing.T) {
		t.Run("list is a proper list", func(t *testing.T) {
			const pair = Atom("-")
			var (
				n       = NewNamedVariable("N")
				elem    = NewNamedVariable("Elem")
				results []Term
			)
			ok, err := Nth1(n, List(Atom("a"), Atom("b"), Atom("c")), elem, func(env *Env) *Promise {
				results = append(results, pair.Apply(env.Resolve(n), env.Resolve(elem)))
				return Bool(false)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)

			assert.Equal(t, []Term{
				pair.Apply(Integer(1), Atom("a")),
				pair.Apply(Integer(2), Atom("b")),
				pair.Apply(Integer(3), Atom("c")),
			}, results)
		})

		t.Run("list is an improper list", func(t *testing.T) {
			_, err := Nth1(NewNamedVariable("N"), ListRest(NewNamedVariable("X"), Atom("a")), NewNamedVariable("Elem"), Failure, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
		})
	})

	t.Run("n is an integer", func(t *testing.T) {
		t.Run("list is a proper list", func(t *testing.T) {
			t.Run("n is a valid index", func(t *testing.T) {
				ok, err := Nth1(Integer(2), List(Atom("a"), Atom("b"), Atom("c")), Atom("b"), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("n is too small for an index", func(t *testing.T) {
				ok, err := Nth1(Integer(0), List(Atom("a"), Atom("b"), Atom("c")), NewVariable(), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.False(t, ok)
			})

			t.Run("n is too big for an index", func(t *testing.T) {
				ok, err := Nth1(Integer(4), List(Atom("a"), Atom("b"), Atom("c")), NewVariable(), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.False(t, ok)
			})
		})

		t.Run("list is an improper list", func(t *testing.T) {
			_, err := Nth1(Integer(2), ListRest(NewVariable(), Atom("a")), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
		})
	})

	t.Run("n is neither a variable nor an integer", func(t *testing.T) {
		_, err := Nth1(Atom("foo"), List(Atom("a"), Atom("b"), Atom("c")), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, Atom("foo"), nil), err)
	})
}

func TestSucc(t *testing.T) {
	t.Run("x is a variable", func(t *testing.T) {
		t.Run("s is a variable", func(t *testing.T) {
			_, err := Succ(NewNamedVariable("X"), NewNamedVariable("S"), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
		})

		t.Run("s is an integer", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				x := NewNamedVariable("X")
				ok, err := Succ(x, Integer(1), func(env *Env) *Promise {
					assert.Equal(t, Integer(0), env.Resolve(x))
					return Bool(true)
				}, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("s < 0", func(t *testing.T) {
				_, err := Succ(NewNamedVariable("X"), Integer(-1), Success, nil).Force(context.Background())
				assert.Equal(t, DomainError(ValidDomainNotLessThanZero, Integer(-1), nil), err)
			})

			t.Run("s = 0", func(t *testing.T) {
				ok, err := Succ(NewNamedVariable("X"), Integer(0), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.False(t, ok)
			})
		})

		t.Run("s is neither a variable nor an integer", func(t *testing.T) {
			_, err := Succ(NewNamedVariable("X"), Float(1), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeInteger, Float(1), nil), err)
		})
	})

	t.Run("x is an integer", func(t *testing.T) {
		t.Run("s is a variable", func(t *testing.T) {
			s := NewNamedVariable("S")
			ok, err := Succ(Integer(0), s, func(env *Env) *Promise {
				assert.Equal(t, Integer(1), env.Resolve(s))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("s is an integer", func(t *testing.T) {
			ok, err := Succ(Integer(0), Integer(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("s is neither a variable nor an integer", func(t *testing.T) {
			_, err := Succ(Integer(0), Float(1), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeInteger, Float(1), nil), err)
		})

		t.Run("x is negative", func(t *testing.T) {
			_, err := Succ(Integer(-1), Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, DomainError(ValidDomainNotLessThanZero, Integer(-1), nil), err)
		})

		t.Run("x is math.MaxInt64", func(t *testing.T) {
			_, err := Succ(Integer(math.MaxInt64), Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, EvaluationError(ExceptionalValueIntOverflow, nil), err)
		})

		t.Run("s is negative", func(t *testing.T) {
			_, err := Succ(Integer(0), Integer(-1), Success, nil).Force(context.Background())
			assert.Equal(t, DomainError(ValidDomainNotLessThanZero, Integer(-1), nil), err)
		})
	})

	t.Run("x is neither a variable nor an integer", func(t *testing.T) {
		_, err := Succ(Float(0), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, Float(0), nil), err)
	})
}

func TestLength(t *testing.T) {
	t.Run("list is a list", func(t *testing.T) {
		t.Run("length is a variable", func(t *testing.T) {
			n := NewNamedVariable("N")
			ok, err := Length(List(Atom("a"), Atom("b"), Atom("c")), n, func(env *Env) *Promise {
				assert.Equal(t, Integer(3), env.Resolve(n))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("length is an integer", func(t *testing.T) {
			t.Run("length is the exact length of list", func(t *testing.T) {
				ok, err := Length(List(Atom("a"), Atom("b"), Atom("c")), Integer(3), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("length is smaller than the length fo list", func(t *testing.T) {
				ok, err := Length(List(Atom("a"), Atom("b"), Atom("c")), Integer(2), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.False(t, ok)
			})
		})
	})

	t.Run("list is a partial list", func(t *testing.T) {
		t.Run("length is a variable", func(t *testing.T) {
			t.Run("length and the suffix of list are different", func(t *testing.T) {
				l := NewNamedVariable("L")
				n := NewNamedVariable("N")
				var count int
				ok, err := Length(ListRest(l, Atom("a"), Atom("b")), n, func(env *Env) *Promise {
					var ret []Variable
					iter := ListIterator{List: l, Env: env}
					for iter.Next() {
						ret = append(ret, env.Resolve(iter.Current()).(Variable))
					}
					assert.NoError(t, iter.Err())

					switch count {
					case 0:
						assert.Len(t, ret, 0)
						assert.Equal(t, Integer(2), env.Resolve(n))
					case 1:
						assert.Len(t, ret, 1)
						assert.Equal(t, Integer(3), env.Resolve(n))
					case 2:
						assert.Len(t, ret, 2)
						assert.Equal(t, Integer(4), env.Resolve(n))
					default:
						return Bool(true)
					}

					count++
					return Bool(false)
				}, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("length and the suffix of list are the same", func(t *testing.T) {
				l := NewNamedVariable("L")
				_, err := Length(ListRest(l, Atom("a"), Atom("b")), l, Success, nil).Force(context.Background())
				assert.Equal(t, ResourceError(ResourceFiniteMemory, nil), err)
			})
		})

		t.Run("length is an integer", func(t *testing.T) {
			l := NewNamedVariable("L")
			ok, err := Length(ListRest(l, Atom("a"), Atom("b")), Integer(3), func(env *Env) *Promise {
				iter := ListIterator{List: l, Env: env}
				assert.True(t, iter.Next())
				assert.False(t, iter.Next())
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("list is neither a list nor a partial list", func(t *testing.T) {
		t.Run("the suffix is an atom", func(t *testing.T) {
			ok, err := Length(Atom("foo"), Integer(3), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("the suffix is a compound", func(t *testing.T) {
			ok, err := Length(Atom("foo").Apply(Atom("bar")), Integer(3), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("the suffix is neither an atom nor a compound", func(t *testing.T) {
			ok, err := Length(Integer(0), Integer(3), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("length is neither a variable nor an integer", func(t *testing.T) {
		_, err := Length(List(Atom("a"), Atom("b"), Atom("c")), Atom("three"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, Atom("three"), nil), err)
	})

	t.Run("length is an integer that is less than zero", func(t *testing.T) {
		_, err := Length(List(Atom("a"), Atom("b"), Atom("c")), Integer(-3), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainNotLessThanZero, Integer(-3), nil), err)
	})

	t.Run("list is so long that an integer cannot represent its length", func(t *testing.T) {
		maxInt = 2
		defer func() {
			maxInt = math.MaxInt64
		}()

		t.Run("list is a list", func(t *testing.T) {
			_, err := Length(List(Atom("a"), Atom("b"), Atom("c")), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, ResourceError(ResourceFiniteMemory, nil), err)
		})

		t.Run("list is a partial list", func(t *testing.T) {
			_, err := Length(NewVariable(), NewVariable(), Failure, nil).Force(context.Background())
			assert.Equal(t, RepresentationError(FlagMaxInteger, nil), err)
		})
	})
}

func TestSkipMaxList(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		t.Run("without max", func(t *testing.T) {
			ok, err := SkipMaxList(Integer(3), NewVariable(), List(Atom("a"), Atom("b"), Atom("c")), Atom("[]"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("with max", func(t *testing.T) {
			ok, err := SkipMaxList(Integer(2), Integer(2), List(Atom("a"), Atom("b"), Atom("c")), List(Atom("c")), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("max is neither a variable nor an integer", func(t *testing.T) {
		_, err := SkipMaxList(Integer(3), Atom("foo"), List(Atom("a"), Atom("b"), Atom("c")), Atom("[]"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, Atom("foo"), nil), err)
	})

	t.Run("max is negative", func(t *testing.T) {
		_, err := SkipMaxList(Integer(3), Integer(-1), List(Atom("a"), Atom("b"), Atom("c")), Atom("[]"), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainNotLessThanZero, Integer(-1), nil), err)
	})
}

func TestState_Repeat(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	c := 0

	var s State
	_, err := s.Repeat(func(*Env) *Promise {
		c++
		cancel()
		return Bool(true)
	}, nil).Force(ctx)
	assert.Equal(t, context.Canceled, err)

	assert.Equal(t, 1, c)
}

func TestState_Negation(t *testing.T) {
	e := errors.New("failed")

	var s State
	s.Register0("true", func(k func(*Env) *Promise, env *Env) *Promise {
		return k(env)
	})
	s.Register0("false", func(k func(*Env) *Promise, env *Env) *Promise {
		return Bool(false)
	})
	s.Register0("error", func(k func(*Env) *Promise, env *Env) *Promise {
		return Error(e)
	})

	ok, err := s.Negation(Atom("true"), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.False(t, ok)

	ok, err = s.Negation(Atom("false"), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	_, err = s.Negation(Atom("error"), Success, nil).Force(context.Background())
	assert.Equal(t, e, err)
}

func TestAppend(t *testing.T) {
	xs, ys, zs := NewNamedVariable("Xs"), NewNamedVariable("Ys"), NewNamedVariable("Zs")
	tests := []struct {
		title      string
		xs, ys, zs Term
		ok         bool
		err        error
		env        []map[Variable]Term
	}{
		// p.p.2.4 Examples
		{title: `append([a,b],[c,d], Xs).`, xs: List(Atom("a"), Atom("b")), ys: List(Atom("c"), Atom("d")), zs: NewNamedVariable("Xs"), ok: true, env: []map[Variable]Term{
			{xs: List(Atom("a"), Atom("b"), Atom("c"), Atom("d"))},
		}},
		{title: `append([a], nonlist, Xs).`, xs: List(Atom("a")), ys: Atom("nonlist"), zs: NewNamedVariable("Xs"), ok: true, env: []map[Variable]Term{
			{xs: ListRest(Atom("nonlist"), Atom("a"))},
		}},
		{title: `append([a], Ys, Zs).`, xs: List(Atom("a")), ys: NewNamedVariable("Ys"), zs: NewNamedVariable("Zs"), ok: true, env: []map[Variable]Term{
			{zs: ListRest(NewNamedVariable("Ys"), Atom("a"))},
		}},
		{title: `append(Xs, Ys, [a,b,c]).`, xs: NewNamedVariable("Xs"), ys: NewNamedVariable("Ys"), zs: List(Atom("a"), Atom("b"), Atom("c")), ok: true, env: []map[Variable]Term{
			{xs: List(), ys: List(Atom("a"), Atom("b"), Atom("c"))},
			{xs: List(Atom("a")), ys: List(Atom("b"), Atom("c"))},
			{xs: List(Atom("a"), Atom("b")), ys: List(Atom("c"))},
			{xs: List(Atom("a"), Atom("b"), Atom("c")), ys: List()},
		}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := Append(tt.xs, tt.ys, tt.zs, func(env *Env) *Promise {
				for k, v := range tt.env[0] {
					_, ok := env.Unify(k, v, false)
					assert.True(t, ok)
				}
				tt.env = tt.env[1:]
				return Bool(len(tt.env) == 0)
			}, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}
