package engine

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
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

		s, ok := state.streams[atomUserInput]
		assert.True(t, ok)
		assert.Equal(t, os.Stdin, s.sourceSink)
	})
}

func TestState_SetUserOutput(t *testing.T) {
	t.Run("file", func(t *testing.T) {
		var state State
		state.SetUserOutput(os.Stdout)

		s, ok := state.streams[atomUserOutput]
		assert.True(t, ok)
		assert.Equal(t, os.Stdout, s.sourceSink)
	})
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
		{title: `undefined atom`, goal: NewAtom("bar"), ok: false, err: ExistenceError(ObjectTypeProcedure, atomSlash.Apply(NewAtom("bar"), Integer(0)), nil)},
		{title: `defined atom`, goal: NewAtom("foo"), ok: true},
		{title: `undefined compound`, goal: NewAtom("bar").Apply(NewVariable(), NewVariable()), ok: false, err: ExistenceError(ObjectTypeProcedure, atomSlash.Apply(NewAtom("bar"), Integer(2)), nil)},
		{title: `defined compound`, goal: NewAtom("foo").Apply(NewVariable(), NewVariable()), ok: true},
		{title: `variable: single predicate`, goal: NewNamedVariable("X"), ok: false, err: InstantiationError(nil)},
		{title: `variable: multiple predicates`, goal: atomComma.Apply(atomFail, NewNamedVariable("X")), ok: false},
		{title: `not callable: single predicate`, goal: Integer(0), ok: false, err: TypeError(ValidTypeCallable, Integer(0), nil)},
		{title: `not callable: conjunction`, goal: atomComma.Apply(atomTrue, Integer(0)), ok: false, err: TypeError(ValidTypeCallable, atomComma.Apply(atomTrue, Integer(0)), nil)},
		{title: `not callable: disjunction`, goal: atomSemiColon.Apply(Integer(1), atomTrue), ok: false, err: TypeError(ValidTypeCallable, atomSemiColon.Apply(Integer(1), atomTrue), nil)},

		{title: `cover all`, goal: atomComma.Apply(atomCut, NewAtom("f").Apply(NewAtom("g").Apply(List(NewAtom("a"), ListRest(NewNamedVariable("X"), NewAtom("b")))))), ok: true},
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
		{title: "ok", closure: NewAtom("p").Apply(NewAtom("a")), additional: [1]Term{NewAtom("b")}, ok: true},
		{title: "closure is a variable", closure: NewNamedVariable("P"), additional: [1]Term{NewAtom("b")}, err: InstantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [1]Term{NewAtom("b")}, err: TypeError(ValidTypeCallable, Integer(3), nil)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			state := State{VM: VM{procedures: map[ProcedureIndicator]procedure{
				{Name: NewAtom("p"), Arity: 2}: predicate2(func(_, _ Term, k func(*Env) *Promise, env *Env) *Promise {
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
		{title: "ok", closure: NewAtom("p").Apply(NewAtom("a")), additional: [2]Term{NewAtom("b"), NewAtom("c")}, ok: true},
		{title: "closure is a variable", closure: NewNamedVariable("P"), additional: [2]Term{NewAtom("b"), NewAtom("c")}, err: InstantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [2]Term{NewAtom("b"), NewAtom("c")}, err: TypeError(ValidTypeCallable, Integer(3), nil)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			state := State{VM: VM{procedures: map[ProcedureIndicator]procedure{
				{Name: NewAtom("p"), Arity: 3}: predicate3(func(_, _, _ Term, k func(*Env) *Promise, env *Env) *Promise {
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
		{title: "ok", closure: NewAtom("p").Apply(NewAtom("a")), additional: [3]Term{NewAtom("b"), NewAtom("c"), NewAtom("d")}, ok: true},
		{title: "closure is a variable", closure: NewNamedVariable("P"), additional: [3]Term{NewAtom("b"), NewAtom("c"), NewAtom("d")}, err: InstantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [3]Term{NewAtom("b"), NewAtom("c"), NewAtom("d")}, err: TypeError(ValidTypeCallable, Integer(3), nil)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			state := State{VM: VM{procedures: map[ProcedureIndicator]procedure{
				{Name: NewAtom("p"), Arity: 4}: predicate4(func(_, _, _, _ Term, k func(*Env) *Promise, env *Env) *Promise {
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
		{title: "ok", closure: NewAtom("p").Apply(NewAtom("a")), additional: [4]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e")}, ok: true},
		{title: "closure is a variable", closure: NewNamedVariable("P"), additional: [4]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e")}, err: InstantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [4]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e")}, err: TypeError(ValidTypeCallable, Integer(3), nil)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			state := State{VM: VM{procedures: map[ProcedureIndicator]procedure{
				{Name: NewAtom("p"), Arity: 5}: predicate5(func(_, _, _, _, _ Term, k func(*Env) *Promise, env *Env) *Promise {
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
		{title: "ok", closure: NewAtom("p").Apply(NewAtom("a")), additional: [5]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f")}, ok: true},
		{title: "closure is a variable", closure: NewNamedVariable("P"), additional: [5]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f")}, err: InstantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [5]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f")}, err: TypeError(ValidTypeCallable, Integer(3), nil)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			state := State{VM: VM{procedures: map[ProcedureIndicator]procedure{
				{Name: NewAtom("p"), Arity: 6}: predicate6(func(_, _, _, _, _, _ Term, k func(*Env) *Promise, env *Env) *Promise {
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
		{title: "ok", closure: NewAtom("p").Apply(NewAtom("a")), additional: [6]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g")}, ok: true},
		{title: "closure is a variable", closure: NewNamedVariable("P"), additional: [6]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g")}, err: InstantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [6]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g")}, err: TypeError(ValidTypeCallable, Integer(3), nil)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			state := State{VM: VM{procedures: map[ProcedureIndicator]procedure{
				{Name: NewAtom("p"), Arity: 7}: predicate7(func(_, _, _, _, _, _, _ Term, k func(*Env) *Promise, env *Env) *Promise {
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
		{title: "ok", closure: NewAtom("p").Apply(NewAtom("a")), additional: [7]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g"), NewAtom("h")}, ok: true},
		{title: "closure is a variable", closure: NewNamedVariable("P"), additional: [7]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g"), NewAtom("h")}, err: InstantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [7]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g"), NewAtom("h")}, err: TypeError(ValidTypeCallable, Integer(3), nil)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			state := State{VM: VM{procedures: map[ProcedureIndicator]procedure{
				{Name: NewAtom("p"), Arity: 8}: predicate8(func(_, _, _, _, _, _, _, _ Term, k func(*Env) *Promise, env *Env) *Promise {
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
				{Name: NewAtom("foo"), Arity: 0}: predicate0(func(k func(*Env) *Promise, env *Env) *Promise {
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
			ok, err := state.CallNth(NewAtom("foo"), NewNamedVariable("Nth"), func(env *Env) *Promise {
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
			ok, err := state.CallNth(NewAtom("foo"), Integer(2), Failure, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("nth is 0", func(t *testing.T) {
		ok, err := state.CallNth(NewAtom("foo"), Integer(0), Success, nil).Force(context.Background())
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
		_, err := state.CallNth(NewAtom("foo"), NewAtom("bar"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, NewAtom("bar"), nil), err)
	})

	t.Run("nth is an integer which is less than zero", func(t *testing.T) {
		_, err := state.CallNth(NewAtom("foo"), Integer(-1), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainNotLessThanZero, Integer(-1), nil), err)
	})

	t.Run("n+1 is larger than max_integer", func(t *testing.T) {
		maxInt = 0
		defer func() {
			maxInt = math.MaxInt64
		}()
		_, err := state.CallNth(NewAtom("foo"), NewVariable(), Success, nil).Force(context.Background())
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
		{title: `'='(X, Y), '='(X, abc).`, premise: NewEnv().Bind(x, NewNamedVariable("Y")), x: NewNamedVariable("X"), y: NewAtom("abc"), ok: true, env: map[Variable]Term{
			x: NewAtom("abc"),
			y: NewAtom("abc"),
		}},
		{title: `'='(f(X, def), f(def, Y)).`, x: NewAtom("f").Apply(NewNamedVariable("X"), NewAtom("def")), y: NewAtom("f").Apply(NewAtom("def"), NewNamedVariable("Y")), ok: true, env: map[Variable]Term{
			x: NewAtom("def"),
			y: NewAtom("def"),
		}},
		{title: `'='(1, 2).`, x: Integer(1), y: Integer(2), ok: false},
		{title: `'='(1, 1.0).`, x: Integer(1), y: Float(1), ok: false},
		{title: `'='(g(X), f(f(X))).`, x: NewAtom("g").Apply(NewNamedVariable("X")), y: NewAtom("f").Apply(NewAtom("f").Apply(NewNamedVariable("X"))), ok: false},
		{title: `'='(f(X, 1), f(a(X))).`, x: NewAtom("f").Apply(NewNamedVariable("X"), Integer(1)), y: NewAtom("f").Apply(NewAtom("a").Apply(NewNamedVariable("X"))), ok: false},
		{title: `'='(f(X, Y, X), f(a(X), a(Y), Y, 2)).`, x: NewAtom("f").Apply(NewNamedVariable("X"), NewNamedVariable("Y"), NewNamedVariable("X")), y: NewAtom("f").Apply(NewAtom("a").Apply(NewNamedVariable("X")), NewAtom("a").Apply(NewNamedVariable("Y")), NewNamedVariable("Y"), Integer(2)), ok: false},
		{title: `'='(X, a(X)).`, x: NewNamedVariable("X"), y: NewAtom("a").Apply(NewNamedVariable("X")), ok: true},
		{title: `'='(f(X, 1), f(a(X), 2)).`, x: NewAtom("f").Apply(NewNamedVariable("X"), Integer(1)), y: NewAtom("f").Apply(NewAtom("a").Apply(NewNamedVariable("X")), Integer(2)), ok: false},
		{title: `'='(f(1, X, 1), f(2, a(X), 2)).`, x: NewAtom("f").Apply(Integer(1), NewNamedVariable("X"), Integer(1)), y: NewAtom("f").Apply(Integer(2), NewAtom("a").Apply(NewNamedVariable("X")), Integer(2)), ok: false},
		{title: `'='(f(1, X), f(2, a(X))).`, x: NewAtom("f").Apply(Integer(1), NewNamedVariable("X")), y: NewAtom("f").Apply(Integer(2), NewAtom("a").Apply(NewNamedVariable("X"))), ok: false},
		// {title: `'='(f(X, Y, X, 1), f(a(X), a(Y), Y, 2)).`, x: NewAtom("f").Apply(NewNamedVariable("X"), NewNamedVariable("Y"), NewNamedVariable("X"), Integer(1)), y: NewAtom("f").Apply(NewAtom("a").Apply(NewNamedVariable("X")), NewAtom("a").Apply(NewNamedVariable("Y")), NewNamedVariable("Y"), Integer(2)), ok: false},
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
		{title: `unify_with_occurs_check(X, Y), unify_with_occurs_check(X, abc).`, premise: NewEnv().Bind(x, NewNamedVariable("Y")), x: NewNamedVariable("X"), y: NewAtom("abc"), ok: true, env: map[Variable]Term{
			x: NewAtom("abc"),
			y: NewAtom("abc"),
		}},
		{title: `unify_with_occurs_check(f(X, def), f(def, Y)).`, x: NewAtom("f").Apply(NewNamedVariable("X"), NewAtom("def")), y: NewAtom("f").Apply(NewAtom("def"), NewNamedVariable("Y")), ok: true, env: map[Variable]Term{
			x: NewAtom("def"),
			y: NewAtom("def"),
		}},
		{title: `unify_with_occurs_check(1, 2).`, x: Integer(1), y: Integer(2), ok: false},
		{title: `unify_with_occurs_check(1, 1.0).`, x: Integer(1), y: Float(1), ok: false},
		{title: `unify_with_occurs_check(g(X), f(f(X))).`, x: NewAtom("g").Apply(NewNamedVariable("X")), y: NewAtom("f").Apply(NewAtom("f").Apply(NewNamedVariable("X"))), ok: false},
		{title: `unify_with_occurs_check(f(X, 1), f(a(X))).`, x: NewAtom("f").Apply(NewNamedVariable("X"), Integer(1)), y: NewAtom("f").Apply(NewAtom("a").Apply(NewNamedVariable("X"))), ok: false},
		{title: `unify_with_occurs_check(f(X, Y, X), f(a(X), a(Y), Y, 2)).`, x: NewAtom("f").Apply(NewNamedVariable("X"), NewNamedVariable("Y"), NewNamedVariable("X")), y: NewAtom("f").Apply(NewAtom("a").Apply(NewNamedVariable("X")), NewAtom("a").Apply(NewNamedVariable("Y")), NewNamedVariable("Y"), Integer(2)), ok: false},
		{title: `unify_with_occurs_check(X, a(X)).`, x: NewNamedVariable("X"), y: NewAtom("a").Apply(NewNamedVariable("X")), ok: false},
		{title: `unify_with_occurs_check(f(X, 1), f(a(X), 2)).`, x: NewAtom("f").Apply(NewNamedVariable("X"), Integer(1)), y: NewAtom("f").Apply(NewAtom("a").Apply(NewNamedVariable("X")), Integer(2)), ok: false},
		{title: `unify_with_occurs_check(f(1, X, 1), f(2, a(X), 2)).`, x: NewAtom("f").Apply(Integer(1), NewNamedVariable("X"), Integer(1)), y: NewAtom("f").Apply(Integer(2), NewAtom("a").Apply(NewNamedVariable("X")), Integer(2)), ok: false},
		{title: `unify_with_occurs_check(f(1, X), f(2, a(X))).`, x: NewAtom("f").Apply(Integer(1), NewNamedVariable("X")), y: NewAtom("f").Apply(Integer(2), NewAtom("a").Apply(NewNamedVariable("X"))), ok: false},
		{title: `unify_with_occurs_check(f(X, Y, X, 1), f(a(X), a(Y), Y, 2)).`, x: NewAtom("f").Apply(NewNamedVariable("X"), NewNamedVariable("Y"), NewNamedVariable("X"), Integer(1)), y: NewAtom("f").Apply(NewAtom("a").Apply(NewNamedVariable("X")), NewAtom("a").Apply(NewNamedVariable("Y")), NewNamedVariable("Y"), Integer(2)), ok: false},
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
		ok, err := SubsumesTerm(NewNamedVariable("X"), NewAtom("a"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not unifiable", func(t *testing.T) {
		ok, err := SubsumesTerm(NewAtom("a"), NewAtom("b"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("specific-general", func(t *testing.T) {
		ok, err := SubsumesTerm(NewAtom("a"), NewNamedVariable("X"), Success, nil).Force(context.Background())
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
		ok, err := TypeVar(NewAtom("foo"), Success, nil).Force(context.Background())
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
		ok, err := TypeFloat(NewAtom("foo"), Success, nil).Force(context.Background())
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
		ok, err := TypeInteger(NewAtom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeAtom(t *testing.T) {
	t.Run("atom", func(t *testing.T) {
		ok, err := TypeAtom(NewAtom("foo"), Success, nil).Force(context.Background())
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
			functor: NewAtom("foo"),
			args:    []Term{NewAtom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not compound", func(t *testing.T) {
		ok, err := TypeCompound(NewAtom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestAcyclicTerm(t *testing.T) {
	t.Run("atomic", func(t *testing.T) {
		ok, err := AcyclicTerm(NewAtom("a"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("compound", func(t *testing.T) {
		t.Run("cyclic", func(t *testing.T) {
			var c = compound{
				functor: NewAtom("f"),
				args: []Term{
					NewAtom("a"),
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
		{title: `functor(foo(a, b, c), foo, 3).`, term: NewAtom("foo").Apply(NewAtom("a"), NewAtom("b"), NewAtom("c")), name: NewAtom("foo"), arity: Integer(3), ok: true},
		{title: `functor(foo(a, b, c), X, Y).`, term: NewAtom("foo").Apply(NewAtom("a"), NewAtom("b"), NewAtom("c")), name: NewNamedVariable("X"), arity: NewNamedVariable("Y"), ok: true, env: map[Variable]Term{
			x: NewAtom("foo"),
			y: Integer(3),
		}},
		{title: `functor(X, foo, 3).`, term: NewNamedVariable("X"), name: NewAtom("foo"), arity: Integer(3), ok: true, env: map[Variable]Term{
			x: NewAtom("foo").Apply(NewVariable(), NewVariable(), NewVariable()),
		}},
		{title: `functor(X, foo, 0).`, term: NewNamedVariable("X"), name: NewAtom("foo"), arity: Integer(0), ok: true, env: map[Variable]Term{
			x: NewAtom("foo"),
		}},
		{title: `functor(mats(A, B), A, B).`, term: NewAtom("mats").Apply(NewNamedVariable("A"), NewNamedVariable("B")), name: NewNamedVariable("A"), arity: NewNamedVariable("B"), ok: true, env: map[Variable]Term{
			a: NewAtom("mats"),
			b: Integer(2),
		}},
		{title: `functor(foo(a), foo, 2).`, term: NewAtom("foo").Apply(NewAtom("a")), name: NewAtom("foo"), arity: Integer(2), ok: false},
		{title: `functor(foo(a), fo, 1).`, term: NewAtom("foo").Apply(NewAtom("a")), name: NewAtom("fo"), arity: Integer(1), ok: false},
		{title: `functor(1, X, Y).`, term: Integer(1), name: NewNamedVariable("X"), arity: NewNamedVariable("Y"), ok: true, env: map[Variable]Term{
			x: Integer(1),
			y: Integer(0),
		}},
		{title: `functor(X, 1.1, 0).`, term: NewNamedVariable("X"), name: Float(1.1), arity: Integer(0), ok: true, env: map[Variable]Term{
			x: Float(1.1),
		}},
		{title: `functor([_|_], '.', 2).`, term: Cons(NewVariable(), NewVariable()), name: atomDot, arity: Integer(2), ok: true},
		{title: `functor([], [], 0).`, term: atomEmptyList, name: atomEmptyList, arity: Integer(0), ok: true},
		{title: `functor(X, Y, 3).`, term: NewNamedVariable("X"), name: NewNamedVariable("Y"), arity: Integer(3), err: InstantiationError(nil)},
		{title: `functor(X, foo, N).`, term: NewNamedVariable("X"), name: NewAtom("foo"), arity: NewNamedVariable("N"), err: InstantiationError(nil)},
		{title: `functor(X, foo, a).`, term: NewNamedVariable("X"), name: NewAtom("foo"), arity: NewAtom("a"), err: TypeError(ValidTypeInteger, NewAtom("a"), nil)},
		{title: `functor(F, 1.5, 1).`, term: NewNamedVariable("F"), name: Float(1.5), arity: Integer(1), err: TypeError(ValidTypeAtom, Float(1.5), nil)},
		{title: `functor(F, foo(a), 1).`, term: NewNamedVariable("F"), name: NewAtom("foo").Apply(NewAtom("a")), arity: Integer(1), err: TypeError(ValidTypeAtomic, NewAtom("foo").Apply(NewAtom("a")), nil)},
		// {title: `current_prolog_flag(max_arity, A), X is A + 1, functor(T, foo, X).`}
		{title: `Minus_1 is 0 - 1, functor(F, foo, Minus_1).`, term: NewNamedVariable("F"), name: NewAtom("foo"), arity: Integer(-1), err: DomainError(ValidDomainNotLessThanZero, Integer(-1), nil)},

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
		ok, err := Arg(NewVariable(), NewAtom("foo"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeCompound, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("nth is a variable", func(t *testing.T) {
		nth := NewVariable()
		_, err := Arg(nth, &compound{
			functor: NewAtom("f"),
			args:    []Term{NewAtom("a"), NewAtom("b"), NewAtom("a")},
		}, NewAtom("a"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
	})

	t.Run("nth is an integer", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			ok, err := Arg(Integer(1), &compound{
				functor: NewAtom("f"),
				args:    []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")},
			}, NewAtom("a"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)

			ok, err = Arg(Integer(2), &compound{
				functor: NewAtom("f"),
				args:    []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")},
			}, NewAtom("b"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)

			ok, err = Arg(Integer(3), &compound{
				functor: NewAtom("f"),
				args:    []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")},
			}, NewAtom("c"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("ng", func(t *testing.T) {
			ok, err := Arg(Integer(0), &compound{
				functor: NewAtom("f"),
				args:    []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")},
			}, NewVariable(), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)

			ok, err = Arg(Integer(4), &compound{
				functor: NewAtom("f"),
				args:    []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")},
			}, NewVariable(), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("negative", func(t *testing.T) {
			ok, err := Arg(Integer(-2), &compound{
				functor: NewAtom("f"),
				args:    []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")},
			}, NewAtom("b"), Success, nil).Force(context.Background())
			assert.Equal(t, DomainError(ValidDomainNotLessThanZero, Integer(-2), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("nth is neither a variable nor an integer", func(t *testing.T) {
		ok, err := Arg(NewAtom("foo"), &compound{
			functor: NewAtom("f"),
			args:    []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")},
		}, NewAtom("b"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, NewAtom("foo"), nil), err)
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
		{title: "1", term: NewAtom("foo").Apply(NewAtom("a"), NewAtom("b")), list: List(NewAtom("foo"), NewAtom("a"), NewAtom("b")), ok: true},
		{title: "2", term: NewNamedVariable("X"), list: List(NewAtom("foo"), NewAtom("a"), NewAtom("b")), ok: true, env: map[Variable]Term{
			x: NewAtom("foo").Apply(NewAtom("a"), NewAtom("b")),
		}},
		{title: "3", term: NewAtom("foo").Apply(NewAtom("a"), NewAtom("b")), list: NewNamedVariable("L"), ok: true, env: map[Variable]Term{
			l: List(NewAtom("foo"), NewAtom("a"), NewAtom("b")),
		}},
		{title: "4", term: NewAtom("foo").Apply(NewNamedVariable("X"), NewAtom("b")), list: List(NewAtom("foo"), NewAtom("a"), NewNamedVariable("Y")), ok: true, env: map[Variable]Term{
			x: NewAtom("a"),
			y: NewAtom("b"),
		}},
		{title: "5", term: Integer(1), list: List(Integer(1)), ok: true},
		{title: "6", term: NewAtom("foo").Apply(NewAtom("a"), NewAtom("b")), list: List(NewAtom("foo"), NewAtom("b"), NewAtom("a")), ok: false},
		{title: "7", term: NewNamedVariable("X"), list: NewNamedVariable("Y"), err: InstantiationError(nil)},
		{title: "8", term: NewNamedVariable("X"), list: ListRest(NewNamedVariable("Y"), NewAtom("foo"), NewAtom("a")), err: InstantiationError(nil)},
		{title: "9", term: NewNamedVariable("X"), list: ListRest(NewAtom("bar"), NewAtom("foo")), err: TypeError(ValidTypeList, ListRest(NewAtom("bar"), NewAtom("foo")), nil)},
		{title: "10", term: NewNamedVariable("X"), list: List(NewNamedVariable("Foo"), NewAtom("bar")), err: InstantiationError(nil)},
		{title: "11", term: NewNamedVariable("X"), list: List(Integer(3), Integer(1)), err: TypeError(ValidTypeAtom, Integer(3), nil)},
		{title: "12", term: NewNamedVariable("X"), list: List(Float(1.1), NewAtom("foo")), err: TypeError(ValidTypeAtom, Float(1.1), nil)},
		{title: "13", term: NewNamedVariable("X"), list: List(NewAtom("a").Apply(NewAtom("b")), Integer(1)), err: TypeError(ValidTypeAtom, NewAtom("a").Apply(NewAtom("b")), nil)},
		{title: "14", term: NewNamedVariable("X"), list: Integer(4), err: TypeError(ValidTypeList, Integer(4), nil)},
		{title: "15", term: NewAtom("f").Apply(NewNamedVariable("X")), list: List(NewAtom("f"), NewAtom("u").Apply(NewNamedVariable("X"))), ok: true, env: map[Variable]Term{
			x: NewAtom("u").Apply(NewNamedVariable("X")),
		}},

		// 8.5.3.3 Errors
		{title: "b: term is a compound", term: NewAtom("f").Apply(NewAtom("a")), list: ListRest(NewAtom("a"), NewAtom("f")), err: TypeError(ValidTypeList, ListRest(NewAtom("a"), NewAtom("f")), nil)},
		{title: "b: term is an atomic", term: Integer(1), list: ListRest(NewAtom("a"), NewAtom("f")), err: TypeError(ValidTypeList, ListRest(NewAtom("a"), NewAtom("f")), nil)},
		{title: "c", term: NewNamedVariable("X"), list: List(NewNamedVariable("Y")), err: InstantiationError(nil)},
		{title: "e", term: NewNamedVariable("X"), list: List(NewAtom("f").Apply(NewAtom("a"))), err: TypeError(ValidTypeAtomic, NewAtom("f").Apply(NewAtom("a")), nil)},
		{title: "f", term: NewNamedVariable("X"), list: List(), err: DomainError(ValidDomainNonEmptyList, List(), nil)},

		{title: "term is a variable, list has exactly one member which is an atomic", term: NewNamedVariable("X"), list: List(Integer(1)), ok: true, env: map[Variable]Term{
			x: Integer(1),
		}},
		{title: "term is an atomic, the length of list is not 1", term: Integer(1), list: List(), ok: false},

		// https://github.com/ichiban/prolog/issues/244
		{title: "term is atomic", term: NewAtom("c"), list: ListRest(NewNamedVariable("As"), NewNamedVariable("A")), ok: true, env: map[Variable]Term{
			a:  NewAtom("c"),
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
		Bind(in, NewAtom("a"))
	ok, err := CopyTerm(in, out, func(env *Env) *Promise {
		assert.Equal(t, NewAtom("a"), env.Resolve(out))
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
		{title: "1", term: NewAtom("t"), vars: NewNamedVariable("Vars"), ok: true, env: map[Variable]Term{
			vars: List(),
		}},
		{title: "2", term: atomMinus.Apply(
			atomPlus.Apply(
				NewNamedVariable("A"),
				atomSlash.Apply(
					NewAtom("*").Apply(
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
		{title: "3", term: NewAtom("t"), vars: ListRest(NewAtom("a"), NewAtom("x"), NewAtom("y")), err: TypeError(ValidTypeList, ListRest(NewAtom("a"), NewAtom("x"), NewAtom("y")), nil)},
		{title: "4, 5", term: NewNamedVariable("S"), vars: NewNamedVariable("Vars"), ok: true, env: map[Variable]Term{
			vars: List(NewNamedVariable("B"), NewNamedVariable("A")),
			vs:   atomPlus.Apply(NewNamedVariable("B"), NewNamedVariable("T")),
			vt:   NewAtom("*").Apply(NewNamedVariable("A"), NewNamedVariable("B")),
		}},
		{title: "6", term: atomPlus.Apply(atomPlus.Apply(NewNamedVariable("A"), NewNamedVariable("B")), NewNamedVariable("B")), vars: ListRest(NewNamedVariable("Vars"), NewNamedVariable("B")), ok: true, env: map[Variable]Term{
			b:    NewNamedVariable("A"),
			vars: List(NewNamedVariable("B")),
		}},
	}

	env := NewEnv().
		Bind(vs, atomPlus.Apply(NewNamedVariable("B"), NewNamedVariable("T"))).
		Bind(vt, NewAtom("*").Apply(NewNamedVariable("A"), NewNamedVariable("B")))
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
			state.operators.define(900, operatorSpecifierXFX, NewAtom(`+++`))
			state.operators.define(1100, operatorSpecifierXFX, NewAtom(`+`))

			ok, err := state.Op(Integer(1000), atomXFX, NewAtom("++"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)

			assert.Equal(t, operators{
				NewAtom(`+++`): {
					operatorClassInfix: {
						priority:  900,
						specifier: operatorSpecifierXFX,
						name:      NewAtom("+++"),
					},
				},
				NewAtom(`++`): {
					operatorClassInfix: {
						priority:  1000,
						specifier: operatorSpecifierXFX,
						name:      NewAtom("++"),
					},
				},
				NewAtom(`+`): {
					operatorClassInfix: {
						priority:  1100,
						specifier: operatorSpecifierXFX,
						name:      atomPlus,
					},
				},
			}, state.operators)
		})

		t.Run("list", func(t *testing.T) {
			state := State{
				operators: operators{
					NewAtom(`+++`): {
						operatorClassInfix: {
							priority:  900,
							specifier: operatorSpecifierXFX,
							name:      NewAtom("+++"),
						},
					},
					NewAtom(`+`): {
						operatorClassInfix: {
							priority:  1100,
							specifier: operatorSpecifierXFX,
							name:      atomPlus,
						},
					},
				},
			}
			ok, err := state.Op(Integer(1000), atomXFX, List(NewAtom("++"), NewAtom("++")), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)

			assert.Equal(t, operators{
				NewAtom(`+++`): {
					operatorClassInfix: {
						priority:  900,
						specifier: operatorSpecifierXFX,
						name:      NewAtom("+++"),
					},
				},
				NewAtom(`++`): {
					operatorClassInfix: {
						priority:  1000,
						specifier: operatorSpecifierXFX,
						name:      NewAtom("++"),
					},
				},
				NewAtom(`+`): {
					operatorClassInfix: {
						priority:  1100,
						specifier: operatorSpecifierXFX,
						name:      atomPlus,
					},
				},
			}, state.operators)
		})
	})

	t.Run("remove", func(t *testing.T) {
		state := State{
			operators: operators{
				NewAtom(`+++`): {
					operatorClassInfix: {
						priority:  900,
						specifier: operatorSpecifierXFX,
						name:      NewAtom("+++"),
					},
				},
				NewAtom(`++`): {
					operatorClassInfix: {
						priority:  1000,
						specifier: operatorSpecifierXFX,
						name:      NewAtom("++"),
					},
				},
				NewAtom(`+`): {
					operatorClassInfix: {
						priority:  1100,
						specifier: operatorSpecifierXFX,
						name:      atomPlus,
					},
				},
			},
		}
		ok, err := state.Op(Integer(0), atomXFX, NewAtom("++"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, operators{
			NewAtom(`+++`): {
				operatorClassInfix: {
					priority:  900,
					specifier: operatorSpecifierXFX,
					name:      NewAtom("+++"),
				},
			},
			NewAtom(`+`): {
				operatorClassInfix: {
					priority:  1100,
					specifier: operatorSpecifierXFX,
					name:      atomPlus,
				},
			},
		}, state.operators)
	})

	t.Run("priority is a variable", func(t *testing.T) {
		var state State
		ok, err := state.Op(NewNamedVariable("X"), atomXFX, atomPlus, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("specifier is a variable", func(t *testing.T) {
		var state State
		ok, err := state.Op(Integer(1000), NewNamedVariable("X"), atomPlus, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("priority is neither a variable nor an integer", func(t *testing.T) {
		var state State
		ok, err := state.Op(NewAtom("foo"), atomXFX, atomPlus, Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("specifier is neither a variable nor an atom", func(t *testing.T) {
		var state State
		ok, err := state.Op(Integer(1000), Integer(0), atomPlus, Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("operator is neither a partial list nor a list nor an atom", func(t *testing.T) {
		var state State
		ok, err := state.Op(Integer(1000), atomXFX, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeList, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the operator list is neither a variable nor an atom", func(t *testing.T) {
		var state State
		ok, err := state.Op(Integer(1000), atomXFX, List(Integer(0)), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("priority is not between 0 and 1200 inclusive", func(t *testing.T) {
		t.Run("priority is negative", func(t *testing.T) {
			var state State
			ok, err := state.Op(Integer(-1), atomXFX, atomPlus, Success, nil).Force(context.Background())
			assert.Equal(t, DomainError(ValidDomainOperatorPriority, Integer(-1), nil), err)
			assert.False(t, ok)
		})

		t.Run("priority is more than 1200", func(t *testing.T) {
			var state State
			ok, err := state.Op(Integer(1201), atomXFX, atomPlus, Success, nil).Force(context.Background())
			assert.Equal(t, DomainError(ValidDomainOperatorPriority, Integer(1201), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("specifier is not a valid operator specifier", func(t *testing.T) {
		var state State
		ok, err := state.Op(Integer(1000), NewAtom("foo"), atomPlus, Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainOperatorSpecifier, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("operator is ','", func(t *testing.T) {
		state := State{operators: operators{}}
		state.operators.define(1000, operatorSpecifierXFY, NewAtom(`,`))
		ok, err := state.Op(Integer(1000), atomXFY, atomComma, Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationModify, PermissionTypeOperator, atomComma, nil), err)
		assert.False(t, ok)
	})

	t.Run("an element of the operator list is ','", func(t *testing.T) {
		state := State{operators: operators{}}
		state.operators.define(1000, operatorSpecifierXFY, NewAtom(`,`))
		ok, err := state.Op(Integer(1000), atomXFY, List(atomComma), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationModify, PermissionTypeOperator, atomComma, nil), err)
		assert.False(t, ok)
	})

	t.Run("operator is an atom, priority is a priority, and specifier is a specifier such that operator would have an invalid set of priorities and specifiers", func(t *testing.T) {
		t.Run("empty list", func(t *testing.T) {
			var state State
			ok, err := state.Op(Integer(1000), atomXFY, atomEmptyList, Success, nil).Force(context.Background())
			assert.Equal(t, PermissionError(OperationCreate, PermissionTypeOperator, atomEmptyList, nil), err)
			assert.False(t, ok)
		})

		t.Run("empty curly brackets", func(t *testing.T) {
			var state State
			ok, err := state.Op(Integer(1000), atomXFY, atomEmptyBlock, Success, nil).Force(context.Background())
			assert.Equal(t, PermissionError(OperationCreate, PermissionTypeOperator, atomEmptyBlock, nil), err)
			assert.False(t, ok)
		})

		t.Run("bar", func(t *testing.T) {
			t.Run("create", func(t *testing.T) {
				var state State
				ok, err := state.Op(Integer(1000), atomXFY, atomBar, Success, nil).Force(context.Background())
				assert.Equal(t, PermissionError(OperationCreate, PermissionTypeOperator, atomBar, nil), err)
				assert.False(t, ok)
			})

			t.Run("modify", func(t *testing.T) {
				state := State{operators: operators{}}
				state.operators.define(1001, operatorSpecifierXFY, NewAtom(`|`))
				ok, err := state.Op(Integer(1000), atomXFY, atomBar, Success, nil).Force(context.Background())
				assert.Equal(t, PermissionError(OperationModify, PermissionTypeOperator, atomBar, nil), err)
				assert.False(t, ok)
			})
		})
	})

	t.Run("operator is a list, priority is a priority, and specifier is a specifier such that an element op of the list operator would have an invalid set of priorities and specifiers", func(t *testing.T) {
		t.Run("empty list", func(t *testing.T) {
			var state State
			ok, err := state.Op(Integer(1000), atomXFY, List(atomEmptyList), Success, nil).Force(context.Background())
			assert.Equal(t, PermissionError(OperationCreate, PermissionTypeOperator, atomEmptyList, nil), err)
			assert.False(t, ok)
		})

		t.Run("empty curly brackets", func(t *testing.T) {
			var state State
			ok, err := state.Op(Integer(1000), atomXFY, List(atomEmptyBlock), Success, nil).Force(context.Background())
			assert.Equal(t, PermissionError(OperationCreate, PermissionTypeOperator, atomEmptyBlock, nil), err)
			assert.False(t, ok)
		})

		t.Run("bar", func(t *testing.T) {
			t.Run("create", func(t *testing.T) {
				var state State
				ok, err := state.Op(Integer(1000), atomXFY, List(atomBar), Success, nil).Force(context.Background())
				assert.Equal(t, PermissionError(OperationCreate, PermissionTypeOperator, atomBar, nil), err)
				assert.False(t, ok)
			})

			t.Run("modify", func(t *testing.T) {
				state := State{operators: operators{}}
				state.operators.define(101, operatorSpecifierXFY, NewAtom(`|`))
				ok, err := state.Op(Integer(1000), atomXFY, List(atomBar), Success, nil).Force(context.Background())
				assert.Equal(t, PermissionError(OperationModify, PermissionTypeOperator, atomBar, nil), err)
				assert.False(t, ok)
			})
		})
	})

	t.Run("There shall not be an infix and a postfix operator with the same name.", func(t *testing.T) {
		t.Run("infix", func(t *testing.T) {
			state := State{operators: operators{}}
			state.operators.define(200, operatorSpecifierYF, NewAtom(`+`))
			ok, err := state.Op(Integer(500), atomYFX, List(atomPlus), Success, nil).Force(context.Background())
			assert.Equal(t, PermissionError(OperationCreate, PermissionTypeOperator, atomPlus, nil), err)
			assert.False(t, ok)
		})

		t.Run("postfix", func(t *testing.T) {
			state := State{operators: operators{}}
			state.operators.define(500, operatorSpecifierYFX, NewAtom(`+`))
			ok, err := state.Op(Integer(200), atomYF, List(atomPlus), Success, nil).Force(context.Background())
			assert.Equal(t, PermissionError(OperationCreate, PermissionTypeOperator, atomPlus, nil), err)
			assert.False(t, ok)
		})
	})
}

func TestState_CurrentOp(t *testing.T) {
	state := State{operators: operators{}}
	state.operators.define(900, operatorSpecifierXFX, NewAtom(`+++`))
	state.operators.define(1000, operatorSpecifierXFX, NewAtom(`++`))
	state.operators.define(1100, operatorSpecifierXFX, NewAtom(`+`))

	t.Run("single solution", func(t *testing.T) {
		ok, err := state.CurrentOp(Integer(1100), atomXFX, atomPlus, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("multiple solutions", func(t *testing.T) {
		priority, specifier, operator := NewNamedVariable("Priority"), NewNamedVariable("Specifier"), NewNamedVariable("Operator")
		ok, err := state.CurrentOp(priority, specifier, operator, func(env *Env) *Promise {
			switch env.Resolve(operator) {
			case NewAtom("+++"):
				assert.Equal(t, Integer(900), env.Resolve(priority))
				assert.Equal(t, atomXFX, env.Resolve(specifier))
			case NewAtom("++"):
				assert.Equal(t, Integer(1000), env.Resolve(priority))
				assert.Equal(t, atomXFX, env.Resolve(specifier))
			case atomPlus:
				assert.Equal(t, Integer(1100), env.Resolve(priority))
				assert.Equal(t, atomXFX, env.Resolve(specifier))
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
			ok, err := state.CurrentOp(NewAtom("foo"), atomXFX, atomPlus, Success, nil).Force(context.Background())
			assert.Equal(t, DomainError(ValidDomainOperatorPriority, NewAtom("foo"), nil), err)
			assert.False(t, ok)
		})

		t.Run("priority is negative", func(t *testing.T) {
			ok, err := state.CurrentOp(Integer(-1), atomXFX, atomPlus, Success, nil).Force(context.Background())
			assert.Equal(t, DomainError(ValidDomainOperatorPriority, Integer(-1), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("specifier is not an operator specifier", func(t *testing.T) {
		t.Run("specifier is not an atom", func(t *testing.T) {
			ok, err := state.CurrentOp(Integer(1100), Integer(0), atomPlus, Success, nil).Force(context.Background())
			assert.Equal(t, DomainError(ValidDomainOperatorSpecifier, Integer(0), nil), err)
			assert.False(t, ok)
		})

		t.Run("specifier is a non-specifier atom", func(t *testing.T) {
			ok, err := state.CurrentOp(Integer(1100), NewAtom("foo"), atomPlus, Success, nil).Force(context.Background())
			assert.Equal(t, DomainError(ValidDomainOperatorSpecifier, NewAtom("foo"), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("operator is not an atom", func(t *testing.T) {
		ok, err := state.CurrentOp(Integer(1100), atomXFX, Integer(0), Success, nil).Force(context.Background())
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
			goal:      atomSemiColon.Apply(atomEqual.Apply(NewNamedVariable("X"), Integer(1)), atomEqual.Apply(NewNamedVariable("X"), Integer(2))),
			instances: NewNamedVariable("S"),
			env: []map[Variable]Term{
				{s: List(Integer(1), Integer(2))},
			},
		},
		{
			title:     "bagof(X, (X=1 ; X=2), X).",
			template:  NewNamedVariable("X"),
			goal:      atomSemiColon.Apply(atomEqual.Apply(NewNamedVariable("X"), Integer(1)), atomEqual.Apply(NewNamedVariable("X"), Integer(2))),
			instances: NewNamedVariable("X"),
			env: []map[Variable]Term{
				{x: List(Integer(1), Integer(2))},
			},
		},
		{
			title:     "bagof(X, (X=Y ; X=Z), S).",
			template:  NewNamedVariable("X"),
			goal:      atomSemiColon.Apply(atomEqual.Apply(NewNamedVariable("X"), NewNamedVariable("Y")), atomEqual.Apply(NewNamedVariable("X"), NewNamedVariable("Z"))),
			instances: NewNamedVariable("S"),
			env: []map[Variable]Term{
				{s: List(NewNamedVariable("Y"), NewNamedVariable("Z"))},
			},
		},
		{
			title:     "bagof(X, fail, S).",
			template:  NewNamedVariable("X"),
			goal:      atomFail,
			instances: NewNamedVariable("S"),
			env:       nil,
		},
		{
			title:     "bagof(1, (Y=1 ; Y=2), L).",
			template:  Integer(1),
			goal:      atomSemiColon.Apply(atomEqual.Apply(NewNamedVariable("Y"), Integer(1)), atomEqual.Apply(NewNamedVariable("Y"), Integer(2))),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{l: List(Integer(1)), y: Integer(1)},
				{l: List(Integer(1)), y: Integer(2)},
			},
		},
		{
			title:     "bagof(f(X, Y), (X=a ; Y=b), L).",
			template:  NewAtom("f").Apply(NewNamedVariable("X"), NewNamedVariable("Y")),
			goal:      atomSemiColon.Apply(atomEqual.Apply(NewNamedVariable("X"), NewAtom("a")), atomEqual.Apply(NewNamedVariable("Y"), NewAtom("b"))),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{l: List(NewAtom("f").Apply(NewAtom("a"), NewVariable()), NewAtom("f").Apply(NewVariable(), NewAtom("b")))},
			},
		},
		{
			title:    "bagof(X, Y^((X=1, Y=1) ; (X=2, Y=2)), S).",
			template: NewNamedVariable("X"),
			goal: atomCaret.Apply(NewNamedVariable("Y"), atomSemiColon.Apply(
				atomComma.Apply(atomEqual.Apply(NewNamedVariable("X"), Integer(1)), atomEqual.Apply(NewNamedVariable("Y"), Integer(1))),
				atomComma.Apply(atomEqual.Apply(NewNamedVariable("X"), Integer(2)), atomEqual.Apply(NewNamedVariable("Y"), Integer(2))),
			)),
			instances: NewNamedVariable("S"),
			env: []map[Variable]Term{
				{s: List(Integer(1), Integer(2))},
			},
		},
		{
			title:    "bagof(X, Y^((X=1 ; Y=1) ; (X=2, Y=2)), S).",
			template: NewNamedVariable("X"),
			goal: atomCaret.Apply(NewNamedVariable("Y"), atomSemiColon.Apply(
				atomSemiColon.Apply(atomEqual.Apply(NewNamedVariable("X"), Integer(1)), atomEqual.Apply(NewNamedVariable("Y"), Integer(1))),
				atomComma.Apply(atomEqual.Apply(NewNamedVariable("X"), Integer(2)), atomEqual.Apply(NewNamedVariable("Y"), Integer(2))),
			)),
			instances: NewNamedVariable("S"),
			env: []map[Variable]Term{
				{s: List(Integer(1), NewVariable(), Integer(2))},
			},
		},
		{
			title:    "bagof(X, (Y^(X=1 ; Y=2) ; X=3), S).",
			template: NewNamedVariable("X"),
			goal: atomSemiColon.Apply(
				atomCaret.Apply(
					NewNamedVariable("Y"),
					atomSemiColon.Apply(
						atomEqual.Apply(NewNamedVariable("X"), Integer(1)),
						atomEqual.Apply(NewNamedVariable("Y"), Integer(2)),
					),
				),
				atomEqual.Apply(NewNamedVariable("X"), Integer(3)),
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
			goal: atomSemiColon.Apply(
				atomEqual.Apply(NewNamedVariable("X"), NewNamedVariable("Y")),
				atomSemiColon.Apply(
					atomEqual.Apply(NewNamedVariable("X"), NewNamedVariable("Z")),
					atomEqual.Apply(NewNamedVariable("Y"), Integer(1)),
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
			goal:      NewAtom("a").Apply(NewNamedVariable("X"), NewNamedVariable("Y")),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{l: List(Integer(1), Integer(2)), y: NewAtom("f").Apply(NewVariable())},
			},
		},
		{
			title:     "bagof(X, b(X, Y), L).",
			template:  NewNamedVariable("X"),
			goal:      NewAtom("b").Apply(NewNamedVariable("X"), NewNamedVariable("Y")),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{l: List(Integer(1), Integer(1), Integer(2)), y: Integer(1)},
				{l: List(Integer(1), Integer(2), Integer(2)), y: Integer(2)},
			},
		},
		{
			title:     "bagof(X, Y^Z, L).",
			template:  NewNamedVariable("X"),
			goal:      atomCaret.Apply(NewNamedVariable("X"), NewNamedVariable("Z")),
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
			template:  NewAtom("t"),
			goal:      atomTrue,
			instances: ListRest(Integer(1), NewAtom("t")),
			err:       TypeError(ValidTypeList, ListRest(Integer(1), NewAtom("t")), nil),
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
		a, f := NewAtom("$a"), NewAtom("f")
		return Delay(func(context.Context) *Promise {
			return Unify(a.Apply(x, y), a.Apply(Integer(1), f.Apply(NewVariable())), k, env)
		}, func(context.Context) *Promise {
			return Unify(a.Apply(x, y), a.Apply(Integer(2), f.Apply(NewVariable())), k, env)
		})
	})
	state.Register2("b", func(x, y Term, k func(*Env) *Promise, env *Env) *Promise {
		b := NewAtom("$b")
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
			goal:      atomSemiColon.Apply(atomEqual.Apply(NewNamedVariable("X"), Integer(1)), atomEqual.Apply(NewNamedVariable("X"), Integer(2))),
			instances: NewNamedVariable("S"),
			env: []map[Variable]Term{
				{s: List(Integer(1), Integer(2))},
			},
		},
		{
			title:     "setof(X, (X=1; X=2), X).",
			template:  NewNamedVariable("X"),
			goal:      atomSemiColon.Apply(atomEqual.Apply(NewNamedVariable("X"), Integer(1)), atomEqual.Apply(NewNamedVariable("X"), Integer(2))),
			instances: NewNamedVariable("X"),
			env: []map[Variable]Term{
				{x: List(Integer(1), Integer(2))},
			},
		},
		{
			title:     "setof(X, (X=2; X=1), S).",
			template:  NewNamedVariable("X"),
			goal:      atomSemiColon.Apply(atomEqual.Apply(NewNamedVariable("X"), Integer(2)), atomEqual.Apply(NewNamedVariable("X"), Integer(1))),
			instances: NewNamedVariable("S"),
			env: []map[Variable]Term{
				{s: List(Integer(1), Integer(2))},
			},
		},
		{
			title:     "setof(X, (X=2; X=2), S).",
			template:  NewNamedVariable("X"),
			goal:      atomSemiColon.Apply(atomEqual.Apply(NewNamedVariable("X"), Integer(2)), atomEqual.Apply(NewNamedVariable("X"), Integer(2))),
			instances: NewNamedVariable("S"),
			env: []map[Variable]Term{
				{s: List(Integer(2))},
			},
		},
		{
			title:     "setof(X, (X=Y ; X=Z), S).",
			template:  NewNamedVariable("X"),
			goal:      atomSemiColon.Apply(atomEqual.Apply(NewNamedVariable("X"), NewNamedVariable("Y")), atomEqual.Apply(NewNamedVariable("X"), NewNamedVariable("Z"))),
			instances: NewNamedVariable("S"),
			env: []map[Variable]Term{
				{s: List(NewNamedVariable("Y"), NewNamedVariable("Z"))},
			},
		},
		{
			title:     "setof(X, fail, S).",
			template:  NewNamedVariable("X"),
			goal:      atomFail,
			instances: NewNamedVariable("S"),
			env:       nil,
		},
		{
			title:     "setof(1, (Y=2; Y=1), L).",
			template:  Integer(1),
			goal:      atomSemiColon.Apply(atomEqual.Apply(NewNamedVariable("Y"), Integer(2)), atomEqual.Apply(NewNamedVariable("Y"), Integer(1))),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{l: List(Integer(1)), y: Integer(2)},
				{l: List(Integer(1)), y: Integer(1)},
			},
		},
		{
			title:     "setof(f(X, Y), (X=a ; Y=b), L).",
			template:  NewAtom("f").Apply(NewNamedVariable("X"), NewNamedVariable("Y")),
			goal:      atomSemiColon.Apply(atomEqual.Apply(NewNamedVariable("X"), NewAtom("a")), atomEqual.Apply(NewNamedVariable("Y"), NewAtom("b"))),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{l: List(NewAtom("f").Apply(NewAtom("a"), NewVariable()), NewAtom("f").Apply(NewVariable(), NewAtom("b")))},
			},
		},
		{
			title:    "setof(X, Y^((X=1, Y=1) ; (X=2, Y=2)), S).",
			template: NewNamedVariable("X"),
			goal: atomCaret.Apply(NewNamedVariable("Y"), atomSemiColon.Apply(
				atomComma.Apply(atomEqual.Apply(NewNamedVariable("X"), Integer(1)), atomEqual.Apply(NewNamedVariable("Y"), Integer(1))),
				atomComma.Apply(atomEqual.Apply(NewNamedVariable("X"), Integer(2)), atomEqual.Apply(NewNamedVariable("Y"), Integer(2))),
			)),
			instances: NewNamedVariable("S"),
			env: []map[Variable]Term{
				{s: List(Integer(1), Integer(2))},
			},
		},
		{
			title:    "setof(X, Y^((X=1 ; Y=1) ; (X=2, Y=2)), S).",
			template: NewNamedVariable("X"),
			goal: atomCaret.Apply(NewNamedVariable("Y"), atomSemiColon.Apply(
				atomSemiColon.Apply(atomEqual.Apply(NewNamedVariable("X"), Integer(1)), atomEqual.Apply(NewNamedVariable("Y"), Integer(1))),
				atomComma.Apply(atomEqual.Apply(NewNamedVariable("X"), Integer(2)), atomEqual.Apply(NewNamedVariable("Y"), Integer(2))),
			)),
			instances: NewNamedVariable("S"),
			env: []map[Variable]Term{
				{s: List(NewVariable(), Integer(1), Integer(2))},
			},
		},
		{
			title:    "setof(X, (Y^(X=1 ; Y=2) ; X=3), S).",
			template: NewNamedVariable("X"),
			goal: atomSemiColon.Apply(
				atomCaret.Apply(
					NewNamedVariable("Y"),
					atomSemiColon.Apply(
						atomEqual.Apply(NewNamedVariable("X"), Integer(1)),
						atomEqual.Apply(NewNamedVariable("Y"), Integer(2)),
					),
				),
				atomEqual.Apply(NewNamedVariable("X"), Integer(3)),
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
			goal: atomSemiColon.Apply(
				atomEqual.Apply(NewNamedVariable("X"), NewNamedVariable("Y")),
				atomSemiColon.Apply(
					atomEqual.Apply(NewNamedVariable("X"), NewNamedVariable("Z")),
					atomEqual.Apply(NewNamedVariable("Y"), Integer(1)),
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
			goal:      NewAtom("a").Apply(NewNamedVariable("X"), NewNamedVariable("Y")),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{l: List(Integer(1), Integer(2)), y: NewAtom("f").Apply(NewVariable())},
			},
		},
		{
			title:     "setof(X, member(X,[f(U,b),f(V,c)]), L).",
			template:  NewNamedVariable("X"),
			goal:      NewAtom("member").Apply(NewNamedVariable("X"), List(NewAtom("f").Apply(NewNamedVariable("U"), NewAtom("b")), NewAtom("f").Apply(NewNamedVariable("V"), NewAtom("c")))),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{l: List(NewAtom("f").Apply(NewNamedVariable("U"), NewAtom("b")), NewAtom("f").Apply(NewNamedVariable("V"), NewAtom("c")))},
			},
		},
		{
			title:     "setof(X, member(X,[f(U,b),f(V,c)]), [f(a,c),f(a,b)]).",
			template:  NewNamedVariable("X"),
			goal:      NewAtom("member").Apply(NewNamedVariable("X"), List(NewAtom("f").Apply(NewNamedVariable("U"), NewAtom("b")), NewAtom("f").Apply(NewNamedVariable("V"), NewAtom("c")))),
			instances: List(NewAtom("f").Apply(NewAtom("a"), NewAtom("c")), NewAtom("f").Apply(NewAtom("a"), NewAtom("b"))),
			env:       nil,
		},
		{
			title:     "setof(X, member(X,[f(b,U),f(c,V)]), [f(b,a),f(c,a)]).",
			template:  NewNamedVariable("X"),
			goal:      NewAtom("member").Apply(NewNamedVariable("X"), List(NewAtom("f").Apply(NewAtom("b"), NewNamedVariable("U")), NewAtom("f").Apply(NewAtom("c"), NewNamedVariable("V")))),
			instances: List(NewAtom("f").Apply(NewAtom("b"), NewAtom("a")), NewAtom("f").Apply(NewAtom("c"), NewAtom("a"))),
			env: []map[Variable]Term{
				{u: NewAtom("a"), v: NewAtom("a")},
			},
		},
		{
			title:     "setof(X, member(X,[V,U,f(U),f(V)]), L).",
			template:  NewNamedVariable("X"),
			goal:      NewAtom("member").Apply(NewNamedVariable("X"), List(NewNamedVariable("V"), NewNamedVariable("U"), NewAtom("f").Apply(NewNamedVariable("U")), NewAtom("f").Apply(NewNamedVariable("V")))),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{l: List(NewNamedVariable("U"), NewNamedVariable("V"), NewAtom("f").Apply(NewNamedVariable("U")), NewAtom("f").Apply(NewNamedVariable("V")))},
			},
		},
		{
			title:     "setof(X, member(X,[V,U,f(U),f(V)]), [a,b,f(a),f(b)]).",
			template:  NewNamedVariable("X"),
			goal:      NewAtom("member").Apply(NewNamedVariable("X"), List(NewNamedVariable("V"), NewNamedVariable("U"), NewAtom("f").Apply(NewNamedVariable("U")), NewAtom("f").Apply(NewNamedVariable("V")))),
			instances: List(NewAtom("a"), NewAtom("b"), NewAtom("f").Apply(NewAtom("a")), NewAtom("f").Apply(NewAtom("b"))),
			env: []map[Variable]Term{
				{u: NewAtom("a"), v: NewAtom("b")},
			},
		},
		{
			title:     "setof(X, member(X,[V,U,f(U),f(V)]), [a,b,f(b),f(a)]).",
			template:  NewNamedVariable("X"),
			goal:      NewAtom("member").Apply(NewNamedVariable("X"), List(NewNamedVariable("V"), NewNamedVariable("U"), NewAtom("f").Apply(NewNamedVariable("U")), NewAtom("f").Apply(NewNamedVariable("V")))),
			instances: List(NewAtom("a"), NewAtom("b"), NewAtom("f").Apply(NewAtom("b")), NewAtom("f").Apply(NewAtom("a"))),
			env:       nil,
		},
		{
			title:    "setof(X, (exists(U,V)^member(X,[V,U,f(U),f(V)])), [a,b,f(b),f(a)]).",
			template: NewNamedVariable("X"),
			goal: atomCaret.Apply(
				NewAtom("exists").Apply(NewNamedVariable("U"), NewNamedVariable("V")),
				NewAtom("member").Apply(NewNamedVariable("X"), List(NewNamedVariable("V"), NewNamedVariable("U"), NewAtom("f").Apply(NewNamedVariable("U")), NewAtom("f").Apply(NewNamedVariable("V")))),
			),
			instances: List(NewAtom("a"), NewAtom("b"), NewAtom("f").Apply(NewAtom("b")), NewAtom("f").Apply(NewAtom("a"))),
			env: []map[Variable]Term{
				{},
			},
		},
		{
			title:     "setof(X, b(X, Y), L).",
			template:  NewNamedVariable("X"),
			goal:      NewAtom("b").Apply(NewNamedVariable("X"), NewNamedVariable("Y")),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{l: List(Integer(1), Integer(2)), y: Integer(1)},
				{l: List(Integer(1), Integer(2)), y: Integer(2)},
			},
		},
		{
			title:    "setof(X-Xs, Y^setof(Y,b(X, Y),Xs), L).",
			template: atomMinus.Apply(NewNamedVariable("X"), NewNamedVariable("Xs")),
			goal: atomCaret.Apply(
				NewNamedVariable("Y"),
				NewAtom("setof").Apply(
					NewNamedVariable("Y"),
					NewAtom("b").Apply(NewNamedVariable("X"), NewNamedVariable("Y")),
					NewNamedVariable("Xs"),
				),
			),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{l: List(
					atomMinus.Apply(Integer(1), List(Integer(1), Integer(2))),
					atomMinus.Apply(Integer(2), List(Integer(1), Integer(2))),
				)},
			},
		},
		{
			title:    "setof(X-Xs, setof(Y,b(X, Y),Xs), L).",
			template: atomMinus.Apply(NewNamedVariable("X"), NewNamedVariable("Xs")),
			goal: atomCaret.Apply(
				NewNamedVariable("Y"),
				NewAtom("setof").Apply(
					NewNamedVariable("Y"),
					NewAtom("b").Apply(NewNamedVariable("X"), NewNamedVariable("Y")),
					NewNamedVariable("Xs"),
				),
			),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{
					l: List(
						atomMinus.Apply(Integer(1), List(Integer(1), Integer(2))),
						atomMinus.Apply(Integer(2), List(Integer(1), Integer(2))),
					),
				},
			},
		},
		{
			title:    "setof(X-Xs, bagof(Y,d(X, Y),Xs), L).",
			template: atomMinus.Apply(NewNamedVariable("X"), NewNamedVariable("Xs")),
			goal: atomCaret.Apply(
				NewNamedVariable("Y"),
				NewAtom("bagof").Apply(
					NewNamedVariable("Y"),
					NewAtom("d").Apply(NewNamedVariable("X"), NewNamedVariable("Y")),
					NewNamedVariable("Xs"),
				),
			),
			instances: NewNamedVariable("L"),
			env: []map[Variable]Term{
				{
					l: List(
						atomMinus.Apply(Integer(1), List(Integer(1), Integer(2), Integer(1))),
						atomMinus.Apply(Integer(2), List(Integer(2), Integer(1), Integer(2))),
					),
				},
			},
		},

		// 8.10.3.3 Errors
		{
			title:     "c",
			template:  NewAtom("t"),
			goal:      atomTrue,
			instances: ListRest(Integer(1), NewAtom("t")),
			err:       TypeError(ValidTypeList, ListRest(Integer(1), NewAtom("t")), nil),
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
		a, f := NewAtom("$a"), NewAtom("f")
		return Delay(func(context.Context) *Promise {
			return Unify(a.Apply(x, y), a.Apply(Integer(1), f.Apply(NewVariable())), k, env)
		}, func(context.Context) *Promise {
			return Unify(a.Apply(x, y), a.Apply(Integer(2), f.Apply(NewVariable())), k, env)
		})
	})
	state.Register2("b", func(x, y Term, k func(*Env) *Promise, env *Env) *Promise {
		b := NewAtom("$b")
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
		d := NewAtom("$d")
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
		{title: "1", template: NewNamedVariable("X"), goal: atomSemiColon.Apply(atomEqual.Apply(NewNamedVariable("X"), Integer(1)), atomEqual.Apply(NewNamedVariable("X"), Integer(2))), instances: NewNamedVariable("S"), ok: true, env: map[Variable]Term{
			s: List(Integer(1), Integer(2)),
		}},
		{title: "2", template: atomPlus.Apply(NewNamedVariable("X"), NewNamedVariable("Y")), goal: atomEqual.Apply(NewNamedVariable("X"), Integer(1)), instances: NewNamedVariable("S"), ok: true, env: map[Variable]Term{
			s: List(atomPlus.Apply(Integer(1), NewVariable())),
		}},
		{title: "3", template: NewNamedVariable("X"), goal: atomFail, instances: NewNamedVariable("L"), ok: true, env: map[Variable]Term{
			l: List(),
		}},
		{title: "4", template: NewNamedVariable("X"), goal: atomSemiColon.Apply(atomEqual.Apply(NewNamedVariable("X"), Integer(1)), atomEqual.Apply(NewNamedVariable("X"), Integer(1))), instances: NewNamedVariable("S"), ok: true, env: map[Variable]Term{
			s: List(Integer(1), Integer(1)),
		}},
		{title: "5", template: NewNamedVariable("X"), goal: atomSemiColon.Apply(atomEqual.Apply(NewNamedVariable("X"), Integer(2)), atomEqual.Apply(NewNamedVariable("X"), Integer(1))), instances: List(Integer(1), Integer(2)), ok: false},
		{title: "6", template: NewNamedVariable("X"), goal: NewNamedVariable("Goal"), instances: NewNamedVariable("S"), err: InstantiationError(nil)},
		{title: "7", template: NewNamedVariable("X"), goal: Integer(4), instances: NewNamedVariable("S"), err: TypeError(ValidTypeCallable, Integer(4), nil)},

		// 8.10.1.3 Errors
		{title: "c", template: NewNamedVariable("X"), goal: atomSemiColon.Apply(atomEqual.Apply(NewNamedVariable("X"), Integer(1)), atomEqual.Apply(NewNamedVariable("X"), Integer(2))), instances: NewAtom("foo"), err: TypeError(ValidTypeList, NewAtom("foo"), nil)},
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
			order: atomLessThan,
		}},
		{title: `compare(Order, d, d).`, order: NewNamedVariable("Order"), x: NewAtom("d"), y: NewAtom("d"), ok: true, env: map[Variable]Term{
			order: atomEqual,
		}},
		{title: `compare(Order, Order, <).`, order: NewNamedVariable("Order"), x: NewNamedVariable("Order"), y: atomLessThan, ok: true, env: map[Variable]Term{
			order: atomLessThan,
		}},
		{title: `compare(<, <, <).`, order: atomLessThan, x: atomLessThan, y: atomLessThan, ok: false},
		{title: `compare(1+2, 3, 3.0).`, order: atomPlus.Apply(Integer(1), Integer(2)), x: Integer(3), y: Float(3.0), ok: false, err: TypeError(ValidTypeAtom, atomPlus.Apply(Integer(1), Integer(2)), nil)},
		{title: `compare(>=, 3, 3.0).`, order: NewAtom(">="), x: Integer(3), y: Float(3.0), ok: false, err: DomainError(ValidDomainOrder, NewAtom(">="), nil)},

		{title: `missing case for >`, order: atomGreaterThan, x: Integer(2), y: Integer(1), ok: true},
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
		_, err := Between(NewAtom("inf"), Integer(2), Integer(1), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, NewAtom("inf"), nil), err)
	})

	t.Run("upper is not an integer", func(t *testing.T) {
		_, err := Between(Integer(1), NewAtom("inf"), Integer(1), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, NewAtom("inf"), nil), err)
	})

	t.Run("value is not an integer or variable", func(t *testing.T) {
		_, err := Between(Integer(1), Integer(1), NewAtom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, NewAtom("foo"), nil), err)
	})
}

func TestSort(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		t.Run("variable", func(t *testing.T) {
			sorted := NewNamedVariable("Sorted")
			ok, err := Sort(List(NewAtom("a"), NewAtom("c"), NewAtom("b"), NewAtom("a")), sorted, func(env *Env) *Promise {
				assert.Equal(t, List(NewAtom("a"), NewAtom("b"), NewAtom("c")), env.Resolve(sorted))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("list", func(t *testing.T) {
			ok, err := Sort(List(NewAtom("a"), NewAtom("c"), NewAtom("b"), NewAtom("a")), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("list is a partial list", func(t *testing.T) {
		_, err := Sort(ListRest(NewNamedVariable("X"), NewAtom("a"), NewAtom("b")), NewNamedVariable("Sorted"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
	})

	t.Run("list is neither a partial list nor a list", func(t *testing.T) {
		_, err := Sort(NewAtom("a"), NewNamedVariable("Sorted"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeList, NewAtom("a"), nil), err)
	})

	t.Run("sorted is neither a partial list nor a list", func(t *testing.T) {
		t.Run("obviously not a list", func(t *testing.T) {
			_, err := Sort(List(NewAtom("a")), NewAtom("a"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeList, NewAtom("a"), nil), err)
		})

		t.Run("list-ish", func(t *testing.T) {
			_, err := Sort(List(NewAtom("a")), &compound{functor: atomDot, args: []Term{NewAtom("a")}}, Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeList, &compound{functor: atomDot, args: []Term{NewAtom("a")}}, nil), err)
		})
	})
}

func TestKeySort(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		t.Run("variable", func(t *testing.T) {
			sorted := NewNamedVariable("Sorted")
			ok, err := KeySort(List(
				Pair(NewAtom("c"), NewAtom("4")),
				Pair(NewAtom("b"), NewAtom("3")),
				Pair(NewAtom("a"), NewAtom("1")),
				Pair(NewAtom("a"), NewAtom("2")),
			), sorted, func(env *Env) *Promise {
				assert.Equal(t, List(
					Pair(NewAtom("a"), NewAtom("1")),
					Pair(NewAtom("a"), NewAtom("2")),
					Pair(NewAtom("b"), NewAtom("3")),
					Pair(NewAtom("c"), NewAtom("4")),
				), env.Resolve(sorted))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("list", func(t *testing.T) {
			second := NewNamedVariable("Second")
			ok, err := KeySort(List(
				Pair(NewAtom("c"), NewAtom("4")),
				Pair(NewAtom("b"), NewAtom("3")),
				Pair(NewAtom("a"), NewAtom("1")),
				Pair(NewAtom("a"), NewAtom("2")),
			), List(
				Pair(NewAtom("a"), NewAtom("1")),
				second,
				Pair(NewAtom("b"), NewAtom("3")),
				Pair(NewAtom("c"), NewAtom("4")),
			), func(env *Env) *Promise {
				assert.Equal(t, Pair(NewAtom("a"), NewAtom("2")), env.Resolve(second))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("pairs is a partial list", func(t *testing.T) {
		_, err := KeySort(ListRest(NewNamedVariable("Rest"), Pair(NewAtom("a"), Integer(1))), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
	})

	t.Run("pairs is neither a partial list nor a list", func(t *testing.T) {
		_, err := KeySort(NewAtom("a"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeList, NewAtom("a"), nil), err)
	})

	t.Run("sorted is neither a partial list nor a list", func(t *testing.T) {
		_, err := KeySort(List(), NewAtom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeList, NewAtom("foo"), nil), err)
	})

	t.Run("an element of a list prefix of pairs is a variable", func(t *testing.T) {
		_, err := KeySort(List(NewNamedVariable("X")), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
	})

	t.Run("an element of a list prefix of pairs is neither a variable nor a compound term with principal functor (-)/2", func(t *testing.T) {
		t.Run("atomic", func(t *testing.T) {
			_, err := KeySort(List(NewAtom("foo")), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypePair, NewAtom("foo"), nil), err)
		})

		t.Run("compound", func(t *testing.T) {
			_, err := KeySort(List(NewAtom("f").Apply(NewAtom("a"))), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypePair, NewAtom("f").Apply(NewAtom("a")), nil), err)
		})
	})

	t.Run("an element of a list prefix of sorted is neither a variable nor a compound term with principal functor (-)/2", func(t *testing.T) {
		t.Run("atomic", func(t *testing.T) {
			_, err := KeySort(List(), List(NewAtom("foo")), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypePair, NewAtom("foo"), nil), err)
		})

		t.Run("compound", func(t *testing.T) {
			_, err := KeySort(List(), List(NewAtom("f").Apply(NewAtom("a"))), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypePair, NewAtom("f").Apply(NewAtom("a")), nil), err)
		})
	})
}

func TestThrow(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		ok, err := Throw(NewAtom("a"), Success, nil).Force(context.Background())
		assert.Equal(t, Exception{term: NewAtom("a")}, err)
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
			functor: NewAtom("throw"),
			args:    []Term{NewAtom("a")},
		}, v, &compound{
			functor: atomEqual,
			args:    []Term{v, NewAtom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not match", func(t *testing.T) {
		ok, err := state.Catch(&compound{
			functor: NewAtom("throw"),
			args:    []Term{NewAtom("a")},
		}, NewAtom("b"), atomFail, Success, nil).Force(context.Background())
		assert.False(t, ok)
		ex, ok := err.(Exception)
		assert.True(t, ok)
		assert.Equal(t, NewAtom("a"), ex.term)
	})

	t.Run("true", func(t *testing.T) {
		ok, err := state.Catch(atomTrue, NewAtom("b"), atomFail, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("false", func(t *testing.T) {
		ok, err := state.Catch(atomFail, NewAtom("b"), atomFail, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("non-exception error", func(t *testing.T) {
		ok, err := state.Catch(atomTrue, NewVariable(), atomTrue, func(env *Env) *Promise {
			return Error(errors.New("failed"))
		}, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestState_CurrentPredicate(t *testing.T) {
	t.Run("user defined predicate", func(t *testing.T) {
		state := State{VM: VM{procedures: map[ProcedureIndicator]procedure{
			{Name: NewAtom("foo"), Arity: 1}: &userDefined{},
		}}}
		ok, err := state.CurrentPredicate(&compound{
			functor: atomSlash,
			args: []Term{
				NewAtom("foo"),
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
			{Name: NewAtom("foo"), Arity: 1}: &userDefined{},
			{Name: NewAtom("bar"), Arity: 1}: &userDefined{},
			{Name: NewAtom("baz"), Arity: 1}: &userDefined{},
		}}}
		ok, err := state.CurrentPredicate(v, func(env *Env) *Promise {
			c, ok := env.Resolve(v).(*compound)
			assert.True(t, ok)
			assert.Equal(t, atomSlash, c.functor)
			assert.Len(t, c.args, 2)
			assert.Equal(t, Integer(1), c.args[1])
			switch c.args[0] {
			case NewAtom("foo"):
				foo = true
			case NewAtom("bar"):
				bar = true
			case NewAtom("baz"):
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
			{Name: atomEqual, Arity: 2}: predicate2(Unify),
		}}}
		ok, err := state.CurrentPredicate(&compound{
			functor: atomSlash,
			args: []Term{
				atomEqual,
				Integer(2),
			},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("pi is neither a variable nor a predicate indicator", func(t *testing.T) {
		t.Run("atom", func(t *testing.T) {
			var state State
			ok, err := state.CurrentPredicate(NewAtom("foo"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypePredicateIndicator, NewAtom("foo"), nil), err)
			assert.False(t, ok)
		})

		t.Run("compound", func(t *testing.T) {
			t.Run("non slash", func(t *testing.T) {
				var state State
				ok, err := state.CurrentPredicate(&compound{
					functor: NewAtom("f"),
					args:    []Term{NewAtom("a")},
				}, Success, nil).Force(context.Background())
				assert.Equal(t, TypeError(ValidTypePredicateIndicator, &compound{
					functor: NewAtom("f"),
					args:    []Term{NewAtom("a")},
				}, nil), err)
				assert.False(t, ok)
			})

			t.Run("slash but number", func(t *testing.T) {
				var state State
				ok, err := state.CurrentPredicate(&compound{
					functor: atomSlash,
					args:    []Term{Integer(0), Integer(0)},
				}, Success, nil).Force(context.Background())
				assert.Equal(t, TypeError(ValidTypePredicateIndicator, &compound{
					functor: atomSlash,
					args:    []Term{Integer(0), Integer(0)},
				}, nil), err)
				assert.False(t, ok)
			})

			t.Run("slash but path", func(t *testing.T) {
				var state State
				ok, err := state.CurrentPredicate(&compound{
					functor: atomSlash,
					args:    []Term{NewAtom("foo"), NewAtom("bar")},
				}, Success, nil).Force(context.Background())
				assert.Equal(t, TypeError(ValidTypePredicateIndicator, &compound{
					functor: atomSlash,
					args:    []Term{NewAtom("foo"), NewAtom("bar")},
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
			functor: NewAtom("foo"),
			args:    []Term{NewAtom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.Assertz(&compound{
			functor: NewAtom("foo"),
			args:    []Term{NewAtom("b")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, &userDefined{dynamic: true, clauses: []clause{
			{
				pi: ProcedureIndicator{
					Name:  NewAtom("foo"),
					Arity: 1,
				},
				raw: &compound{
					functor: NewAtom("foo"),
					args:    []Term{NewAtom("a")},
				},
				xrTable: []Term{NewAtom("a")},
				bytecode: bytecode{
					{opcode: opConst, operand: 0},
					{opcode: opExit},
				},
			},
			{
				pi: ProcedureIndicator{
					Name:  NewAtom("foo"),
					Arity: 1,
				},
				raw: &compound{
					functor: NewAtom("foo"),
					args:    []Term{NewAtom("b")},
				},
				xrTable: []Term{NewAtom("b")},
				bytecode: bytecode{
					{opcode: opConst, operand: 0},
					{opcode: opExit},
				},
			},
		}}, state.procedures[ProcedureIndicator{
			Name:  NewAtom("foo"),
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
			functor: atomIf,
			args:    []Term{head, atomTrue},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("head is neither a variable, nor callable", func(t *testing.T) {
		var state State
		ok, err := state.Assertz(&compound{
			functor: atomIf,
			args:    []Term{Integer(0), atomTrue},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("body contains a term which is not callable", func(t *testing.T) {
		var state State
		ok, err := state.Assertz(&compound{
			functor: atomIf,
			args: []Term{
				NewAtom("foo"),
				&compound{
					functor: atomComma,
					args: []Term{
						atomTrue,
						Integer(0),
					},
				},
			},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeCallable, &compound{
			functor: atomComma,
			args: []Term{
				atomTrue,
				Integer(0),
			},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("static", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: NewAtom("foo"), Arity: 0}: &userDefined{dynamic: false},
				},
			},
		}

		ok, err := state.Assertz(NewAtom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationModify, PermissionTypeStaticProcedure, &compound{
			functor: atomSlash,
			args: []Term{
				NewAtom("foo"),
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
			functor: NewAtom("foo"),
			args:    []Term{NewAtom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.Asserta(&compound{
			functor: NewAtom("foo"),
			args:    []Term{NewAtom("b")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, &userDefined{dynamic: true, clauses: []clause{
			{
				pi: ProcedureIndicator{Name: NewAtom("foo"), Arity: 1},
				raw: &compound{
					functor: NewAtom("foo"),
					args:    []Term{NewAtom("b")},
				},
				xrTable: []Term{NewAtom("b")},
				bytecode: bytecode{
					{opcode: opConst, operand: 0},
					{opcode: opExit},
				},
			},
			{
				pi: ProcedureIndicator{Name: NewAtom("foo"), Arity: 1},
				raw: &compound{
					functor: NewAtom("foo"),
					args:    []Term{NewAtom("a")},
				},
				xrTable: []Term{NewAtom("a")},
				bytecode: bytecode{
					{opcode: opConst, operand: 0},
					{opcode: opExit},
				},
			},
		}}, state.procedures[ProcedureIndicator{Name: NewAtom("foo"), Arity: 1}])
	})

	t.Run("rule", func(t *testing.T) {
		var state State
		ok, err := state.Asserta(&compound{
			functor: atomIf,
			args: []Term{
				NewAtom("foo"),
				&compound{
					functor: NewAtom("p"),
					args:    []Term{NewAtom("b")},
				},
			},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.Asserta(&compound{
			functor: atomIf,
			args: []Term{
				NewAtom("foo"),
				&compound{
					functor: atomComma,
					args: []Term{
						&compound{
							functor: NewAtom("p"),
							args:    []Term{NewAtom("a")},
						},
						atomCut,
					},
				},
			},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, &userDefined{dynamic: true, clauses: []clause{
			{
				pi: ProcedureIndicator{Name: NewAtom("foo"), Arity: 0},
				raw: &compound{
					functor: atomIf,
					args: []Term{
						NewAtom("foo"),
						&compound{
							functor: atomComma,
							args: []Term{
								&compound{
									functor: NewAtom("p"),
									args:    []Term{NewAtom("a")},
								},
								atomCut,
							},
						},
					},
				},
				xrTable: []Term{
					NewAtom("a"),
					ProcedureIndicator{Name: NewAtom("p"), Arity: 1},
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
				pi: ProcedureIndicator{Name: NewAtom("foo"), Arity: 0},
				raw: &compound{
					functor: atomIf,
					args: []Term{
						NewAtom("foo"),
						&compound{
							functor: NewAtom("p"),
							args:    []Term{NewAtom("b")},
						},
					},
				},
				xrTable: []Term{
					NewAtom("b"),
					ProcedureIndicator{Name: NewAtom("p"), Arity: 1},
				},
				bytecode: bytecode{
					{opcode: opEnter},
					{opcode: opConst, operand: 0},
					{opcode: opCall, operand: 1},
					{opcode: opExit},
				},
			},
		}}, state.procedures[ProcedureIndicator{Name: NewAtom("foo"), Arity: 0}])
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
			functor: atomIf,
			args:    []Term{head, atomTrue},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("head is neither a variable, nor callable", func(t *testing.T) {
		var state State
		ok, err := state.Asserta(&compound{
			functor: atomIf,
			args:    []Term{Integer(0), atomTrue},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("body is not callable", func(t *testing.T) {
		var state State
		ok, err := state.Asserta(&compound{
			functor: atomIf,
			args:    []Term{NewAtom("foo"), Integer(0)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("body contains a term which is not callable", func(t *testing.T) {
		var state State
		ok, err := state.Asserta(&compound{
			functor: atomIf,
			args: []Term{
				NewAtom("foo"),
				&compound{
					functor: atomComma,
					args: []Term{
						atomTrue,
						Integer(0)},
				},
			},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeCallable, &compound{
			functor: atomComma,
			args: []Term{
				atomTrue,
				Integer(0)},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("static", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: NewAtom("foo"), Arity: 0}: &userDefined{dynamic: false},
				},
			},
		}

		ok, err := state.Asserta(NewAtom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationModify, PermissionTypeStaticProcedure, &compound{
			functor: atomSlash,
			args: []Term{
				NewAtom("foo"),
				Integer(0),
			},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("cut", func(t *testing.T) {
		var state State
		ok, err := state.Asserta(&compound{
			functor: atomIf,
			args: []Term{
				NewAtom("foo"),
				atomCut,
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
					{Name: NewAtom("foo"), Arity: 1}: &userDefined{dynamic: true, clauses: []clause{
						{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("a")}}},
						{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("b")}}},
						{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("c")}}},
					}},
				},
			},
		}

		ok, err := state.Retract(&compound{
			functor: NewAtom("foo"),
			args:    []Term{NewNamedVariable("X")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, &userDefined{dynamic: true, clauses: []clause{
			{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("b")}}},
			{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("c")}}},
		}}, state.procedures[ProcedureIndicator{Name: NewAtom("foo"), Arity: 1}])
	})

	t.Run("retract the specific one", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: NewAtom("foo"), Arity: 1}: &userDefined{dynamic: true, clauses: []clause{
						{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("a")}}},
						{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("b")}}},
						{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("c")}}},
					}},
				},
			},
		}

		ok, err := state.Retract(&compound{
			functor: NewAtom("foo"),
			args:    []Term{NewAtom("b")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, &userDefined{dynamic: true, clauses: []clause{
			{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("a")}}},
			{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("c")}}},
		}}, state.procedures[ProcedureIndicator{Name: NewAtom("foo"), Arity: 1}])
	})

	t.Run("retract all", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: NewAtom("foo"), Arity: 1}: &userDefined{dynamic: true, clauses: []clause{
						{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("a")}}},
						{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("b")}}},
						{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("c")}}},
					}},
				},
			},
		}

		ok, err := state.Retract(&compound{
			functor: NewAtom("foo"),
			args:    []Term{NewNamedVariable("X")},
		}, Failure, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
		assert.Empty(t, state.procedures[ProcedureIndicator{Name: NewAtom("foo"), Arity: 1}].(*userDefined).clauses)
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
			functor: NewAtom("foo"),
			args:    []Term{NewNamedVariable("X")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("static", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: NewAtom("foo"), Arity: 0}: &userDefined{dynamic: false},
				},
			},
		}

		ok, err := state.Retract(NewAtom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationModify, PermissionTypeStaticProcedure, &compound{
			functor: atomSlash,
			args:    []Term{NewAtom("foo"), Integer(0)},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("exception in continuation", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: NewAtom("foo"), Arity: 1}: &userDefined{dynamic: true, clauses: []clause{
						{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("a")}}},
					}},
				},
			},
		}

		ok, err := state.Retract(&compound{
			functor: NewAtom("foo"),
			args:    []Term{NewNamedVariable("X")},
		}, func(_ *Env) *Promise {
			return Error(errors.New("failed"))
		}, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)

		// removed
		assert.Empty(t, state.procedures[ProcedureIndicator{Name: NewAtom("foo"), Arity: 1}].(*userDefined).clauses)
	})
}

func TestState_Abolish(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: NewAtom("foo"), Arity: 1}: &userDefined{dynamic: true, clauses: []clause{
						{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("a")}}},
						{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("b")}}},
						{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("c")}}},
					}},
				},
			},
		}

		ok, err := state.Abolish(&compound{
			functor: atomSlash,
			args:    []Term{NewAtom("foo"), Integer(1)},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		_, ok = state.procedures[ProcedureIndicator{Name: NewAtom("foo"), Arity: 1}]
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
				functor: atomSlash,
				args:    []Term{NewNamedVariable("Name"), Integer(2)},
			}, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		})

		t.Run("Arity is a variable", func(t *testing.T) {
			var state State
			ok, err := state.Abolish(&compound{
				functor: atomSlash,
				args:    []Term{NewAtom("foo"), NewNamedVariable("Arity")},
			}, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("pi is neither a variable nor a predicate indicator", func(t *testing.T) {
		t.Run("compound", func(t *testing.T) {
			var state State
			ok, err := state.Abolish(atomPlus.Apply(NewAtom("foo"), Integer(1)), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypePredicateIndicator, atomPlus.Apply(NewAtom("foo"), Integer(1)), nil), err)
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
			functor: atomSlash,
			args:    []Term{Integer(0), Integer(2)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and Arity is neither a variable nor an integer", func(t *testing.T) {
		var state State
		ok, err := state.Abolish(&compound{
			functor: atomSlash,
			args:    []Term{NewAtom("foo"), NewAtom("bar")},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, NewAtom("bar"), nil), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and Arity is an integer less than zero", func(t *testing.T) {
		var state State
		ok, err := state.Abolish(&compound{
			functor: atomSlash,
			args:    []Term{NewAtom("foo"), Integer(-2)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainNotLessThanZero, Integer(-2), nil), err)
		assert.False(t, ok)
	})

	t.Run("The predicate indicator pi is that of a static procedure", func(t *testing.T) {
		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: NewAtom("foo"), Arity: 0}: &userDefined{dynamic: false},
				},
			},
		}
		ok, err := state.Abolish(&compound{
			functor: atomSlash,
			args:    []Term{NewAtom("foo"), Integer(0)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationModify, PermissionTypeStaticProcedure, &compound{
			functor: atomSlash,
			args:    []Term{NewAtom("foo"), Integer(0)},
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
		s := &Stream{sourceSink: os.Stdin}
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
		s := &Stream{sourceSink: os.Stdin}
		env := NewEnv().
			Bind(v, s)
		state := State{
			streams: map[Term]*Stream{
				NewAtom("x"): s,
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
		ok, err := state.SetInput(NewAtom("x"), Success, nil).Force(context.Background())
		assert.Equal(t, ExistenceError(ObjectTypeStream, NewAtom("x"), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		v := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(v, &Stream{sourceSink: os.Stdout, mode: ioModeAppend})
		var state State
		ok, err := state.SetInput(v, Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypeStream, v, env), err)
		assert.False(t, ok)
	})
}

func TestState_SetOutput(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		v := NewNamedVariable("Stream")
		s := &Stream{sourceSink: os.Stdout, mode: ioModeAppend}
		env := NewEnv().
			Bind(v, s)
		var state State
		ok, err := state.SetOutput(v, Success, env).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.Equal(t, s, state.output)
	})

	t.Run("alias", func(t *testing.T) {
		s := &Stream{sourceSink: os.Stdout, mode: ioModeAppend}
		state := State{
			streams: map[Term]*Stream{
				NewAtom("x"): s,
			},
		}
		ok, err := state.SetOutput(NewAtom("x"), Success, nil).Force(context.Background())
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
		ok, err := state.SetOutput(NewAtom("x"), Success, nil).Force(context.Background())
		assert.Equal(t, ExistenceError(ObjectTypeStream, NewAtom("x"), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an input stream", func(t *testing.T) {
		s := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(s, &Stream{sourceSink: os.Stdin})

		var state State
		ok, err := state.SetOutput(s, Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationOutput, PermissionTypeStream, s, env), err)
		assert.False(t, ok)
	})
}

func TestState_Open(t *testing.T) {
	var state State

	t.Run("read", func(t *testing.T) {
		f, err := os.CreateTemp("", "open_test_read")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, os.Remove(f.Name()))
		}()

		_, err = fmt.Fprintf(f, "test\n")
		assert.NoError(t, err)

		assert.NoError(t, f.Close())

		t.Run("alias", func(t *testing.T) {
			v := NewNamedVariable("Stream")
			ok, err := state.Open(NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomAlias,
				args:    []Term{atomInput},
			}), func(env *Env) *Promise {
				ref, ok := env.Lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*Stream)
				assert.True(t, ok)

				assert.Equal(t, state.streams[atomInput], s)

				b, err := io.ReadAll(s)
				assert.NoError(t, err)
				assert.Equal(t, "test\n", string(b))

				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("type text", func(t *testing.T) {
			v := NewNamedVariable("Stream")
			ok, err := state.Open(NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomType,
				args:    []Term{atomText},
			}), func(env *Env) *Promise {
				ref, ok := env.Lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*Stream)
				assert.True(t, ok)
				assert.Equal(t, streamTypeText, s.streamType)
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("type binary", func(t *testing.T) {
			v := NewNamedVariable("Stream")
			ok, err := state.Open(NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomType,
				args:    []Term{atomBinary},
			}), func(env *Env) *Promise {
				ref, ok := env.Lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*Stream)
				assert.True(t, ok)
				assert.Equal(t, streamTypeBinary, s.streamType)
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("reposition true", func(t *testing.T) {
			v := NewNamedVariable("Stream")
			ok, err := state.Open(NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomReposition,
				args:    []Term{atomTrue},
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
			ok, err := state.Open(NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomReposition,
				args:    []Term{atomFalse},
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
			ok, err := state.Open(NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomEOFAction,
				args:    []Term{atomError},
			}), func(env *Env) *Promise {
				ref, ok := env.Lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*Stream)
				assert.True(t, ok)
				assert.Equal(t, eofActionError, s.eofAction)
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("eof_action eof_code", func(t *testing.T) {
			v := NewNamedVariable("Stream")
			ok, err := state.Open(NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomEOFAction,
				args:    []Term{atomEOFCode},
			}), func(env *Env) *Promise {
				ref, ok := env.Lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*Stream)
				assert.True(t, ok)
				assert.Equal(t, eofActionEOFCode, s.eofAction)
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("eof_action reset", func(t *testing.T) {
			v := NewNamedVariable("Stream")
			ok, err := state.Open(NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomEOFAction,
				args:    []Term{atomReset},
			}), func(env *Env) *Promise {
				ref, ok := env.Lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*Stream)
				assert.True(t, ok)
				assert.Equal(t, eofActionReset, s.eofAction)
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("unknown option", func(t *testing.T) {
			v := NewNamedVariable("Stream")
			ok, err := state.Open(NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomUnknown,
				args:    []Term{NewAtom("option")},
			}), func(env *Env) *Promise {
				assert.Fail(t, "unreachable")
				return Bool(true)
			}, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})

		t.Run("wrong arity", func(t *testing.T) {
			v := NewNamedVariable("Stream")
			ok, err := state.Open(NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomType,
				args:    []Term{NewAtom("a"), NewAtom("b")},
			}), func(env *Env) *Promise {
				assert.Fail(t, "unreachable")
				return Bool(true)
			}, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})

		t.Run("variable arg", func(t *testing.T) {
			v := NewNamedVariable("Stream")
			ok, err := state.Open(NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomType,
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
			ok, err := state.Open(NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomType,
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

		ok, err := state.Open(NewAtom(n), atomWrite, v, List(&compound{
			functor: atomAlias,
			args:    []Term{atomOutput},
		}), func(env *Env) *Promise {
			ref, ok := env.Lookup(v)
			assert.True(t, ok)
			s, ok := ref.(*Stream)
			assert.True(t, ok)

			assert.Equal(t, state.streams[atomOutput], s)

			w, ok := s.sourceSink.(io.Writer)
			assert.True(t, ok)

			_, err := fmt.Fprintf(w, "test\n")
			assert.NoError(t, err)

			f, err := os.Open(n)
			assert.NoError(t, err)
			defer func() {
				assert.NoError(t, f.Close())
			}()

			b, err := io.ReadAll(f)
			assert.NoError(t, err)
			assert.Equal(t, "test\n", string(b))

			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("append", func(t *testing.T) {
		f, err := os.CreateTemp("", "open_test_append")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, os.Remove(f.Name()))
		}()

		_, err = fmt.Fprintf(f, "test\n")
		assert.NoError(t, err)

		assert.NoError(t, f.Close())

		v := NewNamedVariable("Stream")

		ok, err := state.Open(NewAtom(f.Name()), atomAppend, v, List(&compound{
			functor: atomAlias,
			args:    []Term{atomAppend},
		}), func(env *Env) *Promise {
			ref, ok := env.Lookup(v)
			assert.True(t, ok)
			s, ok := ref.(*Stream)
			assert.True(t, ok)

			assert.Equal(t, state.streams[atomAppend], s)

			w, ok := s.sourceSink.(io.Writer)
			assert.True(t, ok)

			_, err = fmt.Fprintf(w, "test\n")
			assert.NoError(t, err)

			f, err = os.Open(f.Name())
			assert.NoError(t, err)
			defer func() {
				assert.NoError(t, f.Close())
			}()

			b, err := io.ReadAll(f)
			assert.NoError(t, err)
			assert.Equal(t, "test\ntest\n", string(b))

			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("sourceSink is a variable", func(t *testing.T) {
		var state State
		ok, err := state.Open(NewNamedVariable("Source_Sink"), atomRead, NewNamedVariable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("mode is a variable", func(t *testing.T) {
		var state State
		ok, err := state.Open(NewAtom("/dev/null"), NewNamedVariable("Mode"), NewNamedVariable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("options is a partial list or a list with an element E which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			var state State
			ok, err := state.Open(NewAtom("/dev/null"), atomRead, NewNamedVariable("Stream"), ListRest(NewNamedVariable("Rest"),
				&compound{functor: atomType, args: []Term{atomText}},
				&compound{functor: atomAlias, args: []Term{NewAtom("foo")}},
			), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			var state State
			ok, err := state.Open(NewAtom("/dev/null"), atomRead, NewNamedVariable("Stream"), List(
				NewNamedVariable("Option"),
				&compound{functor: atomType, args: []Term{atomText}},
				&compound{functor: atomAlias, args: []Term{NewAtom("foo")}},
			), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("mode is neither a variable nor an atom", func(t *testing.T) {
		var state State
		ok, err := state.Open(NewAtom("/dev/null"), Integer(0), NewNamedVariable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		var state State
		ok, err := state.Open(NewAtom("/dev/null"), atomRead, NewNamedVariable("Stream"), NewAtom("list"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeList, NewAtom("list"), nil), err)
		assert.False(t, ok)
	})

	t.Run("stream is not a variable", func(t *testing.T) {
		var state State
		ok, err := state.Open(NewAtom("/dev/null"), atomRead, NewAtom("stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("sourceSink is neither a variable nor a source/sink", func(t *testing.T) {
		var state State
		ok, err := state.Open(Integer(0), atomRead, NewNamedVariable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainSourceSink, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("mode is an atom but not an input/output mode", func(t *testing.T) {
		var state State
		ok, err := state.Open(NewAtom("/dev/null"), NewAtom("foo"), NewNamedVariable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainIOMode, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the options list is neither a variable nor a stream-option", func(t *testing.T) {
		var state State
		for _, o := range []Term{
			NewAtom("foo"),
			&compound{functor: NewAtom("foo"), args: []Term{NewAtom("bar")}},
			&compound{functor: atomAlias, args: []Term{Integer(0)}},
			&compound{functor: atomType, args: []Term{Integer(0)}},
			&compound{functor: atomReposition, args: []Term{Integer(0)}},
			&compound{functor: atomEOFAction, args: []Term{Integer(0)}},
		} {
			ok, err := state.Open(NewAtom("/dev/null"), atomRead, NewNamedVariable("Stream"), List(o), Success, nil).Force(context.Background())
			assert.Equal(t, DomainError(ValidDomainStreamOption, o, nil), err)
			assert.False(t, ok)
		}
	})

	// Derived from 5.5.12 Options in Cor.3
	t.Run("a component of an element E of the options list is a variable", func(t *testing.T) {
		var state State
		for _, o := range []Term{
			NewNamedVariable("X"),
			&compound{functor: atomAlias, args: []Term{NewNamedVariable("X")}},
			&compound{functor: atomType, args: []Term{NewNamedVariable("X")}},
			&compound{functor: atomReposition, args: []Term{NewNamedVariable("X")}},
			&compound{functor: atomEOFAction, args: []Term{NewNamedVariable("X")}},
		} {
			ok, err := state.Open(NewAtom("/dev/null"), atomRead, NewNamedVariable("Stream"), List(o), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		}
	})

	t.Run("the source/sink specified by sourceSink does not exist", func(t *testing.T) {
		f, err := os.CreateTemp("", "open_test_existence")
		assert.NoError(t, err)
		assert.NoError(t, os.Remove(f.Name()))

		var state State
		ok, err := state.Open(NewAtom(f.Name()), atomRead, NewNamedVariable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, ExistenceError(ObjectTypeSourceSink, NewAtom(f.Name()), nil), err)
		assert.False(t, ok)
	})

	t.Run("the source/sink specified by sourceSink cannot be opened", func(t *testing.T) {
		f, err := os.CreateTemp("", "open_test_permission")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, os.Remove(f.Name()))
		}()

		assert.NoError(t, f.Chmod(0200))

		var state State
		ok, err := state.Open(NewAtom(f.Name()), atomRead, NewNamedVariable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationOpen, PermissionTypeSourceSink, NewAtom(f.Name()), nil), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the options list is alias and A is already associated with an open stream", func(t *testing.T) {
		f, err := os.CreateTemp("", "open_test_dup_alias")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, os.Remove(f.Name()))
		}()

		state := State{
			streams: map[Term]*Stream{
				NewAtom("foo"): nil,
			},
		}
		ok, err := state.Open(NewAtom(f.Name()), atomRead, NewNamedVariable("Stream"), List(&compound{
			functor: atomAlias,
			args:    []Term{NewAtom("foo")},
		}), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationOpen, PermissionTypeSourceSink, &compound{
			functor: atomAlias,
			args:    []Term{NewAtom("foo")},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("system error", func(t *testing.T) {
		openFile = func(name string, flag int, perm os.FileMode) (*os.File, error) {
			return nil, errors.New("failed")
		}
		defer func() {
			openFile = os.OpenFile
		}()

		var state State
		_, err := state.Open(NewAtom("foo"), atomRead, NewNamedVariable("Stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, SystemError(errors.New("failed")), err)
	})
}

func TestState_Close(t *testing.T) {
	t.Run("without options", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			t.Run("stream", func(t *testing.T) {
				var m mockCloser
				m.On("Close").Return(nil).Once()
				defer m.AssertExpectations(t)

				var state State
				ok, err := state.Close(&Stream{sourceSink: &m}, List(), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("alias", func(t *testing.T) {
				var m mockCloser
				m.On("Close").Return(nil).Once()
				defer m.AssertExpectations(t)

				foo := NewAtom("foo")

				state := State{
					streams: map[Term]*Stream{
						foo: {sourceSink: &m, alias: foo},
					},
				}
				ok, err := state.Close(foo, List(), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.True(t, ok)
			})
		})

		t.Run("ng", func(t *testing.T) {
			var m mockCloser
			m.On("Close").Return(errors.New("failed")).Once()
			defer m.AssertExpectations(t)

			var state State
			_, err := state.Close(&Stream{sourceSink: &m}, List(), Success, nil).Force(context.Background())
			assert.Equal(t, SystemError(errors.New("failed")), err)
		})
	})

	t.Run("force false", func(t *testing.T) {
		var m mockCloser
		m.On("Close").Return(errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		var state State
		_, err := state.Close(&Stream{sourceSink: &m}, List(atomForce.Apply(atomFalse)), Success, nil).Force(context.Background())
		assert.Equal(t, SystemError(errors.New("failed")), err)
	})

	t.Run("force true", func(t *testing.T) {
		var m mockCloser
		m.On("Close").Return(errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		var state State
		ok, err := state.Close(&Stream{sourceSink: &m}, List(atomForce.Apply(atomTrue)), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		var m mockCloser
		m.On("Close").Return(nil).Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m}

		state := State{
			streams: map[Term]*Stream{
				NewAtom("foo"): s,
			},
		}
		ok, err := state.Close(NewAtom("foo"), List(), Success, nil).Force(context.Background())
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
				&compound{functor: atomForce, args: []Term{atomTrue}},
			), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			var state State
			ok, err := state.Close(&Stream{}, List(NewNamedVariable("Option"), &compound{functor: atomForce, args: []Term{atomTrue}}), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		var state State
		ok, err := state.Close(&Stream{}, NewAtom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeList, NewAtom("foo"), nil), err)
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
			ok, err := state.Close(&Stream{}, List(NewAtom("foo")), Success, nil).Force(context.Background())
			assert.Equal(t, DomainError(ValidDomainStreamOption, NewAtom("foo"), nil), err)
			assert.False(t, ok)
		})

		t.Run("compound", func(t *testing.T) {
			t.Run("force but arity is not 1", func(t *testing.T) {
				var state State
				ok, err := state.Close(&Stream{}, List(atomForce.Apply(NewAtom("a"), NewAtom("b"))), Success, nil).Force(context.Background())
				assert.Equal(t, DomainError(ValidDomainStreamOption, atomForce.Apply(NewAtom("a"), NewAtom("b")), nil), err)
				assert.False(t, ok)
			})

			t.Run("force but the argument is a variable", func(t *testing.T) {
				var state State
				_, err := state.Close(&Stream{}, List(atomForce.Apply(NewNamedVariable("X"))), Success, nil).Force(context.Background())
				_, ok := NewEnv().Unify(DomainError(ValidDomainStreamOption, atomForce.Apply(NewVariable()), nil).term, err.(Exception).term, false)
				assert.True(t, ok)
			})

			t.Run("force but the argument is neither true nor false", func(t *testing.T) {
				var state State
				ok, err := state.Close(&Stream{}, List(atomForce.Apply(NewAtom("meh"))), Success, nil).Force(context.Background())
				assert.Equal(t, DomainError(ValidDomainStreamOption, atomForce.Apply(NewAtom("meh")), nil), err)
				assert.False(t, ok)
			})
		})
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var state State
		ok, err := state.Close(NewAtom("foo"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, ExistenceError(ObjectTypeStream, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})
}

func TestState_FlushOutput(t *testing.T) {
	f, err := os.CreateTemp("", "")
	assert.NoError(t, err)
	defer func() {
		assert.NoError(t, os.Remove(f.Name()))
	}()

	s := &Stream{sourceSink: f, mode: ioModeWrite}

	t.Run("ok", func(t *testing.T) {
		var state State
		ok, err := state.FlushOutput(s, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("ng", func(t *testing.T) {
		var m mockSyncer
		m.On("Sync").Return(errors.New("ng")).Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m, mode: ioModeWrite}

		var state State
		_, err := state.FlushOutput(s, Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		state := State{
			streams: map[Term]*Stream{
				NewAtom("foo"): s,
			},
		}
		ok, err := state.FlushOutput(NewAtom("foo"), Success, nil).Force(context.Background())
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
		ok, err := state.FlushOutput(NewAtom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, ExistenceError(ObjectTypeStream, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("SorA is an input stream", func(t *testing.T) {
		s := &Stream{sourceSink: os.Stdin}

		var state State
		ok, err := state.FlushOutput(s, Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationOutput, PermissionTypeStream, s, nil), err)
		assert.False(t, ok)
	})
}

func TestState_WriteTerm(t *testing.T) {
	var buf bytes.Buffer
	w := &Stream{sourceSink: &buf, mode: ioModeWrite}
	r := &Stream{sourceSink: &buf, mode: ioModeRead}
	b := &Stream{sourceSink: &buf, mode: ioModeWrite, streamType: streamTypeBinary}

	err := errors.New("failed")

	var m mockWriter
	m.On("Write", mock.Anything).Return(0, err)

	mw := &Stream{sourceSink: &m, mode: ioModeWrite}

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
		{title: `write_canonical([1,2,3]).`, sOrA: w, term: List(Integer(1), Integer(2), Integer(3)), options: List(atomQuoted.Apply(atomTrue), atomIgnoreOps.Apply(atomTrue)), ok: true, output: `'.'(1,'.'(2,'.'(3,[])))`},
		{title: `write_term(S, '1<2', []).`, sOrA: w, term: NewAtom("1<2"), options: List(), ok: true, output: `1<2`},
		{title: `writeq(S, '1<2').`, sOrA: w, term: NewAtom("1<2"), options: List(atomQuoted.Apply(atomTrue), atomNumberVars.Apply(atomTrue)), ok: true, output: `'1<2'`},
		{title: `writeq('$VAR'(0)).`, sOrA: w, term: atomVar.Apply(Integer(0)), options: List(atomQuoted.Apply(atomTrue), atomNumberVars.Apply(atomTrue)), ok: true, output: `A`},
		{title: `write_term(S, '$VAR'(1), [numbervars(false)]).`, sOrA: w, term: atomVar.Apply(Integer(1)), options: List(atomNumberVars.Apply(atomFalse)), ok: true, output: `$VAR(1)`},
		{title: `write_term(S, '$VAR'(51), [numbervars(true)]).`, sOrA: w, term: atomVar.Apply(Integer(51)), options: List(atomNumberVars.Apply(atomTrue)), ok: true, output: `Z1`},
		{title: `write_term(1, [quoted(non_boolean)]).`, sOrA: w, term: Integer(1), options: List(atomQuoted.Apply(NewAtom("non_boolean"))), err: DomainError(ValidDomainWriteOption, atomQuoted.Apply(NewAtom("non_boolean")), nil)},
		{title: `write_term(1, [quoted(B)]).`, sOrA: w, term: Integer(1), options: List(atomQuoted.Apply(NewNamedVariable("B"))), err: InstantiationError(nil)},
		{title: `B = true, write_term(1, [quoted(B)]).`, sOrA: w, env: NewEnv().Bind(NewNamedVariable("B"), atomTrue), term: Integer(1), options: List(atomQuoted.Apply(NewNamedVariable("B"))), ok: true, output: `1`},

		// 8.14.2.3 Errors
		{title: `a`, sOrA: NewNamedVariable("S"), term: NewAtom("foo"), options: List(), err: InstantiationError(nil)},
		{title: `b: partial list`, sOrA: w, term: NewAtom("foo"), options: ListRest(NewNamedVariable("X"), atomQuoted.Apply(atomTrue)), err: InstantiationError(nil)},
		{title: `b: variable element`, sOrA: w, term: NewAtom("foo"), options: List(NewNamedVariable("X")), err: InstantiationError(nil)},
		{title: `b: variable component`, sOrA: w, term: NewAtom("foo"), options: List(atomQuoted.Apply(NewNamedVariable("X"))), err: InstantiationError(nil)},
		{title: `b: variable_names, partial list`, sOrA: w, term: NewAtom("foo"), options: List(atomVariableNames.Apply(NewNamedVariable("L"))), err: InstantiationError(nil)},
		{title: `b: variable_names, element`, sOrA: w, term: NewAtom("foo"), options: List(atomVariableNames.Apply(List(NewNamedVariable("E")))), err: InstantiationError(nil)},
		{title: `b: variable_names, name`, sOrA: w, term: NewNamedVariable("V"), options: List(atomVariableNames.Apply(List(atomEqual.Apply(NewNamedVariable("N"), NewNamedVariable("V"))))), err: InstantiationError(nil)},
		{title: `c`, sOrA: w, term: NewAtom("foo"), options: NewAtom("options"), err: TypeError(ValidTypeList, NewAtom("options"), nil)},
		{title: `d`, sOrA: Integer(0), term: NewAtom("foo"), options: List(), err: DomainError(ValidDomainStreamOrAlias, Integer(0), nil)},
		{title: `e: not a compound`, sOrA: w, term: NewAtom("foo"), options: List(NewAtom("bar")), err: DomainError(ValidDomainWriteOption, NewAtom("bar"), nil)},
		{title: `e: arity is not 1`, sOrA: w, term: NewAtom("foo"), options: List(NewAtom("bar").Apply(NewAtom("a"), NewAtom("b"))), err: DomainError(ValidDomainWriteOption, NewAtom("bar").Apply(NewAtom("a"), NewAtom("b")), nil)},
		{title: `e: variable_names, not a list, atom`, sOrA: w, term: NewAtom("foo"), options: List(atomVariableNames.Apply(NewAtom("a"))), err: DomainError(ValidDomainWriteOption, atomVariableNames.Apply(NewAtom("a")), nil)},
		{title: `e: variable_names, not a list, atomic`, sOrA: w, term: NewAtom("foo"), options: List(atomVariableNames.Apply(Integer(0))), err: DomainError(ValidDomainWriteOption, atomVariableNames.Apply(Integer(0)), nil)},
		{title: `e: variable_names, element is not a pair, atomic`, sOrA: w, term: NewAtom("foo"), options: List(atomVariableNames.Apply(List(NewAtom("a")))), err: DomainError(ValidDomainWriteOption, atomVariableNames.Apply(List(NewAtom("a"))), nil)},
		{title: `e: variable_names, element is not a pair, compound`, sOrA: w, term: NewAtom("foo"), options: List(atomVariableNames.Apply(List(NewAtom("f").Apply(NewAtom("a"))))), err: DomainError(ValidDomainWriteOption, atomVariableNames.Apply(List(NewAtom("f").Apply(NewAtom("a")))), nil)},
		{title: `e: variable_names, name is not an atom`, sOrA: w, term: NewNamedVariable("V"), options: List(atomVariableNames.Apply(List(atomEqual.Apply(Integer(0), NewNamedVariable("V"))))), err: DomainError(ValidDomainWriteOption, atomVariableNames.Apply(List(atomEqual.Apply(Integer(0), NewVariable()))), nil)},
		{title: `e: boolean option, not an atom`, sOrA: w, term: NewAtom("foo"), options: List(atomQuoted.Apply(Integer(0))), err: DomainError(ValidDomainWriteOption, atomQuoted.Apply(Integer(0)), nil)},
		{title: `e: unknown functor`, sOrA: w, term: NewAtom("foo"), options: List(NewAtom("bar").Apply(atomTrue)), err: DomainError(ValidDomainWriteOption, NewAtom("bar").Apply(atomTrue), nil)},
		{title: `f`, sOrA: NewAtom("stream"), term: NewAtom("foo"), options: List(), err: ExistenceError(ObjectTypeStream, NewAtom("stream"), nil)},
		{title: `g`, sOrA: r, term: NewAtom("foo"), options: List(), err: PermissionError(OperationOutput, PermissionTypeStream, r, nil)},
		{title: `h`, sOrA: b, term: NewAtom("foo"), options: List(), err: PermissionError(OperationOutput, PermissionTypeBinaryStream, b, nil)},

		// 7.10.5
		{title: `a`, sOrA: w, term: NewNamedVariable("X"), options: List(), ok: true, outputPattern: regexp.MustCompile(`_\d+`)},

		{title: `variable_names`, sOrA: w, term: NewNamedVariable("V"), options: List(atomVariableNames.Apply(List(
			atomEqual.Apply(NewAtom("n"), NewNamedVariable("V")), // left-most is used
			atomEqual.Apply(NewAtom("m"), NewNamedVariable("V")), // ignored
			atomEqual.Apply(NewAtom("a"), NewAtom("b")),          // ignored
		))), ok: true, output: `n`},

		{title: `failure`, sOrA: mw, term: NewAtom("foo"), options: List(), err: err},
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
		ok, err := CharCode(NewAtom("a"), Integer(97), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("emoji", func(t *testing.T) {
		ok, err := CharCode(NewAtom("😀"), Integer(128512), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("query char", func(t *testing.T) {
		v := NewNamedVariable("Char")

		ok, err := CharCode(v, Integer(128512), func(env *Env) *Promise {
			assert.Equal(t, NewAtom("😀"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("query code", func(t *testing.T) {
		v := NewNamedVariable("Code")
		ok, err := CharCode(NewAtom("😀"), v, func(env *Env) *Promise {
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
			ok, err := CharCode(NewAtom("foo"), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeCharacter, NewAtom("foo"), nil), err)
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
			ok, err := CharCode(NewVariable(), NewAtom("foo"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeInteger, NewAtom("foo"), nil), err)
			assert.False(t, ok)
		})

		t.Run("char is a character", func(t *testing.T) {
			ok, err := CharCode(NewAtom("a"), NewAtom("x"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeInteger, NewAtom("x"), nil), err)
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
		var m mockWriter
		m.On("Write", []byte{97}).Return(1, nil).Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m, mode: ioModeWrite, streamType: streamTypeBinary}

		var state State
		ok, err := state.PutByte(s, Integer(97), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("ng", func(t *testing.T) {
		var m mockWriter
		m.On("Write", []byte{97}).Return(0, errors.New("")).Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m, mode: ioModeWrite, streamType: streamTypeBinary}

		var state State
		_, err := state.PutByte(s, Integer(97), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		var m mockWriter
		m.On("Write", []byte{97}).Return(1, nil).Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m, mode: ioModeWrite, streamType: streamTypeBinary}

		state := State{
			streams: map[Term]*Stream{
				NewAtom("foo"): s,
			},
		}
		ok, err := state.PutByte(NewAtom("foo"), Integer(97), Success, nil).Force(context.Background())
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
		s := &Stream{sourceSink: os.Stdout, mode: ioModeAppend}
		s.streamType = streamTypeBinary

		var state State
		ok, err := state.PutByte(s, NewNamedVariable("Byte"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("byt is neither a variable nor an byte", func(t *testing.T) {
		s := &Stream{sourceSink: os.Stdout, mode: ioModeAppend}
		s.streamType = streamTypeBinary

		t.Run("not even an integer", func(t *testing.T) {
			var state State
			ok, err := state.PutByte(s, NewAtom("byte"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeByte, NewAtom("byte"), nil), err)
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
			Bind(s, &Stream{sourceSink: os.Stdin, mode: ioModeRead, streamType: streamTypeBinary})

		var state State
		ok, err := state.PutByte(s, Integer(97), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationOutput, PermissionTypeStream, s, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a text stream", func(t *testing.T) {
		s := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(s, &Stream{sourceSink: os.Stdout, mode: ioModeAppend})

		var state State
		ok, err := state.PutByte(s, Integer(97), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationOutput, PermissionTypeTextStream, s, env), err)
		assert.False(t, ok)
	})
}

func TestState_PutCode(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		var m mockWriter
		m.On("Write", []byte{0xf0, 0x9f, 0x98, 0x80}).Return(1, nil).Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m, mode: ioModeWrite}

		var state State
		ok, err := state.PutCode(s, Integer('😀'), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("ng", func(t *testing.T) {
		var m mockWriter
		m.On("Write", []byte{0xf0, 0x9f, 0x98, 0x80}).Return(0, errors.New("")).Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m, mode: ioModeWrite}

		var state State
		_, err := state.PutCode(s, Integer('😀'), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		var m mockWriter
		m.On("Write", []byte{0xf0, 0x9f, 0x98, 0x80}).Return(1, nil).Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m, mode: ioModeWrite}

		state := State{
			streams: map[Term]*Stream{
				NewAtom("foo"): s,
			},
		}
		ok, err := state.PutCode(NewAtom("foo"), Integer('😀'), Success, nil).Force(context.Background())
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
		ok, err := state.PutCode(&Stream{sourceSink: os.Stdout, mode: ioModeAppend}, NewNamedVariable("Code"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("code is neither a variable nor an integer", func(t *testing.T) {
		var state State
		ok, err := state.PutCode(&Stream{sourceSink: os.Stdout, mode: ioModeAppend}, NewAtom("code"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, NewAtom("code"), nil), err)
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
		ok, err := state.PutCode(NewAtom("foo"), Integer(97), Success, nil).Force(context.Background())
		assert.Equal(t, ExistenceError(ObjectTypeStream, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an input stream", func(t *testing.T) {
		s := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(s, &Stream{sourceSink: os.Stdin})

		var state State
		ok, err := state.PutCode(s, Integer(97), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationOutput, PermissionTypeStream, s, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		stream := &Stream{sourceSink: os.Stdout, mode: ioModeAppend}
		stream.streamType = streamTypeBinary

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
		ok, err := state.PutCode(&Stream{sourceSink: os.Stdout, mode: ioModeAppend}, Integer(-1), Success, nil).Force(context.Background())
		assert.Equal(t, RepresentationError(FlagCharacterCode, nil), err)
		assert.False(t, ok)
	})

	t.Run("unknown stream alias", func(t *testing.T) {
		var state State
		_, err := state.PutCode(NewAtom("foo"), Integer('😀'), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("not a stream", func(t *testing.T) {
		var state State
		_, err := state.PutCode(NewVariable(), Integer('😀'), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("not a code", func(t *testing.T) {
		s := &Stream{sourceSink: os.Stdout, mode: ioModeAppend}

		t.Run("not an integer", func(t *testing.T) {
			var state State
			_, err := state.PutCode(s, NewAtom("a"), Success, nil).Force(context.Background())
			assert.Error(t, err)
		})
	})
}

func TestState_ReadTerm(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		f, err := os.Open("testdata/foo.pl")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead}

		v := NewNamedVariable("Term")

		var state State
		ok, err := state.ReadTerm(s, v, List(), func(env *Env) *Promise {
			assert.Equal(t, NewAtom("foo"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		f, err := os.Open("testdata/foo.pl")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead}

		v := NewNamedVariable("Term")

		state := State{
			streams: map[Term]*Stream{
				NewAtom("foo"): s,
			},
		}
		ok, err := state.ReadTerm(NewAtom("foo"), v, List(), func(env *Env) *Promise {
			assert.Equal(t, NewAtom("foo"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("singletons", func(t *testing.T) {
		f, err := os.Open("testdata/vars.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead}

		v, singletons := NewNamedVariable("Term"), NewNamedVariable("Singletons")

		var state State
		ok, err := state.ReadTerm(s, v, List(&compound{
			functor: atomSingletons,
			args:    []Term{singletons},
		}), func(env *Env) *Promise {
			c, ok := env.Resolve(v).(*compound)
			assert.True(t, ok)
			assert.Equal(t, NewAtom("f"), c.functor)
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
		f, err := os.Open("testdata/vars.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead}

		v, variables := NewNamedVariable("Term"), NewNamedVariable("Variables")

		var state State
		ok, err := state.ReadTerm(s, v, List(&compound{
			functor: atomVariables,
			args:    []Term{variables},
		}), func(env *Env) *Promise {
			c, ok := env.Resolve(v).(*compound)
			assert.True(t, ok)
			assert.Equal(t, NewAtom("f"), c.functor)
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
		f, err := os.Open("testdata/vars.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead}

		v, variableNames := NewNamedVariable("Term"), NewNamedVariable("VariableNames")

		var state State
		ok, err := state.ReadTerm(s, v, List(&compound{
			functor: atomVariableNames,
			args:    []Term{variableNames},
		}), func(env *Env) *Promise {
			c, ok := env.Resolve(v).(*compound)
			assert.True(t, ok)
			assert.Equal(t, NewAtom("f"), c.functor)
			assert.Len(t, c.args, 3)

			x, ok := c.args[0].(Variable)
			assert.True(t, ok)
			assert.Equal(t, x, c.args[1])

			y, ok := c.args[2].(Variable)
			assert.True(t, ok)

			assert.Equal(t, List(
				&compound{
					functor: atomEqual,
					args:    []Term{NewAtom("X"), x},
				},
				&compound{
					functor: atomEqual,
					args:    []Term{NewAtom("Y"), y},
				},
			), env.Resolve(variableNames))

			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("multiple reads", func(t *testing.T) {
		f, err := os.Open("testdata/multi.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead}

		v := NewNamedVariable("Term")

		var state State

		ok, err := state.ReadTerm(s, v, List(), func(env *Env) *Promise {
			assert.Equal(t, &compound{functor: NewAtom("foo"), args: []Term{NewAtom("a")}}, env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.ReadTerm(s, v, List(), func(env *Env) *Promise {
			assert.Equal(t, &compound{functor: NewAtom("foo"), args: []Term{NewAtom("b")}}, env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.ReadTerm(s, v, List(), func(env *Env) *Promise {
			assert.Equal(t, &compound{functor: NewAtom("foo"), args: []Term{NewAtom("c")}}, env.Resolve(v))
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
			ok, err := state.ReadTerm(&Stream{sourceSink: os.Stdin}, NewVariable(), ListRest(NewNamedVariable("Rest"),
				&compound{functor: atomVariables, args: []Term{NewNamedVariable("VL")}},
			), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			var state State
			ok, err := state.ReadTerm(&Stream{sourceSink: os.Stdin}, NewVariable(), List(NewNamedVariable("Option"), &compound{functor: atomVariables, args: []Term{NewNamedVariable("VL")}}), Success, nil).Force(context.Background())
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
		ok, err := state.ReadTerm(&Stream{sourceSink: os.Stdin}, NewVariable(), NewAtom("options"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeList, NewAtom("options"), nil), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the Options list is neither a variable nor a valid read-option", func(t *testing.T) {
		var state State
		ok, err := state.ReadTerm(&Stream{sourceSink: os.Stdin}, NewVariable(), List(&compound{
			functor: atomUnknown,
			args:    []Term{NewAtom("option")},
		}), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainReadOption, &compound{
			functor: atomUnknown,
			args:    []Term{NewAtom("option")},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var state State
		ok, err := state.ReadTerm(NewAtom("foo"), NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, ExistenceError(ObjectTypeStream, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		s := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(s, &Stream{sourceSink: os.Stdout, mode: ioModeAppend})

		var state State
		ok, err := state.ReadTerm(s, NewVariable(), List(), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypeStream, s, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		stream := &Stream{sourceSink: os.Stdin}
		stream.streamType = streamTypeBinary

		s := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(s, stream)

		var state State
		ok, err := state.ReadTerm(s, NewVariable(), List(), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypeBinaryStream, s, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		f, err := os.Open("testdata/empty.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(s, &Stream{sourceSink: f, mode: ioModeRead, eofAction: eofActionError})

		var state State
		ok, err := state.ReadTerm(s, NewVariable(), List(), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypePastEndOfStream, s, env), err)
		assert.False(t, ok)
	})

	t.Run("one or more characters were input, but they cannot be parsed as a sequence of tokens", func(t *testing.T) {
		t.Run("unexpected token", func(t *testing.T) {
			f, err := os.Open("testdata/unexpected_token.txt")
			assert.NoError(t, err)
			defer func() {
				assert.NoError(t, f.Close())
			}()

			s := &Stream{sourceSink: f, mode: ioModeRead}

			var state State
			ok, err := state.ReadTerm(s, NewVariable(), List(), Success, nil).Force(context.Background())
			assert.Equal(t, SyntaxError(unexpectedTokenError{actual: Token{Kind: TokenLetterDigit, Val: "bar"}}, nil), err)
			assert.False(t, ok)
		})

		t.Run("insufficient", func(t *testing.T) {
			f, err := os.Open("testdata/insufficient.txt")
			assert.NoError(t, err)
			defer func() {
				assert.NoError(t, f.Close())
			}()

			s := &Stream{sourceSink: f, mode: ioModeRead}

			var state State
			ok, err := state.ReadTerm(s, NewVariable(), List(), Success, nil).Force(context.Background())
			assert.Equal(t, SyntaxError(ErrInsufficient, nil), err)
			assert.False(t, ok)
		})

	})

	t.Run("the sequence of tokens cannot be parsed as a term using the current set of operator definitions", func(t *testing.T) {
		f, err := os.Open("testdata/unexpected_op.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead}

		var state State
		ok, err := state.ReadTerm(s, NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, SyntaxError(unexpectedTokenError{actual: Token{Kind: TokenGraphic, Val: "="}}, nil), err)
		assert.False(t, ok)
	})
}

func TestState_GetByte(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		f, err := os.Open("testdata/a.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead, streamType: streamTypeBinary}

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
		f, err := os.Open("testdata/a.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead, streamType: streamTypeBinary}

		v := NewNamedVariable("Byte")

		state := State{
			streams: map[Term]*Stream{
				NewAtom("foo"): s,
			},
		}
		ok, err := state.GetByte(NewAtom("foo"), v, func(env *Env) *Promise {
			assert.Equal(t, Integer(97), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("eof", func(t *testing.T) {
		f, err := os.Open("testdata/empty.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead, streamType: streamTypeBinary}

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
		var m mockReader
		m.On("Read", mock.Anything).Return(0, errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m, mode: ioModeRead, streamType: streamTypeBinary}

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
		s := &Stream{sourceSink: os.Stdin}
		s.streamType = streamTypeBinary

		t.Run("not even an integer", func(t *testing.T) {
			var state State
			ok, err := state.GetByte(s, NewAtom("inByte"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeInByte, NewAtom("inByte"), nil), err)
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
		ok, err := state.GetByte(NewAtom("foo"), NewNamedVariable("InByte"), Success, nil).Force(context.Background())
		assert.Equal(t, ExistenceError(ObjectTypeStream, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, &Stream{sourceSink: os.Stdout, mode: ioModeAppend})

		var state State
		ok, err := state.GetByte(streamOrAlias, NewNamedVariable("InByte"), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypeStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a text stream", func(t *testing.T) {
		streamOrAlias := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, &Stream{sourceSink: os.Stdin})

		var state State
		ok, err := state.GetByte(streamOrAlias, NewNamedVariable("InByte"), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypeTextStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		f, err := os.Open("testdata/empty.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		streamOrAlias := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, &Stream{sourceSink: f, mode: ioModeRead, streamType: streamTypeBinary, eofAction: eofActionError})

		var state State
		ok, err := state.GetByte(streamOrAlias, NewNamedVariable("InByte"), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypePastEndOfStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})
}

func TestState_GetChar(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		f, err := os.Open("testdata/smile.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead}

		v := NewNamedVariable("Char")

		var state State
		ok, err := state.GetChar(s, v, func(env *Env) *Promise {
			assert.Equal(t, NewAtom("😀"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		f, err := os.Open("testdata/smile.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		v := NewNamedVariable("Char")

		state := State{
			streams: map[Term]*Stream{
				NewAtom("foo"): {sourceSink: f, mode: ioModeRead},
			},
		}
		ok, err := state.GetChar(NewAtom("foo"), v, func(env *Env) *Promise {
			assert.Equal(t, NewAtom("😀"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("eof", func(t *testing.T) {
		f, err := os.Open("testdata/empty.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead}

		v := NewNamedVariable("Char")

		var state State
		ok, err := state.GetChar(s, v, func(env *Env) *Promise {
			assert.Equal(t, atomEndOfFile, env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("error", func(t *testing.T) {
		var m mockReader
		m.On("Read", mock.Anything).Return(0, errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		v := NewNamedVariable("V")

		var state State
		ok, err := state.GetChar(&Stream{sourceSink: &m, mode: ioModeRead}, v, Success, nil).Force(context.Background())
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
			ok, err := state.GetChar(&Stream{sourceSink: os.Stdin}, Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeInCharacter, Integer(0), nil), err)
			assert.False(t, ok)
		})

		t.Run("atom", func(t *testing.T) {
			var state State
			ok, err := state.GetChar(&Stream{sourceSink: os.Stdin}, NewAtom("ab"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeInCharacter, NewAtom("ab"), nil), err)
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
			Bind(streamOrAlias, &Stream{sourceSink: os.Stdout, mode: ioModeAppend})

		var state State
		ok, err := state.GetChar(streamOrAlias, NewNamedVariable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypeStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		s := &Stream{sourceSink: os.Stdin}
		s.streamType = streamTypeBinary

		streamOrAlias := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, s)

		var state State
		ok, err := state.GetChar(streamOrAlias, NewNamedVariable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypeBinaryStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		f, err := os.Open("testdata/empty.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		streamOrAlias := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, &Stream{sourceSink: f, mode: ioModeRead, eofAction: eofActionError})

		var state State
		ok, err := state.GetChar(streamOrAlias, NewNamedVariable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypePastEndOfStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("the entity input from the stream is not a character", func(t *testing.T) {
		f, err := os.Open("testdata/replacement.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		streamOrAlias := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, &Stream{sourceSink: f, mode: ioModeRead})

		var state State
		ok, err := state.GetChar(streamOrAlias, NewNamedVariable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, RepresentationError(FlagCharacter, nil), err)
		assert.False(t, ok)
	})
}

func TestState_PeekByte(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		f, err := os.Open("testdata/abc.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead, streamType: streamTypeBinary}

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
		f, err := os.Open("testdata/abc.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		v := NewNamedVariable("Byte")

		state := State{
			streams: map[Term]*Stream{
				NewAtom("foo"): {sourceSink: f, mode: ioModeRead, streamType: streamTypeBinary},
			},
		}
		ok, err := state.PeekByte(NewAtom("foo"), v, func(env *Env) *Promise {
			assert.Equal(t, Integer(97), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("eof", func(t *testing.T) {
		f, err := os.Open("testdata/empty.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead, streamType: streamTypeBinary}

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
		var m mockReader
		m.On("Read", mock.Anything).Return(0, errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m, mode: ioModeRead}
		s.streamType = streamTypeBinary

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
		s := &Stream{sourceSink: os.Stdin}
		s.streamType = streamTypeBinary

		t.Run("not even an integer", func(t *testing.T) {
			var state State
			ok, err := state.PeekByte(s, NewAtom("byte"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeInByte, NewAtom("byte"), nil), err)
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
			Bind(streamOrAlias, &Stream{sourceSink: os.Stdout, mode: ioModeAppend})

		var state State
		ok, err := state.PeekByte(streamOrAlias, NewNamedVariable("Byte"), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypeStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a text stream", func(t *testing.T) {
		streamOrAlias := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, &Stream{sourceSink: os.Stdin})

		var state State
		ok, err := state.PeekByte(streamOrAlias, NewNamedVariable("Byte"), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypeTextStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		f, err := os.Open("testdata/empty.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		streamOrAlias := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, &Stream{sourceSink: f, mode: ioModeRead, streamType: streamTypeBinary, eofAction: eofActionError})

		var state State
		ok, err := state.PeekByte(streamOrAlias, NewNamedVariable("Byte"), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypePastEndOfStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})
}

func TestState_PeekChar(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		f, err := os.Open("testdata/smile.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead}

		v := NewNamedVariable("Char")

		var state State
		ok, err := state.PeekChar(s, v, func(env *Env) *Promise {
			assert.Equal(t, NewAtom("😀"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.PeekChar(s, v, func(env *Env) *Promise {
			assert.Equal(t, NewAtom("😀"), env.Resolve(v)) // '😀' again
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		f, err := os.Open("testdata/smile.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		v := NewNamedVariable("Char")

		state := State{
			streams: map[Term]*Stream{
				NewAtom("foo"): {sourceSink: f, mode: ioModeRead},
			},
		}
		ok, err := state.PeekChar(NewAtom("foo"), v, func(env *Env) *Promise {
			assert.Equal(t, NewAtom("😀"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("eof", func(t *testing.T) {
		f, err := os.Open("testdata/empty.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead}

		v := NewNamedVariable("Char")

		var state State
		ok, err := state.PeekChar(s, v, func(env *Env) *Promise {
			assert.Equal(t, atomEndOfFile, env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("error", func(t *testing.T) {
		var m mockReader
		m.On("Read", mock.Anything).Return(0, errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		v := NewNamedVariable("V")

		var state State
		ok, err := state.PeekChar(&Stream{sourceSink: &m, mode: ioModeRead}, v, Success, nil).Force(context.Background())
		assert.Equal(t, SystemError(errors.New("failed")), err)
		assert.False(t, ok)
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
			ok, err := state.PeekChar(&Stream{sourceSink: os.Stdin}, Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeInCharacter, Integer(0), nil), err)
			assert.False(t, ok)
		})

		t.Run("atom", func(t *testing.T) {
			var state State
			ok, err := state.PeekChar(&Stream{sourceSink: os.Stdin}, NewAtom("ab"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeInCharacter, NewAtom("ab"), nil), err)
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
			Bind(streamOrAlias, &Stream{sourceSink: os.Stdout, mode: ioModeAppend})

		var state State
		ok, err := state.PeekChar(streamOrAlias, NewNamedVariable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypeStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		s := &Stream{sourceSink: os.Stdin}
		s.streamType = streamTypeBinary

		streamOrAlias := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, s)

		var state State
		ok, err := state.PeekChar(streamOrAlias, NewNamedVariable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypeBinaryStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		f, err := os.Open("testdata/empty.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{
			sourceSink: f,
			eofAction:  eofActionError,
		}

		streamOrAlias := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, s)

		var state State
		ok, err := state.PeekChar(streamOrAlias, NewNamedVariable("Char"), Success, env).Force(context.Background())
		assert.Equal(t, PermissionError(OperationInput, PermissionTypePastEndOfStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("the entity input from the stream is not a character", func(t *testing.T) {
		f, err := os.Open("testdata/replacement.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		streamOrAlias := NewNamedVariable("Stream")
		env := NewEnv().
			Bind(streamOrAlias, &Stream{sourceSink: f, mode: ioModeRead})

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
		ok, err := Halt(NewAtom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, NewAtom("foo"), nil), err)
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
					{Name: NewAtom("green"), Arity: 1}: &userDefined{public: true, clauses: []clause{
						{raw: &compound{
							functor: atomIf, args: []Term{
								&compound{functor: NewAtom("green"), args: []Term{x}},
								&compound{functor: NewAtom("moldy"), args: []Term{x}},
							},
						}},
						{raw: &compound{functor: NewAtom("green"), args: []Term{NewAtom("kermit")}}},
					}},
				},
			},
		}
		ok, err := state.Clause(&compound{
			functor: NewAtom("green"),
			args:    []Term{what},
		}, body, func(env *Env) *Promise {
			switch c {
			case 0:
				assert.True(t, env.Resolve(what).(Variable).Anonymous())
				b, ok := env.Resolve(body).(*compound)
				assert.True(t, ok)
				assert.Equal(t, NewAtom("moldy"), b.functor)
				assert.Len(t, b.args, 1)
				assert.True(t, b.args[0].(Variable).Anonymous())
			case 1:
				assert.Equal(t, NewAtom("kermit"), env.Resolve(what))
				assert.Equal(t, atomTrue, env.Resolve(body))
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
		ok, err := state.Clause(NewAtom("foo"), atomTrue, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("head is a variable", func(t *testing.T) {
		var state State
		ok, err := state.Clause(NewNamedVariable("Head"), atomTrue, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("head is neither a variable nor a predication", func(t *testing.T) {
		var state State
		ok, err := state.Clause(Integer(0), atomTrue, Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("the predicate indicator Pred of Head is that of a private (ie. Not public) procedure", func(t *testing.T) {
		what, body := NewNamedVariable("What"), NewNamedVariable("Body")

		state := State{
			VM: VM{
				procedures: map[ProcedureIndicator]procedure{
					{Name: NewAtom("green"), Arity: 1}: predicate1(func(t Term, f func(*Env) *Promise, env *Env) *Promise {
						return Bool(true)
					}),
				},
			},
		}
		ok, err := state.Clause(&compound{
			functor: NewAtom("green"),
			args:    []Term{what},
		}, body, Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationAccess, PermissionTypePrivateProcedure, &compound{
			functor: atomSlash,
			args:    []Term{NewAtom("green"), Integer(1)},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("body is neither a variable nor a callable term", func(t *testing.T) {
		var state State
		ok, err := state.Clause(NewAtom("foo"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})
}

func TestAtomLength(t *testing.T) {
	t.Run("ascii", func(t *testing.T) {
		ok, err := AtomLength(NewAtom("abc"), Integer(3), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("emoji", func(t *testing.T) {
		ok, err := AtomLength(NewAtom("😀"), Integer(1), Success, nil).Force(context.Background())
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
		ok, err := AtomLength(NewAtom("😀"), NewAtom("1"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, NewAtom("1"), nil), err)
		assert.False(t, ok)
	})

	t.Run("length is an integer less than zero", func(t *testing.T) {
		ok, err := AtomLength(NewAtom("😀"), Integer(-1), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainNotLessThanZero, Integer(-1), nil), err)
		assert.False(t, ok)
	})
}

func TestAtomConcat(t *testing.T) {
	t.Run("atom3 is a variable", func(t *testing.T) {
		atom3 := NewNamedVariable("Atom3")

		ok, err := AtomConcat(NewAtom("foo"), NewAtom("bar"), atom3, func(env *Env) *Promise {
			assert.Equal(t, NewAtom("foobar"), env.Resolve(atom3))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("atom3 is an atom", func(t *testing.T) {
		var c int
		v1, v2 := NewNamedVariable("V1"), NewNamedVariable("V2")
		ok, err := AtomConcat(v1, v2, NewAtom("foo"), func(env *Env) *Promise {
			switch c {
			case 0:
				assert.Equal(t, NewAtom(""), env.Resolve(v1))
				assert.Equal(t, NewAtom("foo"), env.Resolve(v2))
			case 1:
				assert.Equal(t, NewAtom("f"), env.Resolve(v1))
				assert.Equal(t, NewAtom("oo"), env.Resolve(v2))
			case 2:
				assert.Equal(t, NewAtom("fo"), env.Resolve(v1))
				assert.Equal(t, NewAtom("o"), env.Resolve(v2))
			case 3:
				assert.Equal(t, NewAtom("foo"), env.Resolve(v1))
				assert.Equal(t, NewAtom(""), env.Resolve(v2))
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

		ok, err := AtomConcat(atom1, NewAtom("bar"), atom3, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("atom2 and atom3 are variables", func(t *testing.T) {
		atom2, atom3 := NewNamedVariable("Atom2"), NewNamedVariable("Atom3")

		ok, err := AtomConcat(NewAtom("foo"), atom2, atom3, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("atom1 is neither a variable nor an atom", func(t *testing.T) {
		t.Run("atom3 is a variable", func(t *testing.T) {
			ok, err := AtomConcat(Integer(1), NewAtom("bar"), NewNamedVariable("Atom3"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeAtom, Integer(1), nil), err)
			assert.False(t, ok)
		})

		t.Run("atom3 is an atom", func(t *testing.T) {
			ok, err := AtomConcat(Integer(1), NewAtom("bar"), NewAtom("foobar"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeAtom, Integer(1), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("atom2 is neither a variable nor an atom", func(t *testing.T) {
		t.Run("atom3 is a variable", func(t *testing.T) {
			ok, err := AtomConcat(NewAtom("foo"), Integer(2), NewNamedVariable("Atom3"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeAtom, Integer(2), nil), err)
			assert.False(t, ok)
		})

		t.Run("atom3 is an atom", func(t *testing.T) {
			ok, err := AtomConcat(NewAtom("foo"), Integer(2), NewAtom("foobar"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeAtom, Integer(2), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("atom3 is neither a variable nor an atom", func(t *testing.T) {
		ok, err := AtomConcat(NewAtom("foo"), NewAtom("bar"), Integer(3), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeAtom, Integer(3), nil), err)
		assert.False(t, ok)
	})
}

func TestSubAtom(t *testing.T) {
	t.Run("multiple solutions", func(t *testing.T) {
		before, length, after := NewNamedVariable("Before"), NewNamedVariable("Length"), NewNamedVariable("After")
		var c int
		ok, err := SubAtom(NewAtom("xATGATGAxATGAxATGAx"), before, length, after, NewAtom("ATGA"), func(env *Env) *Promise {
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
		ok, err := SubAtom(NewAtom("a"), Integer(0), Integer(1), Integer(0), char, func(env *Env) *Promise {
			assert.Equal(t, NewAtom("a"), env.Resolve(char))
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
		ok, err := SubAtom(NewAtom("foo"), NewNamedVariable("Before"), NewNamedVariable("Length"), NewNamedVariable("After"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("before is neither a variable nor an integer", func(t *testing.T) {
		ok, err := SubAtom(NewAtom("foo"), NewAtom("before"), NewNamedVariable("Length"), NewNamedVariable("After"), NewNamedVariable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, NewAtom("before"), nil), err)
		assert.False(t, ok)
	})

	t.Run("length is neither a variable nor an integer", func(t *testing.T) {
		ok, err := SubAtom(NewAtom("foo"), NewNamedVariable("Before"), NewAtom("length"), NewNamedVariable("After"), NewNamedVariable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, NewAtom("length"), nil), err)
		assert.False(t, ok)
	})

	t.Run("after is neither a variable nor an integer", func(t *testing.T) {
		ok, err := SubAtom(NewAtom("foo"), NewNamedVariable("Before"), NewNamedVariable("Length"), NewAtom("after"), NewNamedVariable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, NewAtom("after"), nil), err)
		assert.False(t, ok)
	})

	t.Run("before is an integer less than zero", func(t *testing.T) {
		ok, err := SubAtom(NewAtom("foo"), Integer(-1), NewNamedVariable("Length"), NewNamedVariable("After"), NewNamedVariable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainNotLessThanZero, Integer(-1), nil), err)
		assert.False(t, ok)
	})

	t.Run("length is an integer less than zero", func(t *testing.T) {
		ok, err := SubAtom(NewAtom("foo"), NewNamedVariable("Before"), Integer(-1), NewNamedVariable("After"), NewNamedVariable("SubAtom"), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainNotLessThanZero, Integer(-1), nil), err)
		assert.False(t, ok)
	})

	t.Run("after is an integer less than zero", func(t *testing.T) {
		ok, err := SubAtom(NewAtom("foo"), NewNamedVariable("Before"), NewNamedVariable("Length"), Integer(-1), NewNamedVariable("SubAtom"), Success, nil).Force(context.Background())
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
		{title: "atom_chars('', L).", atom: NewAtom(""), list: NewNamedVariable("L"), ok: true, env: map[Variable]Term{
			l: List(),
		}},
		{title: "atom_chars([], L).", atom: atomEmptyList, list: NewNamedVariable("L"), ok: true, env: map[Variable]Term{
			l: List(NewAtom("["), NewAtom("]")),
		}},
		{title: "atom_chars('''', L).", atom: NewAtom("'"), list: NewNamedVariable("L"), ok: true, env: map[Variable]Term{
			l: List(NewAtom("'")),
		}},
		{title: "atom_chars('ant', L).", atom: NewAtom("ant"), list: NewNamedVariable("L"), ok: true, env: map[Variable]Term{
			l: List(NewAtom("a"), NewAtom("n"), NewAtom("t")),
		}},
		{title: "atom_chars(Str, ['s', 'o', 'p']).", atom: NewNamedVariable("Str"), list: List(NewAtom("s"), NewAtom("o"), NewAtom("p")), ok: true, env: map[Variable]Term{
			str: NewAtom("sop"),
		}},
		{title: "atom_chars('North', ['N' | X]).", atom: NewAtom("North"), list: ListRest(NewNamedVariable("X"), NewAtom("N")), ok: true, env: map[Variable]Term{
			x: List(NewAtom("o"), NewAtom("r"), NewAtom("t"), NewAtom("h")),
		}},
		{title: "atom_chars('soap', ['s', 'o', 'p']).", atom: NewAtom("soap"), list: List(NewAtom("s"), NewAtom("o"), NewAtom("p")), ok: false},
		{title: "atom_chars(X, Y).", atom: NewNamedVariable("X"), list: NewNamedVariable("Y"), err: InstantiationError(nil)},

		// 8.16.4.3 Errors
		{title: "a", atom: NewNamedVariable("X"), list: ListRest(NewNamedVariable("Y"), NewAtom("a")), err: InstantiationError(nil)},
		{title: "b", atom: Integer(0), list: List(NewAtom("a"), NewAtom("b"), NewAtom("c")), err: TypeError(ValidTypeAtom, Integer(0), nil)},
		{title: "c: atom is a variable", atom: NewNamedVariable("X"), list: Integer(0), err: TypeError(ValidTypeList, Integer(0), nil)},
		{title: "c: atom is an atom", atom: NewAtom("a"), list: Integer(0), err: TypeError(ValidTypeList, Integer(0), nil)},
		{title: "d", atom: NewNamedVariable("X"), list: List(NewNamedVariable("Y"), NewAtom("a")), err: InstantiationError(nil)},
		{title: "e: atom is a variable, more than one char", atom: NewNamedVariable("X"), list: List(NewAtom("abc")), err: TypeError(ValidTypeCharacter, NewAtom("abc"), nil)},
		{title: "e: atom is a variable, not an atom", atom: NewNamedVariable("X"), list: List(Integer(0)), err: TypeError(ValidTypeCharacter, Integer(0), nil)},
		{title: "e: atom is an atom, more than one char", atom: NewAtom("abc"), list: List(NewAtom("ab"), NewAtom("c")), err: TypeError(ValidTypeCharacter, NewAtom("ab"), nil)},
		{title: "e: atom is an atom, not an atom", atom: NewAtom("abc"), list: List(Integer('a'), NewAtom("b"), NewAtom("c")), err: TypeError(ValidTypeCharacter, Integer('a'), nil)},

		{title: "atom_chars('ant', ['a', X, 't']).", atom: NewAtom("ant"), list: List(NewAtom("a"), NewNamedVariable("X"), NewAtom("t")), ok: true, env: map[Variable]Term{
			x: NewAtom("n"),
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
		{title: "atom_codes('', L).", atom: NewAtom(""), list: NewNamedVariable("L"), ok: true, env: map[Variable]Term{
			l: List(),
		}},
		{title: "atom_codes([], L).", atom: atomEmptyList, list: NewNamedVariable("L"), ok: true, env: map[Variable]Term{
			l: List(Integer('['), Integer(']')),
		}},
		{title: "atom_codes('''', L).", atom: NewAtom("'"), list: NewNamedVariable("L"), ok: true, env: map[Variable]Term{
			l: List(Integer('\'')),
		}},
		{title: "atom_codes('ant', L).", atom: NewAtom("ant"), list: NewNamedVariable("L"), ok: true, env: map[Variable]Term{
			l: List(Integer('a'), Integer('n'), Integer('t')),
		}},
		{title: "atom_codes(Str, [0's, 0'o, 0'p]).", atom: NewNamedVariable("Str"), list: List(Integer('s'), Integer('o'), Integer('p')), ok: true, env: map[Variable]Term{
			str: NewAtom("sop"),
		}},
		{title: "atom_codes('North', [0'N | X]).", atom: NewAtom("North"), list: ListRest(NewNamedVariable("X"), Integer('N')), ok: true, env: map[Variable]Term{
			x: List(Integer('o'), Integer('r'), Integer('t'), Integer('h')),
		}},
		{title: "atom_codes('soap', [0's, 0'o, 0'p]).", atom: NewAtom("soap"), list: List(Integer('s'), Integer('o'), Integer('p')), ok: false},
		{title: "atom_codes(X, Y).", atom: NewNamedVariable("X"), list: NewNamedVariable("Y"), err: InstantiationError(nil)},

		// 8.16.5.3 Errors
		{title: "a", atom: NewNamedVariable("X"), list: ListRest(NewNamedVariable("Y"), Integer(0)), err: InstantiationError(nil)},
		{title: "b", atom: Integer(0), list: NewNamedVariable("L"), err: TypeError(ValidTypeAtom, Integer(0), nil)},
		{title: "c: atom is a variable", atom: NewNamedVariable("X"), list: Integer(0), err: TypeError(ValidTypeList, Integer(0), nil)},
		{title: "c: atom is an atom", atom: NewAtom("abc"), list: Integer(0), err: TypeError(ValidTypeList, Integer(0), nil)},
		{title: "d", atom: NewNamedVariable("X"), list: List(NewNamedVariable("Y"), Integer('b'), Integer('c')), err: InstantiationError(nil)},
		{title: "e: atom is a variable", atom: NewNamedVariable("X"), list: List(NewAtom("a"), Integer('b'), Integer('c')), err: TypeError(ValidTypeInteger, NewAtom("a"), nil)},
		{title: "e: atom is an atom", atom: NewAtom("abc"), list: List(NewAtom("a"), Integer('b'), Integer('c')), err: TypeError(ValidTypeInteger, NewAtom("a"), nil)},
		{title: "f: atom is a variable", atom: NewNamedVariable("X"), list: List(Integer(-1), Integer('b'), Integer('c')), err: RepresentationError(FlagCharacterCode, nil)},
		{title: "f: atom is an atom", atom: NewAtom("abc"), list: List(Integer(-1), Integer('b'), Integer('c')), err: RepresentationError(FlagCharacterCode, nil)},

		{title: "atom_codes('ant', [0'a, X, 0't]).", atom: NewAtom("ant"), list: List(Integer('a'), NewNamedVariable("X"), Integer('t')), ok: true, env: map[Variable]Term{
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
				assert.Equal(t, List(NewAtom("2"), NewAtom("3"), atomDot, NewAtom("4")), env.Resolve(chars))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("chars is a list with variables", func(t *testing.T) {
			char := NewNamedVariable("Char")

			ok, err := NumberChars(Float(23.4), List(char, NewAtom("3"), atomDot, NewAtom("4")), func(env *Env) *Promise {
				assert.Equal(t, NewAtom("2"), env.Resolve(char))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("chars to number", func(t *testing.T) {
		num := NewNamedVariable("Num")

		ok, err := NumberChars(num, List(NewAtom("2"), NewAtom("3"), atomDot, NewAtom("4")), func(env *Env) *Promise {
			assert.Equal(t, Float(23.4), env.Resolve(num))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("both provided", func(t *testing.T) {
		t.Run("3.3", func(t *testing.T) {
			ok, err := NumberChars(Float(3.3), List(NewAtom("3"), atomDot, NewAtom("3")), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("3.3E+0", func(t *testing.T) {
			ok, err := NumberChars(Float(3.3), List(NewAtom("3"), atomDot, NewAtom("3"), NewAtom("E"), atomPlus, NewAtom("0")), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("num is a variable and chars is a partial list", func(t *testing.T) {
		chars := ListRest(NewNamedVariable("Rest"),
			NewAtom("2"), NewAtom("3"), atomDot, NewAtom("4"),
		)

		ok, err := NumberChars(NewVariable(), chars, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("num is neither a variable nor a number", func(t *testing.T) {
		t.Run("chars is a list of one-char atoms", func(t *testing.T) {
			ok, err := NumberChars(NewAtom("23.4"), List(NewAtom("2"), NewAtom("3"), atomDot, NewAtom("4")), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeNumber, NewAtom("23.4"), nil), err)
			assert.False(t, ok)
		})

		t.Run("chars is not a list of one-char atoms", func(t *testing.T) {
			ok, err := NumberChars(NewAtom("23.4"), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeNumber, NewAtom("23.4"), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("chars is neither a partial list nor a list", func(t *testing.T) {
		t.Run("not even list-ish", func(t *testing.T) {
			ok, err := NumberChars(NewVariable(), NewAtom("foo"), Success, nil).Force(context.Background())
			assert.Equal(t, TypeError(ValidTypeList, NewAtom("foo"), nil), err)
			assert.False(t, ok)
		})

		t.Run("list-ish", func(t *testing.T) {
			_, err := NumberChars(Integer(0), ListRest(NewAtom("b"), NewNamedVariable("A")), Success, nil).Force(context.Background())
			_, ok := NewEnv().Unify(err.(Exception).Term(), TypeError(ValidTypeList, ListRest(NewAtom("b"), NewVariable()), nil).Term(), false)
			assert.True(t, ok)
		})
	})

	t.Run("num is a variable and an element of a list prefix of chars is a variable", func(t *testing.T) {
		ok, err := NumberChars(NewVariable(), List(NewAtom("1"), NewVariable()), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("chars is a list of one-char atoms but is not parsable as a number", func(t *testing.T) {
		t.Run("not a number", func(t *testing.T) {
			ok, err := NumberChars(NewVariable(), List(NewAtom("f"), NewAtom("o"), NewAtom("o")), Success, nil).Force(context.Background())
			assert.Equal(t, SyntaxError(errNotANumber, nil), err)
			assert.False(t, ok)
		})

		t.Run("unexpected token", func(t *testing.T) {
			ok, err := NumberChars(NewVariable(), List(NewAtom("1"), atomDot), Success, nil).Force(context.Background())
			assert.Equal(t, SyntaxError(errNotANumber, nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("an element E of a list prefix of chars is neither a variable nor a one-char atom", func(t *testing.T) {
		t.Run("chars contains a variable", func(t *testing.T) {
			t.Run("not even an atom", func(t *testing.T) {
				ok, err := NumberChars(Integer(100), List(NewVariable(), NewAtom("0"), Integer(0)), Success, nil).Force(context.Background())
				assert.Equal(t, TypeError(ValidTypeCharacter, Integer(0), nil), err)
				assert.False(t, ok)
			})

			t.Run("atom", func(t *testing.T) {
				ok, err := NumberChars(Integer(100), List(NewVariable(), NewAtom("00")), Success, nil).Force(context.Background())
				assert.Equal(t, TypeError(ValidTypeCharacter, NewAtom("00"), nil), err)
				assert.False(t, ok)
			})
		})

		t.Run("chars does not contain a variable", func(t *testing.T) {
			t.Run("not even an atom", func(t *testing.T) {
				ok, err := NumberChars(Integer(100), List(NewAtom("1"), NewAtom("0"), Integer(0)), Success, nil).Force(context.Background())
				assert.Equal(t, TypeError(ValidTypeCharacter, Integer(0), nil), err)
				assert.False(t, ok)
			})

			t.Run("atom", func(t *testing.T) {
				ok, err := NumberChars(Integer(100), List(NewAtom("1"), NewAtom("00")), Success, nil).Force(context.Background())
				assert.Equal(t, TypeError(ValidTypeCharacter, NewAtom("00"), nil), err)
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
		ok, err := NumberCodes(NewAtom("23.4"), List(Integer(50), Integer(51), Integer(46), Integer(52)), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeNumber, NewAtom("23.4"), nil), err)
		assert.False(t, ok)
	})

	t.Run("num is a variable and codes is neither a list nor partial list", func(t *testing.T) {
		ok, err := NumberCodes(NewVariable(), NewAtom("23.4"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeList, NewAtom("23.4"), nil), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the list codes is neither a variable nor a one-character atom", func(t *testing.T) {
		ok, err := NumberCodes(NewVariable(), List(NewAtom("2"), Integer(51), Integer(46), Integer(52)), Success, nil).Force(context.Background())
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
	f, err := os.Open("testdata/empty.txt")
	assert.NoError(t, err)
	defer func() {
		assert.NoError(t, f.Close())
	}()

	t.Run("stream", func(t *testing.T) {
		expected := []Term{
			atomFileName.Apply(NewAtom(f.Name())),
			atomMode.Apply(atomRead),
			atomInput,
			atomAlias.Apply(NewAtom("null")),
			atomPosition.Apply(Integer(0)),
			atomEndOfStream.Apply(atomNot),
			atomEOFAction.Apply(atomEOFCode),
			atomReposition.Apply(atomTrue),
			atomType.Apply(atomText),
		}

		s := &Stream{sourceSink: f, mode: ioModeRead, alias: NewAtom("null"), reposition: true}

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
			atomFileName.Apply(NewAtom(f.Name())),
			atomMode.Apply(atomRead),
			atomInput,
			atomAlias.Apply(NewAtom("null")),
			atomPosition.Apply(Integer(0)),
			atomEndOfStream.Apply(atomNot),
			atomEOFAction.Apply(atomEOFCode),
			atomReposition.Apply(atomFalse),
			atomType.Apply(atomText),
		}

		s := &Stream{sourceSink: f, mode: ioModeRead, reposition: false, alias: NewAtom("null")}

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
			atomFileName.Apply(NewAtom(f.Name())),
			atomMode.Apply(atomWrite),
			atomOutput,
			atomAlias.Apply(NewAtom("null")),
			atomPosition.Apply(Integer(0)),
			atomEndOfStream.Apply(atomNot),
			atomEOFAction.Apply(atomEOFCode),
			atomReposition.Apply(atomFalse),
			atomType.Apply(atomText),
		}

		state := State{
			streams: map[Term]*Stream{
				NewAtom("null"): {sourceSink: f, mode: ioModeWrite, alias: NewAtom("null")},
			},
		}
		v := NewNamedVariable("V")
		c := 0
		ok, err := state.StreamProperty(NewAtom("null"), v, func(env *Env) *Promise {
			assert.Equal(t, expected[c], env.Resolve(v))
			c++
			return Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("correct property value", func(t *testing.T) {
		t.Run("input", func(t *testing.T) {
			s := &Stream{sourceSink: f, mode: ioModeRead}

			var state State
			ok, err := state.StreamProperty(s, atomInput, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("mode", func(t *testing.T) {
			s := &Stream{sourceSink: f, mode: ioModeRead}

			var state State
			ok, err := state.StreamProperty(s, &compound{
				functor: atomMode,
				args:    []Term{atomRead},
			}, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("position", func(t *testing.T) {
			s := &Stream{sourceSink: f, mode: ioModeRead}

			var state State
			ok, err := state.StreamProperty(s, &compound{
				functor: atomPosition,
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
		ok, err := state.StreamProperty(NewVariable(), NewAtom("property"), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainStreamProperty, NewAtom("property"), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var state State
		ok, err := state.StreamProperty(NewAtom("foo"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, ExistenceError(ObjectTypeStream, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("unknown atom", func(t *testing.T) {
		s := &Stream{sourceSink: f, mode: ioModeRead}

		var state State
		ok, err := state.StreamProperty(s, NewAtom("foo"), Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})

	t.Run("unknown compound", func(t *testing.T) {
		s := &Stream{sourceSink: f, mode: ioModeRead}

		var state State
		ok, err := state.StreamProperty(s, &compound{functor: NewAtom("foo"), args: []Term{NewVariable()}}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})

	t.Run("wrong arity", func(t *testing.T) {
		s := &Stream{sourceSink: f, mode: ioModeRead}

		var state State
		ok, err := state.StreamProperty(s, &compound{functor: atomMode, args: []Term{NewVariable(), NewVariable()}}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})

	t.Run("integer", func(t *testing.T) {
		s := &Stream{sourceSink: f, mode: ioModeRead}

		var state State
		ok, err := state.StreamProperty(s, Integer(0), Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})

	t.Run("non-atom for atom property", func(t *testing.T) {
		s := &Stream{sourceSink: f, mode: ioModeRead}

		var state State
		ok, err := state.StreamProperty(s, &compound{functor: atomMode, args: []Term{Integer(0)}}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})

	t.Run("non-integer for integer property", func(t *testing.T) {
		s := &Stream{sourceSink: f, mode: ioModeRead}

		var state State
		ok, err := state.StreamProperty(s, &compound{functor: atomPosition, args: []Term{NewAtom("foo")}}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestState_SetStreamPosition(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		f, err := os.Open("testdata/empty.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead, reposition: true}

		var state State
		ok, err := state.SetStreamPosition(s, Integer(0), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("seek failed", func(t *testing.T) {
		var m mockFile
		m.On("Seek", mock.Anything, mock.Anything).Return(int64(0), errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m, mode: ioModeRead, reposition: true}

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
		f, err := os.Open("testdata/empty.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead, reposition: true}

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
		ok, err := state.SetStreamPosition(NewAtom("foo"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, ExistenceError(ObjectTypeStream, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream property reposition(false)", func(t *testing.T) {
		stream := &Stream{sourceSink: os.Stdin}

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
		ok, err := state.CharConversion(NewAtom("a"), NewAtom("b"), Success, nil).Force(context.Background())
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
		ok, err := state.CharConversion(NewAtom("a"), NewAtom("a"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		_, ok = state.charConversions['a']
		assert.False(t, ok)
	})

	t.Run("inChar is a variable", func(t *testing.T) {
		var state State
		ok, err := state.CharConversion(NewNamedVariable("In"), NewAtom("a"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("outChar is a variable", func(t *testing.T) {
		var state State
		ok, err := state.CharConversion(NewAtom("a"), NewNamedVariable("Out"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("inChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var state State
			ok, err := state.CharConversion(Integer(0), NewAtom("a"), Success, nil).Force(context.Background())
			assert.Equal(t, RepresentationError(FlagCharacter, nil), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var state State
			ok, err := state.CharConversion(NewAtom("foo"), NewAtom("a"), Success, nil).Force(context.Background())
			assert.Equal(t, RepresentationError(FlagCharacter, nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("outChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var state State
			ok, err := state.CharConversion(NewAtom("a"), Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, RepresentationError(FlagCharacter, nil), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var state State
			ok, err := state.CharConversion(NewAtom("a"), NewAtom("foo"), Success, nil).Force(context.Background())
			assert.Equal(t, RepresentationError(FlagCharacter, nil), err)
			assert.False(t, ok)
		})
	})
}

func TestState_CurrentCharConversion(t *testing.T) {
	t.Run("specified", func(t *testing.T) {
		t.Run("as is", func(t *testing.T) {
			var state State
			ok, err := state.CurrentCharConversion(NewAtom("a"), NewAtom("a"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("converted", func(t *testing.T) {
			state := State{
				charConversions: map[rune]rune{
					'a': 'b',
				},
			}
			ok, err := state.CurrentCharConversion(NewAtom("a"), NewAtom("b"), Success, nil).Force(context.Background())
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
			assert.Len(t, []rune(x.String()), 1)

			ref, ok = env.Lookup(y)
			assert.True(t, ok)
			y, ok := ref.(Atom)
			assert.True(t, ok)
			assert.Len(t, []rune(y.String()), 1)

			assert.Equal(t, r, []rune(x.String())[0])
			assert.Equal(t, r, []rune(y.String())[0])
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
			ok, err := state.CurrentCharConversion(Integer(0), NewAtom("b"), Success, nil).Force(context.Background())
			assert.Equal(t, RepresentationError(FlagCharacter, nil), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var state State
			ok, err := state.CurrentCharConversion(NewAtom("foo"), NewAtom("b"), Success, nil).Force(context.Background())
			assert.Equal(t, RepresentationError(FlagCharacter, nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("outChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var state State
			ok, err := state.CurrentCharConversion(NewAtom("a"), Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, RepresentationError(FlagCharacter, nil), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var state State
			ok, err := state.CurrentCharConversion(NewAtom("a"), NewAtom("bar"), Success, nil).Force(context.Background())
			assert.Equal(t, RepresentationError(FlagCharacter, nil), err)
			assert.False(t, ok)
		})
	})
}

func TestState_SetPrologFlag(t *testing.T) {
	t.Run("bounded", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(atomBounded, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationModify, PermissionTypeFlag, atomBounded, nil), err)
		assert.False(t, ok)
	})

	t.Run("max_integer", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(atomMaxInteger, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationModify, PermissionTypeFlag, atomMaxInteger, nil), err)
		assert.False(t, ok)
	})

	t.Run("min_integer", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(atomMinInteger, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationModify, PermissionTypeFlag, atomMinInteger, nil), err)
		assert.False(t, ok)
	})

	t.Run("integer_rounding_function", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(atomIntegerRoundingFunction, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationModify, PermissionTypeFlag, atomIntegerRoundingFunction, nil), err)
		assert.False(t, ok)
	})

	t.Run("char_conversion", func(t *testing.T) {
		t.Run("on", func(t *testing.T) {
			var state State
			ok, err := state.SetPrologFlag(atomCharConversion, atomOn, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.True(t, state.charConvEnabled)
		})

		t.Run("off", func(t *testing.T) {
			state := State{charConvEnabled: true}
			ok, err := state.SetPrologFlag(atomCharConversion, atomOff, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.False(t, state.charConvEnabled)
		})

		t.Run("unknown", func(t *testing.T) {
			state := State{charConvEnabled: true}
			ok, err := state.SetPrologFlag(atomCharConversion, NewAtom("foo"), Success, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("debug", func(t *testing.T) {
		t.Run("on", func(t *testing.T) {
			var state State
			ok, err := state.SetPrologFlag(atomDebug, atomOn, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.True(t, state.debug)
		})

		t.Run("off", func(t *testing.T) {
			state := State{debug: true}
			ok, err := state.SetPrologFlag(atomDebug, atomOff, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.False(t, state.debug)
		})

		t.Run("unknown", func(t *testing.T) {
			state := State{debug: true}
			ok, err := state.SetPrologFlag(atomDebug, NewAtom("foo"), Success, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("max_arity", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(atomMaxArity, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationModify, PermissionTypeFlag, atomMaxArity, nil), err)
		assert.False(t, ok)
	})

	t.Run("unknown", func(t *testing.T) {
		t.Run("error", func(t *testing.T) {
			state := State{VM: VM{unknown: unknownFail}}
			ok, err := state.SetPrologFlag(atomUnknown, atomError, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, unknownError, state.unknown)
		})

		t.Run("warning", func(t *testing.T) {
			var state State
			ok, err := state.SetPrologFlag(atomUnknown, atomWarning, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, unknownWarning, state.unknown)
		})

		t.Run("fail", func(t *testing.T) {
			var state State
			ok, err := state.SetPrologFlag(atomUnknown, atomFail, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, unknownFail, state.unknown)
		})

		t.Run("fail", func(t *testing.T) {
			var state State
			ok, err := state.SetPrologFlag(atomUnknown, NewAtom("foo"), Success, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("double_quotes", func(t *testing.T) {
		t.Run("codes", func(t *testing.T) {
			var state State
			ok, err := state.SetPrologFlag(atomDoubleQuotes, atomCodes, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, doubleQuotesCodes, state.doubleQuotes)
		})

		t.Run("chars", func(t *testing.T) {
			var state State
			ok, err := state.SetPrologFlag(atomDoubleQuotes, atomChars, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, doubleQuotesChars, state.doubleQuotes)
		})

		t.Run("atom", func(t *testing.T) {
			var state State
			ok, err := state.SetPrologFlag(atomDoubleQuotes, atomAtom, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, doubleQuotesAtom, state.doubleQuotes)
		})

		t.Run("unknown", func(t *testing.T) {
			var state State
			ok, err := state.SetPrologFlag(atomDoubleQuotes, NewAtom("foo"), Success, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("flag is a variable", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(NewNamedVariable("Flag"), atomFail, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("value is a variable", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(atomUnknown, NewNamedVariable("Value"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("flag is neither a variable nor an atom", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(Integer(0), atomFail, Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("flag is an atom but an invalid flag for the processor", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(NewAtom("foo"), atomFail, Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainPrologFlag, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("value is inadmissible for flag", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(atomUnknown, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainFlagValue, &compound{
			functor: atomPlus,
			args:    []Term{atomUnknown, Integer(0)},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("value is admissible for flag but the flag is not modifiable", func(t *testing.T) {
		var state State
		ok, err := state.SetPrologFlag(atomBounded, atomTrue, Success, nil).Force(context.Background())
		assert.Equal(t, PermissionError(OperationModify, PermissionTypeFlag, atomBounded, nil), err)
		assert.False(t, ok)
	})
}

func TestState_CurrentPrologFlag(t *testing.T) {
	var state State

	t.Run("specified", func(t *testing.T) {
		ok, err := state.CurrentPrologFlag(atomBounded, atomTrue, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.CurrentPrologFlag(atomMaxInteger, Integer(math.MaxInt64), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.CurrentPrologFlag(atomMinInteger, Integer(math.MinInt64), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.CurrentPrologFlag(atomIntegerRoundingFunction, atomTowardZero, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.CurrentPrologFlag(atomCharConversion, atomOff, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.CurrentPrologFlag(atomDebug, atomOff, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.CurrentPrologFlag(atomMaxArity, atomUnbounded, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = state.CurrentPrologFlag(atomUnknown, atomError, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not specified", func(t *testing.T) {
		flag, value := NewNamedVariable("Flag"), NewNamedVariable("Value")
		var c int
		ok, err := state.CurrentPrologFlag(flag, value, func(env *Env) *Promise {
			switch c {
			case 0:
				assert.Equal(t, atomBounded, env.Resolve(flag))
				assert.Equal(t, atomTrue, env.Resolve(value))
			case 1:
				assert.Equal(t, atomMaxInteger, env.Resolve(flag))
				assert.Equal(t, Integer(math.MaxInt64), env.Resolve(value))
			case 2:
				assert.Equal(t, atomMinInteger, env.Resolve(flag))
				assert.Equal(t, Integer(math.MinInt64), env.Resolve(value))
			case 3:
				assert.Equal(t, atomIntegerRoundingFunction, env.Resolve(flag))
				assert.Equal(t, atomTowardZero, env.Resolve(value))
			case 4:
				assert.Equal(t, atomCharConversion, env.Resolve(flag))
				assert.Equal(t, atomOff, env.Resolve(value))
			case 5:
				assert.Equal(t, atomDebug, env.Resolve(flag))
				assert.Equal(t, atomOff, env.Resolve(value))
			case 6:
				assert.Equal(t, atomMaxArity, env.Resolve(flag))
				assert.Equal(t, atomUnbounded, env.Resolve(value))
			case 7:
				assert.Equal(t, atomUnknown, env.Resolve(flag))
				assert.Equal(t, NewAtom(state.unknown.String()), env.Resolve(value))
			case 8:
				assert.Equal(t, atomDoubleQuotes, env.Resolve(flag))
				assert.Equal(t, NewAtom(state.doubleQuotes.String()), env.Resolve(value))
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
		ok, err := state.CurrentPrologFlag(Integer(0), atomError, Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("flag is an atom but an invalid flag for the processor", func(t *testing.T) {
		var state State
		ok, err := state.CurrentPrologFlag(NewAtom("foo"), atomError, Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainPrologFlag, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})
}

func TestState_ExpandTerm(t *testing.T) {
	t.Run("term_expansion/2 is undefined", func(t *testing.T) {
		var state State
		ok, err := state.ExpandTerm(&compound{
			functor: NewAtom("f"),
			args:    []Term{NewAtom("a")},
		}, &compound{
			functor: NewAtom("f"),
			args:    []Term{NewAtom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("term_expansion/2 is defined", func(t *testing.T) {
		t.Run("not applicable", func(t *testing.T) {
			state := State{
				VM: VM{
					procedures: map[ProcedureIndicator]procedure{
						{Name: atomTermExpansion, Arity: 2}: predicate2(func(Term, Term, func(*Env) *Promise, *Env) *Promise {
							return Bool(false)
						}),
					},
				},
			}
			ok, err := state.ExpandTerm(&compound{
				functor: NewAtom("f"),
				args:    []Term{NewAtom("a")},
			}, &compound{
				functor: NewAtom("f"),
				args:    []Term{NewAtom("a")},
			}, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("throws an exception", func(t *testing.T) {
			state := State{
				VM: VM{
					procedures: map[ProcedureIndicator]procedure{
						{Name: atomTermExpansion, Arity: 2}: predicate2(func(Term, Term, func(*Env) *Promise, *Env) *Promise {
							return Error(errors.New("failed"))
						}),
					},
				},
			}
			ok, err := state.ExpandTerm(&compound{
				functor: NewAtom("f"),
				args:    []Term{NewAtom("a")},
			}, &compound{
				functor: NewAtom("f"),
				args:    []Term{NewAtom("a")},
			}, Success, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})

		t.Run("applicable", func(t *testing.T) {
			state := State{
				VM: VM{
					procedures: map[ProcedureIndicator]procedure{
						{Name: atomTermExpansion, Arity: 2}: predicate2(func(t1, t2 Term, k func(*Env) *Promise, env *Env) *Promise {
							return Unify(t2, &compound{
								functor: NewAtom("g"),
								args:    []Term{NewAtom("b")},
							}, k, env)
						}),
					},
				},
			}
			ok, err := state.ExpandTerm(&compound{
				functor: NewAtom("f"),
				args:    []Term{NewAtom("a")},
			}, &compound{
				functor: NewAtom("g"),
				args:    []Term{NewAtom("b")},
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
			assert.Equal(t, NewAtom("FOO"), env.Resolve(key))
			assert.Equal(t, NewAtom("foo"), env.Resolve(value))
		case 2:
			assert.Equal(t, NewAtom("BAR"), env.Resolve(key))
			assert.Equal(t, NewAtom("bar"), env.Resolve(value))
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
			term, err := state.expand(&compound{functor: atomArrow, args: []Term{NewAtom("s"), List()}}, nil)
			assert.NoError(t, err)
			assert.Equal(t, &compound{functor: atomIf, args: []Term{
				&compound{functor: NewAtom("s"), args: []Term{Variable(offset + 1), Variable(offset + 3)}},
				&compound{functor: atomEqual, args: []Term{Variable(offset + 1), Variable(offset + 3)}},
			}}, term)
		})

		t.Run("terminal sequence", func(t *testing.T) {
			offset := varCounter
			term, err := state.expand(&compound{functor: atomArrow, args: []Term{NewAtom("s"), List(NewAtom("a"))}}, nil)
			assert.NoError(t, err)
			assert.Equal(t, &compound{functor: atomIf, args: []Term{
				&compound{functor: NewAtom("s"), args: []Term{Variable(offset + 1), Variable(offset + 3)}},
				&compound{functor: atomEqual, args: []Term{Variable(offset + 1), ListRest(Variable(offset+3), NewAtom("a"))}},
			}}, term)
		})

		t.Run("concatenation", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				offset := varCounter
				term, err := state.expand(&compound{functor: atomArrow, args: []Term{NewAtom("s"), Seq(atomComma, NewAtom("a"), NewAtom("b"))}}, nil)
				assert.NoError(t, err)
				assert.Equal(t, &compound{functor: atomIf, args: []Term{
					&compound{functor: NewAtom("s"), args: []Term{Variable(offset + 1), Variable(offset + 3)}},
					Seq(atomComma,
						&compound{functor: NewAtom("a"), args: []Term{Variable(offset + 1), Variable(offset + 4)}},
						&compound{functor: NewAtom("b"), args: []Term{Variable(offset + 4), Variable(offset + 3)}},
					),
				}}, term)
			})

			t.Run("lhs is not callable", func(t *testing.T) {
				_, err := state.expand(&compound{functor: atomArrow, args: []Term{NewAtom("s"), Seq(atomComma, Integer(0), NewAtom("b"))}}, nil)
				assert.Error(t, err)
			})

			t.Run("rhs is not callable", func(t *testing.T) {
				_, err := state.expand(&compound{functor: atomArrow, args: []Term{NewAtom("s"), Seq(atomComma, NewAtom("a"), Integer(0))}}, nil)
				assert.Error(t, err)
			})
		})

		t.Run("alternative", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				t.Run("normal", func(t *testing.T) {
					offset := varCounter
					term, err := state.expand(&compound{functor: atomArrow, args: []Term{NewAtom("s"), Seq(atomSemiColon, NewAtom("a"), NewAtom("b"))}}, nil)
					assert.NoError(t, err)
					assert.Equal(t, &compound{functor: atomIf, args: []Term{
						&compound{functor: NewAtom("s"), args: []Term{Variable(offset + 1), Variable(offset + 3)}},
						Seq(atomSemiColon,
							&compound{functor: NewAtom("a"), args: []Term{Variable(offset + 1), Variable(offset + 3)}},
							&compound{functor: NewAtom("b"), args: []Term{Variable(offset + 1), Variable(offset + 3)}},
						),
					}}, term)
				})

				t.Run("if-then-else", func(t *testing.T) {
					offset := varCounter
					term, err := state.expand(&compound{functor: atomArrow, args: []Term{NewAtom("s"), Seq(atomSemiColon, &compound{functor: atomThen, args: []Term{NewAtom("a"), NewAtom("b")}}, NewAtom("c"))}}, nil)
					assert.NoError(t, err)
					assert.Equal(t, &compound{functor: atomIf, args: []Term{
						&compound{functor: NewAtom("s"), args: []Term{Variable(offset + 1), Variable(offset + 3)}},
						Seq(atomSemiColon,
							&compound{functor: atomThen, args: []Term{
								&compound{functor: NewAtom("a"), args: []Term{Variable(offset + 1), Variable(offset + 4)}},
								&compound{functor: NewAtom("b"), args: []Term{Variable(offset + 4), Variable(offset + 3)}},
							}},
							&compound{functor: NewAtom("c"), args: []Term{Variable(offset + 1), Variable(offset + 3)}},
						),
					}}, term)
				})
			})

			t.Run("lhs is not callable", func(t *testing.T) {
				_, err := state.expand(&compound{functor: atomArrow, args: []Term{NewAtom("s"), Seq(atomSemiColon, Integer(0), NewAtom("b"))}}, nil)
				assert.Error(t, err)
			})

			t.Run("rhs is not callable", func(t *testing.T) {
				_, err := state.expand(&compound{functor: atomArrow, args: []Term{NewAtom("s"), Seq(atomSemiColon, NewAtom("a"), Integer(0))}}, nil)
				assert.Error(t, err)
			})
		})

		t.Run("second form of alternative", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				offset := varCounter
				term, err := state.expand(&compound{functor: atomArrow, args: []Term{NewAtom("s"), Seq(atomBar, NewAtom("a"), NewAtom("b"))}}, nil)
				assert.NoError(t, err)
				assert.Equal(t, &compound{functor: atomIf, args: []Term{
					&compound{functor: NewAtom("s"), args: []Term{Variable(offset + 1), Variable(offset + 3)}},
					Seq(atomSemiColon,
						&compound{functor: NewAtom("a"), args: []Term{Variable(offset + 1), Variable(offset + 3)}},
						&compound{functor: NewAtom("b"), args: []Term{Variable(offset + 1), Variable(offset + 3)}},
					),
				}}, term)
			})

			t.Run("lhs is not callable", func(t *testing.T) {
				_, err := state.expand(&compound{functor: atomArrow, args: []Term{NewAtom("s"), Seq(atomBar, Integer(0), NewAtom("b"))}}, nil)
				assert.Error(t, err)
			})

			t.Run("rhs is not callable", func(t *testing.T) {
				_, err := state.expand(&compound{functor: atomArrow, args: []Term{NewAtom("s"), Seq(atomBar, NewAtom("a"), Integer(0))}}, nil)
				assert.Error(t, err)
			})
		})

		t.Run("grammar-body-goal", func(t *testing.T) {
			offset := varCounter
			term, err := state.expand(&compound{functor: atomArrow, args: []Term{NewAtom("s"), &compound{functor: atomEmptyBlock, args: []Term{NewAtom("a")}}}}, nil)
			assert.NoError(t, err)
			assert.Equal(t, &compound{functor: atomIf, args: []Term{
				&compound{functor: NewAtom("s"), args: []Term{Variable(offset + 1), Variable(offset + 3)}},
				Seq(atomComma,
					NewAtom("a"),
					&compound{functor: atomEqual, args: []Term{Variable(offset + 1), Variable(offset + 3)}},
				),
			}}, term)
		})

		t.Run("call", func(t *testing.T) {
			offset := varCounter
			term, err := state.expand(&compound{functor: atomArrow, args: []Term{NewAtom("s"), &compound{functor: atomCall, args: []Term{NewAtom("a")}}}}, nil)
			assert.NoError(t, err)
			assert.Equal(t, &compound{functor: atomIf, args: []Term{
				&compound{functor: NewAtom("s"), args: []Term{Variable(offset + 1), Variable(offset + 3)}},
				&compound{functor: atomCall, args: []Term{NewAtom("a"), Variable(offset + 1), Variable(offset + 3)}},
			}}, term)
		})

		t.Run("phrase", func(t *testing.T) {
			offset := varCounter
			term, err := state.expand(&compound{functor: atomArrow, args: []Term{NewAtom("s"), &compound{functor: atomPhrase, args: []Term{NewAtom("a")}}}}, nil)
			assert.NoError(t, err)
			assert.Equal(t, &compound{functor: atomIf, args: []Term{
				&compound{functor: NewAtom("s"), args: []Term{Variable(offset + 1), Variable(offset + 3)}},
				&compound{functor: atomPhrase, args: []Term{NewAtom("a"), Variable(offset + 1), Variable(offset + 3)}},
			}}, term)
		})

		t.Run("grammar-body-cut", func(t *testing.T) {
			offset := varCounter
			term, err := state.expand(&compound{functor: atomArrow, args: []Term{NewAtom("s"), atomCut}}, nil)
			assert.NoError(t, err)
			assert.Equal(t, &compound{functor: atomIf, args: []Term{
				&compound{functor: NewAtom("s"), args: []Term{Variable(offset + 1), Variable(offset + 3)}},
				Seq(atomComma,
					atomCut,
					&compound{functor: atomEqual, args: []Term{Variable(offset + 1), Variable(offset + 3)}},
				),
			}}, term)
		})

		t.Run("grammar-body-not", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				offset := varCounter
				term, err := state.expand(&compound{functor: atomArrow, args: []Term{NewAtom("s"), &compound{functor: atomNegation, args: []Term{NewAtom("a")}}}}, nil)
				assert.NoError(t, err)
				assert.Equal(t, &compound{functor: atomIf, args: []Term{
					&compound{functor: NewAtom("s"), args: []Term{Variable(offset + 1), Variable(offset + 3)}},
					Seq(atomComma,
						&compound{functor: atomNegation, args: []Term{&compound{functor: NewAtom("a"), args: []Term{Variable(offset + 1), Variable(offset + 4)}}}},
						&compound{functor: atomEqual, args: []Term{Variable(offset + 1), Variable(offset + 3)}},
					),
				}}, term)
			})

			t.Run("goal is not callable", func(t *testing.T) {
				_, err := state.expand(&compound{functor: atomArrow, args: []Term{NewAtom("s"), &compound{functor: atomNegation, args: []Term{Integer(0)}}}}, nil)
				assert.Error(t, err)
			})
		})

		t.Run("if-then", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				offset := varCounter
				term, err := state.expand(&compound{functor: atomArrow, args: []Term{NewAtom("s"), &compound{functor: atomThen, args: []Term{NewAtom("a"), NewAtom("b")}}}}, nil)
				assert.NoError(t, err)
				assert.Equal(t, &compound{functor: atomIf, args: []Term{
					&compound{functor: NewAtom("s"), args: []Term{Variable(offset + 1), Variable(offset + 3)}},
					&compound{
						functor: atomThen,
						args: []Term{
							&compound{functor: NewAtom("a"), args: []Term{Variable(offset + 1), Variable(offset + 4)}},
							&compound{functor: NewAtom("b"), args: []Term{Variable(offset + 4), Variable(offset + 3)}},
						},
					},
				}}, term)
			})

			t.Run("lhs is not callable", func(t *testing.T) {
				_, err := state.expand(&compound{functor: atomArrow, args: []Term{NewAtom("s"), &compound{functor: atomThen, args: []Term{Integer(0), NewAtom("b")}}}}, nil)
				assert.Error(t, err)
			})

			t.Run("rhs is not callable", func(t *testing.T) {
				_, err := state.expand(&compound{functor: atomArrow, args: []Term{NewAtom("s"), &compound{functor: atomThen, args: []Term{NewAtom("a"), Integer(0)}}}}, nil)
				assert.Error(t, err)
			})
		})

		t.Run("with semicontexts", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				offset := varCounter
				term, err := state.expand(atomArrow.Apply(atomComma.Apply(NewAtom("phrase1"), List(NewAtom("word"))), atomComma.Apply(NewAtom("phrase2"), NewAtom("phrase3"))), nil)
				assert.NoError(t, err)
				assert.Equal(t, &compound{
					functor: atomIf,
					args: []Term{
						&compound{functor: NewAtom("phrase1"), args: []Term{Variable(offset + 1), Variable(offset + 3)}},
						&compound{
							functor: atomComma,
							args: []Term{
								&compound{
									functor: atomComma,
									args: []Term{
										&compound{functor: NewAtom("phrase2"), args: []Term{Variable(offset + 1), Variable(offset + 4)}},
										&compound{functor: NewAtom("phrase3"), args: []Term{Variable(offset + 4), Variable(offset + 2)}},
									},
								},
								&compound{functor: atomEqual, args: []Term{Variable(offset + 3), ListRest(Variable(offset+2), NewAtom("word"))}},
							},
						},
					},
				}, term)
			})

			t.Run("head is not callable", func(t *testing.T) {
				_, err := state.expand(atomArrow.Apply(atomComma.Apply(Integer(0), List(NewAtom("word"))), atomComma.Apply(NewAtom("phrase2"), NewAtom("phrase3"))), nil)
				assert.Error(t, err)
			})

			t.Run("semicontext is not callable", func(t *testing.T) {
				_, err := state.expand(atomArrow.Apply(atomComma.Apply(NewAtom("phrase1"), Integer(0)), atomComma.Apply(NewAtom("phrase2"), NewAtom("phrase3"))), nil)
				assert.Error(t, err)
			})

			t.Run("body is not callable", func(t *testing.T) {
				_, err := state.expand(atomArrow.Apply(atomComma.Apply(NewAtom("phrase1"), List(NewAtom("word"))), atomComma.Apply(Integer(0), NewAtom("phrase3"))), nil)
				assert.Error(t, err)
			})
		})
	})
}

func TestNth0(t *testing.T) {
	t.Run("n is a variable", func(t *testing.T) {
		t.Run("list is a proper list", func(t *testing.T) {
			pair := atomMinus
			var (
				n       = NewNamedVariable("N")
				elem    = NewNamedVariable("Elem")
				results []Term
			)
			ok, err := Nth0(n, List(NewAtom("a"), NewAtom("b"), NewAtom("c")), elem, func(env *Env) *Promise {
				results = append(results, pair.Apply(env.Resolve(n), env.Resolve(elem)))
				return Bool(false)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)

			assert.Equal(t, []Term{
				pair.Apply(Integer(0), NewAtom("a")),
				pair.Apply(Integer(1), NewAtom("b")),
				pair.Apply(Integer(2), NewAtom("c")),
			}, results)
		})

		t.Run("list is an improper list", func(t *testing.T) {
			_, err := Nth0(NewNamedVariable("N"), ListRest(NewNamedVariable("X"), NewAtom("a")), NewNamedVariable("Elem"), Failure, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
		})
	})

	t.Run("n is an integer", func(t *testing.T) {
		t.Run("list is a proper list", func(t *testing.T) {
			t.Run("n is a valid index", func(t *testing.T) {
				ok, err := Nth0(Integer(1), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewAtom("b"), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("n is too small for an index", func(t *testing.T) {
				ok, err := Nth0(Integer(-1), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewVariable(), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.False(t, ok)
			})

			t.Run("n is too big for an index", func(t *testing.T) {
				ok, err := Nth0(Integer(3), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewVariable(), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.False(t, ok)
			})
		})

		t.Run("list is an improper list", func(t *testing.T) {
			_, err := Nth0(Integer(1), ListRest(NewVariable(), NewAtom("a")), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
		})
	})

	t.Run("n is neither a variable nor an integer", func(t *testing.T) {
		_, err := Nth0(NewAtom("foo"), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, NewAtom("foo"), nil), err)
	})
}

func TestNth1(t *testing.T) {
	t.Run("n is a variable", func(t *testing.T) {
		t.Run("list is a proper list", func(t *testing.T) {
			pair := atomMinus
			var (
				n       = NewNamedVariable("N")
				elem    = NewNamedVariable("Elem")
				results []Term
			)
			ok, err := Nth1(n, List(NewAtom("a"), NewAtom("b"), NewAtom("c")), elem, func(env *Env) *Promise {
				results = append(results, pair.Apply(env.Resolve(n), env.Resolve(elem)))
				return Bool(false)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)

			assert.Equal(t, []Term{
				pair.Apply(Integer(1), NewAtom("a")),
				pair.Apply(Integer(2), NewAtom("b")),
				pair.Apply(Integer(3), NewAtom("c")),
			}, results)
		})

		t.Run("list is an improper list", func(t *testing.T) {
			_, err := Nth1(NewNamedVariable("N"), ListRest(NewNamedVariable("X"), NewAtom("a")), NewNamedVariable("Elem"), Failure, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
		})
	})

	t.Run("n is an integer", func(t *testing.T) {
		t.Run("list is a proper list", func(t *testing.T) {
			t.Run("n is a valid index", func(t *testing.T) {
				ok, err := Nth1(Integer(2), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewAtom("b"), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("n is too small for an index", func(t *testing.T) {
				ok, err := Nth1(Integer(0), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewVariable(), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.False(t, ok)
			})

			t.Run("n is too big for an index", func(t *testing.T) {
				ok, err := Nth1(Integer(4), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewVariable(), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.False(t, ok)
			})
		})

		t.Run("list is an improper list", func(t *testing.T) {
			_, err := Nth1(Integer(2), ListRest(NewVariable(), NewAtom("a")), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
		})
	})

	t.Run("n is neither a variable nor an integer", func(t *testing.T) {
		_, err := Nth1(NewAtom("foo"), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, NewAtom("foo"), nil), err)
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
			ok, err := Length(List(NewAtom("a"), NewAtom("b"), NewAtom("c")), n, func(env *Env) *Promise {
				assert.Equal(t, Integer(3), env.Resolve(n))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("length is an integer", func(t *testing.T) {
			t.Run("length is the exact length of list", func(t *testing.T) {
				ok, err := Length(List(NewAtom("a"), NewAtom("b"), NewAtom("c")), Integer(3), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("length is smaller than the length fo list", func(t *testing.T) {
				ok, err := Length(List(NewAtom("a"), NewAtom("b"), NewAtom("c")), Integer(2), Success, nil).Force(context.Background())
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
				ok, err := Length(ListRest(l, NewAtom("a"), NewAtom("b")), n, func(env *Env) *Promise {
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
				_, err := Length(ListRest(l, NewAtom("a"), NewAtom("b")), l, Success, nil).Force(context.Background())
				assert.Equal(t, ResourceError(ResourceFiniteMemory, nil), err)
			})
		})

		t.Run("length is an integer", func(t *testing.T) {
			l := NewNamedVariable("L")
			ok, err := Length(ListRest(l, NewAtom("a"), NewAtom("b")), Integer(3), func(env *Env) *Promise {
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
			ok, err := Length(NewAtom("foo"), Integer(3), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("the suffix is a compound", func(t *testing.T) {
			ok, err := Length(NewAtom("foo").Apply(NewAtom("bar")), Integer(3), Success, nil).Force(context.Background())
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
		_, err := Length(List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewAtom("three"), Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, NewAtom("three"), nil), err)
	})

	t.Run("length is an integer that is less than zero", func(t *testing.T) {
		_, err := Length(List(NewAtom("a"), NewAtom("b"), NewAtom("c")), Integer(-3), Success, nil).Force(context.Background())
		assert.Equal(t, DomainError(ValidDomainNotLessThanZero, Integer(-3), nil), err)
	})

	t.Run("list is so long that an integer cannot represent its length", func(t *testing.T) {
		maxInt = 2
		defer func() {
			maxInt = math.MaxInt64
		}()

		t.Run("list is a list", func(t *testing.T) {
			_, err := Length(List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewVariable(), Success, nil).Force(context.Background())
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
			ok, err := SkipMaxList(Integer(3), NewVariable(), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), atomEmptyList, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("with max", func(t *testing.T) {
			ok, err := SkipMaxList(Integer(2), Integer(2), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), List(NewAtom("c")), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("max is neither a variable nor an integer", func(t *testing.T) {
		_, err := SkipMaxList(Integer(3), NewAtom("foo"), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), atomEmptyList, Success, nil).Force(context.Background())
		assert.Equal(t, TypeError(ValidTypeInteger, NewAtom("foo"), nil), err)
	})

	t.Run("max is negative", func(t *testing.T) {
		_, err := SkipMaxList(Integer(3), Integer(-1), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), atomEmptyList, Success, nil).Force(context.Background())
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

	ok, err := s.Negation(atomTrue, Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.False(t, ok)

	ok, err = s.Negation(atomFalse, Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	_, err = s.Negation(atomError, Success, nil).Force(context.Background())
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
		{title: `append([a,b],[c,d], Xs).`, xs: List(NewAtom("a"), NewAtom("b")), ys: List(NewAtom("c"), NewAtom("d")), zs: NewNamedVariable("Xs"), ok: true, env: []map[Variable]Term{
			{xs: List(NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"))},
		}},
		{title: `append([a], nonlist, Xs).`, xs: List(NewAtom("a")), ys: NewAtom("nonlist"), zs: NewNamedVariable("Xs"), ok: true, env: []map[Variable]Term{
			{xs: ListRest(NewAtom("nonlist"), NewAtom("a"))},
		}},
		{title: `append([a], Ys, Zs).`, xs: List(NewAtom("a")), ys: NewNamedVariable("Ys"), zs: NewNamedVariable("Zs"), ok: true, env: []map[Variable]Term{
			{zs: ListRest(NewNamedVariable("Ys"), NewAtom("a"))},
		}},
		{title: `append(Xs, Ys, [a,b,c]).`, xs: NewNamedVariable("Xs"), ys: NewNamedVariable("Ys"), zs: List(NewAtom("a"), NewAtom("b"), NewAtom("c")), ok: true, env: []map[Variable]Term{
			{xs: List(), ys: List(NewAtom("a"), NewAtom("b"), NewAtom("c"))},
			{xs: List(NewAtom("a")), ys: List(NewAtom("b"), NewAtom("c"))},
			{xs: List(NewAtom("a"), NewAtom("b")), ys: List(NewAtom("c"))},
			{xs: List(NewAtom("a"), NewAtom("b"), NewAtom("c")), ys: List()},
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

type mockWriter struct {
	mock.Mock
}

func (m *mockWriter) Write(p []byte) (int, error) {
	args := m.Called(p)
	return args.Int(0), args.Error(1)
}
