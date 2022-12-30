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
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
)

func TestCall(t *testing.T) {
	var vm VM
	vm.Register0(atomFail, func(_ *VM, f Cont, env *Env) *Promise {
		return Bool(false)
	})
	assert.NoError(t, vm.Compile(context.Background(), `
foo.
foo(_, _).
f(g([a, [b|X]])).
`))

	tests := []struct {
		title string
		goal  Term
		ok    bool
		err   error

		mem int64
	}{
		// TODO: redo test cases based on 7.8.3.4 Examples
		{title: `undefined atom`, goal: NewAtom("bar"), ok: false, err: existenceError(objectTypeProcedure, atomSlash.Apply(NewAtom("bar"), Integer(0)), nil)},
		{title: `defined atom`, goal: NewAtom("foo"), ok: true},
		{title: `undefined compound`, goal: NewAtom("bar").Apply(NewVariable(), NewVariable()), ok: false, err: existenceError(objectTypeProcedure, atomSlash.Apply(NewAtom("bar"), Integer(2)), nil)},
		{title: `defined compound`, goal: NewAtom("foo").Apply(NewVariable(), NewVariable()), ok: true},
		{title: `variable: single predicate`, goal: NewVariable(), ok: false, err: instantiationError(nil)},
		{title: `variable: multiple predicates`, goal: atomComma.Apply(atomFail, NewVariable()), ok: false},
		{title: `not callable: single predicate`, goal: Integer(0), ok: false, err: typeError(validTypeCallable, Integer(0), nil)},
		{title: `not callable: conjunction`, goal: atomComma.Apply(atomTrue, Integer(0)), ok: false, err: typeError(validTypeCallable, atomComma.Apply(atomTrue, Integer(0)), nil)},
		{title: `not callable: disjunction`, goal: atomSemiColon.Apply(Integer(1), atomTrue), ok: false, err: typeError(validTypeCallable, atomSemiColon.Apply(Integer(1), atomTrue), nil)},

		{title: `cover all`, goal: atomComma.Apply(atomCut, NewAtom("f").Apply(NewAtom("g").Apply(List(NewAtom("a"), PartialList(NewVariable(), NewAtom("b")))))), ok: true},
		{title: `out of memory`, goal: NewAtom("foo").Apply(NewVariable(), NewVariable()), err: resourceError(resourceMemory, nil), mem: 1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			if tt.mem > 0 {
				orig := memFree
				memFree = func() int64 {
					return tt.mem
				}
				defer func() {
					memFree = orig
				}()
			}
			ok, err := Call(&vm, tt.goal, Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestCall1(t *testing.T) {
	tests := []struct {
		title      string
		closure    Term
		additional [1]Term
		ok         bool
		err        error
		mem        int64
	}{
		{title: "ok", closure: NewAtom("p").Apply(NewAtom("a")), additional: [1]Term{NewAtom("b")}, ok: true},
		{title: "closure is a variable", closure: NewVariable(), additional: [1]Term{NewAtom("b")}, err: instantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [1]Term{NewAtom("b")}, err: typeError(validTypeCallable, Integer(3), nil)},
		{title: "out of memory", closure: NewAtom("p").Apply(NewAtom("a")), additional: [1]Term{NewAtom("b")}, err: resourceError(resourceMemory, nil), mem: 1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			if tt.mem > 0 {
				orig := memFree
				memFree = func() int64 {
					return tt.mem
				}
				defer func() {
					memFree = orig
				}()
			}

			vm := VM{procedures: map[procedureIndicator]procedure{
				{name: NewAtom("p"), arity: 2}: Predicate2(func(_ *VM, _, _ Term, k Cont, env *Env) *Promise {
					return k(env)
				}),
			}}
			ok, err := Call1(&vm, tt.closure, tt.additional[0], Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestCall2(t *testing.T) {
	tests := []struct {
		title      string
		closure    Term
		additional [2]Term
		ok         bool
		err        error
		mem        int64
	}{
		{title: "ok", closure: NewAtom("p").Apply(NewAtom("a")), additional: [2]Term{NewAtom("b"), NewAtom("c")}, ok: true},
		{title: "closure is a variable", closure: NewVariable(), additional: [2]Term{NewAtom("b"), NewAtom("c")}, err: instantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [2]Term{NewAtom("b"), NewAtom("c")}, err: typeError(validTypeCallable, Integer(3), nil)},
		{title: "out of memory", closure: NewAtom("p").Apply(NewAtom("a")), additional: [2]Term{NewAtom("b"), NewAtom("c")}, err: resourceError(resourceMemory, nil), mem: 1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			if tt.mem > 0 {
				orig := memFree
				memFree = func() int64 {
					return tt.mem
				}
				defer func() {
					memFree = orig
				}()
			}

			vm := VM{procedures: map[procedureIndicator]procedure{
				{name: NewAtom("p"), arity: 3}: Predicate3(func(_ *VM, _, _, _ Term, k Cont, env *Env) *Promise {
					return k(env)
				}),
			}}
			ok, err := Call2(&vm, tt.closure, tt.additional[0], tt.additional[1], Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestCall3(t *testing.T) {
	tests := []struct {
		title      string
		closure    Term
		additional [3]Term
		ok         bool
		err        error
		mem        int64
	}{
		{title: "ok", closure: NewAtom("p").Apply(NewAtom("a")), additional: [3]Term{NewAtom("b"), NewAtom("c"), NewAtom("d")}, ok: true},
		{title: "closure is a variable", closure: NewVariable(), additional: [3]Term{NewAtom("b"), NewAtom("c"), NewAtom("d")}, err: instantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [3]Term{NewAtom("b"), NewAtom("c"), NewAtom("d")}, err: typeError(validTypeCallable, Integer(3), nil)},
		{title: "out of memory", closure: NewAtom("p").Apply(NewAtom("a")), additional: [3]Term{NewAtom("b"), NewAtom("c"), NewAtom("d")}, err: resourceError(resourceMemory, nil), mem: 1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			if tt.mem > 0 {
				orig := memFree
				memFree = func() int64 {
					return tt.mem
				}
				defer func() {
					memFree = orig
				}()
			}

			vm := VM{procedures: map[procedureIndicator]procedure{
				{name: NewAtom("p"), arity: 4}: Predicate4(func(_ *VM, _, _, _, _ Term, k Cont, env *Env) *Promise {
					return k(env)
				}),
			}}
			ok, err := Call3(&vm, tt.closure, tt.additional[0], tt.additional[1], tt.additional[2], Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestCall4(t *testing.T) {
	tests := []struct {
		title      string
		closure    Term
		additional [4]Term
		ok         bool
		err        error
		mem        int64
	}{
		{title: "ok", closure: NewAtom("p").Apply(NewAtom("a")), additional: [4]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e")}, ok: true},
		{title: "closure is a variable", closure: NewVariable(), additional: [4]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e")}, err: instantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [4]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e")}, err: typeError(validTypeCallable, Integer(3), nil)},
		{title: "out of memory", closure: NewAtom("p").Apply(NewAtom("a")), additional: [4]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e")}, err: resourceError(resourceMemory, nil), mem: 1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			if tt.mem > 0 {
				orig := memFree
				memFree = func() int64 {
					return tt.mem
				}
				defer func() {
					memFree = orig
				}()
			}

			vm := VM{procedures: map[procedureIndicator]procedure{
				{name: NewAtom("p"), arity: 5}: Predicate5(func(_ *VM, _, _, _, _, _ Term, k Cont, env *Env) *Promise {
					return k(env)
				}),
			}}
			ok, err := Call4(&vm, tt.closure, tt.additional[0], tt.additional[1], tt.additional[2], tt.additional[3], Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestCall5(t *testing.T) {
	tests := []struct {
		title      string
		closure    Term
		additional [5]Term
		ok         bool
		err        error
		mem        int64
	}{
		{title: "ok", closure: NewAtom("p").Apply(NewAtom("a")), additional: [5]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f")}, ok: true},
		{title: "closure is a variable", closure: NewVariable(), additional: [5]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f")}, err: instantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [5]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f")}, err: typeError(validTypeCallable, Integer(3), nil)},
		{title: "out of memory", closure: NewAtom("p").Apply(NewAtom("a")), additional: [5]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f")}, err: resourceError(resourceMemory, nil), mem: 1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			if tt.mem > 0 {
				orig := memFree
				memFree = func() int64 {
					return tt.mem
				}
				defer func() {
					memFree = orig
				}()
			}

			vm := VM{procedures: map[procedureIndicator]procedure{
				{name: NewAtom("p"), arity: 6}: Predicate6(func(_ *VM, _, _, _, _, _, _ Term, k Cont, env *Env) *Promise {
					return k(env)
				}),
			}}
			ok, err := Call5(&vm, tt.closure, tt.additional[0], tt.additional[1], tt.additional[2], tt.additional[3], tt.additional[4], Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestCall6(t *testing.T) {
	tests := []struct {
		title      string
		closure    Term
		additional [6]Term
		ok         bool
		err        error
		mem        int64
	}{
		{title: "ok", closure: NewAtom("p").Apply(NewAtom("a")), additional: [6]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g")}, ok: true},
		{title: "closure is a variable", closure: NewVariable(), additional: [6]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g")}, err: instantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [6]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g")}, err: typeError(validTypeCallable, Integer(3), nil)},
		{title: "out of memory", closure: NewAtom("p").Apply(NewAtom("a")), additional: [6]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g")}, err: resourceError(resourceMemory, nil), mem: 1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			if tt.mem > 0 {
				orig := memFree
				memFree = func() int64 {
					return tt.mem
				}
				defer func() {
					memFree = orig
				}()
			}

			vm := VM{procedures: map[procedureIndicator]procedure{
				{name: NewAtom("p"), arity: 7}: Predicate7(func(_ *VM, _, _, _, _, _, _, _ Term, k Cont, env *Env) *Promise {
					return k(env)
				}),
			}}
			ok, err := Call6(&vm, tt.closure, tt.additional[0], tt.additional[1], tt.additional[2], tt.additional[3], tt.additional[4], tt.additional[5], Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestCall7(t *testing.T) {
	tests := []struct {
		title      string
		closure    Term
		additional [7]Term
		ok         bool
		err        error
		mem        int64
	}{
		{title: "ok", closure: NewAtom("p").Apply(NewAtom("a")), additional: [7]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g"), NewAtom("h")}, ok: true},
		{title: "closure is a variable", closure: NewVariable(), additional: [7]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g"), NewAtom("h")}, err: instantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [7]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g"), NewAtom("h")}, err: typeError(validTypeCallable, Integer(3), nil)},
		{title: "out of memory", closure: NewAtom("p").Apply(NewAtom("a")), additional: [7]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g"), NewAtom("h")}, err: resourceError(resourceMemory, nil), mem: 1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			if tt.mem > 0 {
				orig := memFree
				memFree = func() int64 {
					return tt.mem
				}
				defer func() {
					memFree = orig
				}()
			}

			vm := VM{procedures: map[procedureIndicator]procedure{
				{name: NewAtom("p"), arity: 8}: Predicate8(func(_ *VM, _, _, _, _, _, _, _, _ Term, k Cont, env *Env) *Promise {
					return k(env)
				}),
			}}
			ok, err := Call7(&vm, tt.closure, tt.additional[0], tt.additional[1], tt.additional[2], tt.additional[3], tt.additional[4], tt.additional[5], tt.additional[6], Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestCallNth(t *testing.T) {
	vm := VM{
		procedures: map[procedureIndicator]procedure{
			{name: NewAtom("foo"), arity: 0}: Predicate0(func(_ *VM, k Cont, env *Env) *Promise {
				return Delay(func(context.Context) *Promise {
					return k(env)
				}, func(context.Context) *Promise {
					return k(env)
				}, func(context.Context) *Promise {
					return Error(errors.New("three"))
				})
			}),
		},
	}

	t.Run("ok", func(t *testing.T) {
		t.Run("nth is a variable", func(t *testing.T) {
			nth := NewVariable()

			var ns []Integer
			ok, err := CallNth(&vm, NewAtom("foo"), nth, func(env *Env) *Promise {
				n, ok := env.Resolve(nth).(Integer)
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
			ok, err := CallNth(&vm, NewAtom("foo"), Integer(2), Failure, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("nth is 0", func(t *testing.T) {
		ok, err := CallNth(&vm, NewAtom("foo"), Integer(0), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("goal is a variable and nth is not zero", func(t *testing.T) {
		_, err := CallNth(&vm, NewVariable(), Integer(3), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
	})

	t.Run("goal is neither a variable nor a callable term", func(t *testing.T) {
		_, err := CallNth(&vm, Integer(0), Integer(3), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeCallable, Integer(0), nil), err)
	})

	t.Run("nth is neither a variable nor an integer", func(t *testing.T) {
		_, err := CallNth(&vm, NewAtom("foo"), NewAtom("bar"), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, NewAtom("bar"), nil), err)
	})

	t.Run("nth is an integer which is less than zero", func(t *testing.T) {
		_, err := CallNth(&vm, NewAtom("foo"), Integer(-1), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainNotLessThanZero, Integer(-1), nil), err)
	})

	t.Run("n+1 is larger than max_integer", func(t *testing.T) {
		maxInt = 0
		defer func() {
			maxInt = math.MaxInt64
		}()
		_, err := CallNth(&vm, NewAtom("foo"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, representationError(flagMaxInteger, nil), err)
	})
}

func TestUnify(t *testing.T) {
	x, y := NewVariable(), NewVariable()
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
		{title: `'='(X, 1).`, x: x, y: Integer(1), ok: true, env: map[Variable]Term{
			x: Integer(1),
		}},
		{title: `'='(X, Y).`, x: x, y: y, ok: true, env: map[Variable]Term{
			x: y,
		}},
		{title: `'='(_, _).`, x: NewVariable(), y: NewVariable(), ok: true},
		{title: `'='(X, Y), '='(X, abc).`, premise: NewEnv().bind(x, y), x: x, y: NewAtom("abc"), ok: true, env: map[Variable]Term{
			x: NewAtom("abc"),
			y: NewAtom("abc"),
		}},
		{title: `'='(f(X, def), f(def, Y)).`, x: NewAtom("f").Apply(x, NewAtom("def")), y: NewAtom("f").Apply(NewAtom("def"), y), ok: true, env: map[Variable]Term{
			x: NewAtom("def"),
			y: NewAtom("def"),
		}},
		{title: `'='(1, 2).`, x: Integer(1), y: Integer(2), ok: false},
		{title: `'='(1, 1.0).`, x: Integer(1), y: Float(1), ok: false},
		{title: `'='(g(X), f(f(X))).`, x: NewAtom("g").Apply(x), y: NewAtom("f").Apply(NewAtom("f").Apply(x)), ok: false},
		{title: `'='(f(X, 1), f(a(X))).`, x: NewAtom("f").Apply(x, Integer(1)), y: NewAtom("f").Apply(NewAtom("a").Apply(x)), ok: false},
		{title: `'='(f(X, Y, X), f(a(X), a(Y), Y, 2)).`, x: NewAtom("f").Apply(x, y, x), y: NewAtom("f").Apply(NewAtom("a").Apply(x), NewAtom("a").Apply(y), y, Integer(2)), ok: false},
		{title: `'='(X, a(X)).`, x: x, y: NewAtom("a").Apply(x), ok: true},
		{title: `'='(f(X, 1), f(a(X), 2)).`, x: NewAtom("f").Apply(x, Integer(1)), y: NewAtom("f").Apply(NewAtom("a").Apply(x), Integer(2)), ok: false},
		{title: `'='(f(1, X, 1), f(2, a(X), 2)).`, x: NewAtom("f").Apply(Integer(1), x, Integer(1)), y: NewAtom("f").Apply(Integer(2), NewAtom("a").Apply(x), Integer(2)), ok: false},
		{title: `'='(f(1, X), f(2, a(X))).`, x: NewAtom("f").Apply(Integer(1), x), y: NewAtom("f").Apply(Integer(2), NewAtom("a").Apply(x)), ok: false},
		// {title: `'='(f(X, Y, X, 1), f(a(X), a(Y), Y, 2)).`, x: NewAtom("f").Apply(x, y, x, Integer(1)), y: NewAtom("f").Apply(NewAtom("a").Apply(x), NewAtom("a").Apply(y), y, Integer(2)), ok: false},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := Unify(nil, tt.x, tt.y, func(env *Env) *Promise {
				for k, v := range tt.env {
					_, ok := env.Unify(k, v)
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
	x, y := NewVariable(), NewVariable()
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
		{title: `unify_with_occurs_check(X, 1).`, x: x, y: Integer(1), ok: true, env: map[Variable]Term{
			x: Integer(1),
		}},
		{title: `unify_with_occurs_check(X, Y).`, x: x, y: y, ok: true, env: map[Variable]Term{
			x: y,
		}},
		{title: `unify_with_occurs_check(_, _).`, x: NewVariable(), y: NewVariable(), ok: true},
		{title: `unify_with_occurs_check(X, Y), unify_with_occurs_check(X, abc).`, premise: NewEnv().bind(x, y), x: x, y: NewAtom("abc"), ok: true, env: map[Variable]Term{
			x: NewAtom("abc"),
			y: NewAtom("abc"),
		}},
		{title: `unify_with_occurs_check(f(X, def), f(def, Y)).`, x: NewAtom("f").Apply(x, NewAtom("def")), y: NewAtom("f").Apply(NewAtom("def"), y), ok: true, env: map[Variable]Term{
			x: NewAtom("def"),
			y: NewAtom("def"),
		}},
		{title: `unify_with_occurs_check(1, 2).`, x: Integer(1), y: Integer(2), ok: false},
		{title: `unify_with_occurs_check(1, 1.0).`, x: Integer(1), y: Float(1), ok: false},
		{title: `unify_with_occurs_check(g(X), f(f(X))).`, x: NewAtom("g").Apply(x), y: NewAtom("f").Apply(NewAtom("f").Apply(x)), ok: false},
		{title: `unify_with_occurs_check(f(X, 1), f(a(X))).`, x: NewAtom("f").Apply(x, Integer(1)), y: NewAtom("f").Apply(NewAtom("a").Apply(x)), ok: false},
		{title: `unify_with_occurs_check(f(X, Y, X), f(a(X), a(Y), Y, 2)).`, x: NewAtom("f").Apply(x, y, x), y: NewAtom("f").Apply(NewAtom("a").Apply(x), NewAtom("a").Apply(y), y, Integer(2)), ok: false},
		{title: `unify_with_occurs_check(X, a(X)).`, x: x, y: NewAtom("a").Apply(x), ok: false},
		{title: `unify_with_occurs_check(f(X, 1), f(a(X), 2)).`, x: NewAtom("f").Apply(x, Integer(1)), y: NewAtom("f").Apply(NewAtom("a").Apply(x), Integer(2)), ok: false},
		{title: `unify_with_occurs_check(f(1, X, 1), f(2, a(X), 2)).`, x: NewAtom("f").Apply(Integer(1), x, Integer(1)), y: NewAtom("f").Apply(Integer(2), NewAtom("a").Apply(x), Integer(2)), ok: false},
		{title: `unify_with_occurs_check(f(1, X), f(2, a(X))).`, x: NewAtom("f").Apply(Integer(1), x), y: NewAtom("f").Apply(Integer(2), NewAtom("a").Apply(x)), ok: false},
		{title: `unify_with_occurs_check(f(X, Y, X, 1), f(a(X), a(Y), Y, 2)).`, x: NewAtom("f").Apply(x, y, x, Integer(1)), y: NewAtom("f").Apply(NewAtom("a").Apply(x), NewAtom("a").Apply(y), y, Integer(2)), ok: false},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := UnifyWithOccursCheck(nil, tt.x, tt.y, func(env *Env) *Promise {
				for k, v := range tt.env {
					_, ok := env.Unify(k, v)
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
		ok, err := SubsumesTerm(nil, NewVariable(), NewAtom("a"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not unifiable", func(t *testing.T) {
		ok, err := SubsumesTerm(nil, NewAtom("a"), NewAtom("b"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("specific-general", func(t *testing.T) {
		ok, err := SubsumesTerm(nil, NewAtom("a"), NewVariable(), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeVar(t *testing.T) {
	t.Run("var", func(t *testing.T) {
		ok, err := TypeVar(nil, NewVariable(), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not var", func(t *testing.T) {
		ok, err := TypeVar(nil, NewAtom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeFloat(t *testing.T) {
	t.Run("float", func(t *testing.T) {
		ok, err := TypeFloat(nil, Float(1.0), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not float", func(t *testing.T) {
		ok, err := TypeFloat(nil, NewAtom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeInteger(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		ok, err := TypeInteger(nil, Integer(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not integer", func(t *testing.T) {
		ok, err := TypeInteger(nil, NewAtom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeAtom(t *testing.T) {
	t.Run("atom", func(t *testing.T) {
		ok, err := TypeAtom(nil, NewAtom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not atom", func(t *testing.T) {
		ok, err := TypeAtom(nil, Integer(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeCompound(t *testing.T) {
	t.Run("compound", func(t *testing.T) {
		ok, err := TypeCompound(nil, &compound{
			functor: NewAtom("foo"),
			args:    []Term{NewAtom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not compound", func(t *testing.T) {
		ok, err := TypeCompound(nil, NewAtom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestAcyclicTerm(t *testing.T) {
	t.Run("atomic", func(t *testing.T) {
		ok, err := AcyclicTerm(nil, NewAtom("a"), Success, nil).Force(context.Background())
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

			ok, err := AcyclicTerm(nil, &c, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})
}

func TestFunctor(t *testing.T) {
	x, y := NewVariable(), NewVariable()
	a, b := NewVariable(), NewVariable()
	n := NewVariable()
	f := NewVariable()

	tests := []struct {
		title             string
		term, name, arity Term
		ok                bool
		err               error
		env               map[Variable]Term
	}{
		// 8.5.1.4 Examples
		{title: `functor(foo(a, b, c), foo, 3).`, term: NewAtom("foo").Apply(NewAtom("a"), NewAtom("b"), NewAtom("c")), name: NewAtom("foo"), arity: Integer(3), ok: true},
		{title: `functor(foo(a, b, c), X, Y).`, term: NewAtom("foo").Apply(NewAtom("a"), NewAtom("b"), NewAtom("c")), name: x, arity: y, ok: true, env: map[Variable]Term{
			x: NewAtom("foo"),
			y: Integer(3),
		}},
		{title: `functor(X, foo, 3).`, term: x, name: NewAtom("foo"), arity: Integer(3), ok: true, env: map[Variable]Term{
			x: NewAtom("foo").Apply(NewVariable(), NewVariable(), NewVariable()),
		}},
		{title: `functor(X, foo, 0).`, term: x, name: NewAtom("foo"), arity: Integer(0), ok: true, env: map[Variable]Term{
			x: NewAtom("foo"),
		}},
		{title: `functor(mats(A, B), A, B).`, term: NewAtom("mats").Apply(a, b), name: a, arity: b, ok: true, env: map[Variable]Term{
			a: NewAtom("mats"),
			b: Integer(2),
		}},
		{title: `functor(foo(a), foo, 2).`, term: NewAtom("foo").Apply(NewAtom("a")), name: NewAtom("foo"), arity: Integer(2), ok: false},
		{title: `functor(foo(a), fo, 1).`, term: NewAtom("foo").Apply(NewAtom("a")), name: NewAtom("fo"), arity: Integer(1), ok: false},
		{title: `functor(1, X, Y).`, term: Integer(1), name: x, arity: y, ok: true, env: map[Variable]Term{
			x: Integer(1),
			y: Integer(0),
		}},
		{title: `functor(X, 1.1, 0).`, term: x, name: Float(1.1), arity: Integer(0), ok: true, env: map[Variable]Term{
			x: Float(1.1),
		}},
		{title: `functor([_|_], '.', 2).`, term: Cons(NewVariable(), NewVariable()), name: atomDot, arity: Integer(2), ok: true},
		{title: `functor([], [], 0).`, term: atomEmptyList, name: atomEmptyList, arity: Integer(0), ok: true},
		{title: `functor(X, Y, 3).`, term: x, name: y, arity: Integer(3), err: instantiationError(nil)},
		{title: `functor(X, foo, N).`, term: x, name: NewAtom("foo"), arity: n, err: instantiationError(nil)},
		{title: `functor(X, foo, a).`, term: x, name: NewAtom("foo"), arity: NewAtom("a"), err: typeError(validTypeInteger, NewAtom("a"), nil)},
		{title: `functor(F, 1.5, 1).`, term: f, name: Float(1.5), arity: Integer(1), err: typeError(validTypeAtom, Float(1.5), nil)},
		{title: `functor(F, foo(a), 1).`, term: f, name: NewAtom("foo").Apply(NewAtom("a")), arity: Integer(1), err: typeError(validTypeAtomic, NewAtom("foo").Apply(NewAtom("a")), nil)},
		// {title: `current_prolog_flag(max_arity, A), X is A + 1, functor(T, foo, X).`}
		{title: `Minus_1 is 0 - 1, functor(F, foo, Minus_1).`, term: f, name: NewAtom("foo"), arity: Integer(-1), err: domainError(validDomainNotLessThanZero, Integer(-1), nil)},

		// https://github.com/ichiban/prolog/issues/247
		{title: `functor(X, Y, 0).`, term: x, name: y, arity: Integer(0), err: instantiationError(nil)},

		// https://github.com/ichiban/prolog/issues/226
		{title: `functor(F, f, max_int).`, term: f, name: NewAtom("f"), arity: maxInt, err: resourceError(resourceMemory, nil)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := Functor(nil, tt.term, tt.name, tt.arity, func(env *Env) *Promise {
				for k, v := range tt.env {
					_, ok := env.Unify(k, v)
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
		ok, err := Arg(nil, NewVariable(), v, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("term is not a compound", func(t *testing.T) {
		ok, err := Arg(nil, NewVariable(), NewAtom("foo"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeCompound, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("nth is a variable", func(t *testing.T) {
		nth := NewVariable()
		_, err := Arg(nil, nth, &compound{
			functor: NewAtom("f"),
			args:    []Term{NewAtom("a"), NewAtom("b"), NewAtom("a")},
		}, NewAtom("a"), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
	})

	t.Run("nth is an integer", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			ok, err := Arg(nil, Integer(1), &compound{
				functor: NewAtom("f"),
				args:    []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")},
			}, NewAtom("a"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)

			ok, err = Arg(nil, Integer(2), &compound{
				functor: NewAtom("f"),
				args:    []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")},
			}, NewAtom("b"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)

			ok, err = Arg(nil, Integer(3), &compound{
				functor: NewAtom("f"),
				args:    []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")},
			}, NewAtom("c"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("ng", func(t *testing.T) {
			ok, err := Arg(nil, Integer(0), &compound{
				functor: NewAtom("f"),
				args:    []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")},
			}, NewVariable(), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)

			ok, err = Arg(nil, Integer(4), &compound{
				functor: NewAtom("f"),
				args:    []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")},
			}, NewVariable(), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("negative", func(t *testing.T) {
			ok, err := Arg(nil, Integer(-2), &compound{
				functor: NewAtom("f"),
				args:    []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")},
			}, NewAtom("b"), Success, nil).Force(context.Background())
			assert.Equal(t, domainError(validDomainNotLessThanZero, Integer(-2), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("nth is neither a variable nor an integer", func(t *testing.T) {
		ok, err := Arg(nil, NewAtom("foo"), &compound{
			functor: NewAtom("f"),
			args:    []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")},
		}, NewAtom("b"), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})
}

func TestUniv(t *testing.T) {
	x, y := NewVariable(), NewVariable()
	l := NewVariable()
	a, as := NewVariable(), NewVariable()
	foo := NewVariable()

	tests := []struct {
		title      string
		term, list Term
		ok         bool
		err        error
		env        map[Variable]Term
	}{
		// 8.5.3.4 Examples
		{title: "1", term: NewAtom("foo").Apply(NewAtom("a"), NewAtom("b")), list: List(NewAtom("foo"), NewAtom("a"), NewAtom("b")), ok: true},
		{title: "2", term: x, list: List(NewAtom("foo"), NewAtom("a"), NewAtom("b")), ok: true, env: map[Variable]Term{
			x: NewAtom("foo").Apply(NewAtom("a"), NewAtom("b")),
		}},
		{title: "3", term: NewAtom("foo").Apply(NewAtom("a"), NewAtom("b")), list: l, ok: true, env: map[Variable]Term{
			l: List(NewAtom("foo"), NewAtom("a"), NewAtom("b")),
		}},
		{title: "4", term: NewAtom("foo").Apply(x, NewAtom("b")), list: List(NewAtom("foo"), NewAtom("a"), y), ok: true, env: map[Variable]Term{
			x: NewAtom("a"),
			y: NewAtom("b"),
		}},
		{title: "5", term: Integer(1), list: List(Integer(1)), ok: true},
		{title: "6", term: NewAtom("foo").Apply(NewAtom("a"), NewAtom("b")), list: List(NewAtom("foo"), NewAtom("b"), NewAtom("a")), ok: false},
		{title: "7", term: x, list: y, err: instantiationError(nil)},
		{title: "8", term: x, list: PartialList(y, NewAtom("foo"), NewAtom("a")), err: instantiationError(nil)},
		{title: "9", term: x, list: PartialList(NewAtom("bar"), NewAtom("foo")), err: typeError(validTypeList, PartialList(NewAtom("bar"), NewAtom("foo")), nil)},
		{title: "10", term: x, list: List(foo, NewAtom("bar")), err: instantiationError(nil)},
		{title: "11", term: x, list: List(Integer(3), Integer(1)), err: typeError(validTypeAtom, Integer(3), nil)},
		{title: "12", term: x, list: List(Float(1.1), NewAtom("foo")), err: typeError(validTypeAtom, Float(1.1), nil)},
		{title: "13", term: x, list: List(NewAtom("a").Apply(NewAtom("b")), Integer(1)), err: typeError(validTypeAtom, NewAtom("a").Apply(NewAtom("b")), nil)},
		{title: "14", term: x, list: Integer(4), err: typeError(validTypeList, Integer(4), nil)},
		{title: "15", term: NewAtom("f").Apply(x), list: List(NewAtom("f"), NewAtom("u").Apply(x)), ok: true, env: map[Variable]Term{
			x: NewAtom("u").Apply(x),
		}},

		// 8.5.3.3 Errors
		{title: "b: term is a compound", term: NewAtom("f").Apply(NewAtom("a")), list: PartialList(NewAtom("a"), NewAtom("f")), err: typeError(validTypeList, PartialList(NewAtom("a"), NewAtom("f")), nil)},
		{title: "b: term is an atomic", term: Integer(1), list: PartialList(NewAtom("a"), NewAtom("f")), err: typeError(validTypeList, PartialList(NewAtom("a"), NewAtom("f")), nil)},
		{title: "c", term: x, list: List(y), err: instantiationError(nil)},
		{title: "e", term: x, list: List(NewAtom("f").Apply(NewAtom("a"))), err: typeError(validTypeAtomic, NewAtom("f").Apply(NewAtom("a")), nil)},
		{title: "f", term: x, list: List(), err: domainError(validDomainNonEmptyList, List(), nil)},

		{title: "term is a variable, list has exactly one member which is an atomic", term: x, list: List(Integer(1)), ok: true, env: map[Variable]Term{
			x: Integer(1),
		}},
		{title: "term is an atomic, the length of list is not 1", term: Integer(1), list: List(), ok: false},

		// https://github.com/ichiban/prolog/issues/244
		{title: "term is atomic", term: NewAtom("c"), list: PartialList(as, a), ok: true, env: map[Variable]Term{
			a:  NewAtom("c"),
			as: List(),
		}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := Univ(nil, tt.term, tt.list, func(env *Env) *Promise {
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
	x, y := NewVariable(), NewVariable()
	a, b := NewVariable(), NewVariable()

	tests := []struct {
		title   string
		in, out Term
		ok      bool
		err     error
		env     map[Variable]Term
	}{
		// 8.5.4.4 Examples
		{title: "copy_term(X, Y).", in: x, out: y, ok: true},
		{title: "copy_term(X, 3).", in: x, out: Integer(3), ok: true},
		{title: "copy_term(_, a).", in: NewVariable(), out: NewAtom("a"), ok: true},
		{title: "copy_term(_, _).", in: NewVariable(), out: NewVariable(), ok: true},
		{title: "copy_term(X+X+Y, A+B+B).", in: atomPlus.Apply(atomPlus.Apply(x, x), y), out: atomPlus.Apply(atomPlus.Apply(a, b), b), ok: true, env: map[Variable]Term{
			a: b,
		}},
		{title: "copy_term(a, b).", in: NewAtom("a"), out: NewAtom("b"), ok: false},
		// copy_term(a+X, X+b), copy_term(a+X, X+b).
		{title: "copy_term(demoen(X, X), demoen(Y, f(Y))).", in: NewAtom("demoen").Apply(x, x), out: NewAtom("demoen").Apply(y, NewAtom("f").Apply(y)), ok: true, env: map[Variable]Term{
			y: NewAtom("f").Apply(y),
		}},

		{title: "charList", in: CharList("foo"), out: CharList("foo"), ok: true},
		{title: "codeList", in: CodeList("foo"), out: CodeList("foo"), ok: true},
		{title: "list", in: List(NewAtom("a"), NewAtom("b"), NewAtom("c")), out: List(NewAtom("a"), NewAtom("b"), NewAtom("c")), ok: true},
		{title: "partial", in: PartialList(x, NewAtom("a"), NewAtom("b")), out: PartialList(x, NewAtom("a"), NewAtom("b")), ok: true},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := CopyTerm(nil, tt.in, tt.out, func(env *Env) *Promise {
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

func TestTermVariables(t *testing.T) {
	vars := NewVariable()
	vs, vt := NewVariable(), NewVariable()
	a, b, c, d := NewVariable(), NewVariable(), NewVariable(), NewVariable()

	tests := []struct {
		title      string
		term, vars Term
		ok         bool
		err        error
		env        map[Variable]Term
		mem        int64
	}{
		// 8.5.5.4 Examples
		{title: "1", term: NewAtom("t"), vars: vars, ok: true, env: map[Variable]Term{
			vars: List(),
		}},
		{title: "2", term: atomMinus.Apply(
			atomPlus.Apply(
				a,
				atomSlash.Apply(
					NewAtom("*").Apply(b, c),
					c,
				),
			),
			d,
		), vars: vars, ok: true, env: map[Variable]Term{
			vars: List(a, b, c, d),
		}},
		{title: "3", term: NewAtom("t"), vars: PartialList(NewAtom("a"), NewAtom("x"), NewAtom("y")), err: typeError(validTypeList, PartialList(NewAtom("a"), NewAtom("x"), NewAtom("y")), nil)},
		{title: "4, 5", term: vs, vars: vars, ok: true, env: map[Variable]Term{
			vars: List(b, a),
			vs:   atomPlus.Apply(b, vt),
			vt:   NewAtom("*").Apply(a, b),
		}},
		{title: "6", term: atomPlus.Apply(atomPlus.Apply(a, b), b), vars: PartialList(vars, b), ok: true, env: map[Variable]Term{
			b:    a,
			vars: List(b),
		}},

		{title: "out of memory", term: NewAtom("f").Apply(NewVariable(), NewVariable()), vars: vars, ok: false, err: resourceError(resourceMemory, nil), mem: 1},
	}

	env := NewEnv().
		bind(vs, atomPlus.Apply(b, vt)).
		bind(vt, NewAtom("*").Apply(a, b))
	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			if tt.mem > 0 {
				orig := memFree
				memFree = func() int64 {
					return tt.mem
				}
				defer func() {
					memFree = orig
				}()
			}

			ok, err := TermVariables(nil, tt.term, tt.vars, func(env *Env) *Promise {
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

func TestOp(t *testing.T) {
	t.Run("insert", func(t *testing.T) {
		t.Run("atom", func(t *testing.T) {
			vm := VM{operators: operators{}}
			vm.operators.define(900, operatorSpecifierXFX, NewAtom(`+++`))
			vm.operators.define(1100, operatorSpecifierXFX, NewAtom(`+`))

			ok, err := Op(&vm, Integer(1000), atomXFX, NewAtom("++"), Success, nil).Force(context.Background())
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
			}, vm.operators)
		})

		t.Run("list", func(t *testing.T) {
			vm := VM{
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
			ok, err := Op(&vm, Integer(1000), atomXFX, List(NewAtom("++"), NewAtom("++")), Success, nil).Force(context.Background())
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
			}, vm.operators)
		})
	})

	t.Run("remove", func(t *testing.T) {
		vm := VM{
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
		ok, err := Op(&vm, Integer(0), atomXFX, NewAtom("++"), Success, nil).Force(context.Background())
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
		}, vm.operators)
	})

	t.Run("priority is a variable", func(t *testing.T) {
		var vm VM
		ok, err := Op(&vm, NewVariable(), atomXFX, atomPlus, Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("specifier is a variable", func(t *testing.T) {
		var vm VM
		ok, err := Op(&vm, Integer(1000), NewVariable(), atomPlus, Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("priority is neither a variable nor an integer", func(t *testing.T) {
		var vm VM
		ok, err := Op(&vm, NewAtom("foo"), atomXFX, atomPlus, Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("specifier is neither a variable nor an atom", func(t *testing.T) {
		var vm VM
		ok, err := Op(&vm, Integer(1000), Integer(0), atomPlus, Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("operator is neither a partial list nor a list nor an atom", func(t *testing.T) {
		var vm VM
		ok, err := Op(&vm, Integer(1000), atomXFX, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeList, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the operator list is neither a variable nor an atom", func(t *testing.T) {
		var vm VM
		ok, err := Op(&vm, Integer(1000), atomXFX, List(Integer(0)), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("priority is not between 0 and 1200 inclusive", func(t *testing.T) {
		t.Run("priority is negative", func(t *testing.T) {
			var vm VM
			ok, err := Op(&vm, Integer(-1), atomXFX, atomPlus, Success, nil).Force(context.Background())
			assert.Equal(t, domainError(validDomainOperatorPriority, Integer(-1), nil), err)
			assert.False(t, ok)
		})

		t.Run("priority is more than 1200", func(t *testing.T) {
			var vm VM
			ok, err := Op(&vm, Integer(1201), atomXFX, atomPlus, Success, nil).Force(context.Background())
			assert.Equal(t, domainError(validDomainOperatorPriority, Integer(1201), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("specifier is not a valid operator specifier", func(t *testing.T) {
		var vm VM
		ok, err := Op(&vm, Integer(1000), NewAtom("foo"), atomPlus, Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainOperatorSpecifier, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("operator is ','", func(t *testing.T) {
		vm := VM{operators: operators{}}
		vm.operators.define(1000, operatorSpecifierXFY, NewAtom(`,`))
		ok, err := Op(&vm, Integer(1000), atomXFY, atomComma, Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationModify, permissionTypeOperator, atomComma, nil), err)
		assert.False(t, ok)
	})

	t.Run("an element of the operator list is ','", func(t *testing.T) {
		vm := VM{operators: operators{}}
		vm.operators.define(1000, operatorSpecifierXFY, NewAtom(`,`))
		ok, err := Op(&vm, Integer(1000), atomXFY, List(atomComma), Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationModify, permissionTypeOperator, atomComma, nil), err)
		assert.False(t, ok)
	})

	t.Run("operator is an atom, priority is a priority, and specifier is a specifier such that operator would have an invalid set of priorities and specifiers", func(t *testing.T) {
		t.Run("empty list", func(t *testing.T) {
			var vm VM
			ok, err := Op(&vm, Integer(1000), atomXFY, atomEmptyList, Success, nil).Force(context.Background())
			assert.Equal(t, permissionError(operationCreate, permissionTypeOperator, atomEmptyList, nil), err)
			assert.False(t, ok)
		})

		t.Run("empty curly brackets", func(t *testing.T) {
			var vm VM
			ok, err := Op(&vm, Integer(1000), atomXFY, atomEmptyBlock, Success, nil).Force(context.Background())
			assert.Equal(t, permissionError(operationCreate, permissionTypeOperator, atomEmptyBlock, nil), err)
			assert.False(t, ok)
		})

		t.Run("bar", func(t *testing.T) {
			t.Run("create", func(t *testing.T) {
				var vm VM
				ok, err := Op(&vm, Integer(1000), atomXFY, atomBar, Success, nil).Force(context.Background())
				assert.Equal(t, permissionError(operationCreate, permissionTypeOperator, atomBar, nil), err)
				assert.False(t, ok)
			})

			t.Run("modify", func(t *testing.T) {
				vm := VM{operators: operators{}}
				vm.operators.define(1001, operatorSpecifierXFY, NewAtom(`|`))
				ok, err := Op(&vm, Integer(1000), atomXFY, atomBar, Success, nil).Force(context.Background())
				assert.Equal(t, permissionError(operationModify, permissionTypeOperator, atomBar, nil), err)
				assert.False(t, ok)
			})
		})
	})

	t.Run("operator is a list, priority is a priority, and specifier is a specifier such that an element op of the list operator would have an invalid set of priorities and specifiers", func(t *testing.T) {
		t.Run("empty list", func(t *testing.T) {
			var vm VM
			ok, err := Op(&vm, Integer(1000), atomXFY, List(atomEmptyList), Success, nil).Force(context.Background())
			assert.Equal(t, permissionError(operationCreate, permissionTypeOperator, atomEmptyList, nil), err)
			assert.False(t, ok)
		})

		t.Run("empty curly brackets", func(t *testing.T) {
			var vm VM
			ok, err := Op(&vm, Integer(1000), atomXFY, List(atomEmptyBlock), Success, nil).Force(context.Background())
			assert.Equal(t, permissionError(operationCreate, permissionTypeOperator, atomEmptyBlock, nil), err)
			assert.False(t, ok)
		})

		t.Run("bar", func(t *testing.T) {
			t.Run("create", func(t *testing.T) {
				var vm VM
				ok, err := Op(&vm, Integer(1000), atomXFY, List(atomBar), Success, nil).Force(context.Background())
				assert.Equal(t, permissionError(operationCreate, permissionTypeOperator, atomBar, nil), err)
				assert.False(t, ok)
			})

			t.Run("modify", func(t *testing.T) {
				vm := VM{operators: operators{}}
				vm.operators.define(101, operatorSpecifierXFY, NewAtom(`|`))
				ok, err := Op(&vm, Integer(1000), atomXFY, List(atomBar), Success, nil).Force(context.Background())
				assert.Equal(t, permissionError(operationModify, permissionTypeOperator, atomBar, nil), err)
				assert.False(t, ok)
			})
		})
	})

	t.Run("There shall not be an infix and a postfix operator with the same name.", func(t *testing.T) {
		t.Run("infix", func(t *testing.T) {
			vm := VM{operators: operators{}}
			vm.operators.define(200, operatorSpecifierYF, NewAtom(`+`))
			ok, err := Op(&vm, Integer(500), atomYFX, List(atomPlus), Success, nil).Force(context.Background())
			assert.Equal(t, permissionError(operationCreate, permissionTypeOperator, atomPlus, nil), err)
			assert.False(t, ok)
		})

		t.Run("postfix", func(t *testing.T) {
			vm := VM{operators: operators{}}
			vm.operators.define(500, operatorSpecifierYFX, NewAtom(`+`))
			ok, err := Op(&vm, Integer(200), atomYF, List(atomPlus), Success, nil).Force(context.Background())
			assert.Equal(t, permissionError(operationCreate, permissionTypeOperator, atomPlus, nil), err)
			assert.False(t, ok)
		})
	})
}

func TestCurrentOp(t *testing.T) {
	vm := VM{operators: operators{}}
	vm.operators.define(900, operatorSpecifierXFX, NewAtom(`+++`))
	vm.operators.define(1000, operatorSpecifierXFX, NewAtom(`++`))
	vm.operators.define(1100, operatorSpecifierXFX, NewAtom(`+`))

	t.Run("single solution", func(t *testing.T) {
		ok, err := CurrentOp(&vm, Integer(1100), atomXFX, atomPlus, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("multiple solutions", func(t *testing.T) {
		priority, specifier, operator := NewVariable(), NewVariable(), NewVariable()
		ok, err := CurrentOp(&vm, priority, specifier, operator, func(env *Env) *Promise {
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
			ok, err := CurrentOp(&vm, NewAtom("foo"), atomXFX, atomPlus, Success, nil).Force(context.Background())
			assert.Equal(t, domainError(validDomainOperatorPriority, NewAtom("foo"), nil), err)
			assert.False(t, ok)
		})

		t.Run("priority is negative", func(t *testing.T) {
			ok, err := CurrentOp(&vm, Integer(-1), atomXFX, atomPlus, Success, nil).Force(context.Background())
			assert.Equal(t, domainError(validDomainOperatorPriority, Integer(-1), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("specifier is not an operator specifier", func(t *testing.T) {
		t.Run("specifier is not an atom", func(t *testing.T) {
			ok, err := CurrentOp(&vm, Integer(1100), Integer(0), atomPlus, Success, nil).Force(context.Background())
			assert.Equal(t, domainError(validDomainOperatorSpecifier, Integer(0), nil), err)
			assert.False(t, ok)
		})

		t.Run("specifier is a non-specifier atom", func(t *testing.T) {
			ok, err := CurrentOp(&vm, Integer(1100), NewAtom("foo"), atomPlus, Success, nil).Force(context.Background())
			assert.Equal(t, domainError(validDomainOperatorSpecifier, NewAtom("foo"), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("operator is not an atom", func(t *testing.T) {
		ok, err := CurrentOp(&vm, Integer(1100), atomXFX, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})
}

func TestBagOf(t *testing.T) {
	s := NewVariable()
	x, y, z := NewVariable(), NewVariable(), NewVariable()
	l := NewVariable()

	tests := []struct {
		title                     string
		template, goal, instances Term
		err                       error
		env                       []map[Variable]Term
		warning                   bool
		mem                       int64
	}{
		// 8.10.2.4 Examples
		{
			title:     "bagof(X, (X=1 ; X=2), S).",
			template:  x,
			goal:      atomSemiColon.Apply(atomEqual.Apply(x, Integer(1)), atomEqual.Apply(x, Integer(2))),
			instances: s,
			env: []map[Variable]Term{
				{s: List(Integer(1), Integer(2))},
			},
		},
		{
			title:     "bagof(X, (X=1 ; X=2), X).",
			template:  x,
			goal:      atomSemiColon.Apply(atomEqual.Apply(x, Integer(1)), atomEqual.Apply(x, Integer(2))),
			instances: x,
			env: []map[Variable]Term{
				{x: List(Integer(1), Integer(2))},
			},
		},
		{
			title:     "bagof(X, (X=Y ; X=Z), S).",
			template:  x,
			goal:      atomSemiColon.Apply(atomEqual.Apply(x, y), atomEqual.Apply(x, z)),
			instances: s,
			env: []map[Variable]Term{
				{s: List(y, z)},
			},
		},
		{
			title:     "bagof(X, fail, S).",
			template:  x,
			goal:      atomFail,
			instances: s,
			env:       nil,
		},
		{
			title:     "bagof(1, (Y=1 ; Y=2), L).",
			template:  Integer(1),
			goal:      atomSemiColon.Apply(atomEqual.Apply(y, Integer(1)), atomEqual.Apply(y, Integer(2))),
			instances: l,
			env: []map[Variable]Term{
				{l: List(Integer(1)), y: Integer(1)},
				{l: List(Integer(1)), y: Integer(2)},
			},
		},
		{
			title:     "bagof(f(X, Y), (X=a ; Y=b), L).",
			template:  NewAtom("f").Apply(x, y),
			goal:      atomSemiColon.Apply(atomEqual.Apply(x, NewAtom("a")), atomEqual.Apply(y, NewAtom("b"))),
			instances: l,
			env: []map[Variable]Term{
				{l: List(NewAtom("f").Apply(NewAtom("a"), NewVariable()), NewAtom("f").Apply(NewVariable(), NewAtom("b")))},
			},
		},
		{
			title:    "bagof(X, Y^((X=1, Y=1) ; (X=2, Y=2)), S).",
			template: x,
			goal: atomCaret.Apply(y, atomSemiColon.Apply(
				atomComma.Apply(atomEqual.Apply(x, Integer(1)), atomEqual.Apply(y, Integer(1))),
				atomComma.Apply(atomEqual.Apply(x, Integer(2)), atomEqual.Apply(y, Integer(2))),
			)),
			instances: s,
			env: []map[Variable]Term{
				{s: List(Integer(1), Integer(2))},
			},
		},
		{
			title:    "bagof(X, Y^((X=1 ; Y=1) ; (X=2, Y=2)), S).",
			template: x,
			goal: atomCaret.Apply(y, atomSemiColon.Apply(
				atomSemiColon.Apply(atomEqual.Apply(x, Integer(1)), atomEqual.Apply(y, Integer(1))),
				atomComma.Apply(atomEqual.Apply(x, Integer(2)), atomEqual.Apply(y, Integer(2))),
			)),
			instances: s,
			env: []map[Variable]Term{
				{s: List(Integer(1), NewVariable(), Integer(2))},
			},
		},
		{
			title:    "bagof(X, (Y^(X=1 ; Y=2) ; X=3), S).",
			template: x,
			goal: atomSemiColon.Apply(
				atomCaret.Apply(
					y,
					atomSemiColon.Apply(
						atomEqual.Apply(x, Integer(1)),
						atomEqual.Apply(y, Integer(2)),
					),
				),
				atomEqual.Apply(x, Integer(3)),
			),
			instances: s,
			env: []map[Variable]Term{
				{s: List(Integer(3)), y: NewVariable()},
			},
			warning: true,
		},
		{
			title:    "bagof(X, (X=Y ; X=Z ; Y=1), S).",
			template: x,
			goal: atomSemiColon.Apply(
				atomEqual.Apply(x, y),
				atomSemiColon.Apply(
					atomEqual.Apply(x, z),
					atomEqual.Apply(y, Integer(1)),
				),
			),
			instances: s,
			env: []map[Variable]Term{
				{s: List(y, z)},
				{s: List(NewVariable())},
			},
		},
		{
			title:     "bagof(X, a(X, Y), L).",
			template:  x,
			goal:      NewAtom("a").Apply(x, y),
			instances: l,
			env: []map[Variable]Term{
				{l: List(Integer(1), Integer(2)), y: NewAtom("f").Apply(NewVariable())},
			},
		},
		{
			title:     "bagof(X, b(X, Y), L).",
			template:  x,
			goal:      NewAtom("b").Apply(x, y),
			instances: l,
			env: []map[Variable]Term{
				{l: List(Integer(1), Integer(1), Integer(2)), y: Integer(1)},
				{l: List(Integer(1), Integer(2), Integer(2)), y: Integer(2)},
			},
		},
		{
			title:     "bagof(X, Y^Z, L).",
			template:  x,
			goal:      atomCaret.Apply(y, z),
			instances: l,
			err:       instantiationError(nil),
		},
		{
			title:     "bagof(X, 1, L).",
			template:  x,
			goal:      Integer(1),
			instances: l,
			err:       typeError(validTypeCallable, Integer(1), nil),
		},

		// 8.10.2.3 Errors
		{
			title:     "c",
			template:  NewAtom("t"),
			goal:      atomTrue,
			instances: PartialList(Integer(1), NewAtom("t")),
			err:       typeError(validTypeList, PartialList(Integer(1), NewAtom("t")), nil),
		},

		{
			title:     "out of memory",
			template:  x,
			goal:      atomSemiColon.Apply(atomEqual.Apply(x, y), atomEqual.Apply(x, y)),
			instances: s,
			err:       resourceError(resourceMemory, nil),
			mem:       1,
		},
	}

	vm := VM{
		unknown: unknownWarning,
	}
	vm.Register2(atomEqual, Unify)
	vm.Register2(atomComma, func(vm *VM, g1, g2 Term, k Cont, env *Env) *Promise {
		return Call(vm, g1, func(env *Env) *Promise {
			return Call(vm, g2, k, env)
		}, env)
	})
	vm.Register2(atomSemiColon, func(vm *VM, g1, g2 Term, k Cont, env *Env) *Promise {
		return Delay(func(context.Context) *Promise {
			return Call(vm, g1, k, env)
		}, func(context.Context) *Promise {
			return Call(vm, g2, k, env)
		})
	})
	vm.Register0(atomTrue, func(_ *VM, k Cont, env *Env) *Promise {
		return k(env)
	})
	vm.Register0(atomFail, func(*VM, Cont, *Env) *Promise {
		return Bool(false)
	})
	vm.Register2(NewAtom("a"), func(vm *VM, x, y Term, k Cont, env *Env) *Promise {
		a, f := NewAtom("$a"), NewAtom("f")
		return Delay(func(context.Context) *Promise {
			return Unify(vm, a.Apply(x, y), a.Apply(Integer(1), f.Apply(NewVariable())), k, env)
		}, func(context.Context) *Promise {
			return Unify(vm, a.Apply(x, y), a.Apply(Integer(2), f.Apply(NewVariable())), k, env)
		})
	})
	vm.Register2(NewAtom("b"), func(vm *VM, x, y Term, k Cont, env *Env) *Promise {
		b := NewAtom("$b")
		return Delay(func(context.Context) *Promise {
			return Unify(vm, b.Apply(x, y), b.Apply(Integer(1), Integer(1)), k, env)
		}, func(context.Context) *Promise {
			return Unify(vm, b.Apply(x, y), b.Apply(Integer(1), Integer(1)), k, env)
		}, func(context.Context) *Promise {
			return Unify(vm, b.Apply(x, y), b.Apply(Integer(1), Integer(2)), k, env)
		}, func(context.Context) *Promise {
			return Unify(vm, b.Apply(x, y), b.Apply(Integer(2), Integer(1)), k, env)
		}, func(context.Context) *Promise {
			return Unify(vm, b.Apply(x, y), b.Apply(Integer(2), Integer(2)), k, env)
		}, func(context.Context) *Promise {
			return Unify(vm, b.Apply(x, y), b.Apply(Integer(2), Integer(2)), k, env)
		})
	})

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			if tt.mem > 0 {
				orig := memFree
				memFree = func() int64 {
					return tt.mem
				}
				defer func() {
					memFree = orig
				}()
			}

			vm.Unknown = func(Atom, []Term, *Env) {
				assert.True(t, tt.warning)
			}
			_, err := BagOf(&vm, tt.template, tt.goal, tt.instances, func(env *Env) *Promise {
				for k, v := range tt.env[0] {
					_, ok := env.Unify(v, k)
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

func TestSetOf(t *testing.T) {
	s := NewVariable()
	x, y, z := NewVariable(), NewVariable(), NewVariable()
	xs := NewVariable()
	l := NewVariable()
	u, v := NewVariable(), NewVariable()
	tests := []struct {
		title                     string
		template, goal, instances Term
		err                       error
		env                       []map[Variable]Term
		warning                   bool
		mem                       int64
	}{
		// 8.10.3.4 Examples
		{
			title:     "setof(X, (X=1; X=2), S).",
			template:  x,
			goal:      atomSemiColon.Apply(atomEqual.Apply(x, Integer(1)), atomEqual.Apply(x, Integer(2))),
			instances: s,
			env: []map[Variable]Term{
				{s: List(Integer(1), Integer(2))},
			},
		},
		{
			title:     "setof(X, (X=1; X=2), X).",
			template:  x,
			goal:      atomSemiColon.Apply(atomEqual.Apply(x, Integer(1)), atomEqual.Apply(x, Integer(2))),
			instances: x,
			env: []map[Variable]Term{
				{x: List(Integer(1), Integer(2))},
			},
		},
		{
			title:     "setof(X, (X=2; X=1), S).",
			template:  x,
			goal:      atomSemiColon.Apply(atomEqual.Apply(x, Integer(2)), atomEqual.Apply(x, Integer(1))),
			instances: s,
			env: []map[Variable]Term{
				{s: List(Integer(1), Integer(2))},
			},
		},
		{
			title:     "setof(X, (X=2; X=2), S).",
			template:  x,
			goal:      atomSemiColon.Apply(atomEqual.Apply(x, Integer(2)), atomEqual.Apply(x, Integer(2))),
			instances: s,
			env: []map[Variable]Term{
				{s: List(Integer(2))},
			},
		},
		{
			title:     "setof(X, (X=Y ; X=Z), S).",
			template:  x,
			goal:      atomSemiColon.Apply(atomEqual.Apply(x, y), atomEqual.Apply(x, z)),
			instances: s,
			env: []map[Variable]Term{
				{s: List(y, z)},
			},
		},
		{
			title:     "setof(X, fail, S).",
			template:  x,
			goal:      atomFail,
			instances: s,
			env:       nil,
		},
		{
			title:     "setof(1, (Y=2; Y=1), L).",
			template:  Integer(1),
			goal:      atomSemiColon.Apply(atomEqual.Apply(y, Integer(2)), atomEqual.Apply(y, Integer(1))),
			instances: l,
			env: []map[Variable]Term{
				{l: List(Integer(1)), y: Integer(2)},
				{l: List(Integer(1)), y: Integer(1)},
			},
		},
		{
			title:     "setof(f(X, Y), (X=a ; Y=b), L).",
			template:  NewAtom("f").Apply(x, y),
			goal:      atomSemiColon.Apply(atomEqual.Apply(x, NewAtom("a")), atomEqual.Apply(y, NewAtom("b"))),
			instances: l,
			env: []map[Variable]Term{
				{l: List(NewAtom("f").Apply(NewAtom("a"), NewVariable()), NewAtom("f").Apply(NewVariable(), NewAtom("b")))},
			},
		},
		{
			title:    "setof(X, Y^((X=1, Y=1) ; (X=2, Y=2)), S).",
			template: x,
			goal: atomCaret.Apply(y, atomSemiColon.Apply(
				atomComma.Apply(atomEqual.Apply(x, Integer(1)), atomEqual.Apply(y, Integer(1))),
				atomComma.Apply(atomEqual.Apply(x, Integer(2)), atomEqual.Apply(y, Integer(2))),
			)),
			instances: s,
			env: []map[Variable]Term{
				{s: List(Integer(1), Integer(2))},
			},
		},
		{
			title:    "setof(X, Y^((X=1 ; Y=1) ; (X=2, Y=2)), S).",
			template: x,
			goal: atomCaret.Apply(y, atomSemiColon.Apply(
				atomSemiColon.Apply(atomEqual.Apply(x, Integer(1)), atomEqual.Apply(y, Integer(1))),
				atomComma.Apply(atomEqual.Apply(x, Integer(2)), atomEqual.Apply(y, Integer(2))),
			)),
			instances: s,
			env: []map[Variable]Term{
				{s: List(NewVariable(), Integer(1), Integer(2))},
			},
		},
		{
			title:    "setof(X, (Y^(X=1 ; Y=2) ; X=3), S).",
			template: x,
			goal: atomSemiColon.Apply(
				atomCaret.Apply(
					y,
					atomSemiColon.Apply(
						atomEqual.Apply(x, Integer(1)),
						atomEqual.Apply(y, Integer(2)),
					),
				),
				atomEqual.Apply(x, Integer(3)),
			),
			instances: s,
			env: []map[Variable]Term{
				{s: List(Integer(3)), y: NewVariable()},
			},
			warning: true,
		},
		{
			title:    "setof(X, (X=Y ; X=Z ; Y=1), S).",
			template: x,
			goal: atomSemiColon.Apply(
				atomEqual.Apply(x, y),
				atomSemiColon.Apply(
					atomEqual.Apply(x, z),
					atomEqual.Apply(y, Integer(1)),
				),
			),
			instances: s,
			env: []map[Variable]Term{
				{s: List(y, z)},
				{s: List(NewVariable())},
			},
		},
		{
			title:     "setof(X, a(X, Y), L).",
			template:  x,
			goal:      NewAtom("a").Apply(x, y),
			instances: l,
			env: []map[Variable]Term{
				{l: List(Integer(1), Integer(2)), y: NewAtom("f").Apply(NewVariable())},
			},
		},
		{
			title:     "setof(X, member(X,[f(U,b),f(V,c)]), L).",
			template:  x,
			goal:      NewAtom("member").Apply(x, List(NewAtom("f").Apply(u, NewAtom("b")), NewAtom("f").Apply(v, NewAtom("c")))),
			instances: l,
			env: []map[Variable]Term{
				{l: List(NewAtom("f").Apply(u, NewAtom("b")), NewAtom("f").Apply(v, NewAtom("c")))},
			},
		},
		{
			title:     "setof(X, member(X,[f(U,b),f(V,c)]), [f(a,c),f(a,b)]).",
			template:  x,
			goal:      NewAtom("member").Apply(x, List(NewAtom("f").Apply(u, NewAtom("b")), NewAtom("f").Apply(v, NewAtom("c")))),
			instances: List(NewAtom("f").Apply(NewAtom("a"), NewAtom("c")), NewAtom("f").Apply(NewAtom("a"), NewAtom("b"))),
			env:       nil,
		},
		{
			title:     "setof(X, member(X,[f(b,U),f(c,V)]), [f(b,a),f(c,a)]).",
			template:  x,
			goal:      NewAtom("member").Apply(x, List(NewAtom("f").Apply(NewAtom("b"), u), NewAtom("f").Apply(NewAtom("c"), v))),
			instances: List(NewAtom("f").Apply(NewAtom("b"), NewAtom("a")), NewAtom("f").Apply(NewAtom("c"), NewAtom("a"))),
			env: []map[Variable]Term{
				{u: NewAtom("a"), v: NewAtom("a")},
			},
		},
		{
			title:     "setof(X, member(X,[V,U,f(U),f(V)]), L).",
			template:  x,
			goal:      NewAtom("member").Apply(x, List(v, u, NewAtom("f").Apply(u), NewAtom("f").Apply(v))),
			instances: l,
			env: []map[Variable]Term{
				{l: List(u, v, NewAtom("f").Apply(u), NewAtom("f").Apply(v))},
			},
		},
		{
			title:     "setof(X, member(X,[V,U,f(U),f(V)]), [a,b,f(a),f(b)]).",
			template:  x,
			goal:      NewAtom("member").Apply(x, List(v, u, NewAtom("f").Apply(u), NewAtom("f").Apply(v))),
			instances: List(NewAtom("a"), NewAtom("b"), NewAtom("f").Apply(NewAtom("a")), NewAtom("f").Apply(NewAtom("b"))),
			env: []map[Variable]Term{
				{u: NewAtom("a"), v: NewAtom("b")},
			},
		},
		{
			title:     "setof(X, member(X,[V,U,f(U),f(V)]), [a,b,f(b),f(a)]).",
			template:  x,
			goal:      NewAtom("member").Apply(x, List(v, u, NewAtom("f").Apply(u), NewAtom("f").Apply(v))),
			instances: List(NewAtom("a"), NewAtom("b"), NewAtom("f").Apply(NewAtom("b")), NewAtom("f").Apply(NewAtom("a"))),
			env:       nil,
		},
		{
			title:    "setof(X, (exists(U,V)^member(X,[V,U,f(U),f(V)])), [a,b,f(b),f(a)]).",
			template: x,
			goal: atomCaret.Apply(
				NewAtom("exists").Apply(u, v),
				NewAtom("member").Apply(x, List(v, u, NewAtom("f").Apply(u), NewAtom("f").Apply(v))),
			),
			instances: List(NewAtom("a"), NewAtom("b"), NewAtom("f").Apply(NewAtom("b")), NewAtom("f").Apply(NewAtom("a"))),
			env: []map[Variable]Term{
				{},
			},
		},
		{
			title:     "setof(X, b(X, Y), L).",
			template:  x,
			goal:      NewAtom("b").Apply(x, y),
			instances: l,
			env: []map[Variable]Term{
				{l: List(Integer(1), Integer(2)), y: Integer(1)},
				{l: List(Integer(1), Integer(2)), y: Integer(2)},
			},
		},
		{
			title:    "setof(X-Xs, Y^setof(Y,b(X, Y),Xs), L).",
			template: atomMinus.Apply(x, xs),
			goal: atomCaret.Apply(
				y,
				NewAtom("setof").Apply(
					y,
					NewAtom("b").Apply(x, y),
					xs,
				),
			),
			instances: l,
			env: []map[Variable]Term{
				{l: List(
					atomMinus.Apply(Integer(1), List(Integer(1), Integer(2))),
					atomMinus.Apply(Integer(2), List(Integer(1), Integer(2))),
				)},
			},
		},
		{
			title:    "setof(X-Xs, setof(Y,b(X, Y),Xs), L).",
			template: atomMinus.Apply(x, xs),
			goal: atomCaret.Apply(
				y,
				NewAtom("setof").Apply(
					y,
					NewAtom("b").Apply(x, y),
					xs,
				),
			),
			instances: l,
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
			template: atomMinus.Apply(x, xs),
			goal: NewAtom("bagof").Apply(
				y,
				NewAtom("d").Apply(x, y),
				xs,
			),
			instances: l,
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
			instances: PartialList(Integer(1), NewAtom("t")),
			err:       typeError(validTypeList, PartialList(Integer(1), NewAtom("t")), nil),
		},

		{
			title:     "out of memory",
			template:  x,
			goal:      atomSemiColon.Apply(atomEqual.Apply(x, y), atomEqual.Apply(x, y)),
			instances: s,
			err:       resourceError(resourceMemory, nil),
			mem:       1,
		},
	}

	vm := VM{
		unknown: unknownWarning,
	}
	vm.Register2(atomEqual, Unify)
	vm.Register2(atomComma, func(vm *VM, g1, g2 Term, k Cont, env *Env) *Promise {
		return Call(vm, g1, func(env *Env) *Promise {
			return Call(vm, g2, k, env)
		}, env)
	})
	vm.Register2(atomSemiColon, func(vm *VM, g1, g2 Term, k Cont, env *Env) *Promise {
		return Delay(func(context.Context) *Promise {
			return Call(vm, g1, k, env)
		}, func(context.Context) *Promise {
			return Call(vm, g2, k, env)
		})
	})
	vm.Register0(atomTrue, func(_ *VM, k Cont, env *Env) *Promise {
		return k(env)
	})
	vm.Register0(atomFail, func(*VM, Cont, *Env) *Promise {
		return Bool(false)
	})
	vm.Register2(NewAtom("a"), func(vm *VM, x, y Term, k Cont, env *Env) *Promise {
		a, f := NewAtom("$a"), NewAtom("f")
		return Delay(func(context.Context) *Promise {
			return Unify(vm, a.Apply(x, y), a.Apply(Integer(1), f.Apply(NewVariable())), k, env)
		}, func(context.Context) *Promise {
			return Unify(vm, a.Apply(x, y), a.Apply(Integer(2), f.Apply(NewVariable())), k, env)
		})
	})
	vm.Register2(NewAtom("b"), func(vm *VM, x, y Term, k Cont, env *Env) *Promise {
		b := NewAtom("$b")
		return Delay(func(context.Context) *Promise {
			return Unify(vm, b.Apply(x, y), b.Apply(Integer(1), Integer(1)), k, env)
		}, func(context.Context) *Promise {
			return Unify(vm, b.Apply(x, y), b.Apply(Integer(1), Integer(1)), k, env)
		}, func(context.Context) *Promise {
			return Unify(vm, b.Apply(x, y), b.Apply(Integer(1), Integer(2)), k, env)
		}, func(context.Context) *Promise {
			return Unify(vm, b.Apply(x, y), b.Apply(Integer(2), Integer(1)), k, env)
		}, func(context.Context) *Promise {
			return Unify(vm, b.Apply(x, y), b.Apply(Integer(2), Integer(2)), k, env)
		}, func(context.Context) *Promise {
			return Unify(vm, b.Apply(x, y), b.Apply(Integer(2), Integer(2)), k, env)
		})
	})
	vm.Register2(NewAtom("d"), func(vm *VM, x, y Term, k Cont, env *Env) *Promise {
		d := NewAtom("$d")
		return Delay(func(context.Context) *Promise {
			return Unify(vm, d.Apply(x, y), d.Apply(Integer(1), Integer(1)), k, env)
		}, func(context.Context) *Promise {
			return Unify(vm, d.Apply(x, y), d.Apply(Integer(1), Integer(2)), k, env)
		}, func(context.Context) *Promise {
			return Unify(vm, d.Apply(x, y), d.Apply(Integer(1), Integer(1)), k, env)
		}, func(context.Context) *Promise {
			return Unify(vm, d.Apply(x, y), d.Apply(Integer(2), Integer(2)), k, env)
		}, func(context.Context) *Promise {
			return Unify(vm, d.Apply(x, y), d.Apply(Integer(2), Integer(1)), k, env)
		}, func(context.Context) *Promise {
			return Unify(vm, d.Apply(x, y), d.Apply(Integer(2), Integer(2)), k, env)
		})
	})
	vm.Register2(NewAtom("member"), func(vm *VM, elem, list Term, k Cont, env *Env) *Promise {
		var ks []func(context.Context) *Promise
		iter := ListIterator{List: list, Env: env, AllowPartial: true}
		for iter.Next() {
			e := iter.Current()
			ks = append(ks, func(context.Context) *Promise {
				return Unify(vm, elem, e, k, env)
			})
		}
		if err := iter.Err(); err != nil {
			return Error(err)
		}
		return Delay(ks...)
	})
	vm.Register3(NewAtom("setof"), SetOf)
	vm.Register3(NewAtom("bagof"), BagOf)

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			if tt.mem > 0 {
				orig := memFree
				memFree = func() int64 {
					return tt.mem
				}
				defer func() {
					memFree = orig
				}()
			}

			vm.Unknown = func(Atom, []Term, *Env) {
				assert.True(t, tt.warning)
			}
			_, err := SetOf(&vm, tt.template, tt.goal, tt.instances, func(env *Env) *Promise {
				for k, v := range tt.env[0] {
					_, ok := env.Unify(v, k)
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

func TestFindAll(t *testing.T) {
	x, y := NewVariable(), NewVariable()
	s := NewVariable()
	l := NewVariable()
	goal := NewVariable()

	tests := []struct {
		title                     string
		template, goal, instances Term
		ok                        bool
		err                       error
		env                       map[Variable]Term
	}{
		// 8.10.1.4 Examples
		{title: "1", template: x, goal: atomSemiColon.Apply(atomEqual.Apply(x, Integer(1)), atomEqual.Apply(x, Integer(2))), instances: s, ok: true, env: map[Variable]Term{
			s: List(Integer(1), Integer(2)),
		}},
		{title: "2", template: atomPlus.Apply(x, y), goal: atomEqual.Apply(x, Integer(1)), instances: s, ok: true, env: map[Variable]Term{
			s: List(atomPlus.Apply(Integer(1), NewVariable())),
		}},
		{title: "3", template: x, goal: atomFail, instances: l, ok: true, env: map[Variable]Term{
			l: List(),
		}},
		{title: "4", template: x, goal: atomSemiColon.Apply(atomEqual.Apply(x, Integer(1)), atomEqual.Apply(x, Integer(1))), instances: s, ok: true, env: map[Variable]Term{
			s: List(Integer(1), Integer(1)),
		}},
		{title: "5", template: x, goal: atomSemiColon.Apply(atomEqual.Apply(x, Integer(2)), atomEqual.Apply(x, Integer(1))), instances: List(Integer(1), Integer(2)), ok: false},
		{title: "6", template: x, goal: goal, instances: s, err: instantiationError(nil)},
		{title: "7", template: x, goal: Integer(4), instances: s, err: typeError(validTypeCallable, Integer(4), nil)},

		// 8.10.1.3 Errors
		{title: "c", template: x, goal: atomSemiColon.Apply(atomEqual.Apply(x, Integer(1)), atomEqual.Apply(x, Integer(2))), instances: NewAtom("foo"), err: typeError(validTypeList, NewAtom("foo"), nil)},
	}

	var vm VM
	vm.Register2(atomEqual, Unify)
	vm.Register2(atomSemiColon, func(vm *VM, g1, g2 Term, k Cont, env *Env) *Promise {
		return Delay(func(context.Context) *Promise {
			return Call(vm, g1, k, env)
		}, func(context.Context) *Promise {
			return Call(vm, g2, k, env)
		})
	})
	vm.Register0(atomFail, func(*VM, Cont, *Env) *Promise {
		return Bool(false)
	})

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := FindAll(&vm, tt.template, tt.goal, tt.instances, func(env *Env) *Promise {
				for k, v := range tt.env {
					_, ok := env.Unify(v, k)
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
	order := NewVariable()

	tests := []struct {
		title       string
		order, x, y Term
		ok          bool
		err         error
		env         map[Variable]Term
	}{
		// 8.4.2.4 Examples
		{title: `compare(Order, 3, 5).`, order: order, x: Integer(3), y: Integer(5), ok: true, env: map[Variable]Term{
			order: atomLessThan,
		}},
		{title: `compare(Order, d, d).`, order: order, x: NewAtom("d"), y: NewAtom("d"), ok: true, env: map[Variable]Term{
			order: atomEqual,
		}},
		{title: `compare(Order, Order, <).`, order: order, x: order, y: atomLessThan, ok: true, env: map[Variable]Term{
			order: atomLessThan,
		}},
		{title: `compare(<, <, <).`, order: atomLessThan, x: atomLessThan, y: atomLessThan, ok: false},
		{title: `compare(1+2, 3, 3.0).`, order: atomPlus.Apply(Integer(1), Integer(2)), x: Integer(3), y: Float(3.0), ok: false, err: typeError(validTypeAtom, atomPlus.Apply(Integer(1), Integer(2)), nil)},
		{title: `compare(>=, 3, 3.0).`, order: NewAtom(">="), x: Integer(3), y: Float(3.0), ok: false, err: domainError(validDomainOrder, NewAtom(">="), nil)},

		{title: `missing case for >`, order: atomGreaterThan, x: Integer(2), y: Integer(1), ok: true},
	}

	for _, tt := range tests {
		ok, err := Compare(nil, tt.order, tt.x, tt.y, func(env *Env) *Promise {
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
			ok, err := Between(nil, Integer(1), Integer(3), Integer(2), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("equal to lower", func(t *testing.T) {
			ok, err := Between(nil, Integer(1), Integer(3), Integer(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("equal to upper", func(t *testing.T) {
			ok, err := Between(nil, Integer(1), Integer(3), Integer(3), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("value, lower, higher are all equal", func(t *testing.T) {
			ok, err := Between(nil, Integer(1), Integer(1), Integer(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("value, lower, higher are all MaxInt64", func(t *testing.T) {
			ok, err := Between(nil, Integer(math.MaxInt64), Integer(math.MaxInt64), Integer(math.MaxInt64), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("equal to lower, but lower > upper", func(t *testing.T) {
			ok, err := Between(nil, Integer(3), Integer(1), Integer(3), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("less than lower", func(t *testing.T) {
			ok, err := Between(nil, Integer(1), Integer(3), Integer(0), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("greater than upper", func(t *testing.T) {
			ok, err := Between(nil, Integer(1), Integer(3), Integer(100), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("value is a variable", func(t *testing.T) {
		t.Run("lower and upper are equal integers", func(t *testing.T) {
			value := NewVariable()
			ok, err := Between(nil, Integer(1), Integer(1), value, func(env *Env) *Promise {
				assert.Equal(t, Integer(1), env.Resolve(value))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("lower and upper are MaxInt64", func(t *testing.T) {
			value := NewVariable()
			ok, err := Between(nil, Integer(math.MaxInt64), Integer(math.MaxInt64), value, func(env *Env) *Promise {
				assert.Equal(t, Integer(math.MaxInt64), env.Resolve(value))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("multiple choice points", func(t *testing.T) {
			var n int
			value := NewVariable()
			ok, err := Between(nil, Integer(0), Integer(3), value, func(env *Env) *Promise {
				assert.Equal(t, Integer(n), env.Resolve(value))
				n++
				return Bool(false)
			}, nil).Force(context.Background())
			assert.Equal(t, n, 4)
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("lower > upper", func(t *testing.T) {
			value := NewVariable()
			ok, err := Between(nil, Integer(3), Integer(0), value, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("lower is uninstantiated", func(t *testing.T) {
		_, err := Between(nil, NewVariable(), Integer(2), Integer(1), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
	})

	t.Run("upper is uninstantiated", func(t *testing.T) {
		_, err := Between(nil, Integer(1), NewVariable(), Integer(1), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
	})

	t.Run("lower is not an integer", func(t *testing.T) {
		_, err := Between(nil, NewAtom("inf"), Integer(2), Integer(1), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, NewAtom("inf"), nil), err)
	})

	t.Run("upper is not an integer", func(t *testing.T) {
		_, err := Between(nil, Integer(1), NewAtom("inf"), Integer(1), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, NewAtom("inf"), nil), err)
	})

	t.Run("value is not an integer or variable", func(t *testing.T) {
		_, err := Between(nil, Integer(1), Integer(1), NewAtom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, NewAtom("foo"), nil), err)
	})
}

func TestSort(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		t.Run("variable", func(t *testing.T) {
			sorted := NewVariable()
			ok, err := Sort(nil, List(NewAtom("a"), NewAtom("c"), NewAtom("b"), NewAtom("a")), sorted, func(env *Env) *Promise {
				assert.Equal(t, List(NewAtom("a"), NewAtom("b"), NewAtom("c")), env.Resolve(sorted))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("list", func(t *testing.T) {
			ok, err := Sort(nil, List(NewAtom("a"), NewAtom("c"), NewAtom("b"), NewAtom("a")), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("list is a partial list", func(t *testing.T) {
		_, err := Sort(nil, PartialList(NewVariable(), NewAtom("a"), NewAtom("b")), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
	})

	t.Run("list is neither a partial list nor a list", func(t *testing.T) {
		_, err := Sort(nil, NewAtom("a"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeList, NewAtom("a"), nil), err)
	})

	t.Run("sorted is neither a partial list nor a list", func(t *testing.T) {
		t.Run("obviously not a list", func(t *testing.T) {
			_, err := Sort(nil, List(NewAtom("a")), NewAtom("a"), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeList, NewAtom("a"), nil), err)
		})

		t.Run("list-ish", func(t *testing.T) {
			_, err := Sort(nil, List(NewAtom("a")), &compound{functor: atomDot, args: []Term{NewAtom("a")}}, Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeList, &compound{functor: atomDot, args: []Term{NewAtom("a")}}, nil), err)
		})
	})
}

func TestKeySort(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		t.Run("variable", func(t *testing.T) {
			sorted := NewVariable()
			ok, err := KeySort(nil, List(
				pair(NewAtom("c"), NewAtom("4")),
				pair(NewAtom("b"), NewAtom("3")),
				pair(NewAtom("a"), NewAtom("1")),
				pair(NewAtom("a"), NewAtom("2")),
			), sorted, func(env *Env) *Promise {
				assert.Equal(t, List(
					pair(NewAtom("a"), NewAtom("1")),
					pair(NewAtom("a"), NewAtom("2")),
					pair(NewAtom("b"), NewAtom("3")),
					pair(NewAtom("c"), NewAtom("4")),
				), env.Resolve(sorted))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("list", func(t *testing.T) {
			second := NewVariable()
			ok, err := KeySort(nil, List(
				pair(NewAtom("c"), NewAtom("4")),
				pair(NewAtom("b"), NewAtom("3")),
				pair(NewAtom("a"), NewAtom("1")),
				pair(NewAtom("a"), NewAtom("2")),
			), List(
				pair(NewAtom("a"), NewAtom("1")),
				second,
				pair(NewAtom("b"), NewAtom("3")),
				pair(NewAtom("c"), NewAtom("4")),
			), func(env *Env) *Promise {
				assert.Equal(t, pair(NewAtom("a"), NewAtom("2")), env.Resolve(second))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("pairs is a partial list", func(t *testing.T) {
		_, err := KeySort(nil, PartialList(NewVariable(), pair(NewAtom("a"), Integer(1))), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
	})

	t.Run("pairs is neither a partial list nor a list", func(t *testing.T) {
		_, err := KeySort(nil, NewAtom("a"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeList, NewAtom("a"), nil), err)
	})

	t.Run("sorted is neither a partial list nor a list", func(t *testing.T) {
		_, err := KeySort(nil, List(), NewAtom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeList, NewAtom("foo"), nil), err)
	})

	t.Run("an element of a list prefix of pairs is a variable", func(t *testing.T) {
		_, err := KeySort(nil, List(NewVariable()), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
	})

	t.Run("an element of a list prefix of pairs is neither a variable nor a compound term with principal functor (-)/2", func(t *testing.T) {
		t.Run("atomic", func(t *testing.T) {
			_, err := KeySort(nil, List(NewAtom("foo")), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypePair, NewAtom("foo"), nil), err)
		})

		t.Run("compound", func(t *testing.T) {
			_, err := KeySort(nil, List(NewAtom("f").Apply(NewAtom("a"))), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypePair, NewAtom("f").Apply(NewAtom("a")), nil), err)
		})
	})

	t.Run("an element of a list prefix of sorted is neither a variable nor a compound term with principal functor (-)/2", func(t *testing.T) {
		t.Run("atomic", func(t *testing.T) {
			_, err := KeySort(nil, List(), List(NewAtom("foo")), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypePair, NewAtom("foo"), nil), err)
		})

		t.Run("compound", func(t *testing.T) {
			_, err := KeySort(nil, List(), List(NewAtom("f").Apply(NewAtom("a"))), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypePair, NewAtom("f").Apply(NewAtom("a")), nil), err)
		})
	})
}

func TestThrow(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		ok, err := Throw(nil, NewAtom("a"), Success, nil).Force(context.Background())
		assert.Equal(t, Exception{term: NewAtom("a")}, err)
		assert.False(t, ok)
	})

	t.Run("ball is a variable", func(t *testing.T) {
		ok, err := Throw(nil, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})
}

func TestCatch(t *testing.T) {
	var vm VM
	vm.Register2(atomEqual, Unify)
	vm.Register1(NewAtom("throw"), Throw)
	vm.Register0(atomTrue, func(_ *VM, k Cont, env *Env) *Promise {
		return k(env)
	})
	vm.Register0(atomFail, func(*VM, Cont, *Env) *Promise {
		return Bool(false)
	})

	t.Run("match", func(t *testing.T) {
		v := NewVariable()
		ok, err := Catch(&vm, &compound{
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
		ok, err := Catch(&vm, &compound{
			functor: NewAtom("throw"),
			args:    []Term{NewAtom("a")},
		}, NewAtom("b"), atomFail, Success, nil).Force(context.Background())
		assert.False(t, ok)
		ex, ok := err.(Exception)
		assert.True(t, ok)
		assert.Equal(t, NewAtom("a"), ex.term)
	})

	t.Run("true", func(t *testing.T) {
		ok, err := Catch(&vm, atomTrue, NewAtom("b"), atomFail, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("false", func(t *testing.T) {
		ok, err := Catch(&vm, atomFail, NewAtom("b"), atomFail, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("non-exception error", func(t *testing.T) {
		ok, err := Catch(&vm, atomTrue, NewVariable(), atomTrue, func(env *Env) *Promise {
			return Error(errors.New("failed"))
		}, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestCurrentPredicate(t *testing.T) {
	t.Run("user defined predicate", func(t *testing.T) {
		vm := VM{procedures: map[procedureIndicator]procedure{
			{name: NewAtom("foo"), arity: 1}: &userDefined{},
		}}
		ok, err := CurrentPredicate(&vm, &compound{
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

		v := NewVariable()

		vm := VM{procedures: map[procedureIndicator]procedure{
			{name: NewAtom("foo"), arity: 1}: &userDefined{},
			{name: NewAtom("bar"), arity: 1}: &userDefined{},
			{name: NewAtom("baz"), arity: 1}: &userDefined{},
		}}
		ok, err := CurrentPredicate(&vm, v, func(env *Env) *Promise {
			c, ok := env.Resolve(v).(*compound)
			assert.True(t, ok)
			assert.Equal(t, atomSlash, c.Functor())
			assert.Equal(t, 2, c.Arity())
			assert.Equal(t, Integer(1), c.Arg(1))
			switch c.Arg(0) {
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
		vm := VM{procedures: map[procedureIndicator]procedure{
			{name: atomEqual, arity: 2}: Predicate2(Unify),
		}}
		ok, err := CurrentPredicate(&vm, &compound{
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
			var vm VM
			ok, err := CurrentPredicate(&vm, NewAtom("foo"), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypePredicateIndicator, NewAtom("foo"), nil), err)
			assert.False(t, ok)
		})

		t.Run("compound", func(t *testing.T) {
			t.Run("non slash", func(t *testing.T) {
				var vm VM
				ok, err := CurrentPredicate(&vm, &compound{
					functor: NewAtom("f"),
					args:    []Term{NewAtom("a")},
				}, Success, nil).Force(context.Background())
				assert.Equal(t, typeError(validTypePredicateIndicator, &compound{
					functor: NewAtom("f"),
					args:    []Term{NewAtom("a")},
				}, nil), err)
				assert.False(t, ok)
			})

			t.Run("slash but number", func(t *testing.T) {
				var vm VM
				ok, err := CurrentPredicate(&vm, &compound{
					functor: atomSlash,
					args:    []Term{Integer(0), Integer(0)},
				}, Success, nil).Force(context.Background())
				assert.Equal(t, typeError(validTypePredicateIndicator, &compound{
					functor: atomSlash,
					args:    []Term{Integer(0), Integer(0)},
				}, nil), err)
				assert.False(t, ok)
			})

			t.Run("slash but path", func(t *testing.T) {
				var vm VM
				ok, err := CurrentPredicate(&vm, &compound{
					functor: atomSlash,
					args:    []Term{NewAtom("foo"), NewAtom("bar")},
				}, Success, nil).Force(context.Background())
				assert.Equal(t, typeError(validTypePredicateIndicator, &compound{
					functor: atomSlash,
					args:    []Term{NewAtom("foo"), NewAtom("bar")},
				}, nil), err)
				assert.False(t, ok)
			})
		})
	})
}

func TestAssertz(t *testing.T) {
	t.Run("append", func(t *testing.T) {
		var vm VM

		ok, err := Assertz(&vm, &compound{
			functor: NewAtom("foo"),
			args:    []Term{NewAtom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Assertz(&vm, &compound{
			functor: NewAtom("foo"),
			args:    []Term{NewAtom("b")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, &userDefined{dynamic: true, clauses: []clause{
			{
				pi: procedureIndicator{
					name:  NewAtom("foo"),
					arity: 1,
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
				pi: procedureIndicator{
					name:  NewAtom("foo"),
					arity: 1,
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
		}}, vm.procedures[procedureIndicator{
			name:  NewAtom("foo"),
			arity: 1,
		}])
	})

	t.Run("clause is a variable", func(t *testing.T) {
		var vm VM
		ok, err := Assertz(&vm, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("clause is neither a variable, nor callable", func(t *testing.T) {
		var vm VM
		ok, err := Assertz(&vm, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("head is a variable", func(t *testing.T) {
		var vm VM
		ok, err := Assertz(&vm, &compound{
			functor: atomIf,
			args:    []Term{NewVariable(), atomTrue},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("head is neither a variable, nor callable", func(t *testing.T) {
		var vm VM
		ok, err := Assertz(&vm, &compound{
			functor: atomIf,
			args:    []Term{Integer(0), atomTrue},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("body contains a term which is not callable", func(t *testing.T) {
		var vm VM
		ok, err := Assertz(&vm, &compound{
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
		assert.Equal(t, typeError(validTypeCallable, &compound{
			functor: atomComma,
			args: []Term{
				atomTrue,
				Integer(0),
			},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("static", func(t *testing.T) {
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: NewAtom("foo"), arity: 0}: &userDefined{dynamic: false},
			},
		}

		ok, err := Assertz(&vm, NewAtom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationModify, permissionTypeStaticProcedure, &compound{
			functor: atomSlash,
			args: []Term{
				NewAtom("foo"),
				Integer(0),
			},
		}, nil), err)
		assert.False(t, ok)
	})
}

func TestAsserta(t *testing.T) {
	t.Run("fact", func(t *testing.T) {
		var vm VM
		ok, err := Asserta(&vm, &compound{
			functor: NewAtom("foo"),
			args:    []Term{NewAtom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Asserta(&vm, &compound{
			functor: NewAtom("foo"),
			args:    []Term{NewAtom("b")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, &userDefined{dynamic: true, clauses: []clause{
			{
				pi: procedureIndicator{name: NewAtom("foo"), arity: 1},
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
				pi: procedureIndicator{name: NewAtom("foo"), arity: 1},
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
		}}, vm.procedures[procedureIndicator{name: NewAtom("foo"), arity: 1}])
	})

	t.Run("rule", func(t *testing.T) {
		var vm VM
		ok, err := Asserta(&vm, &compound{
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

		ok, err = Asserta(&vm, &compound{
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
				pi: procedureIndicator{name: NewAtom("foo"), arity: 0},
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
					procedureIndicator{name: NewAtom("p"), arity: 1},
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
				pi: procedureIndicator{name: NewAtom("foo"), arity: 0},
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
					procedureIndicator{name: NewAtom("p"), arity: 1},
				},
				bytecode: bytecode{
					{opcode: opEnter},
					{opcode: opConst, operand: 0},
					{opcode: opCall, operand: 1},
					{opcode: opExit},
				},
			},
		}}, vm.procedures[procedureIndicator{name: NewAtom("foo"), arity: 0}])
	})

	t.Run("clause is a variable", func(t *testing.T) {
		var vm VM
		ok, err := Asserta(&vm, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("clause is neither a variable, nor callable", func(t *testing.T) {
		var vm VM
		ok, err := Asserta(&vm, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("head is a variable", func(t *testing.T) {
		var vm VM
		ok, err := Asserta(&vm, &compound{
			functor: atomIf,
			args:    []Term{NewVariable(), atomTrue},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("head is neither a variable, nor callable", func(t *testing.T) {
		var vm VM
		ok, err := Asserta(&vm, &compound{
			functor: atomIf,
			args:    []Term{Integer(0), atomTrue},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("body is not callable", func(t *testing.T) {
		var vm VM
		ok, err := Asserta(&vm, &compound{
			functor: atomIf,
			args:    []Term{NewAtom("foo"), Integer(0)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("body contains a term which is not callable", func(t *testing.T) {
		var vm VM
		ok, err := Asserta(&vm, &compound{
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
		assert.Equal(t, typeError(validTypeCallable, &compound{
			functor: atomComma,
			args: []Term{
				atomTrue,
				Integer(0)},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("static", func(t *testing.T) {
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: NewAtom("foo"), arity: 0}: &userDefined{dynamic: false},
			},
		}

		ok, err := Asserta(&vm, NewAtom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationModify, permissionTypeStaticProcedure, &compound{
			functor: atomSlash,
			args: []Term{
				NewAtom("foo"),
				Integer(0),
			},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("cut", func(t *testing.T) {
		var vm VM
		ok, err := Asserta(&vm, &compound{
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

func TestRetract(t *testing.T) {
	t.Run("retract the first one", func(t *testing.T) {
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: NewAtom("foo"), arity: 1}: &userDefined{dynamic: true, clauses: []clause{
					{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("a")}}},
					{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("b")}}},
					{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("c")}}},
				}},
			},
		}

		ok, err := Retract(&vm, &compound{
			functor: NewAtom("foo"),
			args:    []Term{NewVariable()},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, &userDefined{dynamic: true, clauses: []clause{
			{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("b")}}},
			{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("c")}}},
		}}, vm.procedures[procedureIndicator{name: NewAtom("foo"), arity: 1}])
	})

	t.Run("retract the specific one", func(t *testing.T) {
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: NewAtom("foo"), arity: 1}: &userDefined{dynamic: true, clauses: []clause{
					{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("a")}}},
					{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("b")}}},
					{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("c")}}},
				}},
			},
		}

		ok, err := Retract(&vm, &compound{
			functor: NewAtom("foo"),
			args:    []Term{NewAtom("b")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, &userDefined{dynamic: true, clauses: []clause{
			{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("a")}}},
			{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("c")}}},
		}}, vm.procedures[procedureIndicator{name: NewAtom("foo"), arity: 1}])
	})

	t.Run("retract all", func(t *testing.T) {
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: NewAtom("foo"), arity: 1}: &userDefined{dynamic: true, clauses: []clause{
					{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("a")}}},
					{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("b")}}},
					{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("c")}}},
				}},
			},
		}

		ok, err := Retract(&vm, &compound{
			functor: NewAtom("foo"),
			args:    []Term{NewVariable()},
		}, Failure, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
		assert.Empty(t, vm.procedures[procedureIndicator{name: NewAtom("foo"), arity: 1}].(*userDefined).clauses)
	})

	t.Run("variable", func(t *testing.T) {
		var vm VM
		ok, err := Retract(&vm, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("not callable", func(t *testing.T) {
		var vm VM
		ok, err := Retract(&vm, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("no clause matches", func(t *testing.T) {
		var vm VM

		ok, err := Retract(&vm, &compound{
			functor: NewAtom("foo"),
			args:    []Term{NewVariable()},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("static", func(t *testing.T) {
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: NewAtom("foo"), arity: 0}: &userDefined{dynamic: false},
			},
		}

		ok, err := Retract(&vm, NewAtom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationModify, permissionTypeStaticProcedure, &compound{
			functor: atomSlash,
			args:    []Term{NewAtom("foo"), Integer(0)},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("exception in continuation", func(t *testing.T) {
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: NewAtom("foo"), arity: 1}: &userDefined{dynamic: true, clauses: []clause{
					{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("a")}}},
				}},
			},
		}

		ok, err := Retract(&vm, &compound{
			functor: NewAtom("foo"),
			args:    []Term{NewVariable()},
		}, func(_ *Env) *Promise {
			return Error(errors.New("failed"))
		}, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)

		// removed
		assert.Empty(t, vm.procedures[procedureIndicator{name: NewAtom("foo"), arity: 1}].(*userDefined).clauses)
	})
}

func TestAbolish(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: NewAtom("foo"), arity: 1}: &userDefined{dynamic: true, clauses: []clause{
					{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("a")}}},
					{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("b")}}},
					{raw: &compound{functor: NewAtom("foo"), args: []Term{NewAtom("c")}}},
				}},
			},
		}

		ok, err := Abolish(&vm, &compound{
			functor: atomSlash,
			args:    []Term{NewAtom("foo"), Integer(1)},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		_, ok = vm.procedures[procedureIndicator{name: NewAtom("foo"), arity: 1}]
		assert.False(t, ok)
	})

	t.Run("pi is a variable", func(t *testing.T) {
		var vm VM
		ok, err := Abolish(&vm, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and either Name or Arity is a variable", func(t *testing.T) {
		t.Run("Name is a variable", func(t *testing.T) {
			var vm VM
			ok, err := Abolish(&vm, &compound{
				functor: atomSlash,
				args:    []Term{NewVariable(), Integer(2)},
			}, Success, nil).Force(context.Background())
			assert.Equal(t, instantiationError(nil), err)
			assert.False(t, ok)
		})

		t.Run("Arity is a variable", func(t *testing.T) {
			var vm VM
			ok, err := Abolish(&vm, &compound{
				functor: atomSlash,
				args:    []Term{NewAtom("foo"), NewVariable()},
			}, Success, nil).Force(context.Background())
			assert.Equal(t, instantiationError(nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("pi is neither a variable nor a predicate indicator", func(t *testing.T) {
		t.Run("compound", func(t *testing.T) {
			var vm VM
			ok, err := Abolish(&vm, atomPlus.Apply(NewAtom("foo"), Integer(1)), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypePredicateIndicator, atomPlus.Apply(NewAtom("foo"), Integer(1)), nil), err)
			assert.False(t, ok)
		})

		t.Run("not a comnpound", func(t *testing.T) {
			var vm VM
			ok, err := Abolish(&vm, Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypePredicateIndicator, Integer(0), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("pi is a term Name/Arity and Name is neither a variable nor an atom", func(t *testing.T) {
		var vm VM
		ok, err := Abolish(&vm, &compound{
			functor: atomSlash,
			args:    []Term{Integer(0), Integer(2)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and Arity is neither a variable nor an integer", func(t *testing.T) {
		var vm VM
		ok, err := Abolish(&vm, &compound{
			functor: atomSlash,
			args:    []Term{NewAtom("foo"), NewAtom("bar")},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, NewAtom("bar"), nil), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and Arity is an integer less than zero", func(t *testing.T) {
		var vm VM
		ok, err := Abolish(&vm, &compound{
			functor: atomSlash,
			args:    []Term{NewAtom("foo"), Integer(-2)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainNotLessThanZero, Integer(-2), nil), err)
		assert.False(t, ok)
	})

	t.Run("The predicate indicator pi is that of a static procedure", func(t *testing.T) {
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: NewAtom("foo"), arity: 0}: &userDefined{dynamic: false},
			},
		}
		ok, err := Abolish(&vm, &compound{
			functor: atomSlash,
			args:    []Term{NewAtom("foo"), Integer(0)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationModify, permissionTypeStaticProcedure, &compound{
			functor: atomSlash,
			args:    []Term{NewAtom("foo"), Integer(0)},
		}, nil), err)
		assert.False(t, ok)
	})
}

func TestCurrentInput(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		var s Stream
		vm := VM{
			input: &s,
		}

		ok, err := CurrentInput(&vm, &s, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("stream is neither a variable nor a stream", func(t *testing.T) {
		var vm VM
		ok, err := CurrentInput(&vm, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainStream, Integer(0), nil), err)
		assert.False(t, ok)
	})
}

func TestCurrentOutput(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		var s Stream
		vm := VM{
			output: &s,
		}

		ok, err := CurrentOutput(&vm, &s, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("stream is neither a variable nor a stream", func(t *testing.T) {
		var vm VM
		ok, err := CurrentOutput(&vm, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainStream, Integer(0), nil), err)
		assert.False(t, ok)
	})
}

func TestSetInput(t *testing.T) {
	foo, bar := NewAtom("foo"), NewAtom("bar")
	input := Stream{mode: ioModeRead, alias: foo}
	output := Stream{mode: ioModeAppend}
	stream := NewVariable()

	var vm VM
	vm.streams.add(&input)

	tests := []struct {
		title         string
		streamOrAlias Term
		ok            bool
		err           error
		input         *Stream
	}{
		{title: "stream", streamOrAlias: &input, ok: true, input: &input},
		{title: "alias", streamOrAlias: foo, ok: true, input: &input},

		// 8.11.3.3 Errors
		{title: "a", streamOrAlias: stream, err: instantiationError(nil)},
		{title: "b", streamOrAlias: Integer(0), err: domainError(validDomainStreamOrAlias, Integer(0), nil)},
		{title: "c", streamOrAlias: bar, err: existenceError(objectTypeStream, bar, nil)},
		{title: "d", streamOrAlias: &output, err: permissionError(operationInput, permissionTypeStream, &output, nil)},
	}

	for _, tt := range tests {
		ok, err := SetInput(&vm, tt.streamOrAlias, Success, nil).Force(context.Background())
		assert.Equal(t, tt.ok, ok)
		assert.Equal(t, tt.err, err)
		if err == nil {
			assert.Equal(t, tt.input, vm.input)
		}
	}
}

func TestSetOutput(t *testing.T) {
	foo, bar := NewAtom("foo"), NewAtom("bar")
	input := Stream{mode: ioModeRead}
	output := Stream{mode: ioModeAppend, alias: foo}
	stream := NewVariable()

	var vm VM
	vm.streams.add(&output)

	tests := []struct {
		title         string
		streamOrAlias Term
		ok            bool
		err           error
		output        *Stream
	}{
		{title: "stream", streamOrAlias: &output, ok: true, output: &output},
		{title: "alias", streamOrAlias: foo, ok: true, output: &output},

		// 8.11.4.3 Errors
		{title: "a", streamOrAlias: stream, err: instantiationError(nil)},
		{title: "b", streamOrAlias: Integer(0), err: domainError(validDomainStreamOrAlias, Integer(0), nil)},
		{title: "c", streamOrAlias: bar, err: existenceError(objectTypeStream, bar, nil)},
		{title: "d", streamOrAlias: &input, err: permissionError(operationOutput, permissionTypeStream, &input, nil)},
	}

	for _, tt := range tests {
		ok, err := SetOutput(&vm, tt.streamOrAlias, Success, nil).Force(context.Background())
		assert.Equal(t, tt.ok, ok)
		assert.Equal(t, tt.err, err)
		if err == nil {
			assert.Equal(t, tt.output, vm.output)
		}
	}
}

func TestOpen(t *testing.T) {
	var vm VM

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
			v := NewVariable()
			ok, err := Open(&vm, NewAtom(f.Name()), atomRead, v, List(
				atomAlias.Apply(atomInput),
			), func(env *Env) *Promise {
				ref, ok := env.lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*Stream)
				assert.True(t, ok)

				l, ok := vm.streams.lookup(atomInput)
				assert.True(t, ok)
				assert.Equal(t, l, s)

				b, err := io.ReadAll(s.buf)
				assert.NoError(t, err)
				assert.Equal(t, "test\n", string(b))

				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("type text", func(t *testing.T) {
			v := NewVariable()
			ok, err := Open(&vm, NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomType,
				args:    []Term{atomText},
			}), func(env *Env) *Promise {
				ref, ok := env.lookup(v)
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
			v := NewVariable()
			ok, err := Open(&vm, NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomType,
				args:    []Term{atomBinary},
			}), func(env *Env) *Promise {
				ref, ok := env.lookup(v)
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
			v := NewVariable()
			ok, err := Open(&vm, NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomReposition,
				args:    []Term{atomTrue},
			}), func(env *Env) *Promise {
				ref, ok := env.lookup(v)
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
			v := NewVariable()
			ok, err := Open(&vm, NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomReposition,
				args:    []Term{atomFalse},
			}), func(env *Env) *Promise {
				ref, ok := env.lookup(v)
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
			v := NewVariable()
			ok, err := Open(&vm, NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomEOFAction,
				args:    []Term{atomError},
			}), func(env *Env) *Promise {
				ref, ok := env.lookup(v)
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
			v := NewVariable()
			ok, err := Open(&vm, NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomEOFAction,
				args:    []Term{atomEOFCode},
			}), func(env *Env) *Promise {
				ref, ok := env.lookup(v)
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
			v := NewVariable()
			ok, err := Open(&vm, NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomEOFAction,
				args:    []Term{atomReset},
			}), func(env *Env) *Promise {
				ref, ok := env.lookup(v)
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
			v := NewVariable()
			ok, err := Open(&vm, NewAtom(f.Name()), atomRead, v, List(&compound{
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
			v := NewVariable()
			ok, err := Open(&vm, NewAtom(f.Name()), atomRead, v, List(&compound{
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
			v := NewVariable()
			ok, err := Open(&vm, NewAtom(f.Name()), atomRead, v, List(&compound{
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
			v := NewVariable()
			ok, err := Open(&vm, NewAtom(f.Name()), atomRead, v, List(&compound{
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

		v := NewVariable()

		ok, err := Open(&vm, NewAtom(n), atomWrite, v, List(&compound{
			functor: atomAlias,
			args:    []Term{atomOutput},
		}), func(env *Env) *Promise {
			ref, ok := env.lookup(v)
			assert.True(t, ok)
			s, ok := ref.(*Stream)
			assert.True(t, ok)

			l, ok := vm.streams.lookup(atomOutput)
			assert.True(t, ok)
			assert.Equal(t, l, s)

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

		v := NewVariable()

		ok, err := Open(&vm, NewAtom(f.Name()), atomAppend, v, List(&compound{
			functor: atomAlias,
			args:    []Term{atomAppend},
		}), func(env *Env) *Promise {
			ref, ok := env.lookup(v)
			assert.True(t, ok)
			s, ok := ref.(*Stream)
			assert.True(t, ok)

			l, ok := vm.streams.lookup(atomAppend)
			assert.True(t, ok)
			assert.Equal(t, l, s)

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
		var vm VM
		ok, err := Open(&vm, NewVariable(), atomRead, NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("mode is a variable", func(t *testing.T) {
		var vm VM
		ok, err := Open(&vm, NewAtom("/dev/null"), NewVariable(), NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("options is a partial list or a list with an element E which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			var vm VM
			ok, err := Open(&vm, NewAtom("/dev/null"), atomRead, NewVariable(), PartialList(NewVariable(),
				atomType.Apply(atomText),
				atomAlias.Apply(NewAtom("foo")),
			), Success, nil).Force(context.Background())
			assert.Equal(t, instantiationError(nil), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			var vm VM
			ok, err := Open(&vm, NewAtom("/dev/null"), atomRead, NewVariable(), List(
				NewVariable(),
				&compound{functor: atomType, args: []Term{atomText}},
				&compound{functor: atomAlias, args: []Term{NewAtom("foo")}},
			), Success, nil).Force(context.Background())
			assert.Equal(t, instantiationError(nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("mode is neither a variable nor an atom", func(t *testing.T) {
		var vm VM
		ok, err := Open(&vm, NewAtom("/dev/null"), Integer(0), NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		var vm VM
		ok, err := Open(&vm, NewAtom("/dev/null"), atomRead, NewVariable(), NewAtom("list"), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeList, NewAtom("list"), nil), err)
		assert.False(t, ok)
	})

	t.Run("stream is not a variable", func(t *testing.T) {
		var vm VM
		ok, err := Open(&vm, NewAtom("/dev/null"), atomRead, NewAtom("stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("sourceSink is neither a variable nor a source/sink", func(t *testing.T) {
		var vm VM
		ok, err := Open(&vm, Integer(0), atomRead, NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainSourceSink, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("mode is an atom but not an input/output mode", func(t *testing.T) {
		var vm VM
		ok, err := Open(&vm, NewAtom("/dev/null"), NewAtom("foo"), NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainIOMode, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the options list is neither a variable nor a stream-option", func(t *testing.T) {
		var vm VM
		for _, o := range []Term{
			NewAtom("foo"),
			&compound{functor: NewAtom("foo"), args: []Term{NewAtom("bar")}},
			&compound{functor: atomAlias, args: []Term{Integer(0)}},
			&compound{functor: atomType, args: []Term{Integer(0)}},
			&compound{functor: atomReposition, args: []Term{Integer(0)}},
			&compound{functor: atomEOFAction, args: []Term{Integer(0)}},
		} {
			ok, err := Open(&vm, NewAtom("/dev/null"), atomRead, NewVariable(), List(o), Success, nil).Force(context.Background())
			assert.Equal(t, domainError(validDomainStreamOption, o, nil), err)
			assert.False(t, ok)
		}
	})

	// Derived from 5.5.12 Options in Cor.3
	t.Run("a component of an element E of the options list is a variable", func(t *testing.T) {
		var vm VM
		for _, o := range []Term{
			NewVariable(),
			&compound{functor: atomAlias, args: []Term{NewVariable()}},
			&compound{functor: atomType, args: []Term{NewVariable()}},
			&compound{functor: atomReposition, args: []Term{NewVariable()}},
			&compound{functor: atomEOFAction, args: []Term{NewVariable()}},
		} {
			ok, err := Open(&vm, NewAtom("/dev/null"), atomRead, NewVariable(), List(o), Success, nil).Force(context.Background())
			assert.Equal(t, instantiationError(nil), err)
			assert.False(t, ok)
		}
	})

	t.Run("the source/sink specified by sourceSink does not exist", func(t *testing.T) {
		f, err := os.CreateTemp("", "open_test_existence")
		assert.NoError(t, err)
		assert.NoError(t, os.Remove(f.Name()))

		var vm VM
		ok, err := Open(&vm, NewAtom(f.Name()), atomRead, NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, existenceError(objectTypeSourceSink, NewAtom(f.Name()), nil), err)
		assert.False(t, ok)
	})

	t.Run("the source/sink specified by sourceSink cannot be opened", func(t *testing.T) {
		f, err := os.CreateTemp("", "open_test_permission")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, os.Remove(f.Name()))
		}()

		assert.NoError(t, f.Chmod(0200))

		var vm VM
		ok, err := Open(&vm, NewAtom(f.Name()), atomRead, NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationOpen, permissionTypeSourceSink, NewAtom(f.Name()), nil), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the options list is alias and A is already associated with an open stream", func(t *testing.T) {
		f, err := os.CreateTemp("", "open_test_dup_alias")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, os.Remove(f.Name()))
		}()

		var vm VM
		vm.streams.add(&Stream{alias: NewAtom("foo")})
		ok, err := Open(&vm, NewAtom(f.Name()), atomRead, NewVariable(), List(&compound{
			functor: atomAlias,
			args:    []Term{NewAtom("foo")},
		}), Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationOpen, permissionTypeSourceSink, &compound{
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

		var vm VM
		_, err := Open(&vm, NewAtom("foo"), atomRead, NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, errors.New("failed"), err)
	})
}

func TestClose(t *testing.T) {
	t.Run("without options", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			t.Run("stream", func(t *testing.T) {
				var m mockCloser
				m.On("Close").Return(nil).Once()
				defer m.AssertExpectations(t)

				var vm VM
				ok, err := Close(&vm, &Stream{sourceSink: &m}, List(), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("alias", func(t *testing.T) {
				var m mockCloser
				m.On("Close").Return(nil).Once()
				defer m.AssertExpectations(t)

				foo := NewAtom("foo")

				var vm VM
				vm.streams.add(&Stream{sourceSink: &m, alias: foo})
				ok, err := Close(&vm, foo, List(), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.True(t, ok)
			})
		})

		t.Run("ng", func(t *testing.T) {
			var m mockCloser
			m.On("Close").Return(errors.New("failed")).Once()
			defer m.AssertExpectations(t)

			var vm VM
			_, err := Close(&vm, &Stream{sourceSink: &m}, List(), Success, nil).Force(context.Background())
			assert.Equal(t, errors.New("failed"), err)
		})
	})

	t.Run("force false", func(t *testing.T) {
		var m mockCloser
		m.On("Close").Return(errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		var vm VM
		_, err := Close(&vm, &Stream{sourceSink: &m}, List(atomForce.Apply(atomFalse)), Success, nil).Force(context.Background())
		assert.Equal(t, errors.New("failed"), err)
	})

	t.Run("force true", func(t *testing.T) {
		var m mockCloser
		m.On("Close").Return(errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		var vm VM
		ok, err := Close(&vm, &Stream{sourceSink: &m}, List(atomForce.Apply(atomTrue)), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		var m mockCloser
		m.On("Close").Return(nil).Once()
		defer m.AssertExpectations(t)

		foo := NewAtom("foo")
		s := &Stream{sourceSink: &m, alias: foo}

		var vm VM
		vm.streams.add(s)
		ok, err := Close(&vm, NewAtom("foo"), List(), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias ia a variable", func(t *testing.T) {
		var vm VM
		ok, err := Close(&vm, NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("options is a partial list or a list with an element E which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			var vm VM
			ok, err := Close(&vm, &Stream{}, PartialList(NewVariable(),
				atomForce.Apply(atomTrue),
			), Success, nil).Force(context.Background())
			assert.Equal(t, instantiationError(nil), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			var vm VM
			ok, err := Close(&vm, &Stream{}, List(NewVariable(), atomForce.Apply(atomTrue)), Success, nil).Force(context.Background())
			assert.Equal(t, instantiationError(nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		var vm VM
		ok, err := Close(&vm, &Stream{}, NewAtom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeList, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream-term or alias", func(t *testing.T) {
		var vm VM
		ok, err := Close(&vm, Integer(0), List(), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the Options list is neither a variable nor a stream-option", func(t *testing.T) {
		t.Run("not a compound", func(t *testing.T) {
			var vm VM
			ok, err := Close(&vm, &Stream{}, List(NewAtom("foo")), Success, nil).Force(context.Background())
			assert.Equal(t, domainError(validDomainStreamOption, NewAtom("foo"), nil), err)
			assert.False(t, ok)
		})

		t.Run("compound", func(t *testing.T) {
			t.Run("force but arity is not 1", func(t *testing.T) {
				var vm VM
				ok, err := Close(&vm, &Stream{}, List(atomForce.Apply(NewAtom("a"), NewAtom("b"))), Success, nil).Force(context.Background())
				assert.Equal(t, domainError(validDomainStreamOption, atomForce.Apply(NewAtom("a"), NewAtom("b")), nil), err)
				assert.False(t, ok)
			})

			t.Run("force but the argument is a variable", func(t *testing.T) {
				var vm VM
				_, err := Close(&vm, &Stream{}, List(atomForce.Apply(NewVariable())), Success, nil).Force(context.Background())
				_, ok := NewEnv().Unify(domainError(validDomainStreamOption, atomForce.Apply(NewVariable()), nil).term, err.(Exception).term)
				assert.True(t, ok)
			})

			t.Run("force but the argument is neither true nor false", func(t *testing.T) {
				var vm VM
				ok, err := Close(&vm, &Stream{}, List(atomForce.Apply(NewAtom("meh"))), Success, nil).Force(context.Background())
				assert.Equal(t, domainError(validDomainStreamOption, atomForce.Apply(NewAtom("meh")), nil), err)
				assert.False(t, ok)
			})
		})
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var vm VM
		ok, err := Close(&vm, NewAtom("foo"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, existenceError(objectTypeStream, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})
}

func TestFlushOutput(t *testing.T) {
	f, err := os.CreateTemp("", "")
	assert.NoError(t, err)
	defer func() {
		assert.NoError(t, os.Remove(f.Name()))
	}()

	foo := NewAtom("foo")
	s := &Stream{sourceSink: f, mode: ioModeWrite, alias: foo}

	t.Run("ok", func(t *testing.T) {
		var vm VM
		ok, err := FlushOutput(&vm, s, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("ng", func(t *testing.T) {
		var m mockSyncer
		m.On("Sync").Return(errors.New("ng")).Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m, mode: ioModeWrite}

		var vm VM
		_, err := FlushOutput(&vm, s, Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		var vm VM
		vm.streams.add(s)
		ok, err := FlushOutput(&vm, foo, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		var vm VM
		ok, err := FlushOutput(&vm, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream-term or alias", func(t *testing.T) {
		var vm VM
		ok, err := FlushOutput(&vm, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var vm VM
		ok, err := FlushOutput(&vm, NewAtom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, existenceError(objectTypeStream, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("SorA is an input stream", func(t *testing.T) {
		s := &Stream{sourceSink: os.Stdin}

		var vm VM
		ok, err := FlushOutput(&vm, s, Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationOutput, permissionTypeStream, s, nil), err)
		assert.False(t, ok)
	})
}

func TestWriteTerm(t *testing.T) {
	var buf bytes.Buffer
	w := &Stream{sourceSink: &buf, mode: ioModeWrite}
	r := &Stream{sourceSink: &buf, mode: ioModeRead}
	b := &Stream{sourceSink: &buf, mode: ioModeWrite, streamType: streamTypeBinary}

	B := NewVariable()
	s := NewVariable()
	x := NewVariable()
	l := NewVariable()
	e := NewVariable()
	n, v := NewVariable(), NewVariable()

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
		{title: `write_term(1, [quoted(non_boolean)]).`, sOrA: w, term: Integer(1), options: List(atomQuoted.Apply(NewAtom("non_boolean"))), err: domainError(validDomainWriteOption, atomQuoted.Apply(NewAtom("non_boolean")), nil)},
		{title: `write_term(1, [quoted(B)]).`, sOrA: w, term: Integer(1), options: List(atomQuoted.Apply(B)), err: instantiationError(nil)},
		{title: `B = true, write_term(1, [quoted(B)]).`, sOrA: w, env: NewEnv().bind(B, atomTrue), term: Integer(1), options: List(atomQuoted.Apply(B)), ok: true, output: `1`},

		// 8.14.2.3 Errors
		{title: `a`, sOrA: s, term: NewAtom("foo"), options: List(), err: instantiationError(nil)},
		{title: `b: partial list`, sOrA: w, term: NewAtom("foo"), options: PartialList(x, atomQuoted.Apply(atomTrue)), err: instantiationError(nil)},
		{title: `b: variable element`, sOrA: w, term: NewAtom("foo"), options: List(x), err: instantiationError(nil)},
		{title: `b: variable component`, sOrA: w, term: NewAtom("foo"), options: List(atomQuoted.Apply(x)), err: instantiationError(nil)},
		{title: `b: variable_names, partial list`, sOrA: w, term: NewAtom("foo"), options: List(atomVariableNames.Apply(l)), err: instantiationError(nil)},
		{title: `b: variable_names, element`, sOrA: w, term: NewAtom("foo"), options: List(atomVariableNames.Apply(List(e))), err: instantiationError(nil)},
		{title: `b: variable_names, name`, sOrA: w, term: x, options: List(atomVariableNames.Apply(List(atomEqual.Apply(n, v)))), err: instantiationError(nil)},
		{title: `c`, sOrA: w, term: NewAtom("foo"), options: NewAtom("options"), err: typeError(validTypeList, NewAtom("options"), nil)},
		{title: `d`, sOrA: Integer(0), term: NewAtom("foo"), options: List(), err: domainError(validDomainStreamOrAlias, Integer(0), nil)},
		{title: `e: not a compound`, sOrA: w, term: NewAtom("foo"), options: List(NewAtom("bar")), err: domainError(validDomainWriteOption, NewAtom("bar"), nil)},
		{title: `e: arity is not 1`, sOrA: w, term: NewAtom("foo"), options: List(NewAtom("bar").Apply(NewAtom("a"), NewAtom("b"))), err: domainError(validDomainWriteOption, NewAtom("bar").Apply(NewAtom("a"), NewAtom("b")), nil)},
		{title: `e: variable_names, not a list, atom`, sOrA: w, term: NewAtom("foo"), options: List(atomVariableNames.Apply(NewAtom("a"))), err: domainError(validDomainWriteOption, atomVariableNames.Apply(NewAtom("a")), nil)},
		{title: `e: variable_names, not a list, atomic`, sOrA: w, term: NewAtom("foo"), options: List(atomVariableNames.Apply(Integer(0))), err: domainError(validDomainWriteOption, atomVariableNames.Apply(Integer(0)), nil)},
		{title: `e: variable_names, element is not a pair, atomic`, sOrA: w, term: NewAtom("foo"), options: List(atomVariableNames.Apply(List(NewAtom("a")))), err: domainError(validDomainWriteOption, atomVariableNames.Apply(List(NewAtom("a"))), nil)},
		{title: `e: variable_names, element is not a pair, compound`, sOrA: w, term: NewAtom("foo"), options: List(atomVariableNames.Apply(List(NewAtom("f").Apply(NewAtom("a"))))), err: domainError(validDomainWriteOption, atomVariableNames.Apply(List(NewAtom("f").Apply(NewAtom("a")))), nil)},
		{title: `e: variable_names, name is not an atom`, sOrA: w, term: v, options: List(atomVariableNames.Apply(List(atomEqual.Apply(Integer(0), v)))), err: domainError(validDomainWriteOption, atomVariableNames.Apply(List(atomEqual.Apply(Integer(0), NewVariable()))), nil)},
		{title: `e: boolean option, not an atom`, sOrA: w, term: NewAtom("foo"), options: List(atomQuoted.Apply(Integer(0))), err: domainError(validDomainWriteOption, atomQuoted.Apply(Integer(0)), nil)},
		{title: `e: unknown functor`, sOrA: w, term: NewAtom("foo"), options: List(NewAtom("bar").Apply(atomTrue)), err: domainError(validDomainWriteOption, NewAtom("bar").Apply(atomTrue), nil)},
		{title: `f`, sOrA: NewAtom("stream"), term: NewAtom("foo"), options: List(), err: existenceError(objectTypeStream, NewAtom("stream"), nil)},
		{title: `g`, sOrA: r, term: NewAtom("foo"), options: List(), err: permissionError(operationOutput, permissionTypeStream, r, nil)},
		{title: `h`, sOrA: b, term: NewAtom("foo"), options: List(), err: permissionError(operationOutput, permissionTypeBinaryStream, b, nil)},

		// 7.10.5
		{title: `a`, sOrA: w, term: x, options: List(), ok: true, outputPattern: regexp.MustCompile(`_\d+`)},

		{title: `variable_names`, sOrA: w, term: v, options: List(atomVariableNames.Apply(List(
			atomEqual.Apply(NewAtom("n"), v),            // left-most is used
			atomEqual.Apply(NewAtom("m"), v),            // ignored
			atomEqual.Apply(NewAtom("a"), NewAtom("b")), // ignored
		))), ok: true, output: `n`},

		{title: `failure`, sOrA: mw, term: NewAtom("foo"), options: List(), err: err},
	}

	var vm VM
	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			buf.Reset()
			ok, err := WriteTerm(&vm, tt.sOrA, tt.term, tt.options, Success, tt.env).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			if tt.err == nil {
				assert.NoError(t, err)
			} else if te, ok := tt.err.(*Exception); ok {
				_, ok := NewEnv().Unify(te.term, err.(*Exception).term)
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
	writeOptions
}

func (m *mockTerm) String() string {
	args := m.Called()
	return args.String(0)
}

func TestCharCode(t *testing.T) {
	t.Run("ascii", func(t *testing.T) {
		ok, err := CharCode(nil, NewAtom("a"), Integer(97), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("emoji", func(t *testing.T) {
		ok, err := CharCode(nil, NewAtom("😀"), Integer(128512), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("query char", func(t *testing.T) {
		v := NewVariable()

		ok, err := CharCode(nil, v, Integer(128512), func(env *Env) *Promise {
			assert.Equal(t, NewAtom("😀"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("query code", func(t *testing.T) {
		v := NewVariable()
		ok, err := CharCode(nil, NewAtom("😀"), v, func(env *Env) *Promise {
			assert.Equal(t, Integer(128512), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("char and code are variables", func(t *testing.T) {
		char, code := NewVariable(), NewVariable()

		ok, err := CharCode(nil, char, code, Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("char is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("atom", func(t *testing.T) {
			ok, err := CharCode(nil, NewAtom("foo"), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeCharacter, NewAtom("foo"), nil), err)
			assert.False(t, ok)
		})

		t.Run("non-atom", func(t *testing.T) {
			ok, err := CharCode(nil, Integer(0), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeCharacter, Integer(0), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("code is neither a variable nor an integer", func(t *testing.T) {
		t.Run("char is variable", func(t *testing.T) {
			ok, err := CharCode(nil, NewVariable(), NewAtom("foo"), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeInteger, NewAtom("foo"), nil), err)
			assert.False(t, ok)
		})

		t.Run("char is a character", func(t *testing.T) {
			ok, err := CharCode(nil, NewAtom("a"), NewAtom("x"), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeInteger, NewAtom("x"), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("code is neither a variable nor a character-code", func(t *testing.T) {
		ok, err := CharCode(nil, NewVariable(), Integer(-1), Success, nil).Force(context.Background())
		assert.Equal(t, representationError(flagCharacterCode, nil), err)
		assert.False(t, ok)
	})
}

func TestPutByte(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		var m mockWriter
		m.On("Write", []byte{97}).Return(1, nil).Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m, mode: ioModeWrite, streamType: streamTypeBinary}

		var vm VM
		ok, err := PutByte(&vm, s, Integer(97), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("ng", func(t *testing.T) {
		var m mockWriter
		m.On("Write", []byte{97}).Return(0, errors.New("")).Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m, mode: ioModeWrite, streamType: streamTypeBinary}

		var vm VM
		_, err := PutByte(&vm, s, Integer(97), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		var m mockWriter
		m.On("Write", []byte{97}).Return(1, nil).Once()
		defer m.AssertExpectations(t)

		foo := NewAtom("foo")
		s := &Stream{sourceSink: &m, mode: ioModeWrite, streamType: streamTypeBinary, alias: foo}

		var vm VM
		vm.streams.add(s)
		ok, err := PutByte(&vm, NewAtom("foo"), Integer(97), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		var vm VM
		ok, err := PutByte(&vm, NewVariable(), Integer(97), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("byt is a variable", func(t *testing.T) {
		s := &Stream{sourceSink: os.Stdout, mode: ioModeAppend}
		s.streamType = streamTypeBinary

		var vm VM
		ok, err := PutByte(&vm, s, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("byt is neither a variable nor an byte", func(t *testing.T) {
		s := &Stream{sourceSink: os.Stdout, mode: ioModeAppend}
		s.streamType = streamTypeBinary

		t.Run("not even an integer", func(t *testing.T) {
			var vm VM
			ok, err := PutByte(&vm, s, NewAtom("byte"), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeByte, NewAtom("byte"), nil), err)
			assert.False(t, ok)
		})

		t.Run("integer", func(t *testing.T) {
			var vm VM
			ok, err := PutByte(&vm, s, Integer(256), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeByte, Integer(256), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var vm VM
		ok, err := PutByte(&vm, Integer(0), Integer(97), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an input stream", func(t *testing.T) {
		s := NewVariable()
		env := NewEnv().
			bind(s, &Stream{sourceSink: os.Stdin, mode: ioModeRead, streamType: streamTypeBinary})

		var vm VM
		ok, err := PutByte(&vm, s, Integer(97), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationOutput, permissionTypeStream, s, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a text stream", func(t *testing.T) {
		s := NewVariable()
		env := NewEnv().
			bind(s, &Stream{sourceSink: os.Stdout, mode: ioModeAppend})

		var vm VM
		ok, err := PutByte(&vm, s, Integer(97), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationOutput, permissionTypeTextStream, s, env), err)
		assert.False(t, ok)
	})
}

func TestPutChar(t *testing.T) {
	tests := []struct {
		title         string
		streamOrAlias func() (Term, func(*testing.T))
		char          Term
		ok            bool
		err           error
	}{
		// 8.12.3.4 Examples
		{title: "put_char(t)", streamOrAlias: func() (Term, func(*testing.T)) {
			var sb strings.Builder
			sb.WriteString("qwer")
			return NewOutputTextStream(&sb), func(t *testing.T) {
				assert.Equal(t, "qwert", sb.String())
			}
		}, char: NewAtom("t"), ok: true},
		{title: "put_char(st_o, 'A')", streamOrAlias: func() (Term, func(*testing.T)) {
			var sb strings.Builder
			sb.WriteString("qwer")
			return NewOutputTextStream(&sb), func(t *testing.T) {
				assert.Equal(t, "qwerA", sb.String())
			}
		}, char: NewAtom("A"), ok: true},

		// 8.12.3.3 Errors
		{title: "a", streamOrAlias: func() (Term, func(*testing.T)) {
			return NewVariable(), nil
		}, char: NewAtom("a"), err: instantiationError(nil)},
		{title: "b", streamOrAlias: func() (Term, func(*testing.T)) {
			return NewOutputTextStream(nil), nil
		}, char: NewVariable(), err: instantiationError(nil)},
		{title: "b: atom but not one-char", streamOrAlias: func() (Term, func(*testing.T)) {
			return NewOutputTextStream(nil), nil
		}, char: NewAtom("foo"), err: typeError(validTypeCharacter, NewAtom("foo"), nil)},
		{title: "b: not even atom", streamOrAlias: func() (Term, func(*testing.T)) {
			return NewOutputTextStream(nil), nil
		}, char: Integer(1), err: typeError(validTypeCharacter, Integer(1), nil)},
		{title: "f", streamOrAlias: func() (Term, func(*testing.T)) {
			return Integer(1), nil
		}, char: NewAtom("a"), err: domainError(validDomainStreamOrAlias, Integer(1), nil)},
		{title: "g", streamOrAlias: func() (Term, func(*testing.T)) {
			return NewAtom("foo"), nil
		}, char: NewAtom("a"), err: existenceError(objectTypeStream, NewAtom("foo"), nil)},
		{title: "h", streamOrAlias: func() (Term, func(*testing.T)) {
			return NewInputTextStream(nil), nil
		}, char: NewAtom("a"), err: permissionError(operationOutput, permissionTypeStream, NewInputTextStream(nil), nil)},
		{title: "i", streamOrAlias: func() (Term, func(*testing.T)) {
			return NewOutputBinaryStream(nil), nil
		}, char: NewAtom("a"), err: permissionError(operationOutput, permissionTypeBinaryStream, NewOutputBinaryStream(nil), nil)},

		{title: "error on write", streamOrAlias: func() (Term, func(*testing.T)) {
			var m mockWriter
			m.On("Write", mock.Anything).Return(0, errors.New("failed"))
			return NewOutputTextStream(&m), nil
		}, char: NewAtom("a"), err: errors.New("failed")},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			sOrA, test := tt.streamOrAlias()
			if test != nil {
				defer test(t)
			}

			var vm VM
			ok, err := PutChar(&vm, sOrA, tt.char, Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestReadTerm(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		f, err := os.Open("testdata/foo.pl")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead}

		v := NewVariable()

		var vm VM
		ok, err := ReadTerm(&vm, s, v, List(), func(env *Env) *Promise {
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

		foo := NewAtom("foo")
		s := &Stream{sourceSink: f, mode: ioModeRead, alias: foo}

		v := NewVariable()

		var vm VM
		vm.streams.add(s)
		ok, err := ReadTerm(&vm, NewAtom("foo"), v, List(), func(env *Env) *Promise {
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

		v, singletons := NewVariable(), NewVariable()

		var vm VM
		ok, err := ReadTerm(&vm, s, v, List(&compound{
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

		v, variables := NewVariable(), NewVariable()

		var vm VM
		ok, err := ReadTerm(&vm, s, v, List(&compound{
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

		v, variableNames := NewVariable(), NewVariable()

		var vm VM
		ok, err := ReadTerm(&vm, s, v, List(&compound{
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

		v := NewVariable()

		var vm VM

		ok, err := ReadTerm(&vm, s, v, List(), func(env *Env) *Promise {
			assert.Equal(t, &compound{functor: NewAtom("foo"), args: []Term{NewAtom("a")}}, env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = ReadTerm(&vm, s, v, List(), func(env *Env) *Promise {
			assert.Equal(t, &compound{functor: NewAtom("foo"), args: []Term{NewAtom("b")}}, env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = ReadTerm(&vm, s, v, List(), func(env *Env) *Promise {
			assert.Equal(t, &compound{functor: NewAtom("foo"), args: []Term{NewAtom("c")}}, env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		var vm VM
		ok, err := ReadTerm(&vm, NewVariable(), NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("options is a partial list or a list with an element which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			var vm VM
			ok, err := ReadTerm(&vm, &Stream{sourceSink: os.Stdin}, NewVariable(), PartialList(NewVariable(),
				atomVariables.Apply(NewVariable()),
			), Success, nil).Force(context.Background())
			assert.Equal(t, instantiationError(nil), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			var vm VM
			ok, err := ReadTerm(&vm, &Stream{sourceSink: os.Stdin}, NewVariable(), List(NewVariable(), atomVariables.Apply(NewVariable())), Success, nil).Force(context.Background())
			assert.Equal(t, instantiationError(nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var vm VM
		ok, err := ReadTerm(&vm, Integer(0), NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		var vm VM
		ok, err := ReadTerm(&vm, &Stream{sourceSink: os.Stdin}, NewVariable(), NewAtom("options"), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeList, NewAtom("options"), nil), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the Options list is neither a variable nor a valid read-option", func(t *testing.T) {
		var vm VM
		ok, err := ReadTerm(&vm, &Stream{sourceSink: os.Stdin}, NewVariable(), List(&compound{
			functor: atomUnknown,
			args:    []Term{NewAtom("option")},
		}), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainReadOption, &compound{
			functor: atomUnknown,
			args:    []Term{NewAtom("option")},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var vm VM
		ok, err := ReadTerm(&vm, NewAtom("foo"), NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, existenceError(objectTypeStream, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		s := NewVariable()
		env := NewEnv().
			bind(s, &Stream{sourceSink: os.Stdout, mode: ioModeAppend})

		var vm VM
		ok, err := ReadTerm(&vm, s, NewVariable(), List(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypeStream, s, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		stream := &Stream{sourceSink: os.Stdin}
		stream.streamType = streamTypeBinary

		s := NewVariable()
		env := NewEnv().
			bind(s, stream)

		var vm VM
		ok, err := ReadTerm(&vm, s, NewVariable(), List(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypeBinaryStream, s, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		var m mockReader
		defer m.AssertExpectations(t)

		s := NewVariable()
		env := NewEnv().
			bind(s, &Stream{
				sourceSink:  &m,
				mode:        ioModeRead,
				eofAction:   eofActionError,
				endOfStream: endOfStreamPast,
			})

		var vm VM
		ok, err := ReadTerm(&vm, s, NewVariable(), List(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypePastEndOfStream, s, env), err)
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

			var vm VM
			ok, err := ReadTerm(&vm, s, NewVariable(), List(), Success, nil).Force(context.Background())
			assert.Equal(t, syntaxError(unexpectedTokenError{actual: Token{kind: tokenLetterDigit, val: "bar"}}, nil), err)
			assert.False(t, ok)
		})

		t.Run("insufficient", func(t *testing.T) {
			f, err := os.Open("testdata/insufficient.txt")
			assert.NoError(t, err)
			defer func() {
				assert.NoError(t, f.Close())
			}()

			s := &Stream{sourceSink: f, mode: ioModeRead}

			out := NewVariable()
			var vm VM
			ok, err := ReadTerm(&vm, s, out, List(), func(env *Env) *Promise {
				assert.Equal(t, atomEndOfFile, env.Resolve(out))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

	})

	t.Run("the sequence of tokens cannot be parsed as a term using the current set of operator definitions", func(t *testing.T) {
		f, err := os.Open("testdata/unexpected_op.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead}

		var vm VM
		ok, err := ReadTerm(&vm, s, NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, syntaxError(unexpectedTokenError{actual: Token{kind: tokenGraphic, val: "="}}, nil), err)
		assert.False(t, ok)
	})
}

func TestGetByte(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		f, err := os.Open("testdata/a.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead, streamType: streamTypeBinary}

		v := NewVariable()

		var vm VM
		ok, err := GetByte(&vm, s, v, func(env *Env) *Promise {
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

		foo := NewAtom("foo")
		s := &Stream{sourceSink: f, mode: ioModeRead, streamType: streamTypeBinary, alias: foo}

		v := NewVariable()

		var vm VM
		vm.streams.add(s)
		ok, err := GetByte(&vm, foo, v, func(env *Env) *Promise {
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

		v := NewVariable()

		var vm VM
		ok, err := GetByte(&vm, s, v, func(env *Env) *Promise {
			assert.Equal(t, Integer(-1), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("error", func(t *testing.T) {
		var m mockReader
		m.On("Read", mock.Anything).Return(0, errors.New("failed")).Twice()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m, mode: ioModeRead, streamType: streamTypeBinary}

		var vm VM

		v := NewVariable()
		_, err := GetByte(&vm, s, v, Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		var vm VM
		ok, err := GetByte(&vm, NewVariable(), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("inByte is neither a variable nor an in-byte", func(t *testing.T) {
		s := &Stream{sourceSink: os.Stdin}
		s.streamType = streamTypeBinary

		t.Run("not even an integer", func(t *testing.T) {
			var vm VM
			ok, err := GetByte(&vm, s, NewAtom("inByte"), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeInByte, NewAtom("inByte"), nil), err)
			assert.False(t, ok)
		})

		t.Run("integer", func(t *testing.T) {
			var vm VM
			ok, err := GetByte(&vm, s, Integer(256), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeInByte, Integer(256), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream-term or alias", func(t *testing.T) {
		var vm VM
		ok, err := GetByte(&vm, Integer(0), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var vm VM
		ok, err := GetByte(&vm, NewAtom("foo"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, existenceError(objectTypeStream, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &Stream{sourceSink: os.Stdout, mode: ioModeAppend})

		var vm VM
		ok, err := GetByte(&vm, streamOrAlias, NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypeStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a text stream", func(t *testing.T) {
		streamOrAlias := NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &Stream{sourceSink: os.Stdin})

		var vm VM
		ok, err := GetByte(&vm, streamOrAlias, NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypeTextStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		var m mockReader
		defer m.AssertExpectations(t)

		streamOrAlias := NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &Stream{
				sourceSink:  &m,
				mode:        ioModeRead,
				streamType:  streamTypeBinary,
				eofAction:   eofActionError,
				endOfStream: endOfStreamPast,
			})

		var vm VM
		ok, err := GetByte(&vm, streamOrAlias, NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypePastEndOfStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})
}

func TestGetChar(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		f, err := os.Open("testdata/smile.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead}

		v := NewVariable()

		var vm VM
		ok, err := GetChar(&vm, s, v, func(env *Env) *Promise {
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

		v := NewVariable()

		foo := NewAtom("foo")
		var vm VM
		vm.streams.add(&Stream{sourceSink: f, mode: ioModeRead, alias: foo})
		ok, err := GetChar(&vm, foo, v, func(env *Env) *Promise {
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

		v := NewVariable()

		var vm VM
		ok, err := GetChar(&vm, s, v, func(env *Env) *Promise {
			assert.Equal(t, atomEndOfFile, env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("error", func(t *testing.T) {
		var m mockReader
		m.On("Read", mock.Anything).Return(0, errors.New("failed")).Times(2)
		defer m.AssertExpectations(t)

		v := NewVariable()

		var vm VM
		ok, err := GetChar(&vm, &Stream{sourceSink: &m, mode: ioModeRead}, v, Success, nil).Force(context.Background())
		assert.Equal(t, errors.New("failed"), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		var vm VM
		ok, err := GetChar(&vm, NewVariable(), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("char is neither a variable nor an in-character", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var vm VM
			ok, err := GetChar(&vm, &Stream{sourceSink: os.Stdin}, Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeInCharacter, Integer(0), nil), err)
			assert.False(t, ok)
		})

		t.Run("atom", func(t *testing.T) {
			var vm VM
			ok, err := GetChar(&vm, &Stream{sourceSink: os.Stdin}, NewAtom("ab"), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeInCharacter, NewAtom("ab"), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var vm VM
		ok, err := GetChar(&vm, Integer(0), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &Stream{sourceSink: os.Stdout, mode: ioModeAppend})

		var vm VM
		ok, err := GetChar(&vm, streamOrAlias, NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypeStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		s := &Stream{sourceSink: os.Stdin}
		s.streamType = streamTypeBinary

		streamOrAlias := NewVariable()
		env := NewEnv().
			bind(streamOrAlias, s)

		var vm VM
		ok, err := GetChar(&vm, streamOrAlias, NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypeBinaryStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		var m mockReader
		defer m.AssertExpectations(t)

		streamOrAlias := NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &Stream{
				sourceSink:  &m,
				mode:        ioModeRead,
				eofAction:   eofActionError,
				endOfStream: endOfStreamPast,
			})

		var vm VM
		ok, err := GetChar(&vm, streamOrAlias, NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypePastEndOfStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("the entity input from the stream is not a character", func(t *testing.T) {
		f, err := os.Open("testdata/replacement.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		streamOrAlias := NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &Stream{sourceSink: f, mode: ioModeRead})

		var vm VM
		ok, err := GetChar(&vm, streamOrAlias, NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, representationError(flagCharacter, nil), err)
		assert.False(t, ok)
	})
}

func TestPeekByte(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		f, err := os.Open("testdata/abc.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead, streamType: streamTypeBinary}

		v := NewVariable()

		var vm VM
		ok, err := PeekByte(&vm, s, v, func(env *Env) *Promise {
			assert.Equal(t, Integer(97), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = PeekByte(&vm, s, v, Success, nil).Force(context.Background()) // 'a' again
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		f, err := os.Open("testdata/abc.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		v := NewVariable()

		foo := NewAtom("foo")
		var vm VM
		vm.streams.add(&Stream{sourceSink: f, mode: ioModeRead, streamType: streamTypeBinary, alias: foo})
		ok, err := PeekByte(&vm, NewAtom("foo"), v, func(env *Env) *Promise {
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

		v := NewVariable()

		var vm VM
		ok, err := PeekByte(&vm, s, v, func(env *Env) *Promise {
			assert.Equal(t, Integer(-1), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("error", func(t *testing.T) {
		var m mockReader
		m.On("Read", mock.Anything).Return(0, errors.New("failed")).Twice()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m, mode: ioModeRead}
		s.streamType = streamTypeBinary

		v := NewVariable()

		var vm VM
		ok, err := PeekByte(&vm, s, v, Success, nil).Force(context.Background())
		assert.Equal(t, errors.New("failed"), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		var vm VM
		ok, err := PeekByte(&vm, NewVariable(), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("inByte is neither a variable nor an in-byte", func(t *testing.T) {
		s := &Stream{sourceSink: os.Stdin}
		s.streamType = streamTypeBinary

		t.Run("not even an integer", func(t *testing.T) {
			var vm VM
			ok, err := PeekByte(&vm, s, NewAtom("byte"), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeInByte, NewAtom("byte"), nil), err)
			assert.False(t, ok)
		})

		t.Run("integer", func(t *testing.T) {
			var vm VM
			ok, err := PeekByte(&vm, s, Integer(256), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeInByte, Integer(256), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var vm VM
		ok, err := PeekByte(&vm, Integer(0), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &Stream{sourceSink: os.Stdout, mode: ioModeAppend})

		var vm VM
		ok, err := PeekByte(&vm, streamOrAlias, NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypeStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a text stream", func(t *testing.T) {
		streamOrAlias := NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &Stream{sourceSink: os.Stdin})

		var vm VM
		ok, err := PeekByte(&vm, streamOrAlias, NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypeTextStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		var m mockReader
		defer m.AssertExpectations(t)

		streamOrAlias := NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &Stream{
				sourceSink:  &m,
				mode:        ioModeRead,
				streamType:  streamTypeBinary,
				eofAction:   eofActionError,
				endOfStream: endOfStreamPast,
			})

		var vm VM
		ok, err := PeekByte(&vm, streamOrAlias, NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypePastEndOfStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})
}

func TestPeekChar(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		f, err := os.Open("testdata/smile.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead}

		v := NewVariable()

		var vm VM
		ok, err := PeekChar(&vm, s, v, func(env *Env) *Promise {
			assert.Equal(t, NewAtom("😀"), env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = PeekChar(&vm, s, v, func(env *Env) *Promise {
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

		v := NewVariable()

		foo := NewAtom("foo")
		var vm VM
		vm.streams.add(&Stream{sourceSink: f, mode: ioModeRead, alias: foo})
		ok, err := PeekChar(&vm, foo, v, func(env *Env) *Promise {
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

		v := NewVariable()

		var vm VM
		ok, err := PeekChar(&vm, s, v, func(env *Env) *Promise {
			assert.Equal(t, atomEndOfFile, env.Resolve(v))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("error", func(t *testing.T) {
		var m mockReader
		m.On("Read", mock.Anything).Return(0, errors.New("failed")).Twice()
		defer m.AssertExpectations(t)

		v := NewVariable()

		var vm VM
		ok, err := PeekChar(&vm, &Stream{sourceSink: &m, mode: ioModeRead}, v, Success, nil).Force(context.Background())
		assert.Equal(t, errors.New("failed"), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		var vm VM
		ok, err := PeekChar(&vm, NewVariable(), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("char is neither a variable nor an in-character", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var vm VM
			ok, err := PeekChar(&vm, &Stream{sourceSink: os.Stdin}, Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeInCharacter, Integer(0), nil), err)
			assert.False(t, ok)
		})

		t.Run("atom", func(t *testing.T) {
			var vm VM
			ok, err := PeekChar(&vm, &Stream{sourceSink: os.Stdin}, NewAtom("ab"), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeInCharacter, NewAtom("ab"), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var vm VM
		ok, err := PeekChar(&vm, Integer(0), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &Stream{sourceSink: os.Stdout, mode: ioModeAppend})

		var vm VM
		ok, err := PeekChar(&vm, streamOrAlias, NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypeStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		s := &Stream{sourceSink: os.Stdin}
		s.streamType = streamTypeBinary

		streamOrAlias := NewVariable()
		env := NewEnv().
			bind(streamOrAlias, s)

		var vm VM
		ok, err := PeekChar(&vm, streamOrAlias, NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypeBinaryStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		var m mockReader
		defer m.AssertExpectations(t)

		streamOrAlias := NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &Stream{
				sourceSink:  &m,
				eofAction:   eofActionError,
				endOfStream: endOfStreamPast,
			})

		var vm VM
		ok, err := PeekChar(&vm, streamOrAlias, NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypePastEndOfStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("the entity input from the stream is not a character", func(t *testing.T) {
		f, err := os.Open("testdata/replacement.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		streamOrAlias := NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &Stream{sourceSink: f, mode: ioModeRead})

		var vm VM
		ok, err := PeekChar(&vm, streamOrAlias, NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, representationError(flagCharacter, nil), err)
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

		ok, err := Halt(nil, Integer(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.True(t, exitCalled)
	})

	t.Run("n is a variable", func(t *testing.T) {
		n := NewVariable()

		ok, err := Halt(nil, n, Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("n is neither a variable nor an integer", func(t *testing.T) {
		ok, err := Halt(nil, NewAtom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})
}

func TestClause(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		x := NewVariable()
		what, body := NewVariable(), NewVariable()

		var c int

		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: NewAtom("green"), arity: 1}: &userDefined{public: true, clauses: []clause{
					{raw: &compound{
						functor: atomIf, args: []Term{
							&compound{functor: NewAtom("green"), args: []Term{x}},
							&compound{functor: NewAtom("moldy"), args: []Term{x}},
						},
					}},
					{raw: &compound{functor: NewAtom("green"), args: []Term{NewAtom("kermit")}}},
				}},
			},
		}
		ok, err := Clause(&vm, &compound{
			functor: NewAtom("green"),
			args:    []Term{what},
		}, body, func(env *Env) *Promise {
			switch c {
			case 0:
				b, ok := env.Resolve(body).(*compound)
				assert.True(t, ok)
				assert.Equal(t, NewAtom("moldy"), b.functor)
				assert.Len(t, b.args, 1)
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
		var vm VM
		ok, err := Clause(&vm, NewAtom("foo"), atomTrue, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("head is a variable", func(t *testing.T) {
		var vm VM
		ok, err := Clause(&vm, NewVariable(), atomTrue, Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("head is neither a variable nor a predication", func(t *testing.T) {
		var vm VM
		ok, err := Clause(&vm, Integer(0), atomTrue, Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("the predicate indicator Pred of Head is that of a private (ie. Not public) procedure", func(t *testing.T) {
		what, body := NewVariable(), NewVariable()

		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: NewAtom("green"), arity: 1}: Predicate1(func(_ *VM, t Term, f Cont, env *Env) *Promise {
					return Bool(true)
				}),
			},
		}
		ok, err := Clause(&vm, &compound{
			functor: NewAtom("green"),
			args:    []Term{what},
		}, body, Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationAccess, permissionTypePrivateProcedure, &compound{
			functor: atomSlash,
			args:    []Term{NewAtom("green"), Integer(1)},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("body is neither a variable nor a callable term", func(t *testing.T) {
		var vm VM
		ok, err := Clause(&vm, NewAtom("foo"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})
}

func TestAtomLength(t *testing.T) {
	t.Run("ascii", func(t *testing.T) {
		ok, err := AtomLength(nil, NewAtom("abc"), Integer(3), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("emoji", func(t *testing.T) {
		ok, err := AtomLength(nil, NewAtom("😀"), Integer(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("atom is a variable", func(t *testing.T) {
		atom := NewVariable()
		ok, err := AtomLength(nil, atom, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("atom is neither a variable nor an atom", func(t *testing.T) {
		ok, err := AtomLength(nil, Integer(2), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeAtom, Integer(2), nil), err)
		assert.False(t, ok)
	})

	t.Run("length is neither a variable nor an integer", func(t *testing.T) {
		ok, err := AtomLength(nil, NewAtom("😀"), NewAtom("1"), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, NewAtom("1"), nil), err)
		assert.False(t, ok)
	})

	t.Run("length is an integer less than zero", func(t *testing.T) {
		ok, err := AtomLength(nil, NewAtom("😀"), Integer(-1), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainNotLessThanZero, Integer(-1), nil), err)
		assert.False(t, ok)
	})
}

func TestAtomConcat(t *testing.T) {
	t.Run("atom3 is a variable", func(t *testing.T) {
		atom3 := NewVariable()

		ok, err := AtomConcat(nil, NewAtom("foo"), NewAtom("bar"), atom3, func(env *Env) *Promise {
			assert.Equal(t, NewAtom("foobar"), env.Resolve(atom3))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("atom3 is an atom", func(t *testing.T) {
		var c int
		v1, v2 := NewVariable(), NewVariable()
		ok, err := AtomConcat(nil, v1, v2, NewAtom("foo"), func(env *Env) *Promise {
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
		atom1, atom3 := NewVariable(), NewVariable()

		ok, err := AtomConcat(nil, atom1, NewAtom("bar"), atom3, Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("atom2 and atom3 are variables", func(t *testing.T) {
		atom2, atom3 := NewVariable(), NewVariable()

		ok, err := AtomConcat(nil, NewAtom("foo"), atom2, atom3, Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("atom1 is neither a variable nor an atom", func(t *testing.T) {
		t.Run("atom3 is a variable", func(t *testing.T) {
			ok, err := AtomConcat(nil, Integer(1), NewAtom("bar"), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeAtom, Integer(1), nil), err)
			assert.False(t, ok)
		})

		t.Run("atom3 is an atom", func(t *testing.T) {
			ok, err := AtomConcat(nil, Integer(1), NewAtom("bar"), NewAtom("foobar"), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeAtom, Integer(1), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("atom2 is neither a variable nor an atom", func(t *testing.T) {
		t.Run("atom3 is a variable", func(t *testing.T) {
			ok, err := AtomConcat(nil, NewAtom("foo"), Integer(2), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeAtom, Integer(2), nil), err)
			assert.False(t, ok)
		})

		t.Run("atom3 is an atom", func(t *testing.T) {
			ok, err := AtomConcat(nil, NewAtom("foo"), Integer(2), NewAtom("foobar"), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeAtom, Integer(2), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("atom3 is neither a variable nor an atom", func(t *testing.T) {
		ok, err := AtomConcat(nil, NewAtom("foo"), NewAtom("bar"), Integer(3), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeAtom, Integer(3), nil), err)
		assert.False(t, ok)
	})
}

func TestSubAtom(t *testing.T) {
	t.Run("multiple solutions", func(t *testing.T) {
		before, length, after := NewVariable(), NewVariable(), NewVariable()
		var c int
		ok, err := SubAtom(nil, NewAtom("xATGATGAxATGAxATGAx"), before, length, after, NewAtom("ATGA"), func(env *Env) *Promise {
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
		char := NewVariable()
		ok, err := SubAtom(nil, NewAtom("a"), Integer(0), Integer(1), Integer(0), char, func(env *Env) *Promise {
			assert.Equal(t, NewAtom("a"), env.Resolve(char))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("atom is a variable", func(t *testing.T) {
		ok, err := SubAtom(nil, NewVariable(), NewVariable(), NewVariable(), NewVariable(), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("atom is neither a variable nor an atom", func(t *testing.T) {
		ok, err := SubAtom(nil, Integer(0), NewVariable(), NewVariable(), NewVariable(), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("subAtom is neither a variable nor an atom", func(t *testing.T) {
		ok, err := SubAtom(nil, NewAtom("foo"), NewVariable(), NewVariable(), NewVariable(), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("before is neither a variable nor an integer", func(t *testing.T) {
		ok, err := SubAtom(nil, NewAtom("foo"), NewAtom("before"), NewVariable(), NewVariable(), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, NewAtom("before"), nil), err)
		assert.False(t, ok)
	})

	t.Run("length is neither a variable nor an integer", func(t *testing.T) {
		ok, err := SubAtom(nil, NewAtom("foo"), NewVariable(), NewAtom("length"), NewVariable(), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, NewAtom("length"), nil), err)
		assert.False(t, ok)
	})

	t.Run("after is neither a variable nor an integer", func(t *testing.T) {
		ok, err := SubAtom(nil, NewAtom("foo"), NewVariable(), NewVariable(), NewAtom("after"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, NewAtom("after"), nil), err)
		assert.False(t, ok)
	})

	t.Run("before is an integer less than zero", func(t *testing.T) {
		ok, err := SubAtom(nil, NewAtom("foo"), Integer(-1), NewVariable(), NewVariable(), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainNotLessThanZero, Integer(-1), nil), err)
		assert.False(t, ok)
	})

	t.Run("length is an integer less than zero", func(t *testing.T) {
		ok, err := SubAtom(nil, NewAtom("foo"), NewVariable(), Integer(-1), NewVariable(), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainNotLessThanZero, Integer(-1), nil), err)
		assert.False(t, ok)
	})

	t.Run("after is an integer less than zero", func(t *testing.T) {
		ok, err := SubAtom(nil, NewAtom("foo"), NewVariable(), NewVariable(), Integer(-1), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainNotLessThanZero, Integer(-1), nil), err)
		assert.False(t, ok)
	})
}

func TestAtomChars(t *testing.T) {
	l := NewVariable()
	str := NewVariable()
	x, y := NewVariable(), NewVariable()

	tests := []struct {
		title      string
		atom, list Term
		ok         bool
		err        error
		env        map[Variable]Term
	}{
		// 8.16.4.4 Examples
		{title: "atom_chars('', L).", atom: NewAtom(""), list: l, ok: true, env: map[Variable]Term{
			l: List(),
		}},
		{title: "atom_chars([], L).", atom: atomEmptyList, list: l, ok: true, env: map[Variable]Term{
			l: List(NewAtom("["), NewAtom("]")),
		}},
		{title: "atom_chars('''', L).", atom: NewAtom("'"), list: l, ok: true, env: map[Variable]Term{
			l: List(NewAtom("'")),
		}},
		{title: "atom_chars('ant', L).", atom: NewAtom("ant"), list: l, ok: true, env: map[Variable]Term{
			l: List(NewAtom("a"), NewAtom("n"), NewAtom("t")),
		}},
		{title: "atom_chars(Str, ['s', 'o', 'p']).", atom: str, list: List(NewAtom("s"), NewAtom("o"), NewAtom("p")), ok: true, env: map[Variable]Term{
			str: NewAtom("sop"),
		}},
		{title: "atom_chars('North', ['N' | X]).", atom: NewAtom("North"), list: PartialList(x, NewAtom("N")), ok: true, env: map[Variable]Term{
			x: List(NewAtom("o"), NewAtom("r"), NewAtom("t"), NewAtom("h")),
		}},
		{title: "atom_chars('soap', ['s', 'o', 'p']).", atom: NewAtom("soap"), list: List(NewAtom("s"), NewAtom("o"), NewAtom("p")), ok: false},
		{title: "atom_chars(X, Y).", atom: x, list: y, err: instantiationError(nil)},

		// 8.16.4.3 Errors
		{title: "a", atom: x, list: PartialList(y, NewAtom("a")), err: instantiationError(nil)},
		{title: "b", atom: Integer(0), list: List(NewAtom("a"), NewAtom("b"), NewAtom("c")), err: typeError(validTypeAtom, Integer(0), nil)},
		{title: "c: atom is a variable", atom: x, list: Integer(0), err: typeError(validTypeList, Integer(0), nil)},
		{title: "c: atom is an atom", atom: NewAtom("a"), list: Integer(0), err: typeError(validTypeList, Integer(0), nil)},
		{title: "d", atom: x, list: List(y, NewAtom("a")), err: instantiationError(nil)},
		{title: "e: atom is a variable, more than one char", atom: x, list: List(NewAtom("abc")), err: typeError(validTypeCharacter, NewAtom("abc"), nil)},
		{title: "e: atom is a variable, not an atom", atom: x, list: List(Integer(0)), err: typeError(validTypeCharacter, Integer(0), nil)},
		{title: "e: atom is an atom, more than one char", atom: NewAtom("abc"), list: List(NewAtom("ab"), NewAtom("c")), err: typeError(validTypeCharacter, NewAtom("ab"), nil)},
		{title: "e: atom is an atom, not an atom", atom: NewAtom("abc"), list: List(Integer('a'), NewAtom("b"), NewAtom("c")), err: typeError(validTypeCharacter, Integer('a'), nil)},

		{title: "atom_chars('ant', ['a', X, 't']).", atom: NewAtom("ant"), list: List(NewAtom("a"), x, NewAtom("t")), ok: true, env: map[Variable]Term{
			x: NewAtom("n"),
		}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := AtomChars(nil, tt.atom, tt.list, func(env *Env) *Promise {
				for k, v := range tt.env {
					_, ok := env.Unify(k, v)
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
	l := NewVariable()
	str := NewVariable()
	x, y := NewVariable(), NewVariable()

	tests := []struct {
		title      string
		atom, list Term
		ok         bool
		err        error
		env        map[Variable]Term
	}{
		// 8.16.5.4 Examples
		{title: "atom_codes('', L).", atom: NewAtom(""), list: l, ok: true, env: map[Variable]Term{
			l: List(),
		}},
		{title: "atom_codes([], L).", atom: atomEmptyList, list: l, ok: true, env: map[Variable]Term{
			l: List(Integer('['), Integer(']')),
		}},
		{title: "atom_codes('''', L).", atom: NewAtom("'"), list: l, ok: true, env: map[Variable]Term{
			l: List(Integer('\'')),
		}},
		{title: "atom_codes('ant', L).", atom: NewAtom("ant"), list: l, ok: true, env: map[Variable]Term{
			l: List(Integer('a'), Integer('n'), Integer('t')),
		}},
		{title: "atom_codes(Str, [0's, 0'o, 0'p]).", atom: str, list: List(Integer('s'), Integer('o'), Integer('p')), ok: true, env: map[Variable]Term{
			str: NewAtom("sop"),
		}},
		{title: "atom_codes('North', [0'N | X]).", atom: NewAtom("North"), list: PartialList(x, Integer('N')), ok: true, env: map[Variable]Term{
			x: List(Integer('o'), Integer('r'), Integer('t'), Integer('h')),
		}},
		{title: "atom_codes('soap', [0's, 0'o, 0'p]).", atom: NewAtom("soap"), list: List(Integer('s'), Integer('o'), Integer('p')), ok: false},
		{title: "atom_codes(X, Y).", atom: x, list: y, err: instantiationError(nil)},

		// 8.16.5.3 Errors
		{title: "a", atom: x, list: PartialList(y, Integer(0)), err: instantiationError(nil)},
		{title: "b", atom: Integer(0), list: l, err: typeError(validTypeAtom, Integer(0), nil)},
		{title: "c: atom is a variable", atom: x, list: Integer(0), err: typeError(validTypeList, Integer(0), nil)},
		{title: "c: atom is an atom", atom: NewAtom("abc"), list: Integer(0), err: typeError(validTypeList, Integer(0), nil)},
		{title: "d", atom: x, list: List(y, Integer('b'), Integer('c')), err: instantiationError(nil)},
		{title: "e: atom is a variable", atom: x, list: List(NewAtom("a"), Integer('b'), Integer('c')), err: typeError(validTypeInteger, NewAtom("a"), nil)},
		{title: "e: atom is an atom", atom: NewAtom("abc"), list: List(NewAtom("a"), Integer('b'), Integer('c')), err: typeError(validTypeInteger, NewAtom("a"), nil)},
		{title: "f: atom is a variable", atom: x, list: List(Integer(-1), Integer('b'), Integer('c')), err: representationError(flagCharacterCode, nil)},
		{title: "f: atom is an atom", atom: NewAtom("abc"), list: List(Integer(-1), Integer('b'), Integer('c')), err: representationError(flagCharacterCode, nil)},

		{title: "atom_codes('ant', [0'a, X, 0't]).", atom: NewAtom("ant"), list: List(Integer('a'), x, Integer('t')), ok: true, env: map[Variable]Term{
			x: Integer('n'),
		}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := AtomCodes(nil, tt.atom, tt.list, func(env *Env) *Promise {
				for k, v := range tt.env {
					_, ok := env.Unify(k, v)
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
			chars := NewVariable()

			ok, err := NumberChars(nil, Float(23.4), chars, func(env *Env) *Promise {
				assert.Equal(t, List(NewAtom("2"), NewAtom("3"), atomDot, NewAtom("4")), env.Resolve(chars))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("chars is a list with variables", func(t *testing.T) {
			char := NewVariable()

			ok, err := NumberChars(nil, Float(23.4), List(char, NewAtom("3"), atomDot, NewAtom("4")), func(env *Env) *Promise {
				assert.Equal(t, NewAtom("2"), env.Resolve(char))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("chars to number", func(t *testing.T) {
		num := NewVariable()

		ok, err := NumberChars(nil, num, List(NewAtom("2"), NewAtom("3"), atomDot, NewAtom("4")), func(env *Env) *Promise {
			assert.Equal(t, Float(23.4), env.Resolve(num))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("both provided", func(t *testing.T) {
		t.Run("3.3", func(t *testing.T) {
			ok, err := NumberChars(nil, Float(3.3), List(NewAtom("3"), atomDot, NewAtom("3")), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("3.3E+0", func(t *testing.T) {
			ok, err := NumberChars(nil, Float(3.3), List(NewAtom("3"), atomDot, NewAtom("3"), NewAtom("E"), atomPlus, NewAtom("0")), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("num is a variable and chars is a partial list", func(t *testing.T) {
		chars := PartialList(NewVariable(),
			NewAtom("2"), NewAtom("3"), atomDot, NewAtom("4"),
		)

		ok, err := NumberChars(nil, NewVariable(), chars, Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("num is neither a variable nor a number", func(t *testing.T) {
		t.Run("chars is a list of one-char atoms", func(t *testing.T) {
			ok, err := NumberChars(nil, NewAtom("23.4"), List(NewAtom("2"), NewAtom("3"), atomDot, NewAtom("4")), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeNumber, NewAtom("23.4"), nil), err)
			assert.False(t, ok)
		})

		t.Run("chars is not a list of one-char atoms", func(t *testing.T) {
			ok, err := NumberChars(nil, NewAtom("23.4"), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeNumber, NewAtom("23.4"), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("chars is neither a partial list nor a list", func(t *testing.T) {
		t.Run("not even list-ish", func(t *testing.T) {
			ok, err := NumberChars(nil, NewVariable(), NewAtom("foo"), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeList, NewAtom("foo"), nil), err)
			assert.False(t, ok)
		})

		t.Run("list-ish", func(t *testing.T) {
			_, err := NumberChars(nil, Integer(0), PartialList(NewAtom("b"), NewVariable()), Success, nil).Force(context.Background())
			_, ok := NewEnv().Unify(err.(Exception).Term(), typeError(validTypeList, PartialList(NewAtom("b"), NewVariable()), nil).Term())
			assert.True(t, ok)
		})
	})

	t.Run("num is a variable and an element of a list prefix of chars is a variable", func(t *testing.T) {
		ok, err := NumberChars(nil, NewVariable(), List(NewAtom("1"), NewVariable()), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("chars is a list of one-char atoms but is not parsable as a number", func(t *testing.T) {
		t.Run("not a number", func(t *testing.T) {
			ok, err := NumberChars(nil, NewVariable(), List(NewAtom("f"), NewAtom("o"), NewAtom("o")), Success, nil).Force(context.Background())
			assert.Equal(t, syntaxError(errNotANumber, nil), err)
			assert.False(t, ok)
		})

		t.Run("unexpected token", func(t *testing.T) {
			ok, err := NumberChars(nil, NewVariable(), List(NewAtom("1"), atomDot), Success, nil).Force(context.Background())
			assert.Equal(t, syntaxError(errNotANumber, nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("an element E of a list prefix of chars is neither a variable nor a one-char atom", func(t *testing.T) {
		t.Run("chars contains a variable", func(t *testing.T) {
			t.Run("not even an atom", func(t *testing.T) {
				ok, err := NumberChars(nil, Integer(100), List(NewVariable(), NewAtom("0"), Integer(0)), Success, nil).Force(context.Background())
				assert.Equal(t, typeError(validTypeCharacter, Integer(0), nil), err)
				assert.False(t, ok)
			})

			t.Run("atom", func(t *testing.T) {
				ok, err := NumberChars(nil, Integer(100), List(NewVariable(), NewAtom("00")), Success, nil).Force(context.Background())
				assert.Equal(t, typeError(validTypeCharacter, NewAtom("00"), nil), err)
				assert.False(t, ok)
			})
		})

		t.Run("chars does not contain a variable", func(t *testing.T) {
			t.Run("not even an atom", func(t *testing.T) {
				ok, err := NumberChars(nil, Integer(100), List(NewAtom("1"), NewAtom("0"), Integer(0)), Success, nil).Force(context.Background())
				assert.Equal(t, typeError(validTypeCharacter, Integer(0), nil), err)
				assert.False(t, ok)
			})

			t.Run("atom", func(t *testing.T) {
				ok, err := NumberChars(nil, Integer(100), List(NewAtom("1"), NewAtom("00")), Success, nil).Force(context.Background())
				assert.Equal(t, typeError(validTypeCharacter, NewAtom("00"), nil), err)
				assert.False(t, ok)
			})
		})
	})

	t.Run("out of memory", func(t *testing.T) {

	})
}

func TestNumberCodes(t *testing.T) {
	t.Run("number to codes", func(t *testing.T) {
		codes := NewVariable()

		ok, err := NumberCodes(nil, Float(23.4), codes, func(env *Env) *Promise {
			assert.Equal(t, List(Integer(50), Integer(51), Integer(46), Integer(52)), env.Resolve(codes))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("codes to number", func(t *testing.T) {
		num := NewVariable()

		ok, err := NumberCodes(nil, num, List(Integer(50), Integer(51), Integer(46), Integer(52)), func(env *Env) *Promise {
			assert.Equal(t, Float(23.4), env.Resolve(num))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("both provided", func(t *testing.T) {
		t.Run("33.0", func(t *testing.T) {
			ok, err := NumberCodes(nil, Float(33.0), List(Integer(51), Integer(51), Integer(46), Integer(48)), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("33", func(t *testing.T) {
			ok, err := NumberCodes(nil, Float(33.0), List(Integer(51), Integer(51)), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("3.3E+01", func(t *testing.T) {
			ok, err := NumberCodes(nil, Float(33.0), List(Integer(51), Integer(46), Integer(51), Integer(69), Integer(43), Integer(48), Integer(49)), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("num is a variable and codes is a partial list or list with an element which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			codes := PartialList(NewVariable(),
				Integer(50), Integer(51), Integer(46), Integer(52),
			)

			ok, err := NumberCodes(nil, NewVariable(), codes, Success, nil).Force(context.Background())
			assert.Equal(t, instantiationError(nil), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			code := NewVariable()

			ok, err := NumberCodes(nil, NewVariable(), List(code, Integer(50), Integer(51), Integer(46), Integer(52)), Success, nil).Force(context.Background())
			assert.Equal(t, instantiationError(nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("num is neither a variable nor a number", func(t *testing.T) {
		ok, err := NumberCodes(nil, NewAtom("23.4"), List(Integer(50), Integer(51), Integer(46), Integer(52)), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeNumber, NewAtom("23.4"), nil), err)
		assert.False(t, ok)
	})

	t.Run("num is a variable and codes is neither a list nor partial list", func(t *testing.T) {
		ok, err := NumberCodes(nil, NewVariable(), NewAtom("23.4"), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeList, NewAtom("23.4"), nil), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the list codes is neither a variable nor a one-character atom", func(t *testing.T) {
		ok, err := NumberCodes(nil, NewVariable(), List(NewAtom("2"), Integer(51), Integer(46), Integer(52)), Success, nil).Force(context.Background())
		assert.Equal(t, representationError(flagCharacterCode, nil), err)
		assert.False(t, ok)
	})

	t.Run("codes is a list of one-char atoms but is not parsable as a number", func(t *testing.T) {
		ok, err := NumberCodes(nil, NewVariable(), List(Integer(102), Integer(111), Integer(111)), Success, nil).Force(context.Background())
		assert.Equal(t, syntaxError(errNotANumber, nil), err)
		assert.False(t, ok)
	})
}

func TestStreamProperty(t *testing.T) {
	f, err := os.Open("testdata/empty.txt")
	assert.NoError(t, err)
	defer func() {
		assert.NoError(t, f.Close())
	}()

	ss := []*Stream{
		{sourceSink: f, mode: ioModeRead, alias: NewAtom("a"), reposition: true},
		{sourceSink: f, mode: ioModeWrite, alias: NewAtom("b"), reposition: false},
		{sourceSink: f, mode: ioModeAppend, alias: NewAtom("c"), reposition: true},
	}

	var vm VM
	for _, s := range ss {
		vm.streams.add(s)
	}

	p, s := NewVariable(), NewVariable()

	tests := []struct {
		title            string
		stream, property Term
		ok               bool
		err              error
		env              []map[Variable]Term
	}{
		{
			title:    "stream",
			stream:   &Stream{sourceSink: f, mode: ioModeRead, alias: NewAtom("null"), reposition: true},
			property: p,
			ok:       true,
			env: []map[Variable]Term{
				{p: atomFileName.Apply(NewAtom(f.Name()))},
				{p: atomMode.Apply(atomRead)},
				{p: atomInput},
				{p: atomAlias.Apply(NewAtom("null"))},
				{p: atomPosition.Apply(Integer(0))},
				{p: atomEndOfStream.Apply(atomNot)},
				{p: atomEOFAction.Apply(atomEOFCode)},
				{p: atomReposition.Apply(atomTrue)},
				{p: atomType.Apply(atomText)},
			},
		},
		{
			title:    "output",
			stream:   s,
			property: atomOutput,
			ok:       true,
			env: []map[Variable]Term{
				{s: ss[1]},
				{s: ss[2]},
			},
		},
		{
			title:    "alias",
			stream:   s,
			property: atomAlias.Apply(NewAtom("b")),
			ok:       true,
			env: []map[Variable]Term{
				{s: ss[1]},
			},
		},
		{
			title:    "position",
			stream:   s,
			property: atomPosition.Apply(Integer(0)),
			ok:       true,
			env: []map[Variable]Term{
				{s: ss[0]},
				{s: ss[1]},
				{s: ss[2]},
			},
		},

		// 8.11.8.3 Errors
		{title: "b", stream: Integer(0), property: p, err: domainError(validDomainStream, Integer(0), nil)},
		{title: "c: unknown atom", stream: s, property: NewAtom("foo"), err: domainError(validDomainStreamProperty, NewAtom("foo"), nil)},
		{title: "c: compound with multiple args", stream: s, property: NewAtom("f").Apply(NewAtom("a"), NewAtom("b")), err: domainError(validDomainStreamProperty, NewAtom("f").Apply(NewAtom("a"), NewAtom("b")), nil)},
		{title: "c: compound with an unexpected integer arg", stream: s, property: atomAlias.Apply(Integer(0)), err: domainError(validDomainStreamProperty, atomAlias.Apply(Integer(0)), nil)},
		{title: "c: compound with an unexpected atom arg", stream: s, property: atomPosition.Apply(NewAtom("foo")), err: domainError(validDomainStreamProperty, atomPosition.Apply(NewAtom("foo")), nil)},
		{title: "c: unknown compound", stream: s, property: NewAtom("foo").Apply(NewAtom("bar")), err: domainError(validDomainStreamProperty, NewAtom("foo").Apply(NewAtom("bar")), nil)},
		{title: "c: unexpected arg", stream: s, property: Integer(0), err: domainError(validDomainStreamProperty, Integer(0), nil)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := StreamProperty(&vm, tt.stream, tt.property, func(env *Env) *Promise {
				for k, v := range tt.env[0] {
					_, ok := env.Unify(k, v)
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

func TestSetStreamPosition(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		f, err := os.Open("testdata/empty.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead, reposition: true}

		var vm VM
		ok, err := SetStreamPosition(&vm, s, Integer(0), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("seek failed", func(t *testing.T) {
		var m mockFile
		m.On("Seek", mock.Anything, mock.Anything).Return(int64(0), errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m, mode: ioModeRead, reposition: true}

		var vm VM
		ok, err := SetStreamPosition(&vm, s, Integer(0), Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		var vm VM
		ok, err := SetStreamPosition(&vm, NewVariable(), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("position is a variable", func(t *testing.T) {
		f, err := os.Open("testdata/empty.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead, reposition: true}

		var vm VM
		ok, err := SetStreamPosition(&vm, s, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var vm VM
		ok, err := SetStreamPosition(&vm, Integer(2), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainStreamOrAlias, Integer(2), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var vm VM
		ok, err := SetStreamPosition(&vm, NewAtom("foo"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, existenceError(objectTypeStream, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream property reposition(false)", func(t *testing.T) {
		stream := &Stream{sourceSink: os.Stdin}

		assert.False(t, stream.reposition)

		s := NewVariable()
		env := NewEnv().
			bind(s, stream)

		var vm VM
		ok, err := SetStreamPosition(&vm, s, Integer(0), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationReposition, permissionTypeStream, s, env), err)
		assert.False(t, ok)
	})
}

func TestCharConversion(t *testing.T) {
	t.Run("register", func(t *testing.T) {
		var vm VM
		ok, err := CharConversion(&vm, NewAtom("a"), NewAtom("b"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, 'b', vm.charConversions['a'])
	})

	t.Run("remove", func(t *testing.T) {
		vm := VM{
			charConversions: map[rune]rune{
				'a': 'b',
			},
		}
		ok, err := CharConversion(&vm, NewAtom("a"), NewAtom("a"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		_, ok = vm.charConversions['a']
		assert.False(t, ok)
	})

	t.Run("inChar is a variable", func(t *testing.T) {
		var vm VM
		ok, err := CharConversion(&vm, NewVariable(), NewAtom("a"), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("outChar is a variable", func(t *testing.T) {
		var vm VM
		ok, err := CharConversion(&vm, NewAtom("a"), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("inChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var vm VM
			ok, err := CharConversion(&vm, Integer(0), NewAtom("a"), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(flagCharacter, nil), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var vm VM
			ok, err := CharConversion(&vm, NewAtom("foo"), NewAtom("a"), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(flagCharacter, nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("outChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var vm VM
			ok, err := CharConversion(&vm, NewAtom("a"), Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(flagCharacter, nil), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var vm VM
			ok, err := CharConversion(&vm, NewAtom("a"), NewAtom("foo"), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(flagCharacter, nil), err)
			assert.False(t, ok)
		})
	})
}

func TestCurrentCharConversion(t *testing.T) {
	t.Run("specified", func(t *testing.T) {
		t.Run("as is", func(t *testing.T) {
			var vm VM
			ok, err := CurrentCharConversion(&vm, NewAtom("a"), NewAtom("a"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("converted", func(t *testing.T) {
			vm := VM{
				charConversions: map[rune]rune{
					'a': 'b',
				},
			}
			ok, err := CurrentCharConversion(&vm, NewAtom("a"), NewAtom("b"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("not specified", func(t *testing.T) {
		x, y := NewVariable(), NewVariable()

		var r rune
		var vm VM
		ok, err := CurrentCharConversion(&vm, x, y, func(env *Env) *Promise {
			ref, ok := env.lookup(x)
			assert.True(t, ok)
			x, ok := ref.(Atom)
			assert.True(t, ok)
			assert.Len(t, []rune(x.String()), 1)

			ref, ok = env.lookup(y)
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
			var vm VM
			ok, err := CurrentCharConversion(&vm, Integer(0), NewAtom("b"), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(flagCharacter, nil), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var vm VM
			ok, err := CurrentCharConversion(&vm, NewAtom("foo"), NewAtom("b"), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(flagCharacter, nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("outChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var vm VM
			ok, err := CurrentCharConversion(&vm, NewAtom("a"), Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(flagCharacter, nil), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var vm VM
			ok, err := CurrentCharConversion(&vm, NewAtom("a"), NewAtom("bar"), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(flagCharacter, nil), err)
			assert.False(t, ok)
		})
	})
}

func TestSetPrologFlag(t *testing.T) {
	t.Run("bounded", func(t *testing.T) {
		var vm VM
		ok, err := SetPrologFlag(&vm, atomBounded, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationModify, permissionTypeFlag, atomBounded, nil), err)
		assert.False(t, ok)
	})

	t.Run("max_integer", func(t *testing.T) {
		var vm VM
		ok, err := SetPrologFlag(&vm, atomMaxInteger, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationModify, permissionTypeFlag, atomMaxInteger, nil), err)
		assert.False(t, ok)
	})

	t.Run("min_integer", func(t *testing.T) {
		var vm VM
		ok, err := SetPrologFlag(&vm, atomMinInteger, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationModify, permissionTypeFlag, atomMinInteger, nil), err)
		assert.False(t, ok)
	})

	t.Run("integer_rounding_function", func(t *testing.T) {
		var vm VM
		ok, err := SetPrologFlag(&vm, atomIntegerRoundingFunction, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationModify, permissionTypeFlag, atomIntegerRoundingFunction, nil), err)
		assert.False(t, ok)
	})

	t.Run("char_conversion", func(t *testing.T) {
		t.Run("on", func(t *testing.T) {
			var vm VM
			ok, err := SetPrologFlag(&vm, atomCharConversion, atomOn, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.True(t, vm.charConvEnabled)
		})

		t.Run("off", func(t *testing.T) {
			vm := VM{charConvEnabled: true}
			ok, err := SetPrologFlag(&vm, atomCharConversion, atomOff, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.False(t, vm.charConvEnabled)
		})

		t.Run("unknown", func(t *testing.T) {
			vm := VM{charConvEnabled: true}
			ok, err := SetPrologFlag(&vm, atomCharConversion, NewAtom("foo"), Success, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("debug", func(t *testing.T) {
		t.Run("on", func(t *testing.T) {
			var vm VM
			ok, err := SetPrologFlag(&vm, atomDebug, atomOn, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.True(t, vm.debug)
		})

		t.Run("off", func(t *testing.T) {
			vm := VM{debug: true}
			ok, err := SetPrologFlag(&vm, atomDebug, atomOff, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.False(t, vm.debug)
		})

		t.Run("unknown", func(t *testing.T) {
			vm := VM{debug: true}
			ok, err := SetPrologFlag(&vm, atomDebug, NewAtom("foo"), Success, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("max_arity", func(t *testing.T) {
		var vm VM
		ok, err := SetPrologFlag(&vm, atomMaxArity, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationModify, permissionTypeFlag, atomMaxArity, nil), err)
		assert.False(t, ok)
	})

	t.Run("unknown", func(t *testing.T) {
		t.Run("error", func(t *testing.T) {
			vm := VM{unknown: unknownFail}
			ok, err := SetPrologFlag(&vm, atomUnknown, atomError, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, unknownError, vm.unknown)
		})

		t.Run("warning", func(t *testing.T) {
			var vm VM
			ok, err := SetPrologFlag(&vm, atomUnknown, atomWarning, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, unknownWarning, vm.unknown)
		})

		t.Run("fail", func(t *testing.T) {
			var vm VM
			ok, err := SetPrologFlag(&vm, atomUnknown, atomFail, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, unknownFail, vm.unknown)
		})

		t.Run("fail", func(t *testing.T) {
			var vm VM
			ok, err := SetPrologFlag(&vm, atomUnknown, NewAtom("foo"), Success, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("double_quotes", func(t *testing.T) {
		t.Run("codes", func(t *testing.T) {
			var vm VM
			ok, err := SetPrologFlag(&vm, atomDoubleQuotes, atomCodes, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, doubleQuotesCodes, vm.doubleQuotes)
		})

		t.Run("chars", func(t *testing.T) {
			var vm VM
			ok, err := SetPrologFlag(&vm, atomDoubleQuotes, atomChars, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, doubleQuotesChars, vm.doubleQuotes)
		})

		t.Run("atom", func(t *testing.T) {
			var vm VM
			ok, err := SetPrologFlag(&vm, atomDoubleQuotes, atomAtom, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, doubleQuotesAtom, vm.doubleQuotes)
		})

		t.Run("unknown", func(t *testing.T) {
			var vm VM
			ok, err := SetPrologFlag(&vm, atomDoubleQuotes, NewAtom("foo"), Success, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("flag is a variable", func(t *testing.T) {
		var vm VM
		ok, err := SetPrologFlag(&vm, NewVariable(), atomFail, Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("value is a variable", func(t *testing.T) {
		var vm VM
		ok, err := SetPrologFlag(&vm, atomUnknown, NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, instantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("flag is neither a variable nor an atom", func(t *testing.T) {
		var vm VM
		ok, err := SetPrologFlag(&vm, Integer(0), atomFail, Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("flag is an atom but an invalid flag for the processor", func(t *testing.T) {
		var vm VM
		ok, err := SetPrologFlag(&vm, NewAtom("foo"), atomFail, Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainPrologFlag, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("value is inadmissible for flag", func(t *testing.T) {
		var vm VM
		ok, err := SetPrologFlag(&vm, atomUnknown, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainFlagValue, &compound{
			functor: atomPlus,
			args:    []Term{atomUnknown, Integer(0)},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("value is admissible for flag but the flag is not modifiable", func(t *testing.T) {
		var vm VM
		ok, err := SetPrologFlag(&vm, atomBounded, atomTrue, Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationModify, permissionTypeFlag, atomBounded, nil), err)
		assert.False(t, ok)
	})
}

func TestCurrentPrologFlag(t *testing.T) {
	var vm VM

	t.Run("specified", func(t *testing.T) {
		ok, err := CurrentPrologFlag(&vm, atomBounded, atomTrue, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = CurrentPrologFlag(&vm, atomMaxInteger, Integer(math.MaxInt64), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = CurrentPrologFlag(&vm, atomMinInteger, Integer(math.MinInt64), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = CurrentPrologFlag(&vm, atomIntegerRoundingFunction, atomTowardZero, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = CurrentPrologFlag(&vm, atomCharConversion, atomOff, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = CurrentPrologFlag(&vm, atomDebug, atomOff, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = CurrentPrologFlag(&vm, atomMaxArity, atomUnbounded, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = CurrentPrologFlag(&vm, atomUnknown, atomError, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not specified", func(t *testing.T) {
		flag, value := NewVariable(), NewVariable()
		var c int
		ok, err := CurrentPrologFlag(&vm, flag, value, func(env *Env) *Promise {
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
				assert.Equal(t, NewAtom(vm.unknown.String()), env.Resolve(value))
			case 8:
				assert.Equal(t, atomDoubleQuotes, env.Resolve(flag))
				assert.Equal(t, NewAtom(vm.doubleQuotes.String()), env.Resolve(value))
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
		var vm VM
		ok, err := CurrentPrologFlag(&vm, Integer(0), atomError, Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("flag is an atom but an invalid flag for the processor", func(t *testing.T) {
		var vm VM
		ok, err := CurrentPrologFlag(&vm, NewAtom("foo"), atomError, Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainPrologFlag, NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})
}

func TestExpandTerm(t *testing.T) {
	f, g := NewAtom("f"), NewAtom("g")
	a, b, c := NewAtom("a"), NewAtom("b"), NewAtom("c")
	s := NewAtom("s")

	x := NewVariable()

	var vm VM
	assert.NoError(t, vm.Compile(context.Background(), `
term_expansion(f(X), g(X)).
`))

	tests := []struct {
		title string
		in    Term
		out   func() Term
		ok    bool
		err   error
	}{
		{title: "not applicable", in: g.Apply(a), out: func() Term { return g.Apply(a) }, ok: true},
		{title: "applicable", in: f.Apply(a), out: func() Term { return g.Apply(a) }, ok: true},

		// DCG rules
		{
			title: "terminal sequence: empty",
			in:    atomArrow.Apply(s.Apply(a), List()),
			out: func() Term {
				return atomIf.Apply(
					s.Apply(a, lastVariable()+1, lastVariable()+3),
					atomEqual.Apply(lastVariable()+1, lastVariable()+3),
				)
			},
			ok: true,
		},
		{
			title: "terminal sequence: ok",
			in:    atomArrow.Apply(s.Apply(a), List(b)),
			out: func() Term {
				return atomIf.Apply(
					s.Apply(a, lastVariable()+1, lastVariable()+3),
					atomEqual.Apply(lastVariable()+1, PartialList(lastVariable()+3, b)),
				)
			},
			ok: true,
		},
		{
			title: "terminal sequence: variable in head",
			in:    atomArrow.Apply(x, List(b)),
			out:   func() Term { return atomArrow.Apply(x, List(b)) },
			ok:    true,
		},
		{
			title: "terminal sequence: variable in body",
			in:    atomArrow.Apply(s.Apply(a), x),
			out: func() Term {
				return atomIf.Apply(
					s.Apply(a, lastVariable()+1, lastVariable()+3),
					atomPhrase.Apply(x, lastVariable()+1, PartialList(lastVariable()+3, b)),
				)
			},
			ok: true,
		},
		{
			title: "concatenation: ok",
			in:    atomArrow.Apply(s, seq(atomComma, a, b)),
			out: func() Term {
				return atomIf.Apply(
					s.Apply(lastVariable()+1, lastVariable()+3),
					seq(atomComma,
						a.Apply(lastVariable()+1, lastVariable()+4),
						b.Apply(lastVariable()+4, lastVariable()+3),
					),
				)
			},
			ok: true,
		},
		{
			title: "concatenation: lhs is not callable",
			in:    atomArrow.Apply(s, seq(atomComma, Integer(0), b)),
			out:   func() Term { return atomArrow.Apply(s, seq(atomComma, Integer(0), b)) },
			ok:    true,
		},
		{
			title: "concatenation: rhs is not callable",
			in:    atomArrow.Apply(s, seq(atomComma, a, Integer(0))),
			out:   func() Term { return atomArrow.Apply(s, seq(atomComma, a, Integer(0))) },
			ok:    true,
		},
		{
			title: "alternative: ok",
			in:    atomArrow.Apply(s, seq(atomSemiColon, a, b)),
			out: func() Term {
				return atomIf.Apply(
					s.Apply(lastVariable()+1, lastVariable()+3),
					seq(atomSemiColon,
						a.Apply(lastVariable()+1, lastVariable()+3),
						b.Apply(lastVariable()+1, lastVariable()+3),
					),
				)
			},
			ok: true,
		},
		{
			title: "alternative: if-then-else",
			in:    atomArrow.Apply(s, seq(atomSemiColon, atomThen.Apply(a, b), c)),
			out: func() Term {
				return atomIf.Apply(
					s.Apply(lastVariable()+1, lastVariable()+3),
					seq(atomSemiColon,
						atomThen.Apply(
							a.Apply(lastVariable()+1, lastVariable()+4),
							b.Apply(lastVariable()+4, lastVariable()+3),
						),
						c.Apply(lastVariable()+1, lastVariable()+3),
					),
				)
			},
			ok: true,
		},
		{
			title: "alternative: lhs is not callable",
			in:    atomArrow.Apply(s, seq(atomSemiColon, Integer(0), b)),
			out:   func() Term { return atomArrow.Apply(s, seq(atomSemiColon, Integer(0), b)) },
			ok:    true,
		},
		{
			title: "alternative: rhs is not callable",
			in:    atomArrow.Apply(s, seq(atomSemiColon, a, Integer(0))),
			out:   func() Term { return atomArrow.Apply(s, seq(atomSemiColon, a, Integer(0))) },
			ok:    true,
		},
		{
			title: "second form of alternative: ok",
			in:    atomArrow.Apply(s, seq(atomBar, a, b)),
			out: func() Term {
				return atomIf.Apply(
					s.Apply(lastVariable()+1, lastVariable()+3),
					seq(atomSemiColon,
						a.Apply(lastVariable()+1, lastVariable()+3),
						b.Apply(lastVariable()+1, lastVariable()+3),
					),
				)
			},
			ok: true,
		},
		{
			title: "second form of alternative: lhs is not callable",
			in:    atomArrow.Apply(s, seq(atomBar, Integer(0), b)),
			out:   func() Term { return atomArrow.Apply(s, seq(atomBar, Integer(0), b)) },
			ok:    true,
		},
		{
			title: "second form of alternative: rhs is not callable",
			in:    atomArrow.Apply(s, seq(atomBar, a, Integer(0))),
			out:   func() Term { return atomArrow.Apply(s, seq(atomBar, a, Integer(0))) },
			ok:    true,
		},
		{
			title: "grammar-body-goal",
			in:    atomArrow.Apply(s, atomEmptyBlock.Apply(a)),
			out: func() Term {
				return atomIf.Apply(
					s.Apply(lastVariable()+1, lastVariable()+3),
					seq(atomComma,
						a,
						atomEqual.Apply(lastVariable()+1, lastVariable()+3),
					),
				)
			},
			ok: true,
		},
		{
			title: "call",
			in:    atomArrow.Apply(s, atomCall.Apply(a)),
			out: func() Term {
				return atomIf.Apply(
					s.Apply(lastVariable()+1, lastVariable()+3),
					atomCall.Apply(a, lastVariable()+1, lastVariable()+3),
				)
			},
			ok: true,
		},
		{
			title: "phrase",
			in:    atomArrow.Apply(s, atomPhrase.Apply(a)),
			out: func() Term {
				return atomIf.Apply(
					s.Apply(lastVariable()+1, lastVariable()+3),
					atomPhrase.Apply(a, lastVariable()+1, lastVariable()+3),
				)
			},
			ok: true,
		},
		{
			title: "grammar-body-cut",
			in:    atomArrow.Apply(s, atomCut),
			out: func() Term {
				return atomIf.Apply(
					s.Apply(lastVariable()+1, lastVariable()+3),
					seq(atomComma,
						atomCut,
						atomEqual.Apply(lastVariable()+1, lastVariable()+3),
					),
				)
			},
			ok: true,
		},
		{
			title: "grammar-body-not: ok",
			in:    atomArrow.Apply(s, atomNegation.Apply(a)),
			out: func() Term {
				return atomIf.Apply(
					s.Apply(lastVariable()+1, lastVariable()+3),
					seq(atomComma,
						atomNegation.Apply(a.Apply(lastVariable()+1, lastVariable()+4)),
						atomEqual.Apply(lastVariable()+1, lastVariable()+3),
					),
				)
			},
			ok: true,
		},
		{
			title: "grammar-body-not: goal is not callable",
			in:    atomArrow.Apply(s, atomNegation.Apply(Integer(0))),
			out:   func() Term { return atomArrow.Apply(s, atomNegation.Apply(Integer(0))) },
			ok:    true,
		},
		{
			title: "if-then: ok",
			in:    atomArrow.Apply(s, atomThen.Apply(a, b)),
			out: func() Term {
				return atomIf.Apply(
					s.Apply(lastVariable()+1, lastVariable()+3),
					atomThen.Apply(
						a.Apply(lastVariable()+1, lastVariable()+4),
						b.Apply(lastVariable()+4, lastVariable()+3),
					),
				)
			},
			ok: true,
		},
		{
			title: "if-then: lhs is not callable",
			in:    atomArrow.Apply(s, atomThen.Apply(Integer(0), b)),
			out:   func() Term { return atomArrow.Apply(s, atomThen.Apply(Integer(0), b)) },
			ok:    true,
		},
		{
			title: "if-then: rhs is not callable",
			in:    atomArrow.Apply(s, atomThen.Apply(a, Integer(0))),
			out:   func() Term { return atomArrow.Apply(s, atomThen.Apply(a, Integer(0))) },
			ok:    true,
		},
		{
			title: "with semicontexts: ok",
			in:    atomArrow.Apply(atomComma.Apply(NewAtom("phrase1"), List(NewAtom("word"))), atomComma.Apply(NewAtom("phrase2"), NewAtom("phrase3"))),
			out: func() Term {
				return atomIf.Apply(
					NewAtom("phrase1").Apply(lastVariable()+1, lastVariable()+3),
					atomComma.Apply(
						atomComma.Apply(
							NewAtom("phrase2").Apply(lastVariable()+1, lastVariable()+4),
							NewAtom("phrase3").Apply(lastVariable()+4, lastVariable()+2),
						),
						atomEqual.Apply(lastVariable()+3, PartialList(lastVariable()+2, NewAtom("word"))),
					),
				)
			},
			ok: true,
		},
		{
			title: "with semicontexts: head is not callable",
			in:    atomArrow.Apply(atomComma.Apply(Integer(0), List(NewAtom("word"))), atomComma.Apply(NewAtom("phrase2"), NewAtom("phrase3"))),
			out: func() Term {
				return atomArrow.Apply(atomComma.Apply(Integer(0), List(NewAtom("word"))), atomComma.Apply(NewAtom("phrase2"), NewAtom("phrase3")))
			},
			ok: true,
		},
		{
			title: "with semicontexts: semicontext is not callable",
			in:    atomArrow.Apply(atomComma.Apply(NewAtom("phrase1"), Integer(0)), atomComma.Apply(NewAtom("phrase2"), NewAtom("phrase3"))),
			out: func() Term {
				return atomArrow.Apply(atomComma.Apply(NewAtom("phrase1"), Integer(0)), atomComma.Apply(NewAtom("phrase2"), NewAtom("phrase3")))
			},
			ok: true,
		},
		{
			title: "with semicontexts: body is not callable",
			in:    atomArrow.Apply(atomComma.Apply(NewAtom("phrase1"), List(NewAtom("word"))), atomComma.Apply(Integer(0), NewAtom("phrase3"))),
			out: func() Term {
				return atomArrow.Apply(atomComma.Apply(NewAtom("phrase1"), List(NewAtom("word"))), atomComma.Apply(Integer(0), NewAtom("phrase3")))
			},
			ok: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := ExpandTerm(&vm, tt.in, tt.out(), Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestNth0(t *testing.T) {
	t.Run("n is a variable", func(t *testing.T) {
		t.Run("list is a proper list", func(t *testing.T) {
			pair := atomMinus
			var (
				n       = NewVariable()
				elem    = NewVariable()
				results []Term
			)
			ok, err := Nth0(nil, n, List(NewAtom("a"), NewAtom("b"), NewAtom("c")), elem, func(env *Env) *Promise {
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
			_, err := Nth0(nil, NewVariable(), PartialList(NewVariable(), NewAtom("a")), NewVariable(), Failure, nil).Force(context.Background())
			assert.Equal(t, instantiationError(nil), err)
		})
	})

	t.Run("n is an integer", func(t *testing.T) {
		t.Run("list is a proper list", func(t *testing.T) {
			t.Run("n is a valid index", func(t *testing.T) {
				ok, err := Nth0(nil, Integer(1), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewAtom("b"), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("n is too small for an index", func(t *testing.T) {
				ok, err := Nth0(nil, Integer(-1), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewVariable(), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.False(t, ok)
			})

			t.Run("n is too big for an index", func(t *testing.T) {
				ok, err := Nth0(nil, Integer(3), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewVariable(), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.False(t, ok)
			})
		})

		t.Run("list is an improper list", func(t *testing.T) {
			_, err := Nth0(nil, Integer(1), PartialList(NewVariable(), NewAtom("a")), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, instantiationError(nil), err)
		})
	})

	t.Run("n is neither a variable nor an integer", func(t *testing.T) {
		_, err := Nth0(nil, NewAtom("foo"), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, NewAtom("foo"), nil), err)
	})
}

func TestNth1(t *testing.T) {
	t.Run("n is a variable", func(t *testing.T) {
		t.Run("list is a proper list", func(t *testing.T) {
			pair := atomMinus
			var (
				n       = NewVariable()
				elem    = NewVariable()
				results []Term
			)
			ok, err := Nth1(nil, n, List(NewAtom("a"), NewAtom("b"), NewAtom("c")), elem, func(env *Env) *Promise {
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
			_, err := Nth1(nil, NewVariable(), PartialList(NewVariable(), NewAtom("a")), NewVariable(), Failure, nil).Force(context.Background())
			assert.Equal(t, instantiationError(nil), err)
		})
	})

	t.Run("n is an integer", func(t *testing.T) {
		t.Run("list is a proper list", func(t *testing.T) {
			t.Run("n is a valid index", func(t *testing.T) {
				ok, err := Nth1(nil, Integer(2), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewAtom("b"), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("n is too small for an index", func(t *testing.T) {
				ok, err := Nth1(nil, Integer(0), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewVariable(), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.False(t, ok)
			})

			t.Run("n is too big for an index", func(t *testing.T) {
				ok, err := Nth1(nil, Integer(4), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewVariable(), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.False(t, ok)
			})
		})

		t.Run("list is an improper list", func(t *testing.T) {
			_, err := Nth1(nil, Integer(2), PartialList(NewVariable(), NewAtom("a")), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, instantiationError(nil), err)
		})
	})

	t.Run("n is neither a variable nor an integer", func(t *testing.T) {
		_, err := Nth1(nil, NewAtom("foo"), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, NewAtom("foo"), nil), err)
	})
}

func TestSucc(t *testing.T) {
	t.Run("x is a variable", func(t *testing.T) {
		t.Run("s is a variable", func(t *testing.T) {
			_, err := Succ(nil, NewVariable(), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, instantiationError(nil), err)
		})

		t.Run("s is an integer", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				x := NewVariable()
				ok, err := Succ(nil, x, Integer(1), func(env *Env) *Promise {
					assert.Equal(t, Integer(0), env.Resolve(x))
					return Bool(true)
				}, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("s < 0", func(t *testing.T) {
				_, err := Succ(nil, NewVariable(), Integer(-1), Success, nil).Force(context.Background())
				assert.Equal(t, domainError(validDomainNotLessThanZero, Integer(-1), nil), err)
			})

			t.Run("s = 0", func(t *testing.T) {
				ok, err := Succ(nil, NewVariable(), Integer(0), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.False(t, ok)
			})
		})

		t.Run("s is neither a variable nor an integer", func(t *testing.T) {
			_, err := Succ(nil, NewVariable(), Float(1), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeInteger, Float(1), nil), err)
		})
	})

	t.Run("x is an integer", func(t *testing.T) {
		t.Run("s is a variable", func(t *testing.T) {
			s := NewVariable()
			ok, err := Succ(nil, Integer(0), s, func(env *Env) *Promise {
				assert.Equal(t, Integer(1), env.Resolve(s))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("s is an integer", func(t *testing.T) {
			ok, err := Succ(nil, Integer(0), Integer(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("s is neither a variable nor an integer", func(t *testing.T) {
			_, err := Succ(nil, Integer(0), Float(1), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeInteger, Float(1), nil), err)
		})

		t.Run("x is negative", func(t *testing.T) {
			_, err := Succ(nil, Integer(-1), Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, domainError(validDomainNotLessThanZero, Integer(-1), nil), err)
		})

		t.Run("x is math.MaxInt64", func(t *testing.T) {
			_, err := Succ(nil, Integer(math.MaxInt64), Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, evaluationError(exceptionalValueIntOverflow, nil), err)
		})

		t.Run("s is negative", func(t *testing.T) {
			_, err := Succ(nil, Integer(0), Integer(-1), Success, nil).Force(context.Background())
			assert.Equal(t, domainError(validDomainNotLessThanZero, Integer(-1), nil), err)
		})
	})

	t.Run("x is neither a variable nor an integer", func(t *testing.T) {
		_, err := Succ(nil, Float(0), NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, Float(0), nil), err)
	})
}

func TestLength(t *testing.T) {
	t.Run("list is a list", func(t *testing.T) {
		t.Run("length is a variable", func(t *testing.T) {
			n := NewVariable()
			ok, err := Length(nil, List(NewAtom("a"), NewAtom("b"), NewAtom("c")), n, func(env *Env) *Promise {
				assert.Equal(t, Integer(3), env.Resolve(n))
				return Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("length is an integer", func(t *testing.T) {
			t.Run("length is the exact length of list", func(t *testing.T) {
				ok, err := Length(nil, List(NewAtom("a"), NewAtom("b"), NewAtom("c")), Integer(3), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("length is smaller than the length fo list", func(t *testing.T) {
				ok, err := Length(nil, List(NewAtom("a"), NewAtom("b"), NewAtom("c")), Integer(2), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.False(t, ok)
			})
		})
	})

	t.Run("list is a partial list", func(t *testing.T) {
		t.Run("length is a variable", func(t *testing.T) {
			t.Run("length and the suffix of list are different", func(t *testing.T) {
				l := NewVariable()
				n := NewVariable()
				var count int
				ok, err := Length(nil, PartialList(l, NewAtom("a"), NewAtom("b")), n, func(env *Env) *Promise {
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
				l := NewVariable()
				_, err := Length(nil, PartialList(l, NewAtom("a"), NewAtom("b")), l, Success, nil).Force(context.Background())
				assert.Equal(t, resourceError(resourceFiniteMemory, nil), err)
			})
		})

		t.Run("length is an integer", func(t *testing.T) {
			t.Run("small", func(t *testing.T) {
				l := NewVariable()
				ok, err := Length(nil, PartialList(l, NewAtom("a"), NewAtom("b")), Integer(3), func(env *Env) *Promise {
					iter := ListIterator{List: l, Env: env}
					assert.True(t, iter.Next())
					assert.False(t, iter.Next())
					return Bool(true)
				}, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("large", func(t *testing.T) {
				l := NewVariable()
				_, err := Length(nil, PartialList(l, NewAtom("a"), NewAtom("b")), Integer(math.MaxInt64), Success, nil).Force(context.Background())
				assert.Equal(t, resourceError(resourceMemory, nil), err)
			})

			t.Run("out of memory", func(t *testing.T) {
				orig := memFree
				memFree = func() int64 {
					return 0
				}
				defer func() {
					memFree = orig
				}()

				l := NewVariable()
				_, err := Length(nil, PartialList(l, NewAtom("a"), NewAtom("b")), Integer(100*1024*1024), Success, nil).Force(context.Background())
				assert.Equal(t, resourceError(resourceMemory, nil), err)
			})
		})
	})

	t.Run("list is neither a list nor a partial list", func(t *testing.T) {
		t.Run("the suffix is an atom", func(t *testing.T) {
			ok, err := Length(nil, NewAtom("foo"), Integer(3), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("the suffix is a compound", func(t *testing.T) {
			ok, err := Length(nil, NewAtom("foo").Apply(NewAtom("bar")), Integer(3), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("the suffix is neither an atom nor a compound", func(t *testing.T) {
			ok, err := Length(nil, Integer(0), Integer(3), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("length is neither a variable nor an integer", func(t *testing.T) {
		_, err := Length(nil, List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewAtom("three"), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, NewAtom("three"), nil), err)
	})

	t.Run("length is an integer that is less than zero", func(t *testing.T) {
		_, err := Length(nil, List(NewAtom("a"), NewAtom("b"), NewAtom("c")), Integer(-3), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainNotLessThanZero, Integer(-3), nil), err)
	})

	t.Run("list is so long that an integer cannot represent its length", func(t *testing.T) {
		maxInt = 2
		defer func() {
			maxInt = math.MaxInt64
		}()

		t.Run("list is a list", func(t *testing.T) {
			_, err := Length(nil, List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, resourceError(resourceFiniteMemory, nil), err)
		})

		t.Run("list is a partial list", func(t *testing.T) {
			_, err := Length(nil, NewVariable(), NewVariable(), Failure, nil).Force(context.Background())
			assert.Equal(t, representationError(flagMaxInteger, nil), err)
		})
	})
}

func TestSkipMaxList(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		t.Run("without max", func(t *testing.T) {
			ok, err := SkipMaxList(nil, Integer(3), NewVariable(), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), atomEmptyList, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("with max", func(t *testing.T) {
			ok, err := SkipMaxList(nil, Integer(2), Integer(2), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), List(NewAtom("c")), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("max is neither a variable nor an integer", func(t *testing.T) {
		_, err := SkipMaxList(nil, Integer(3), NewAtom("foo"), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), atomEmptyList, Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, NewAtom("foo"), nil), err)
	})

	t.Run("max is negative", func(t *testing.T) {
		_, err := SkipMaxList(nil, Integer(3), Integer(-1), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), atomEmptyList, Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainNotLessThanZero, Integer(-1), nil), err)
	})
}

func TestRepeat(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	c := 0

	_, err := Repeat(nil, func(*Env) *Promise {
		c++
		cancel()
		return Bool(true)
	}, nil).Force(ctx)
	assert.Equal(t, context.Canceled, err)

	assert.Equal(t, 1, c)
}

func TestNegation(t *testing.T) {
	e := errors.New("failed")

	var vm VM
	vm.Register0(atomTrue, func(_ *VM, k Cont, env *Env) *Promise {
		return k(env)
	})
	vm.Register0(atomFalse, func(*VM, Cont, *Env) *Promise {
		return Bool(false)
	})
	vm.Register0(atomError, func(*VM, Cont, *Env) *Promise {
		return Error(e)
	})

	ok, err := Negate(&vm, atomTrue, Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.False(t, ok)

	ok, err = Negate(&vm, atomFalse, Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	_, err = Negate(&vm, atomError, Success, nil).Force(context.Background())
	assert.Equal(t, e, err)
}

func TestAppend(t *testing.T) {
	xs, ys, zs := NewVariable(), NewVariable(), NewVariable()
	tests := []struct {
		title      string
		xs, ys, zs Term
		ok         bool
		err        error
		env        []map[Variable]Term
	}{
		// p.p.2.4 Examples
		{title: `append([a,b],[c,d], Xs).`, xs: List(NewAtom("a"), NewAtom("b")), ys: List(NewAtom("c"), NewAtom("d")), zs: xs, ok: true, env: []map[Variable]Term{
			{xs: List(NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"))},
		}},
		{title: `append([a], nonlist, Xs).`, xs: List(NewAtom("a")), ys: NewAtom("nonlist"), zs: xs, ok: true, env: []map[Variable]Term{
			{xs: PartialList(NewAtom("nonlist"), NewAtom("a"))},
		}},
		{title: `append([a], Ys, Zs).`, xs: List(NewAtom("a")), ys: ys, zs: zs, ok: true, env: []map[Variable]Term{
			{zs: PartialList(ys, NewAtom("a"))},
		}},
		{title: `append(Xs, Ys, [a,b,c]).`, xs: xs, ys: ys, zs: List(NewAtom("a"), NewAtom("b"), NewAtom("c")), ok: true, env: []map[Variable]Term{
			{xs: List(), ys: List(NewAtom("a"), NewAtom("b"), NewAtom("c"))},
			{xs: List(NewAtom("a")), ys: List(NewAtom("b"), NewAtom("c"))},
			{xs: List(NewAtom("a"), NewAtom("b")), ys: List(NewAtom("c"))},
			{xs: List(NewAtom("a"), NewAtom("b"), NewAtom("c")), ys: List()},
		}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := Append(nil, tt.xs, tt.ys, tt.zs, func(env *Env) *Promise {
				for k, v := range tt.env[0] {
					_, ok := env.Unify(k, v)
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

type mockStringWriter struct {
	mock.Mock
}

func (m *mockStringWriter) WriteString(s string) (int, error) {
	args := m.Called(s)
	return args.Int(0), args.Error(1)
}
