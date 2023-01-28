package engine

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
	"io"
	"math"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
	"unicode/utf8"
)

func TestCall(t *testing.T) {
	var vm VM
	vm.Register0(atomFail, func(_ context.Context) *Promise {
		return Bool(false)
	})
	assert.NoError(t, vm.Compile(context.Background(), `
foo.
foo(_, _).
f(g([a, [b|X]])).
`))
	ctx := withVM(context.Background(), &vm)

	tests := []struct {
		title string
		goal  Term
		ok    bool
		err   error

		mem int64
	}{
		// TODO: redo test cases based on 7.8.3.4 Examples
		{title: `undefined atom`, goal: NewAtom("bar"), ok: false, err: existenceError(context.Background(), objectTypeProcedure, atomSlash.Apply(NewAtom("bar"), Integer(0)))},
		{title: `defined atom`, goal: NewAtom("foo"), ok: true},
		{title: `undefined compound`, goal: NewAtom("bar").Apply(NewVariable(), NewVariable()), ok: false, err: existenceError(context.Background(), objectTypeProcedure, atomSlash.Apply(NewAtom("bar"), Integer(2)))},
		{title: `defined compound`, goal: NewAtom("foo").Apply(NewVariable(), NewVariable()), ok: true},
		{title: `variable: single predicate`, goal: NewVariable(), ok: false, err: InstantiationError(context.Background())},
		{title: `variable: multiple predicates`, goal: atomComma.Apply(atomFail, NewVariable()), ok: false},
		{title: `not callable: single predicate`, goal: Integer(0), ok: false, err: typeError(context.Background(), validTypeCallable, Integer(0))},
		{title: `not callable: conjunction`, goal: atomComma.Apply(atomTrue, Integer(0)), ok: false, err: typeError(context.Background(), validTypeCallable, atomComma.Apply(atomTrue, Integer(0)))},
		{title: `not callable: disjunction`, goal: atomSemiColon.Apply(Integer(1), atomTrue), ok: false, err: typeError(context.Background(), validTypeCallable, atomSemiColon.Apply(Integer(1), atomTrue))},

		{title: `cover all`, goal: atomComma.Apply(atomCut, NewAtom("f").Apply(NewAtom("g").Apply(List(NewAtom("a"), PartialList(NewVariable(), NewAtom("b")))))), ok: true},
		{title: `out of memory`, goal: NewAtom("foo").Apply(NewVariable(), NewVariable()), err: resourceError(context.Background(), resourceMemory), mem: 1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			defer setMemFree(tt.mem)()

			ok, err := Call(ctx, tt.goal).Force()
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
		{title: "closure is a variable", closure: NewVariable(), additional: [1]Term{NewAtom("b")}, err: InstantiationError(context.Background())},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [1]Term{NewAtom("b")}, err: typeError(context.Background(), validTypeCallable, Integer(3))},
		{title: "out of memory", closure: NewAtom("p").Apply(NewAtom("a")), additional: [1]Term{NewAtom("b")}, err: resourceError(context.Background(), resourceMemory), mem: 1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			defer setMemFree(tt.mem)()

			vm := VM{procedures: map[procedureIndicator]procedure{
				{name: NewAtom("p"), arity: 2}: Predicate2(func(ctx context.Context, _, _ Term) *Promise {
					return Continue(ctx)
				}),
			}}
			ctx := withVM(context.Background(), &vm)
			ok, err := Call1(ctx, tt.closure, tt.additional[0]).Force()
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
		{title: "closure is a variable", closure: NewVariable(), additional: [2]Term{NewAtom("b"), NewAtom("c")}, err: InstantiationError(context.Background())},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [2]Term{NewAtom("b"), NewAtom("c")}, err: typeError(context.Background(), validTypeCallable, Integer(3))},
		{title: "out of memory", closure: NewAtom("p").Apply(NewAtom("a")), additional: [2]Term{NewAtom("b"), NewAtom("c")}, err: resourceError(context.Background(), resourceMemory), mem: 1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			defer setMemFree(tt.mem)()

			vm := VM{procedures: map[procedureIndicator]procedure{
				{name: NewAtom("p"), arity: 3}: Predicate3(func(ctx context.Context, _, _, _ Term) *Promise {
					return Continue(ctx)
				}),
			}}
			ctx := withVM(context.Background(), &vm)
			ok, err := Call2(ctx, tt.closure, tt.additional[0], tt.additional[1]).Force()
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
		{title: "closure is a variable", closure: NewVariable(), additional: [3]Term{NewAtom("b"), NewAtom("c"), NewAtom("d")}, err: InstantiationError(context.Background())},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [3]Term{NewAtom("b"), NewAtom("c"), NewAtom("d")}, err: typeError(context.Background(), validTypeCallable, Integer(3))},
		{title: "out of memory", closure: NewAtom("p").Apply(NewAtom("a")), additional: [3]Term{NewAtom("b"), NewAtom("c"), NewAtom("d")}, err: resourceError(context.Background(), resourceMemory), mem: 1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			defer setMemFree(tt.mem)()

			vm := VM{procedures: map[procedureIndicator]procedure{
				{name: NewAtom("p"), arity: 4}: Predicate4(func(ctx context.Context, _, _, _, _ Term) *Promise {
					return Continue(ctx)
				}),
			}}
			ctx := withVM(context.Background(), &vm)
			ok, err := Call3(ctx, tt.closure, tt.additional[0], tt.additional[1], tt.additional[2]).Force()
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
		{title: "closure is a variable", closure: NewVariable(), additional: [4]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e")}, err: InstantiationError(context.Background())},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [4]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e")}, err: typeError(context.Background(), validTypeCallable, Integer(3))},
		{title: "out of memory", closure: NewAtom("p").Apply(NewAtom("a")), additional: [4]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e")}, err: resourceError(context.Background(), resourceMemory), mem: 1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			defer setMemFree(tt.mem)()

			vm := VM{procedures: map[procedureIndicator]procedure{
				{name: NewAtom("p"), arity: 5}: Predicate5(func(ctx context.Context, _, _, _, _, _ Term) *Promise {
					return Continue(ctx)
				}),
			}}
			ctx := withVM(context.Background(), &vm)
			ok, err := Call4(ctx, tt.closure, tt.additional[0], tt.additional[1], tt.additional[2], tt.additional[3]).Force()
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
		{title: "closure is a variable", closure: NewVariable(), additional: [5]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f")}, err: InstantiationError(context.Background())},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [5]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f")}, err: typeError(context.Background(), validTypeCallable, Integer(3))},
		{title: "out of memory", closure: NewAtom("p").Apply(NewAtom("a")), additional: [5]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f")}, err: resourceError(context.Background(), resourceMemory), mem: 1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			defer setMemFree(tt.mem)()

			vm := VM{procedures: map[procedureIndicator]procedure{
				{name: NewAtom("p"), arity: 6}: Predicate6(func(ctx context.Context, _, _, _, _, _, _ Term) *Promise {
					return Continue(ctx)
				}),
			}}
			ctx := withVM(context.Background(), &vm)
			ok, err := Call5(ctx, tt.closure, tt.additional[0], tt.additional[1], tt.additional[2], tt.additional[3], tt.additional[4]).Force()
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
		{title: "closure is a variable", closure: NewVariable(), additional: [6]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g")}, err: InstantiationError(context.Background())},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [6]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g")}, err: typeError(context.Background(), validTypeCallable, Integer(3))},
		{title: "out of memory", closure: NewAtom("p").Apply(NewAtom("a")), additional: [6]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g")}, err: resourceError(context.Background(), resourceMemory), mem: 1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			defer setMemFree(tt.mem)()

			vm := VM{procedures: map[procedureIndicator]procedure{
				{name: NewAtom("p"), arity: 7}: Predicate7(func(ctx context.Context, _, _, _, _, _, _, _ Term) *Promise {
					return Continue(ctx)
				}),
			}}
			ctx := withVM(context.Background(), &vm)
			ok, err := Call6(ctx, tt.closure, tt.additional[0], tt.additional[1], tt.additional[2], tt.additional[3], tt.additional[4], tt.additional[5]).Force()
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
		{title: "closure is a variable", closure: NewVariable(), additional: [7]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g"), NewAtom("h")}, err: InstantiationError(context.Background())},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [7]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g"), NewAtom("h")}, err: typeError(context.Background(), validTypeCallable, Integer(3))},
		{title: "out of memory", closure: NewAtom("p").Apply(NewAtom("a")), additional: [7]Term{NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g"), NewAtom("h")}, err: resourceError(context.Background(), resourceMemory), mem: 1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			defer setMemFree(tt.mem)()

			vm := VM{procedures: map[procedureIndicator]procedure{
				{name: NewAtom("p"), arity: 8}: Predicate8(func(ctx context.Context, _, _, _, _, _, _, _, _ Term) *Promise {
					return Continue(ctx)
				}),
			}}
			ctx := withVM(context.Background(), &vm)
			ok, err := Call7(ctx, tt.closure, tt.additional[0], tt.additional[1], tt.additional[2], tt.additional[3], tt.additional[4], tt.additional[5], tt.additional[6]).Force()
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestCallNth(t *testing.T) {
	vm := VM{
		procedures: map[procedureIndicator]procedure{
			{name: NewAtom("foo"), arity: 0}: Predicate0(func(ctx context.Context) *Promise {
				return Delay(func() *Promise {
					return Continue(ctx)
				}, func() *Promise {
					return Continue(ctx)
				}, func() *Promise {
					return Error(errors.New("three"))
				})
			}),
		},
	}
	ctx := withVM(context.Background(), &vm)

	t.Run("ok", func(t *testing.T) {
		t.Run("nth is a variable", func(t *testing.T) {
			nth := NewVariable()

			var ns []Integer
			ctx := WithCont(ctx, func(ctx context.Context) *Promise {
				n, ok := Resolve(ctx, nth).(Integer)
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
			})
			ok, err := CallNth(ctx, NewAtom("foo"), nth).Force()
			assert.NoError(t, err)
			assert.True(t, ok)

			assert.Equal(t, []Integer{1, 2}, ns)
		})

		t.Run("nth is an integer", func(t *testing.T) {
			ctx := WithCont(ctx, func(context.Context) *Promise {
				return Bool(false)
			})
			ok, err := CallNth(ctx, NewAtom("foo"), Integer(2)).Force()
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("nth is 0", func(t *testing.T) {
		ok, err := CallNth(ctx, NewAtom("foo"), Integer(0)).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("goal is a variable and nth is not zero", func(t *testing.T) {
		_, err := CallNth(ctx, NewVariable(), Integer(3)).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
	})

	t.Run("goal is neither a variable nor a callable term", func(t *testing.T) {
		_, err := CallNth(ctx, Integer(0), Integer(3)).Force()
		assert.Equal(t, typeError(context.Background(), validTypeCallable, Integer(0)), err)
	})

	t.Run("nth is neither a variable nor an integer", func(t *testing.T) {
		_, err := CallNth(ctx, NewAtom("foo"), NewAtom("bar")).Force()
		assert.Equal(t, typeError(context.Background(), validTypeInteger, NewAtom("bar")), err)
	})

	t.Run("nth is an integer which is less than zero", func(t *testing.T) {
		_, err := CallNth(ctx, NewAtom("foo"), Integer(-1)).Force()
		assert.Equal(t, domainError(context.Background(), validDomainNotLessThanZero, Integer(-1)), err)
	})

	t.Run("n+1 is larger than max_integer", func(t *testing.T) {
		maxInt = 0
		defer func() {
			maxInt = math.MaxInt64
		}()
		_, err := CallNth(ctx, NewAtom("foo"), NewVariable()).Force()
		assert.Equal(t, representationError(context.Background(), flagMaxInteger), err)
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
			ctx := withEnv(context.Background(), tt.premise)
			ctx = WithCont(ctx, func(ctx context.Context) *Promise {
				for k, v := range tt.env {
					_, ok := withUnification(ctx, k, v)
					assert.True(t, ok)
				}
				return Bool(true)
			})
			ok, err := Unify(ctx, tt.x, tt.y).Force()
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
			ctx := withEnv(context.Background(), tt.premise)
			ctx = WithCont(ctx, func(ctx context.Context) *Promise {
				for k, v := range tt.env {
					_, ok := withUnification(ctx, k, v)
					assert.True(t, ok)
				}
				return Bool(true)
			})
			ok, err := UnifyWithOccursCheck(ctx, tt.x, tt.y).Force()
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestSubsumesTerm(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		ok, err := SubsumesTerm(context.Background(), NewVariable(), NewAtom("a")).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not unifiable", func(t *testing.T) {
		ok, err := SubsumesTerm(context.Background(), NewAtom("a"), NewAtom("b")).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("specific-general", func(t *testing.T) {
		ok, err := SubsumesTerm(context.Background(), NewAtom("a"), NewVariable()).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeVar(t *testing.T) {
	t.Run("var", func(t *testing.T) {
		ok, err := TypeVar(context.Background(), NewVariable()).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not var", func(t *testing.T) {
		ok, err := TypeVar(context.Background(), NewAtom("foo")).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeFloat(t *testing.T) {
	t.Run("float", func(t *testing.T) {
		ok, err := TypeFloat(context.Background(), Float(1.0)).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not float", func(t *testing.T) {
		ok, err := TypeFloat(context.Background(), NewAtom("foo")).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeInteger(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		ok, err := TypeInteger(context.Background(), Integer(1)).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not integer", func(t *testing.T) {
		ok, err := TypeInteger(context.Background(), NewAtom("foo")).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeAtom(t *testing.T) {
	t.Run("atom", func(t *testing.T) {
		ok, err := TypeAtom(context.Background(), NewAtom("foo")).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not atom", func(t *testing.T) {
		ok, err := TypeAtom(context.Background(), Integer(1)).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeCompound(t *testing.T) {
	t.Run("compound", func(t *testing.T) {
		ok, err := TypeCompound(context.Background(), &compound{
			functor: NewAtom("foo"),
			args:    []Term{NewAtom("a")},
		}).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not compound", func(t *testing.T) {
		ok, err := TypeCompound(context.Background(), NewAtom("foo")).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestAcyclicTerm(t *testing.T) {
	t.Run("atomic", func(t *testing.T) {
		ok, err := AcyclicTerm(context.Background(), NewAtom("a")).Force()
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

			ok, err := AcyclicTerm(context.Background(), &c).Force()
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
		{title: `functor(X, Y, 3).`, term: x, name: y, arity: Integer(3), err: InstantiationError(context.Background())},
		{title: `functor(X, foo, N).`, term: x, name: NewAtom("foo"), arity: n, err: InstantiationError(context.Background())},
		{title: `functor(X, foo, a).`, term: x, name: NewAtom("foo"), arity: NewAtom("a"), err: typeError(context.Background(), validTypeInteger, NewAtom("a"))},
		{title: `functor(F, 1.5, 1).`, term: f, name: Float(1.5), arity: Integer(1), err: typeError(context.Background(), validTypeAtom, Float(1.5))},
		{title: `functor(F, foo(a), 1).`, term: f, name: NewAtom("foo").Apply(NewAtom("a")), arity: Integer(1), err: typeError(context.Background(), validTypeAtomic, NewAtom("foo").Apply(NewAtom("a")))},
		// {title: `current_prolog_flag(max_arity, A), X is A + 1, functor(T, foo, X).`}
		{title: `Minus_1 is 0 - 1, functor(F, foo, Minus_1).`, term: f, name: NewAtom("foo"), arity: Integer(-1), err: domainError(context.Background(), validDomainNotLessThanZero, Integer(-1))},

		// https://github.com/ichiban/prolog/issues/247
		{title: `functor(X, Y, 0).`, term: x, name: y, arity: Integer(0), err: InstantiationError(context.Background())},

		// https://github.com/ichiban/prolog/issues/226
		{title: `functor(F, f, max_int).`, term: f, name: NewAtom("f"), arity: maxInt, err: resourceError(context.Background(), resourceMemory)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
				for k, v := range tt.env {
					_, ok := withUnification(ctx, k, v)
					assert.True(t, ok)
				}
				return Bool(true)
			})
			ok, err := Functor(ctx, tt.term, tt.name, tt.arity).Force()
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestArg(t *testing.T) {
	t.Run("term is a variable", func(t *testing.T) {
		v := NewVariable()
		ok, err := Arg(context.Background(), NewVariable(), v, NewVariable()).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("term is not a compound", func(t *testing.T) {
		ok, err := Arg(context.Background(), NewVariable(), NewAtom("foo"), NewVariable()).Force()
		assert.Equal(t, typeError(context.Background(), validTypeCompound, NewAtom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("nth is a variable", func(t *testing.T) {
		nth := NewVariable()
		_, err := Arg(context.Background(), nth, &compound{
			functor: NewAtom("f"),
			args:    []Term{NewAtom("a"), NewAtom("b"), NewAtom("a")},
		}, NewAtom("a")).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
	})

	t.Run("nth is an integer", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			ok, err := Arg(context.Background(), Integer(1), &compound{
				functor: NewAtom("f"),
				args:    []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")},
			}, NewAtom("a")).Force()
			assert.NoError(t, err)
			assert.True(t, ok)

			ok, err = Arg(context.Background(), Integer(2), &compound{
				functor: NewAtom("f"),
				args:    []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")},
			}, NewAtom("b")).Force()
			assert.NoError(t, err)
			assert.True(t, ok)

			ok, err = Arg(context.Background(), Integer(3), &compound{
				functor: NewAtom("f"),
				args:    []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")},
			}, NewAtom("c")).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("ng", func(t *testing.T) {
			ok, err := Arg(context.Background(), Integer(0), &compound{
				functor: NewAtom("f"),
				args:    []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")},
			}, NewVariable()).Force()
			assert.NoError(t, err)
			assert.False(t, ok)

			ok, err = Arg(context.Background(), Integer(4), &compound{
				functor: NewAtom("f"),
				args:    []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")},
			}, NewVariable()).Force()
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("negative", func(t *testing.T) {
			ok, err := Arg(context.Background(), Integer(-2), &compound{
				functor: NewAtom("f"),
				args:    []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")},
			}, NewAtom("b")).Force()
			assert.Equal(t, domainError(context.Background(), validDomainNotLessThanZero, Integer(-2)), err)
			assert.False(t, ok)
		})
	})

	t.Run("nth is neither a variable nor an integer", func(t *testing.T) {
		ok, err := Arg(context.Background(), NewAtom("foo"), &compound{
			functor: NewAtom("f"),
			args:    []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")},
		}, NewAtom("b")).Force()
		assert.Equal(t, typeError(context.Background(), validTypeInteger, NewAtom("foo")), err)
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
		{title: "7", term: x, list: y, err: InstantiationError(context.Background())},
		{title: "8", term: x, list: PartialList(y, NewAtom("foo"), NewAtom("a")), err: InstantiationError(context.Background())},
		{title: "9", term: x, list: PartialList(NewAtom("bar"), NewAtom("foo")), err: typeError(context.Background(), validTypeList, PartialList(NewAtom("bar"), NewAtom("foo")))},
		{title: "10", term: x, list: List(foo, NewAtom("bar")), err: InstantiationError(context.Background())},
		{title: "11", term: x, list: List(Integer(3), Integer(1)), err: typeError(context.Background(), validTypeAtom, Integer(3))},
		{title: "12", term: x, list: List(Float(1.1), NewAtom("foo")), err: typeError(context.Background(), validTypeAtom, Float(1.1))},
		{title: "13", term: x, list: List(NewAtom("a").Apply(NewAtom("b")), Integer(1)), err: typeError(context.Background(), validTypeAtom, NewAtom("a").Apply(NewAtom("b")))},
		{title: "14", term: x, list: Integer(4), err: typeError(context.Background(), validTypeList, Integer(4))},
		{title: "15", term: NewAtom("f").Apply(x), list: List(NewAtom("f"), NewAtom("u").Apply(x)), ok: true, env: map[Variable]Term{
			x: NewAtom("u").Apply(x),
		}},

		// 8.5.3.3 Errors
		{title: "b: term is a compound", term: NewAtom("f").Apply(NewAtom("a")), list: PartialList(NewAtom("a"), NewAtom("f")), err: typeError(context.Background(), validTypeList, PartialList(NewAtom("a"), NewAtom("f")))},
		{title: "b: term is an atomic", term: Integer(1), list: PartialList(NewAtom("a"), NewAtom("f")), err: typeError(context.Background(), validTypeList, PartialList(NewAtom("a"), NewAtom("f")))},
		{title: "c", term: x, list: List(y), err: InstantiationError(context.Background())},
		{title: "e", term: x, list: List(NewAtom("f").Apply(NewAtom("a"))), err: typeError(context.Background(), validTypeAtomic, NewAtom("f").Apply(NewAtom("a")))},
		{title: "f", term: x, list: List(), err: domainError(context.Background(), validDomainNonEmptyList, List())},

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
			ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
				for k, v := range tt.env {
					assert.Equal(t, v, Resolve(ctx, k))
				}
				return Bool(true)
			})
			ok, err := Univ(ctx, tt.term, tt.list).Force()
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
			ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
				for k, v := range tt.env {
					assert.Equal(t, v, Resolve(ctx, k))
				}
				return Bool(true)
			})
			ok, err := CopyTerm(ctx, tt.in, tt.out).Force()
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
		{title: "3", term: NewAtom("t"), vars: PartialList(NewAtom("a"), NewAtom("x"), NewAtom("y")), err: typeError(context.Background(), validTypeList, PartialList(NewAtom("a"), NewAtom("x"), NewAtom("y")))},
		{title: "4, 5", term: vs, vars: vars, ok: true, env: map[Variable]Term{
			vars: List(b, a),
			vs:   atomPlus.Apply(b, vt),
			vt:   NewAtom("*").Apply(a, b),
		}},
		{title: "6", term: atomPlus.Apply(atomPlus.Apply(a, b), b), vars: PartialList(vars, b), ok: true, env: map[Variable]Term{
			b:    a,
			vars: List(b),
		}},

		{title: "out of memory", term: NewAtom("f").Apply(NewVariable(), NewVariable()), vars: vars, ok: false, err: resourceError(context.Background(), resourceMemory), mem: 1},
	}

	ctx, _ := withUnification(context.Background(),
		tuple(vs, vt),
		tuple(atomPlus.Apply(b, vt), NewAtom("*").Apply(a, b)),
	)
	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			defer setMemFree(tt.mem)()

			ctx := WithCont(ctx, func(ctx context.Context) *Promise {
				for k, v := range tt.env {
					assert.Equal(t, v, Resolve(ctx, k))
				}
				return Bool(true)
			})
			ok, err := TermVariables(ctx, tt.term, tt.vars).Force()
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
			ctx := withVM(context.Background(), &vm)

			ok, err := Op(ctx, Integer(1000), atomXFX, NewAtom("++")).Force()
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
			ctx := withVM(context.Background(), &vm)
			ok, err := Op(ctx, Integer(1000), atomXFX, List(NewAtom("++"), NewAtom("++"))).Force()
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
		ctx := withVM(context.Background(), &vm)
		ok, err := Op(ctx, Integer(0), atomXFX, NewAtom("++")).Force()
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
		ok, err := Op(context.Background(), NewVariable(), atomXFX, atomPlus).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("specifier is a variable", func(t *testing.T) {
		ok, err := Op(context.Background(), Integer(1000), NewVariable(), atomPlus).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("priority is neither a variable nor an integer", func(t *testing.T) {
		ok, err := Op(context.Background(), NewAtom("foo"), atomXFX, atomPlus).Force()
		assert.Equal(t, typeError(context.Background(), validTypeInteger, NewAtom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("specifier is neither a variable nor an atom", func(t *testing.T) {
		ok, err := Op(context.Background(), Integer(1000), Integer(0), atomPlus).Force()
		assert.Equal(t, typeError(context.Background(), validTypeAtom, Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("operator is neither a partial list nor a list nor an atom", func(t *testing.T) {
		ok, err := Op(context.Background(), Integer(1000), atomXFX, Integer(0)).Force()
		assert.Equal(t, typeError(context.Background(), validTypeList, Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the operator list is neither a variable nor an atom", func(t *testing.T) {
		ok, err := Op(context.Background(), Integer(1000), atomXFX, List(Integer(0))).Force()
		assert.Equal(t, typeError(context.Background(), validTypeAtom, Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("priority is not between 0 and 1200 inclusive", func(t *testing.T) {
		t.Run("priority is negative", func(t *testing.T) {
			ok, err := Op(context.Background(), Integer(-1), atomXFX, atomPlus).Force()
			assert.Equal(t, domainError(context.Background(), validDomainOperatorPriority, Integer(-1)), err)
			assert.False(t, ok)
		})

		t.Run("priority is more than 1200", func(t *testing.T) {
			ok, err := Op(context.Background(), Integer(1201), atomXFX, atomPlus).Force()
			assert.Equal(t, domainError(context.Background(), validDomainOperatorPriority, Integer(1201)), err)
			assert.False(t, ok)
		})
	})

	t.Run("specifier is not a valid operator specifier", func(t *testing.T) {
		ok, err := Op(context.Background(), Integer(1000), NewAtom("foo"), atomPlus).Force()
		assert.Equal(t, domainError(context.Background(), validDomainOperatorSpecifier, NewAtom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("operator is ','", func(t *testing.T) {
		vm := VM{operators: operators{}}
		vm.operators.define(1000, operatorSpecifierXFY, NewAtom(`,`))
		ctx := withVM(context.Background(), &vm)
		ok, err := Op(ctx, Integer(1000), atomXFY, atomComma).Force()
		assert.Equal(t, permissionError(context.Background(), operationModify, permissionTypeOperator, atomComma), err)
		assert.False(t, ok)
	})

	t.Run("an element of the operator list is ','", func(t *testing.T) {
		vm := VM{operators: operators{}}
		vm.operators.define(1000, operatorSpecifierXFY, NewAtom(`,`))
		ctx := withVM(context.Background(), &vm)
		ok, err := Op(ctx, Integer(1000), atomXFY, List(atomComma)).Force()
		assert.Equal(t, permissionError(context.Background(), operationModify, permissionTypeOperator, atomComma), err)
		assert.False(t, ok)
	})

	t.Run("operator is an atom, priority is a priority, and specifier is a specifier such that operator would have an invalid set of priorities and specifiers", func(t *testing.T) {
		t.Run("empty list", func(t *testing.T) {
			ok, err := Op(context.Background(), Integer(1000), atomXFY, atomEmptyList).Force()
			assert.Equal(t, permissionError(context.Background(), operationCreate, permissionTypeOperator, atomEmptyList), err)
			assert.False(t, ok)
		})

		t.Run("empty curly brackets", func(t *testing.T) {
			ok, err := Op(context.Background(), Integer(1000), atomXFY, atomEmptyBlock).Force()
			assert.Equal(t, permissionError(context.Background(), operationCreate, permissionTypeOperator, atomEmptyBlock), err)
			assert.False(t, ok)
		})

		t.Run("bar", func(t *testing.T) {
			t.Run("create", func(t *testing.T) {
				ok, err := Op(context.Background(), Integer(1000), atomXFY, atomBar).Force()
				assert.Equal(t, permissionError(context.Background(), operationCreate, permissionTypeOperator, atomBar), err)
				assert.False(t, ok)
			})

			t.Run("modify", func(t *testing.T) {
				vm := VM{operators: operators{}}
				vm.operators.define(1001, operatorSpecifierXFY, NewAtom(`|`))
				ctx := withVM(context.Background(), &vm)
				ok, err := Op(ctx, Integer(1000), atomXFY, atomBar).Force()
				assert.Equal(t, permissionError(context.Background(), operationModify, permissionTypeOperator, atomBar), err)
				assert.False(t, ok)
			})
		})
	})

	t.Run("operator is a list, priority is a priority, and specifier is a specifier such that an element op of the list operator would have an invalid set of priorities and specifiers", func(t *testing.T) {
		t.Run("empty list", func(t *testing.T) {
			ok, err := Op(context.Background(), Integer(1000), atomXFY, List(atomEmptyList)).Force()
			assert.Equal(t, permissionError(context.Background(), operationCreate, permissionTypeOperator, atomEmptyList), err)
			assert.False(t, ok)
		})

		t.Run("empty curly brackets", func(t *testing.T) {
			ok, err := Op(context.Background(), Integer(1000), atomXFY, List(atomEmptyBlock)).Force()
			assert.Equal(t, permissionError(context.Background(), operationCreate, permissionTypeOperator, atomEmptyBlock), err)
			assert.False(t, ok)
		})

		t.Run("bar", func(t *testing.T) {
			t.Run("create", func(t *testing.T) {
				ok, err := Op(context.Background(), Integer(1000), atomXFY, List(atomBar)).Force()
				assert.Equal(t, permissionError(context.Background(), operationCreate, permissionTypeOperator, atomBar), err)
				assert.False(t, ok)
			})

			t.Run("modify", func(t *testing.T) {
				vm := VM{operators: operators{}}
				vm.operators.define(101, operatorSpecifierXFY, NewAtom(`|`))
				ctx := withVM(context.Background(), &vm)
				ok, err := Op(ctx, Integer(1000), atomXFY, List(atomBar)).Force()
				assert.Equal(t, permissionError(context.Background(), operationModify, permissionTypeOperator, atomBar), err)
				assert.False(t, ok)
			})
		})
	})

	t.Run("There shall not be an infix and a postfix operator with the same name.", func(t *testing.T) {
		t.Run("infix", func(t *testing.T) {
			vm := VM{operators: operators{}}
			vm.operators.define(200, operatorSpecifierYF, NewAtom(`+`))
			ctx := withVM(context.Background(), &vm)
			ok, err := Op(ctx, Integer(500), atomYFX, List(atomPlus)).Force()
			assert.Equal(t, permissionError(context.Background(), operationCreate, permissionTypeOperator, atomPlus), err)
			assert.False(t, ok)
		})

		t.Run("postfix", func(t *testing.T) {
			vm := VM{operators: operators{}}
			vm.operators.define(500, operatorSpecifierYFX, NewAtom(`+`))
			ctx := withVM(context.Background(), &vm)
			ok, err := Op(ctx, Integer(200), atomYF, List(atomPlus)).Force()
			assert.Equal(t, permissionError(context.Background(), operationCreate, permissionTypeOperator, atomPlus), err)
			assert.False(t, ok)
		})
	})
}

func TestCurrentOp(t *testing.T) {
	vm := VM{operators: operators{}}
	vm.operators.define(900, operatorSpecifierXFX, NewAtom(`+++`))
	vm.operators.define(1000, operatorSpecifierXFX, NewAtom(`++`))
	vm.operators.define(1100, operatorSpecifierXFX, NewAtom(`+`))
	ctx := withVM(context.Background(), &vm)

	t.Run("single solution", func(t *testing.T) {
		ok, err := CurrentOp(ctx, Integer(1100), atomXFX, atomPlus).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("multiple solutions", func(t *testing.T) {
		priority, specifier, operator := NewVariable(), NewVariable(), NewVariable()
		ctx := WithCont(ctx, func(ctx context.Context) *Promise {
			switch Resolve(ctx, operator) {
			case NewAtom("+++"):
				assert.Equal(t, Integer(900), Resolve(ctx, priority))
				assert.Equal(t, atomXFX, Resolve(ctx, specifier))
			case NewAtom("++"):
				assert.Equal(t, Integer(1000), Resolve(ctx, priority))
				assert.Equal(t, atomXFX, Resolve(ctx, specifier))
			case atomPlus:
				assert.Equal(t, Integer(1100), Resolve(ctx, priority))
				assert.Equal(t, atomXFX, Resolve(ctx, specifier))
			default:
				assert.Fail(t, "unreachable")
			}
			return Bool(false)
		})
		ok, err := CurrentOp(ctx, priority, specifier, operator).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("priority is not an operator priority", func(t *testing.T) {
		t.Run("priority is not an integer", func(t *testing.T) {
			ok, err := CurrentOp(ctx, NewAtom("foo"), atomXFX, atomPlus).Force()
			assert.Equal(t, domainError(context.Background(), validDomainOperatorPriority, NewAtom("foo")), err)
			assert.False(t, ok)
		})

		t.Run("priority is negative", func(t *testing.T) {
			ok, err := CurrentOp(ctx, Integer(-1), atomXFX, atomPlus).Force()
			assert.Equal(t, domainError(context.Background(), validDomainOperatorPriority, Integer(-1)), err)
			assert.False(t, ok)
		})
	})

	t.Run("specifier is not an operator specifier", func(t *testing.T) {
		t.Run("specifier is not an atom", func(t *testing.T) {
			ok, err := CurrentOp(ctx, Integer(1100), Integer(0), atomPlus).Force()
			assert.Equal(t, domainError(context.Background(), validDomainOperatorSpecifier, Integer(0)), err)
			assert.False(t, ok)
		})

		t.Run("specifier is a non-specifier atom", func(t *testing.T) {
			ok, err := CurrentOp(ctx, Integer(1100), NewAtom("foo"), atomPlus).Force()
			assert.Equal(t, domainError(context.Background(), validDomainOperatorSpecifier, NewAtom("foo")), err)
			assert.False(t, ok)
		})
	})

	t.Run("operator is not an atom", func(t *testing.T) {
		ok, err := CurrentOp(ctx, Integer(1100), atomXFX, Integer(0)).Force()
		assert.Equal(t, typeError(context.Background(), validTypeAtom, Integer(0)), err)
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
			err:       InstantiationError(context.Background()),
		},
		{
			title:     "bagof(X, 1, L).",
			template:  x,
			goal:      Integer(1),
			instances: l,
			err:       typeError(context.Background(), validTypeCallable, Integer(1)),
		},

		// 8.10.2.3 Errors
		{
			title:     "c",
			template:  NewAtom("t"),
			goal:      atomTrue,
			instances: PartialList(Integer(1), NewAtom("t")),
			err:       typeError(context.Background(), validTypeList, PartialList(Integer(1), NewAtom("t"))),
		},

		{
			title:     "out of memory",
			template:  x,
			goal:      atomSemiColon.Apply(atomEqual.Apply(x, y), atomEqual.Apply(x, y)),
			instances: s,
			err:       resourceError(context.Background(), resourceMemory),
			mem:       1,
		},
	}

	vm := VM{
		unknown: unknownWarning,
	}
	vm.Register2(atomEqual, Unify)
	vm.Register2(atomComma, func(ctx context.Context, g1, g2 Term) *Promise {
		ctx = WithCont(ctx, func(ctx context.Context) *Promise {
			return Call(ctx, g2)
		})
		return Call(ctx, g1)
	})
	vm.Register2(atomSemiColon, func(ctx context.Context, g1, g2 Term) *Promise {
		return Delay(func() *Promise {
			return Call(ctx, g1)
		}, func() *Promise {
			return Call(ctx, g2)
		})
	})
	vm.Register0(atomTrue, func(ctx context.Context) *Promise {
		return Continue(ctx)
	})
	vm.Register0(atomFail, func(ctx context.Context) *Promise {
		return Bool(false)
	})
	vm.Register2(NewAtom("a"), func(ctx context.Context, x, y Term) *Promise {
		a, f := NewAtom("$a"), NewAtom("f")
		return Delay(func() *Promise {
			return Unify(ctx, a.Apply(x, y), a.Apply(Integer(1), f.Apply(NewVariable())))
		}, func() *Promise {
			return Unify(ctx, a.Apply(x, y), a.Apply(Integer(2), f.Apply(NewVariable())))
		})
	})
	vm.Register2(NewAtom("b"), func(ctx context.Context, x, y Term) *Promise {
		b := NewAtom("$b")
		return Delay(func() *Promise {
			return Unify(ctx, b.Apply(x, y), b.Apply(Integer(1), Integer(1)))
		}, func() *Promise {
			return Unify(ctx, b.Apply(x, y), b.Apply(Integer(1), Integer(1)))
		}, func() *Promise {
			return Unify(ctx, b.Apply(x, y), b.Apply(Integer(1), Integer(2)))
		}, func() *Promise {
			return Unify(ctx, b.Apply(x, y), b.Apply(Integer(2), Integer(1)))
		}, func() *Promise {
			return Unify(ctx, b.Apply(x, y), b.Apply(Integer(2), Integer(2)))
		}, func() *Promise {
			return Unify(ctx, b.Apply(x, y), b.Apply(Integer(2), Integer(2)))
		})
	})

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			defer setMemFree(tt.mem)()

			vm.Unknown = func(context.Context, Atom, []Term) {
				assert.True(t, tt.warning)
			}
			ctx := withVM(context.Background(), &vm)
			ctx = WithCont(ctx, func(ctx context.Context) *Promise {
				for k, v := range tt.env[0] {
					_, ok := withUnification(ctx, v, k)
					assert.True(t, ok)
				}
				tt.env = tt.env[1:]
				return Bool(false)
			})
			_, err := BagOf(ctx, tt.template, tt.goal, tt.instances).Force()
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
			err:       typeError(context.Background(), validTypeList, PartialList(Integer(1), NewAtom("t"))),
		},

		{
			title:     "out of memory",
			template:  x,
			goal:      atomSemiColon.Apply(atomEqual.Apply(x, y), atomEqual.Apply(x, y)),
			instances: s,
			err:       resourceError(context.Background(), resourceMemory),
			mem:       1,
		},
	}

	vm := VM{
		unknown: unknownWarning,
	}
	vm.Register2(atomEqual, Unify)
	vm.Register2(atomComma, func(ctx context.Context, g1, g2 Term) *Promise {
		ctx = WithCont(ctx, func(ctx context.Context) *Promise {
			return Call(ctx, g2)
		})
		return Call(ctx, g1)
	})
	vm.Register2(atomSemiColon, func(ctx context.Context, g1, g2 Term) *Promise {
		return Delay(func() *Promise {
			return Call(ctx, g1)
		}, func() *Promise {
			return Call(ctx, g2)
		})
	})
	vm.Register0(atomTrue, func(ctx context.Context) *Promise {
		return Continue(ctx)
	})
	vm.Register0(atomFail, func(ctx context.Context) *Promise {
		return Bool(false)
	})
	vm.Register2(NewAtom("a"), func(ctx context.Context, x, y Term) *Promise {
		a, f := NewAtom("$a"), NewAtom("f")
		return Delay(func() *Promise {
			return Unify(ctx, a.Apply(x, y), a.Apply(Integer(1), f.Apply(NewVariable())))
		}, func() *Promise {
			return Unify(ctx, a.Apply(x, y), a.Apply(Integer(2), f.Apply(NewVariable())))
		})
	})
	vm.Register2(NewAtom("b"), func(ctx context.Context, x, y Term) *Promise {
		b := NewAtom("$b")
		return Delay(func() *Promise {
			return Unify(ctx, b.Apply(x, y), b.Apply(Integer(1), Integer(1)))
		}, func() *Promise {
			return Unify(ctx, b.Apply(x, y), b.Apply(Integer(1), Integer(1)))
		}, func() *Promise {
			return Unify(ctx, b.Apply(x, y), b.Apply(Integer(1), Integer(2)))
		}, func() *Promise {
			return Unify(ctx, b.Apply(x, y), b.Apply(Integer(2), Integer(1)))
		}, func() *Promise {
			return Unify(ctx, b.Apply(x, y), b.Apply(Integer(2), Integer(2)))
		}, func() *Promise {
			return Unify(ctx, b.Apply(x, y), b.Apply(Integer(2), Integer(2)))
		})
	})
	vm.Register2(NewAtom("d"), func(ctx context.Context, x, y Term) *Promise {
		d := NewAtom("$d")
		return Delay(func() *Promise {
			return Unify(ctx, d.Apply(x, y), d.Apply(Integer(1), Integer(1)))
		}, func() *Promise {
			return Unify(ctx, d.Apply(x, y), d.Apply(Integer(1), Integer(2)))
		}, func() *Promise {
			return Unify(ctx, d.Apply(x, y), d.Apply(Integer(1), Integer(1)))
		}, func() *Promise {
			return Unify(ctx, d.Apply(x, y), d.Apply(Integer(2), Integer(2)))
		}, func() *Promise {
			return Unify(ctx, d.Apply(x, y), d.Apply(Integer(2), Integer(1)))
		}, func() *Promise {
			return Unify(ctx, d.Apply(x, y), d.Apply(Integer(2), Integer(2)))
		})
	})
	vm.Register2(NewAtom("member"), func(ctx context.Context, elem, list Term) *Promise {
		var ks []func() *Promise
		iter := ListIterator{List: list, AllowPartial: true}
		for iter.Next(ctx) {
			e := iter.Current()
			ks = append(ks, func() *Promise {
				return Unify(ctx, elem, e)
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
			defer setMemFree(tt.mem)()

			vm.Unknown = func(context.Context, Atom, []Term) {
				assert.True(t, tt.warning)
			}
			ctx := withVM(context.Background(), &vm)
			ctx = WithCont(ctx, func(ctx context.Context) *Promise {
				for k, v := range tt.env[0] {
					_, ok := withUnification(ctx, v, k)
					assert.True(t, ok)
				}
				tt.env = tt.env[1:]
				return Bool(false)
			})
			_, err := SetOf(ctx, tt.template, tt.goal, tt.instances).Force()
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
		{title: "6", template: x, goal: goal, instances: s, err: InstantiationError(context.Background())},
		{title: "7", template: x, goal: Integer(4), instances: s, err: typeError(context.Background(), validTypeCallable, Integer(4))},

		// 8.10.1.3 Errors
		{title: "c", template: x, goal: atomSemiColon.Apply(atomEqual.Apply(x, Integer(1)), atomEqual.Apply(x, Integer(2))), instances: NewAtom("foo"), err: typeError(context.Background(), validTypeList, NewAtom("foo"))},
	}

	var vm VM
	vm.Register2(atomEqual, Unify)
	vm.Register2(atomSemiColon, func(ctx context.Context, g1, g2 Term) *Promise {
		return Delay(func() *Promise {
			return Call(ctx, g1)
		}, func() *Promise {
			return Call(ctx, g2)
		})
	})
	vm.Register0(atomFail, func(context.Context) *Promise {
		return Bool(false)
	})
	ctx := withVM(context.Background(), &vm)

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ctx := WithCont(ctx, func(ctx context.Context) *Promise {
				for k, v := range tt.env {
					_, ok := withUnification(ctx, v, k)
					assert.True(t, ok)
				}
				return Bool(true)
			})
			ok, err := FindAll(ctx, tt.template, tt.goal, tt.instances).Force()
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
		{title: `compare(1+2, 3, 3.0).`, order: atomPlus.Apply(Integer(1), Integer(2)), x: Integer(3), y: Float(3.0), ok: false, err: typeError(context.Background(), validTypeAtom, atomPlus.Apply(Integer(1), Integer(2)))},
		{title: `compare(>=, 3, 3.0).`, order: NewAtom(">="), x: Integer(3), y: Float(3.0), ok: false, err: domainError(context.Background(), validDomainOrder, NewAtom(">="))},

		{title: `missing case for >`, order: atomGreaterThan, x: Integer(2), y: Integer(1), ok: true},
	}

	for _, tt := range tests {
		ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
			for k, v := range tt.env {
				assert.Equal(t, v, Resolve(ctx, k))
			}
			return Bool(true)
		})
		ok, err := Compare(ctx, tt.order, tt.x, tt.y).Force()
		assert.Equal(t, tt.ok, ok)
		assert.Equal(t, tt.err, err)
	}
}

func TestBetween(t *testing.T) {
	t.Run("value is an integer", func(t *testing.T) {
		t.Run("between lower and upper", func(t *testing.T) {
			ok, err := Between(context.Background(), Integer(1), Integer(3), Integer(2)).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("equal to lower", func(t *testing.T) {
			ok, err := Between(context.Background(), Integer(1), Integer(3), Integer(1)).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("equal to upper", func(t *testing.T) {
			ok, err := Between(context.Background(), Integer(1), Integer(3), Integer(3)).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("value, lower, higher are all equal", func(t *testing.T) {
			ok, err := Between(context.Background(), Integer(1), Integer(1), Integer(1)).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("value, lower, higher are all MaxInt64", func(t *testing.T) {
			ok, err := Between(context.Background(), Integer(math.MaxInt64), Integer(math.MaxInt64), Integer(math.MaxInt64)).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("equal to lower, but lower > upper", func(t *testing.T) {
			ok, err := Between(context.Background(), Integer(3), Integer(1), Integer(3)).Force()
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("less than lower", func(t *testing.T) {
			ok, err := Between(context.Background(), Integer(1), Integer(3), Integer(0)).Force()
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("greater than upper", func(t *testing.T) {
			ok, err := Between(context.Background(), Integer(1), Integer(3), Integer(100)).Force()
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("value is a variable", func(t *testing.T) {
		t.Run("lower and upper are equal integers", func(t *testing.T) {
			value := NewVariable()
			ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
				assert.Equal(t, Integer(1), Resolve(ctx, value))
				return Bool(true)
			})
			ok, err := Between(ctx, Integer(1), Integer(1), value).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("lower and upper are MaxInt64", func(t *testing.T) {
			value := NewVariable()
			ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
				assert.Equal(t, Integer(math.MaxInt64), Resolve(ctx, value))
				return Bool(true)
			})
			ok, err := Between(ctx, Integer(math.MaxInt64), Integer(math.MaxInt64), value).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("multiple choice points", func(t *testing.T) {
			var n int
			value := NewVariable()
			ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
				assert.Equal(t, Integer(n), Resolve(ctx, value))
				n++
				return Bool(false)
			})
			ok, err := Between(ctx, Integer(0), Integer(3), value).Force()
			assert.Equal(t, n, 4)
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("lower > upper", func(t *testing.T) {
			value := NewVariable()
			ok, err := Between(context.Background(), Integer(3), Integer(0), value).Force()
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("lower is uninstantiated", func(t *testing.T) {
		_, err := Between(context.Background(), NewVariable(), Integer(2), Integer(1)).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
	})

	t.Run("upper is uninstantiated", func(t *testing.T) {
		_, err := Between(context.Background(), Integer(1), NewVariable(), Integer(1)).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
	})

	t.Run("lower is not an integer", func(t *testing.T) {
		_, err := Between(context.Background(), NewAtom("inf"), Integer(2), Integer(1)).Force()
		assert.Equal(t, typeError(context.Background(), validTypeInteger, NewAtom("inf")), err)
	})

	t.Run("upper is not an integer", func(t *testing.T) {
		_, err := Between(context.Background(), Integer(1), NewAtom("inf"), Integer(1)).Force()
		assert.Equal(t, typeError(context.Background(), validTypeInteger, NewAtom("inf")), err)
	})

	t.Run("value is not an integer or variable", func(t *testing.T) {
		_, err := Between(context.Background(), Integer(1), Integer(1), NewAtom("foo")).Force()
		assert.Equal(t, typeError(context.Background(), validTypeInteger, NewAtom("foo")), err)
	})
}

func TestSort(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		t.Run("variable", func(t *testing.T) {
			sorted := NewVariable()
			ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
				assert.Equal(t, List(NewAtom("a"), NewAtom("b"), NewAtom("c")), Resolve(ctx, sorted))
				return Bool(true)
			})
			ok, err := Sort(ctx, List(NewAtom("a"), NewAtom("c"), NewAtom("b"), NewAtom("a")), sorted).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("list", func(t *testing.T) {
			ok, err := Sort(context.Background(), List(NewAtom("a"), NewAtom("c"), NewAtom("b"), NewAtom("a")), List(NewAtom("a"), NewAtom("b"), NewAtom("c"))).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("list is a partial list", func(t *testing.T) {
		_, err := Sort(context.Background(), PartialList(NewVariable(), NewAtom("a"), NewAtom("b")), NewVariable()).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
	})

	t.Run("list is neither a partial list nor a list", func(t *testing.T) {
		_, err := Sort(context.Background(), NewAtom("a"), NewVariable()).Force()
		assert.Equal(t, typeError(context.Background(), validTypeList, NewAtom("a")), err)
	})

	t.Run("sorted is neither a partial list nor a list", func(t *testing.T) {
		t.Run("obviously not a list", func(t *testing.T) {
			_, err := Sort(context.Background(), List(NewAtom("a")), NewAtom("a")).Force()
			assert.Equal(t, typeError(context.Background(), validTypeList, NewAtom("a")), err)
		})

		t.Run("list-ish", func(t *testing.T) {
			_, err := Sort(context.Background(), List(NewAtom("a")), &compound{functor: atomDot, args: []Term{NewAtom("a")}}).Force()
			assert.Equal(t, typeError(context.Background(), validTypeList, &compound{functor: atomDot, args: []Term{NewAtom("a")}}), err)
		})
	})
}

func TestKeySort(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		t.Run("variable", func(t *testing.T) {
			sorted := NewVariable()
			ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
				assert.Equal(t, List(
					pair(NewAtom("a"), NewAtom("1")),
					pair(NewAtom("a"), NewAtom("2")),
					pair(NewAtom("b"), NewAtom("3")),
					pair(NewAtom("c"), NewAtom("4")),
				), Resolve(ctx, sorted))
				return Bool(true)
			})
			ok, err := KeySort(ctx, List(
				pair(NewAtom("c"), NewAtom("4")),
				pair(NewAtom("b"), NewAtom("3")),
				pair(NewAtom("a"), NewAtom("1")),
				pair(NewAtom("a"), NewAtom("2")),
			), sorted).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("list", func(t *testing.T) {
			second := NewVariable()
			ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
				assert.Equal(t, pair(NewAtom("a"), NewAtom("2")), Resolve(ctx, second))
				return Bool(true)
			})
			ok, err := KeySort(ctx, List(
				pair(NewAtom("c"), NewAtom("4")),
				pair(NewAtom("b"), NewAtom("3")),
				pair(NewAtom("a"), NewAtom("1")),
				pair(NewAtom("a"), NewAtom("2")),
			), List(
				pair(NewAtom("a"), NewAtom("1")),
				second,
				pair(NewAtom("b"), NewAtom("3")),
				pair(NewAtom("c"), NewAtom("4")),
			)).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("pairs is a partial list", func(t *testing.T) {
		_, err := KeySort(context.Background(), PartialList(NewVariable(), pair(NewAtom("a"), Integer(1))), NewVariable()).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
	})

	t.Run("pairs is neither a partial list nor a list", func(t *testing.T) {
		_, err := KeySort(context.Background(), NewAtom("a"), NewVariable()).Force()
		assert.Equal(t, typeError(context.Background(), validTypeList, NewAtom("a")), err)
	})

	t.Run("sorted is neither a partial list nor a list", func(t *testing.T) {
		_, err := KeySort(context.Background(), List(), NewAtom("foo")).Force()
		assert.Equal(t, typeError(context.Background(), validTypeList, NewAtom("foo")), err)
	})

	t.Run("an element of a list prefix of pairs is a variable", func(t *testing.T) {
		_, err := KeySort(context.Background(), List(NewVariable()), NewVariable()).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
	})

	t.Run("an element of a list prefix of pairs is neither a variable nor a compound term with principal functor (-)/2", func(t *testing.T) {
		t.Run("atomic", func(t *testing.T) {
			_, err := KeySort(context.Background(), List(NewAtom("foo")), NewVariable()).Force()
			assert.Equal(t, typeError(context.Background(), validTypePair, NewAtom("foo")), err)
		})

		t.Run("compound", func(t *testing.T) {
			_, err := KeySort(context.Background(), List(NewAtom("f").Apply(NewAtom("a"))), NewVariable()).Force()
			assert.Equal(t, typeError(context.Background(), validTypePair, NewAtom("f").Apply(NewAtom("a"))), err)
		})
	})

	t.Run("an element of a list prefix of sorted is neither a variable nor a compound term with principal functor (-)/2", func(t *testing.T) {
		t.Run("atomic", func(t *testing.T) {
			_, err := KeySort(context.Background(), List(), List(NewAtom("foo"))).Force()
			assert.Equal(t, typeError(context.Background(), validTypePair, NewAtom("foo")), err)
		})

		t.Run("compound", func(t *testing.T) {
			_, err := KeySort(context.Background(), List(), List(NewAtom("f").Apply(NewAtom("a")))).Force()
			assert.Equal(t, typeError(context.Background(), validTypePair, NewAtom("f").Apply(NewAtom("a"))), err)
		})
	})
}

func TestThrow(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		ok, err := Throw(context.Background(), NewAtom("a")).Force()
		assert.Equal(t, Exception{term: NewAtom("a")}, err)
		assert.False(t, ok)
	})

	t.Run("ball is a variable", func(t *testing.T) {
		ok, err := Throw(context.Background(), NewVariable()).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})
}

func TestCatch(t *testing.T) {
	var vm VM
	vm.Register2(atomEqual, Unify)
	vm.Register1(NewAtom("throw"), Throw)
	vm.Register0(atomTrue, func(ctx context.Context) *Promise {
		return Continue(ctx)
	})
	vm.Register0(atomFail, func(context.Context) *Promise {
		return Bool(false)
	})

	t.Run("match", func(t *testing.T) {
		v := NewVariable()
		ok, err := Catch(context.Background(), &compound{
			functor: NewAtom("throw"),
			args:    []Term{NewAtom("a")},
		}, v, &compound{
			functor: atomEqual,
			args:    []Term{v, NewAtom("a")},
		}).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not match", func(t *testing.T) {
		ok, err := Catch(context.Background(), &compound{
			functor: NewAtom("throw"),
			args:    []Term{NewAtom("a")},
		}, NewAtom("b"), atomFail).Force()
		assert.False(t, ok)
		ex, ok := err.(Exception)
		assert.True(t, ok)
		assert.Equal(t, NewAtom("a"), ex.term)
	})

	t.Run("true", func(t *testing.T) {
		ok, err := Catch(context.Background(), atomTrue, NewAtom("b"), atomFail).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("false", func(t *testing.T) {
		ok, err := Catch(context.Background(), atomFail, NewAtom("b"), atomFail).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("non-exception error", func(t *testing.T) {
		ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
			return Error(errors.New("failed"))
		})
		ok, err := Catch(ctx, atomTrue, NewVariable(), atomTrue).Force()
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestCurrentPredicate(t *testing.T) {
	t.Run("user defined predicate", func(t *testing.T) {
		vm := VM{procedures: map[procedureIndicator]procedure{
			{name: NewAtom("foo"), arity: 1}: &userDefined{},
		}}
		ctx := withVM(context.Background(), &vm)
		ok, err := CurrentPredicate(ctx, &compound{
			functor: atomSlash,
			args: []Term{
				NewAtom("foo"),
				Integer(1),
			},
		}).Force()
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
		ctx := withVM(context.Background(), &vm)
		ctx = WithCont(ctx, func(ctx context.Context) *Promise {
			c, ok := Resolve(ctx, v).(*compound)
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
		})
		ok, err := CurrentPredicate(ctx, v).Force()
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
		ctx := withVM(context.Background(), &vm)
		ok, err := CurrentPredicate(ctx, atomSlash.Apply(atomEqual, Integer(2))).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("pi is neither a variable nor a predicate indicator", func(t *testing.T) {
		t.Run("atom", func(t *testing.T) {
			ok, err := CurrentPredicate(context.Background(), NewAtom("foo")).Force()
			assert.Equal(t, typeError(context.Background(), validTypePredicateIndicator, NewAtom("foo")), err)
			assert.False(t, ok)
		})

		t.Run("compound", func(t *testing.T) {
			t.Run("non slash", func(t *testing.T) {
				ok, err := CurrentPredicate(context.Background(), NewAtom("f").Apply(NewAtom("a"))).Force()
				assert.Equal(t, typeError(context.Background(), validTypePredicateIndicator, NewAtom("f").Apply(NewAtom("a"))), err)
				assert.False(t, ok)
			})

			t.Run("slash but number", func(t *testing.T) {
				ok, err := CurrentPredicate(context.Background(), atomSlash.Apply(Integer(0), Integer(0))).Force()
				assert.Equal(t, typeError(context.Background(), validTypePredicateIndicator, atomSlash.Apply(Integer(0), Integer(0))), err)
				assert.False(t, ok)
			})

			t.Run("slash but path", func(t *testing.T) {
				ok, err := CurrentPredicate(context.Background(), atomSlash.Apply(NewAtom("foo"), NewAtom("bar"))).Force()
				assert.Equal(t, typeError(context.Background(), validTypePredicateIndicator, atomSlash.Apply(NewAtom("foo"), NewAtom("bar"))), err)
				assert.False(t, ok)
			})
		})
	})
}

func TestAssertz(t *testing.T) {
	t.Run("append", func(t *testing.T) {
		var vm VM
		ctx := withVM(context.Background(), &vm)

		ok, err := Assertz(ctx, &compound{
			functor: NewAtom("foo"),
			args:    []Term{NewAtom("a")},
		}).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Assertz(ctx, &compound{
			functor: NewAtom("foo"),
			args:    []Term{NewAtom("b")},
		}).Force()
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
		ok, err := Assertz(context.Background(), NewVariable()).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("clause is neither a variable, nor callable", func(t *testing.T) {
		ok, err := Assertz(context.Background(), Integer(0)).Force()
		assert.Equal(t, typeError(context.Background(), validTypeCallable, Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("head is a variable", func(t *testing.T) {
		ok, err := Assertz(context.Background(), atomIf.Apply(NewVariable(), atomTrue)).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("head is neither a variable, nor callable", func(t *testing.T) {
		ok, err := Assertz(context.Background(), atomIf.Apply(Integer(0), atomTrue)).Force()
		assert.Equal(t, typeError(context.Background(), validTypeCallable, Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("body contains a term which is not callable", func(t *testing.T) {
		ok, err := Assertz(context.Background(), atomIf.Apply(
			NewAtom("foo"),
			atomComma.Apply(atomTrue, Integer(0)),
		)).Force()
		assert.Equal(t, typeError(context.Background(), validTypeCallable, atomComma.Apply(atomTrue, Integer(0))), err)
		assert.False(t, ok)
	})

	t.Run("static", func(t *testing.T) {
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: NewAtom("foo"), arity: 0}: &userDefined{dynamic: false},
			},
		}
		ctx := withVM(context.Background(), &vm)

		ok, err := Assertz(ctx, NewAtom("foo")).Force()
		assert.Equal(t, permissionError(context.Background(), operationModify, permissionTypeStaticProcedure, atomSlash.Apply(NewAtom("foo"), Integer(0))), err)
		assert.False(t, ok)
	})
}

func TestAsserta(t *testing.T) {
	t.Run("fact", func(t *testing.T) {
		var vm VM
		ctx := withVM(context.Background(), &vm)

		ok, err := Asserta(ctx, NewAtom("foo").Apply(NewAtom("a"))).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Asserta(ctx, NewAtom("foo").Apply(NewAtom("b"))).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, &userDefined{dynamic: true, clauses: []clause{
			{
				pi:      procedureIndicator{name: NewAtom("foo"), arity: 1},
				raw:     NewAtom("foo").Apply(NewAtom("b")),
				xrTable: []Term{NewAtom("b")},
				bytecode: bytecode{
					{opcode: opConst, operand: 0},
					{opcode: opExit},
				},
			},
			{
				pi:      procedureIndicator{name: NewAtom("foo"), arity: 1},
				raw:     NewAtom("foo").Apply(NewAtom("a")),
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
		ctx := withVM(context.Background(), &vm)
		ok, err := Asserta(ctx, &compound{
			functor: atomIf,
			args: []Term{
				NewAtom("foo"),
				&compound{
					functor: NewAtom("p"),
					args:    []Term{NewAtom("b")},
				},
			},
		}).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Asserta(ctx, &compound{
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
		}).Force()
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
		ok, err := Asserta(context.Background(), NewVariable()).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("clause is neither a variable, nor callable", func(t *testing.T) {
		ok, err := Asserta(context.Background(), Integer(0)).Force()
		assert.Equal(t, typeError(context.Background(), validTypeCallable, Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("head is a variable", func(t *testing.T) {
		ok, err := Asserta(context.Background(), atomIf.Apply(NewVariable(), atomTrue)).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("head is neither a variable, nor callable", func(t *testing.T) {
		ok, err := Asserta(context.Background(), atomIf.Apply(Integer(0), atomTrue)).Force()
		assert.Equal(t, typeError(context.Background(), validTypeCallable, Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("body is not callable", func(t *testing.T) {
		ok, err := Asserta(context.Background(), atomIf.Apply(NewAtom("foo"), Integer(0))).Force()
		assert.Equal(t, typeError(context.Background(), validTypeCallable, Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("body contains a term which is not callable", func(t *testing.T) {
		ok, err := Asserta(context.Background(), atomIf.Apply(
			NewAtom("foo"),
			atomComma.Apply(atomTrue, Integer(0))),
		).Force()
		assert.Equal(t, typeError(context.Background(), validTypeCallable, atomComma.Apply(atomTrue, Integer(0))), err)
		assert.False(t, ok)
	})

	t.Run("static", func(t *testing.T) {
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: NewAtom("foo"), arity: 0}: &userDefined{dynamic: false},
			},
		}
		ctx := withVM(context.Background(), &vm)

		ok, err := Asserta(ctx, NewAtom("foo")).Force()
		assert.Equal(t, permissionError(context.Background(), operationModify, permissionTypeStaticProcedure, atomSlash.Apply(NewAtom("foo"), Integer(0))), err)
		assert.False(t, ok)
	})

	t.Run("cut", func(t *testing.T) {
		var vm VM
		ctx := withVM(context.Background(), &vm)
		ok, err := Asserta(ctx, atomIf.Apply(NewAtom("foo"), atomCut)).Force()
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
		ctx := withVM(context.Background(), &vm)

		ok, err := Retract(ctx, NewAtom("foo").Apply(NewVariable())).Force()
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
		ctx := withVM(context.Background(), &vm)

		ok, err := Retract(ctx, NewAtom("foo").Apply(NewAtom("b"))).Force()
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
		ctx := withVM(context.Background(), &vm)

		ctx = WithCont(ctx, func(context.Context) *Promise {
			return Bool(false)
		})
		ok, err := Retract(ctx, NewAtom("foo").Apply(NewVariable())).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
		assert.Empty(t, vm.procedures[procedureIndicator{name: NewAtom("foo"), arity: 1}].(*userDefined).clauses)
	})

	t.Run("variable", func(t *testing.T) {
		ok, err := Retract(context.Background(), NewVariable()).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("not callable", func(t *testing.T) {
		ok, err := Retract(context.Background(), Integer(0)).Force()
		assert.Equal(t, typeError(context.Background(), validTypeCallable, Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("no clause matches", func(t *testing.T) {
		var vm VM
		ctx := withVM(context.Background(), &vm)
		ok, err := Retract(ctx, NewAtom("foo").Apply(NewVariable())).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("static", func(t *testing.T) {
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: NewAtom("foo"), arity: 0}: &userDefined{dynamic: false},
			},
		}
		ctx := withVM(context.Background(), &vm)

		ok, err := Retract(ctx, NewAtom("foo")).Force()
		assert.Equal(t, permissionError(context.Background(), operationModify, permissionTypeStaticProcedure, atomSlash.Apply(NewAtom("foo"), Integer(0))), err)
		assert.False(t, ok)
	})

	t.Run("exception in continuation", func(t *testing.T) {
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: NewAtom("foo"), arity: 1}: &userDefined{dynamic: true, clauses: []clause{
					{raw: NewAtom("foo").Apply(NewAtom("a"))},
				}},
			},
		}
		ctx := withVM(context.Background(), &vm)
		ctx = WithCont(ctx, func(ctx context.Context) *Promise {
			return Error(errors.New("failed"))
		})

		ok, err := Retract(ctx, NewAtom("foo").Apply(NewVariable())).Force()
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
		ctx := withVM(context.Background(), &vm)

		ok, err := Abolish(ctx, atomSlash.Apply(NewAtom("foo"), Integer(1))).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		_, ok = vm.procedures[procedureIndicator{name: NewAtom("foo"), arity: 1}]
		assert.False(t, ok)
	})

	t.Run("pi is a variable", func(t *testing.T) {
		ok, err := Abolish(context.Background(), NewVariable()).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and either Name or Arity is a variable", func(t *testing.T) {
		t.Run("Name is a variable", func(t *testing.T) {
			ok, err := Abolish(context.Background(), atomSlash.Apply(NewVariable(), Integer(2))).Force()
			assert.Equal(t, InstantiationError(context.Background()), err)
			assert.False(t, ok)
		})

		t.Run("Arity is a variable", func(t *testing.T) {
			ok, err := Abolish(context.Background(), atomSlash.Apply(NewAtom("foo"), NewVariable())).Force()
			assert.Equal(t, InstantiationError(context.Background()), err)
			assert.False(t, ok)
		})
	})

	t.Run("pi is neither a variable nor a predicate indicator", func(t *testing.T) {
		t.Run("compound", func(t *testing.T) {
			ok, err := Abolish(context.Background(), atomPlus.Apply(NewAtom("foo"), Integer(1))).Force()
			assert.Equal(t, typeError(context.Background(), validTypePredicateIndicator, atomPlus.Apply(NewAtom("foo"), Integer(1))), err)
			assert.False(t, ok)
		})

		t.Run("not a compound", func(t *testing.T) {
			ok, err := Abolish(context.Background(), Integer(0)).Force()
			assert.Equal(t, typeError(context.Background(), validTypePredicateIndicator, Integer(0)), err)
			assert.False(t, ok)
		})
	})

	t.Run("pi is a term Name/Arity and Name is neither a variable nor an atom", func(t *testing.T) {
		ok, err := Abolish(context.Background(), atomSlash.Apply(Integer(0), Integer(2))).Force()
		assert.Equal(t, typeError(context.Background(), validTypeAtom, Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and Arity is neither a variable nor an integer", func(t *testing.T) {
		ok, err := Abolish(context.Background(), atomSlash.Apply(NewAtom("foo"), NewAtom("bar"))).Force()
		assert.Equal(t, typeError(context.Background(), validTypeInteger, NewAtom("bar")), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and Arity is an integer less than zero", func(t *testing.T) {
		ok, err := Abolish(context.Background(), atomSlash.Apply(NewAtom("foo"), Integer(-2))).Force()
		assert.Equal(t, domainError(context.Background(), validDomainNotLessThanZero, Integer(-2)), err)
		assert.False(t, ok)
	})

	t.Run("The predicate indicator pi is that of a static procedure", func(t *testing.T) {
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: NewAtom("foo"), arity: 0}: &userDefined{dynamic: false},
			},
		}
		ctx := withVM(context.Background(), &vm)
		ok, err := Abolish(ctx, atomSlash.Apply(NewAtom("foo"), Integer(0))).Force()
		assert.Equal(t, permissionError(context.Background(), operationModify, permissionTypeStaticProcedure, atomSlash.Apply(NewAtom("foo"), Integer(0))), err)
		assert.False(t, ok)
	})
}

func TestCurrentInput(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		var s Stream
		vm := VM{
			input: &s,
		}
		ctx := withVM(context.Background(), &vm)

		ok, err := CurrentInput(ctx, &s).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("stream is neither a variable nor a stream", func(t *testing.T) {
		ok, err := CurrentInput(context.Background(), Integer(0)).Force()
		assert.Equal(t, domainError(context.Background(), validDomainStream, Integer(0)), err)
		assert.False(t, ok)
	})
}

func TestCurrentOutput(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		var s Stream
		vm := VM{
			output: &s,
		}
		ctx := withVM(context.Background(), &vm)

		ok, err := CurrentOutput(ctx, &s).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("stream is neither a variable nor a stream", func(t *testing.T) {
		ok, err := CurrentOutput(context.Background(), Integer(0)).Force()
		assert.Equal(t, domainError(context.Background(), validDomainStream, Integer(0)), err)
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
	ctx := withVM(context.Background(), &vm)

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
		{title: "a", streamOrAlias: stream, err: InstantiationError(context.Background())},
		{title: "b", streamOrAlias: Integer(0), err: domainError(context.Background(), validDomainStreamOrAlias, Integer(0))},
		{title: "c", streamOrAlias: bar, err: existenceError(context.Background(), objectTypeStream, bar)},
		{title: "d", streamOrAlias: &output, err: permissionError(context.Background(), operationInput, permissionTypeStream, &output)},
	}

	for _, tt := range tests {
		ok, err := SetInput(ctx, tt.streamOrAlias).Force()
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
	ctx := withVM(context.Background(), &vm)

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
		{title: "a", streamOrAlias: stream, err: InstantiationError(context.Background())},
		{title: "b", streamOrAlias: Integer(0), err: domainError(context.Background(), validDomainStreamOrAlias, Integer(0))},
		{title: "c", streamOrAlias: bar, err: existenceError(context.Background(), objectTypeStream, bar)},
		{title: "d", streamOrAlias: &input, err: permissionError(context.Background(), operationOutput, permissionTypeStream, &input)},
	}

	for _, tt := range tests {
		ok, err := SetOutput(ctx, tt.streamOrAlias).Force()
		assert.Equal(t, tt.ok, ok)
		assert.Equal(t, tt.err, err)
		if err == nil {
			assert.Equal(t, tt.output, vm.output)
		}
	}
}

func TestOpen(t *testing.T) {
	var vm VM
	ctx := withVM(context.Background(), &vm)

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
			ctx := WithCont(ctx, func(ctx context.Context) *Promise {
				env := env(ctx)
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
			})
			ok, err := Open(ctx, NewAtom(f.Name()), atomRead, v, List(atomAlias.Apply(atomInput))).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("type text", func(t *testing.T) {
			v := NewVariable()
			ctx = WithCont(ctx, func(ctx context.Context) *Promise {
				env := env(ctx)
				ref, ok := env.lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*Stream)
				assert.True(t, ok)
				assert.Equal(t, streamTypeText, s.streamType)
				return Bool(true)
			})
			ok, err := Open(ctx, NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomType,
				args:    []Term{atomText},
			})).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("type binary", func(t *testing.T) {
			v := NewVariable()
			ctx = WithCont(ctx, func(ctx context.Context) *Promise {
				env := env(ctx)
				ref, ok := env.lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*Stream)
				assert.True(t, ok)
				assert.Equal(t, streamTypeBinary, s.streamType)
				return Bool(true)
			})
			ok, err := Open(ctx, NewAtom(f.Name()), atomRead, v, List(atomType.Apply(atomBinary))).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("reposition true", func(t *testing.T) {
			v := NewVariable()
			ctx := WithCont(ctx, func(ctx context.Context) *Promise {
				env := env(ctx)
				ref, ok := env.lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*Stream)
				assert.True(t, ok)
				assert.True(t, s.reposition)
				return Bool(true)
			})
			ok, err := Open(ctx, NewAtom(f.Name()), atomRead, v, List(atomReposition.Apply(atomTrue))).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("reposition true", func(t *testing.T) {
			v := NewVariable()
			ctx := WithCont(ctx, func(ctx context.Context) *Promise {
				env := env(ctx)
				ref, ok := env.lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*Stream)
				assert.True(t, ok)
				assert.False(t, s.reposition)
				return Bool(true)
			})
			ok, err := Open(ctx, NewAtom(f.Name()), atomRead, v, List(atomReposition.Apply(atomFalse))).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("eof_action error", func(t *testing.T) {
			v := NewVariable()
			ctx := WithCont(ctx, func(ctx context.Context) *Promise {
				env := env(ctx)
				ref, ok := env.lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*Stream)
				assert.True(t, ok)
				assert.Equal(t, eofActionError, s.eofAction)
				return Bool(true)
			})
			ok, err := Open(ctx, NewAtom(f.Name()), atomRead, v, List(atomEOFAction.Apply(atomError))).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("eof_action eof_code", func(t *testing.T) {
			v := NewVariable()
			ctx := WithCont(ctx, func(ctx context.Context) *Promise {
				env := env(ctx)
				ref, ok := env.lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*Stream)
				assert.True(t, ok)
				assert.Equal(t, eofActionEOFCode, s.eofAction)
				return Bool(true)
			})
			ok, err := Open(ctx, NewAtom(f.Name()), atomRead, v, List(atomEOFAction.Apply(atomEOFCode))).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("eof_action reset", func(t *testing.T) {
			v := NewVariable()
			ctx := WithCont(ctx, func(ctx context.Context) *Promise {
				env := env(ctx)
				ref, ok := env.lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*Stream)
				assert.True(t, ok)
				assert.Equal(t, eofActionReset, s.eofAction)
				return Bool(true)
			})
			ok, err := Open(ctx, NewAtom(f.Name()), atomRead, v, List(atomEOFAction.Apply(atomReset))).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("unknown option", func(t *testing.T) {
			v := NewVariable()
			ctx := WithCont(ctx, func(ctx context.Context) *Promise {
				assert.Fail(t, "unreachable")
				return Bool(true)
			})
			ok, err := Open(ctx, NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomUnknown,
				args:    []Term{NewAtom("option")},
			})).Force()
			assert.Error(t, err)
			assert.False(t, ok)
		})

		t.Run("wrong arity", func(t *testing.T) {
			v := NewVariable()
			ctx := WithCont(ctx, func(ctx context.Context) *Promise {
				assert.Fail(t, "unreachable")
				return Bool(true)
			})
			ok, err := Open(ctx, NewAtom(f.Name()), atomRead, v, List(atomType.Apply(NewAtom("a"), NewAtom("b")))).Force()
			assert.Error(t, err)
			assert.False(t, ok)
		})

		t.Run("variable arg", func(t *testing.T) {
			v := NewVariable()
			ctx := WithCont(ctx, func(ctx context.Context) *Promise {
				assert.Fail(t, "unreachable")
				return Bool(true)
			})
			ok, err := Open(ctx, NewAtom(f.Name()), atomRead, v, List(atomType.Apply(NewVariable()))).Force()
			assert.Error(t, err)
			assert.False(t, ok)
		})

		t.Run("non-atom arg", func(t *testing.T) {
			v := NewVariable()
			ctx := WithCont(ctx, func(ctx context.Context) *Promise {
				assert.Fail(t, "unreachable")
				return Bool(true)
			})
			ok, err := Open(ctx, NewAtom(f.Name()), atomRead, v, List(atomType.Apply(Integer(0)))).Force()
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
		ctx := WithCont(ctx, func(ctx context.Context) *Promise {
			env := env(ctx)
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
		})
		ok, err := Open(ctx, NewAtom(n), atomWrite, v, List(atomAlias.Apply(atomOutput))).Force()
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
		ctx := WithCont(ctx, func(ctx context.Context) *Promise {
			env := env(ctx)
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
		})
		ok, err := Open(ctx, NewAtom(f.Name()), atomAppend, v, List(atomAlias.Apply(atomAppend))).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("sourceSink is a variable", func(t *testing.T) {
		ok, err := Open(context.Background(), NewVariable(), atomRead, NewVariable(), List()).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("mode is a variable", func(t *testing.T) {
		ok, err := Open(context.Background(), NewAtom("/dev/null"), NewVariable(), NewVariable(), List()).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("options is a partial list or a list with an element E which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			ok, err := Open(context.Background(), NewAtom("/dev/null"), atomRead, NewVariable(), PartialList(NewVariable(),
				atomType.Apply(atomText),
				atomAlias.Apply(NewAtom("foo")),
			)).Force()
			assert.Equal(t, InstantiationError(context.Background()), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			ok, err := Open(context.Background(), NewAtom("/dev/null"), atomRead, NewVariable(), List(
				NewVariable(),
				atomType.Apply(atomText),
				atomAlias.Apply(NewAtom("foo")),
			)).Force()
			assert.Equal(t, InstantiationError(context.Background()), err)
			assert.False(t, ok)
		})
	})

	t.Run("mode is neither a variable nor an atom", func(t *testing.T) {
		ok, err := Open(context.Background(), NewAtom("/dev/null"), Integer(0), NewVariable(), List()).Force()
		assert.Equal(t, typeError(context.Background(), validTypeAtom, Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		ok, err := Open(context.Background(), NewAtom("/dev/null"), atomRead, NewVariable(), NewAtom("list")).Force()
		assert.Equal(t, typeError(context.Background(), validTypeList, NewAtom("list")), err)
		assert.False(t, ok)
	})

	t.Run("stream is not a variable", func(t *testing.T) {
		ok, err := Open(context.Background(), NewAtom("/dev/null"), atomRead, NewAtom("stream"), List()).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("sourceSink is neither a variable nor a source/sink", func(t *testing.T) {
		ok, err := Open(context.Background(), Integer(0), atomRead, NewVariable(), List()).Force()
		assert.Equal(t, domainError(context.Background(), validDomainSourceSink, Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("mode is an atom but not an input/output mode", func(t *testing.T) {
		ok, err := Open(context.Background(), NewAtom("/dev/null"), NewAtom("foo"), NewVariable(), List()).Force()
		assert.Equal(t, domainError(context.Background(), validDomainIOMode, NewAtom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the options list is neither a variable nor a stream-option", func(t *testing.T) {
		for _, o := range []Term{
			NewAtom("foo"),
			NewAtom("foo").Apply(NewAtom("bar")),
			atomAlias.Apply(Integer(0)),
			atomType.Apply(Integer(0)),
			atomReposition.Apply(Integer(0)),
			atomEOFAction.Apply(Integer(0)),
		} {
			ok, err := Open(context.Background(), NewAtom("/dev/null"), atomRead, NewVariable(), List(o)).Force()
			assert.Equal(t, domainError(context.Background(), validDomainStreamOption, o), err)
			assert.False(t, ok)
		}
	})

	// Derived from 5.5.12 Options in Cor.3
	t.Run("a component of an element E of the options list is a variable", func(t *testing.T) {
		for _, o := range []Term{
			NewVariable(),
			atomAlias.Apply(NewVariable()),
			atomType.Apply(NewVariable()),
			atomReposition.Apply(NewVariable()),
			atomEOFAction.Apply(NewVariable()),
		} {
			ok, err := Open(context.Background(), NewAtom("/dev/null"), atomRead, NewVariable(), List(o)).Force()
			assert.Equal(t, InstantiationError(context.Background()), err)
			assert.False(t, ok)
		}
	})

	t.Run("the source/sink specified by sourceSink does not exist", func(t *testing.T) {
		f, err := os.CreateTemp("", "open_test_existence")
		assert.NoError(t, err)
		assert.NoError(t, os.Remove(f.Name()))

		ok, err := Open(context.Background(), NewAtom(f.Name()), atomRead, NewVariable(), List()).Force()
		assert.Equal(t, existenceError(context.Background(), objectTypeSourceSink, NewAtom(f.Name())), err)
		assert.False(t, ok)
	})

	t.Run("the source/sink specified by sourceSink cannot be opened", func(t *testing.T) {
		f, err := os.CreateTemp("", "open_test_permission")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, os.Remove(f.Name()))
		}()

		assert.NoError(t, f.Chmod(0200))

		ok, err := Open(context.Background(), NewAtom(f.Name()), atomRead, NewVariable(), List()).Force()
		assert.Equal(t, permissionError(context.Background(), operationOpen, permissionTypeSourceSink, NewAtom(f.Name())), err)
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
		ctx := withVM(ctx, &vm)
		ok, err := Open(ctx, NewAtom(f.Name()), atomRead, NewVariable(), List(atomAlias.Apply(NewAtom("foo")))).Force()
		assert.Equal(t, permissionError(context.Background(), operationOpen, permissionTypeSourceSink, atomAlias.Apply(NewAtom("foo"))), err)
		assert.False(t, ok)
	})

	t.Run("system error", func(t *testing.T) {
		openFile = func(name string, flag int, perm os.FileMode) (*os.File, error) {
			return nil, errors.New("failed")
		}
		defer func() {
			openFile = os.OpenFile
		}()

		_, err := Open(context.Background(), NewAtom("foo"), atomRead, NewVariable(), List()).Force()
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

				ok, err := Close(context.Background(), &Stream{sourceSink: &m}, List()).Force()
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
				ctx := withVM(context.Background(), &vm)
				ok, err := Close(ctx, foo, List()).Force()
				assert.NoError(t, err)
				assert.True(t, ok)
			})
		})

		t.Run("ng", func(t *testing.T) {
			var m mockCloser
			m.On("Close").Return(errors.New("failed")).Once()
			defer m.AssertExpectations(t)

			_, err := Close(context.Background(), &Stream{sourceSink: &m}, List()).Force()
			assert.Equal(t, errors.New("failed"), err)
		})
	})

	t.Run("force false", func(t *testing.T) {
		var m mockCloser
		m.On("Close").Return(errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		_, err := Close(context.Background(), &Stream{sourceSink: &m}, List(atomForce.Apply(atomFalse))).Force()
		assert.Equal(t, errors.New("failed"), err)
	})

	t.Run("force true", func(t *testing.T) {
		var m mockCloser
		m.On("Close").Return(errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		ok, err := Close(context.Background(), &Stream{sourceSink: &m}, List(atomForce.Apply(atomTrue))).Force()
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
		ctx := withVM(context.Background(), &vm)
		ok, err := Close(ctx, NewAtom("foo"), List()).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		ok, err := Close(context.Background(), NewVariable(), List()).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("options is a partial list or a list with an element E which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			ok, err := Close(context.Background(), &Stream{}, PartialList(NewVariable(),
				atomForce.Apply(atomTrue),
			)).Force()
			assert.Equal(t, InstantiationError(context.Background()), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			ok, err := Close(context.Background(), &Stream{}, List(NewVariable(), atomForce.Apply(atomTrue))).Force()
			assert.Equal(t, InstantiationError(context.Background()), err)
			assert.False(t, ok)
		})
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		ok, err := Close(context.Background(), &Stream{}, NewAtom("foo")).Force()
		assert.Equal(t, typeError(context.Background(), validTypeList, NewAtom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream-term or alias", func(t *testing.T) {
		ok, err := Close(context.Background(), Integer(0), List()).Force()
		assert.Equal(t, domainError(context.Background(), validDomainStreamOrAlias, Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the Options list is neither a variable nor a stream-option", func(t *testing.T) {
		t.Run("not a compound", func(t *testing.T) {
			ok, err := Close(context.Background(), &Stream{}, List(NewAtom("foo"))).Force()
			assert.Equal(t, domainError(context.Background(), validDomainStreamOption, NewAtom("foo")), err)
			assert.False(t, ok)
		})

		t.Run("compound", func(t *testing.T) {
			t.Run("force but arity is not 1", func(t *testing.T) {
				ok, err := Close(context.Background(), &Stream{}, List(atomForce.Apply(NewAtom("a"), NewAtom("b")))).Force()
				assert.Equal(t, domainError(context.Background(), validDomainStreamOption, atomForce.Apply(NewAtom("a"), NewAtom("b"))), err)
				assert.False(t, ok)
			})

			t.Run("force but the argument is a variable", func(t *testing.T) {
				_, err := Close(context.Background(), &Stream{}, List(atomForce.Apply(NewVariable()))).Force()
				_, ok := NewEnv().Unify(domainError(context.Background(), validDomainStreamOption, atomForce.Apply(NewVariable())).term, err.(Exception).term)
				assert.True(t, ok)
			})

			t.Run("force but the argument is neither true nor false", func(t *testing.T) {
				ok, err := Close(context.Background(), &Stream{}, List(atomForce.Apply(NewAtom("meh")))).Force()
				assert.Equal(t, domainError(context.Background(), validDomainStreamOption, atomForce.Apply(NewAtom("meh"))), err)
				assert.False(t, ok)
			})
		})
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		ok, err := Close(context.Background(), NewAtom("foo"), List()).Force()
		assert.Equal(t, existenceError(context.Background(), objectTypeStream, NewAtom("foo")), err)
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
		ok, err := FlushOutput(context.Background(), s).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("ng", func(t *testing.T) {
		var m mockSyncer
		m.On("Sync").Return(errors.New("ng")).Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m, mode: ioModeWrite}

		_, err := FlushOutput(context.Background(), s).Force()
		assert.Error(t, err)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		var vm VM
		vm.streams.add(s)
		ctx := withVM(context.Background(), &vm)
		ok, err := FlushOutput(ctx, foo).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		ok, err := FlushOutput(context.Background(), NewVariable()).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream-term or alias", func(t *testing.T) {
		ok, err := FlushOutput(context.Background(), Integer(0)).Force()
		assert.Equal(t, domainError(context.Background(), validDomainStreamOrAlias, Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		ok, err := FlushOutput(context.Background(), NewAtom("foo")).Force()
		assert.Equal(t, existenceError(context.Background(), objectTypeStream, NewAtom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("SorA is an input stream", func(t *testing.T) {
		s := &Stream{sourceSink: os.Stdin}

		ok, err := FlushOutput(context.Background(), s).Force()
		assert.Equal(t, permissionError(context.Background(), operationOutput, permissionTypeStream, s), err)
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
		{title: `write_term(1, [quoted(non_boolean)]).`, sOrA: w, term: Integer(1), options: List(atomQuoted.Apply(NewAtom("non_boolean"))), err: domainError(context.Background(), validDomainWriteOption, atomQuoted.Apply(NewAtom("non_boolean")))},
		{title: `write_term(1, [quoted(B)]).`, sOrA: w, term: Integer(1), options: List(atomQuoted.Apply(B)), err: InstantiationError(context.Background())},
		{title: `B = true, write_term(1, [quoted(B)]).`, sOrA: w, env: NewEnv().bind(B, atomTrue), term: Integer(1), options: List(atomQuoted.Apply(B)), ok: true, output: `1`},

		// 8.14.2.3 Errors
		{title: `a`, sOrA: s, term: NewAtom("foo"), options: List(), err: InstantiationError(context.Background())},
		{title: `b: partial list`, sOrA: w, term: NewAtom("foo"), options: PartialList(x, atomQuoted.Apply(atomTrue)), err: InstantiationError(context.Background())},
		{title: `b: variable element`, sOrA: w, term: NewAtom("foo"), options: List(x), err: InstantiationError(context.Background())},
		{title: `b: variable component`, sOrA: w, term: NewAtom("foo"), options: List(atomQuoted.Apply(x)), err: InstantiationError(context.Background())},
		{title: `b: variable_names, partial list`, sOrA: w, term: NewAtom("foo"), options: List(atomVariableNames.Apply(l)), err: InstantiationError(context.Background())},
		{title: `b: variable_names, element`, sOrA: w, term: NewAtom("foo"), options: List(atomVariableNames.Apply(List(e))), err: InstantiationError(context.Background())},
		{title: `b: variable_names, name`, sOrA: w, term: x, options: List(atomVariableNames.Apply(List(atomEqual.Apply(n, v)))), err: InstantiationError(context.Background())},
		{title: `c`, sOrA: w, term: NewAtom("foo"), options: NewAtom("options"), err: typeError(context.Background(), validTypeList, NewAtom("options"))},
		{title: `d`, sOrA: Integer(0), term: NewAtom("foo"), options: List(), err: domainError(context.Background(), validDomainStreamOrAlias, Integer(0))},
		{title: `e: not a compound`, sOrA: w, term: NewAtom("foo"), options: List(NewAtom("bar")), err: domainError(context.Background(), validDomainWriteOption, NewAtom("bar"))},
		{title: `e: arity is not 1`, sOrA: w, term: NewAtom("foo"), options: List(NewAtom("bar").Apply(NewAtom("a"), NewAtom("b"))), err: domainError(context.Background(), validDomainWriteOption, NewAtom("bar").Apply(NewAtom("a"), NewAtom("b")))},
		{title: `e: variable_names, not a list, atom`, sOrA: w, term: NewAtom("foo"), options: List(atomVariableNames.Apply(NewAtom("a"))), err: domainError(context.Background(), validDomainWriteOption, atomVariableNames.Apply(NewAtom("a")))},
		{title: `e: variable_names, not a list, atomic`, sOrA: w, term: NewAtom("foo"), options: List(atomVariableNames.Apply(Integer(0))), err: domainError(context.Background(), validDomainWriteOption, atomVariableNames.Apply(Integer(0)))},
		{title: `e: variable_names, element is not a pair, atomic`, sOrA: w, term: NewAtom("foo"), options: List(atomVariableNames.Apply(List(NewAtom("a")))), err: domainError(context.Background(), validDomainWriteOption, atomVariableNames.Apply(List(NewAtom("a"))))},
		{title: `e: variable_names, element is not a pair, compound`, sOrA: w, term: NewAtom("foo"), options: List(atomVariableNames.Apply(List(NewAtom("f").Apply(NewAtom("a"))))), err: domainError(context.Background(), validDomainWriteOption, atomVariableNames.Apply(List(NewAtom("f").Apply(NewAtom("a")))))},
		{title: `e: variable_names, name is not an atom`, sOrA: w, term: v, options: List(atomVariableNames.Apply(List(atomEqual.Apply(Integer(0), v)))), err: domainError(context.Background(), validDomainWriteOption, atomVariableNames.Apply(List(atomEqual.Apply(Integer(0), NewVariable()))))},
		{title: `e: boolean option, not an atom`, sOrA: w, term: NewAtom("foo"), options: List(atomQuoted.Apply(Integer(0))), err: domainError(context.Background(), validDomainWriteOption, atomQuoted.Apply(Integer(0)))},
		{title: `e: unknown functor`, sOrA: w, term: NewAtom("foo"), options: List(NewAtom("bar").Apply(atomTrue)), err: domainError(context.Background(), validDomainWriteOption, NewAtom("bar").Apply(atomTrue))},
		{title: `f`, sOrA: NewAtom("stream"), term: NewAtom("foo"), options: List(), err: existenceError(context.Background(), objectTypeStream, NewAtom("stream"))},
		{title: `g`, sOrA: r, term: NewAtom("foo"), options: List(), err: permissionError(context.Background(), operationOutput, permissionTypeStream, r)},
		{title: `h`, sOrA: b, term: NewAtom("foo"), options: List(), err: permissionError(context.Background(), operationOutput, permissionTypeBinaryStream, b)},

		// 7.10.5
		{title: `a`, sOrA: w, term: x, options: List(), ok: true, outputPattern: regexp.MustCompile(`_\d+`)},

		{title: `variable_names`, sOrA: w, term: v, options: List(atomVariableNames.Apply(List(
			atomEqual.Apply(NewAtom("n"), v),            // left-most is used
			atomEqual.Apply(NewAtom("m"), v),            // ignored
			atomEqual.Apply(NewAtom("a"), NewAtom("b")), // ignored
		))), ok: true, output: `n`},

		{title: `failure`, sOrA: mw, term: NewAtom("foo"), options: List(), err: err},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			buf.Reset()
			ctx := withEnv(context.Background(), tt.env)
			ok, err := WriteTerm(ctx, tt.sOrA, tt.term, tt.options).Force()
			assert.Equal(t, tt.ok, ok)
			if tt.err == nil {
				assert.NoError(t, err)
			} else if te, ok := tt.err.(Exception); ok {
				_, ok := NewEnv().Unify(te.term, err.(Exception).term)
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
		ok, err := CharCode(context.Background(), NewAtom("a"), Integer(97)).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("emoji", func(t *testing.T) {
		ok, err := CharCode(context.Background(), NewAtom("😀"), Integer(128512)).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("query char", func(t *testing.T) {
		v := NewVariable()
		ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
			assert.Equal(t, NewAtom("😀"), Resolve(ctx, v))
			return Bool(true)
		})
		ok, err := CharCode(ctx, v, Integer(128512)).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("query code", func(t *testing.T) {
		v := NewVariable()
		ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
			assert.Equal(t, Integer(128512), Resolve(ctx, v))
			return Bool(true)
		})
		ok, err := CharCode(ctx, NewAtom("😀"), v).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("char and code are variables", func(t *testing.T) {
		char, code := NewVariable(), NewVariable()

		ok, err := CharCode(context.Background(), char, code).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("char is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("atom", func(t *testing.T) {
			ok, err := CharCode(context.Background(), NewAtom("foo"), NewVariable()).Force()
			assert.Equal(t, typeError(context.Background(), validTypeCharacter, NewAtom("foo")), err)
			assert.False(t, ok)
		})

		t.Run("non-atom", func(t *testing.T) {
			ok, err := CharCode(context.Background(), Integer(0), NewVariable()).Force()
			assert.Equal(t, typeError(context.Background(), validTypeCharacter, Integer(0)), err)
			assert.False(t, ok)
		})
	})

	t.Run("code is neither a variable nor an integer", func(t *testing.T) {
		t.Run("char is variable", func(t *testing.T) {
			ok, err := CharCode(context.Background(), NewVariable(), NewAtom("foo")).Force()
			assert.Equal(t, typeError(context.Background(), validTypeInteger, NewAtom("foo")), err)
			assert.False(t, ok)
		})

		t.Run("char is a character", func(t *testing.T) {
			ok, err := CharCode(context.Background(), NewAtom("a"), NewAtom("x")).Force()
			assert.Equal(t, typeError(context.Background(), validTypeInteger, NewAtom("x")), err)
			assert.False(t, ok)
		})
	})

	t.Run("code is neither a variable nor a character-code", func(t *testing.T) {
		ok, err := CharCode(context.Background(), NewVariable(), Integer(-1)).Force()
		assert.Equal(t, representationError(context.Background(), flagCharacterCode), err)
		assert.False(t, ok)
	})
}

func TestPutByte(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		var m mockWriter
		m.On("Write", []byte{97}).Return(1, nil).Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m, mode: ioModeWrite, streamType: streamTypeBinary}

		ok, err := PutByte(context.Background(), s, Integer(97)).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("ng", func(t *testing.T) {
		var m mockWriter
		m.On("Write", []byte{97}).Return(0, errors.New("")).Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m, mode: ioModeWrite, streamType: streamTypeBinary}

		_, err := PutByte(context.Background(), s, Integer(97)).Force()
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
		ctx := withVM(context.Background(), &vm)
		ok, err := PutByte(ctx, NewAtom("foo"), Integer(97)).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		ok, err := PutByte(context.Background(), NewVariable(), Integer(97)).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("byt is a variable", func(t *testing.T) {
		s := &Stream{sourceSink: os.Stdout, mode: ioModeAppend}
		s.streamType = streamTypeBinary

		ok, err := PutByte(context.Background(), s, NewVariable()).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("byt is neither a variable nor an byte", func(t *testing.T) {
		s := &Stream{sourceSink: os.Stdout, mode: ioModeAppend}
		s.streamType = streamTypeBinary

		t.Run("not even an integer", func(t *testing.T) {
			ok, err := PutByte(context.Background(), s, NewAtom("byte")).Force()
			assert.Equal(t, typeError(context.Background(), validTypeByte, NewAtom("byte")), err)
			assert.False(t, ok)
		})

		t.Run("integer", func(t *testing.T) {
			ok, err := PutByte(context.Background(), s, Integer(256)).Force()
			assert.Equal(t, typeError(context.Background(), validTypeByte, Integer(256)), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		ok, err := PutByte(context.Background(), Integer(0), Integer(97)).Force()
		assert.Equal(t, domainError(context.Background(), validDomainStreamOrAlias, Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an input stream", func(t *testing.T) {
		s := NewVariable()
		env := NewEnv().
			bind(s, &Stream{sourceSink: os.Stdin, mode: ioModeRead, streamType: streamTypeBinary})
		ctx := withEnv(context.Background(), env)

		ok, err := PutByte(ctx, s, Integer(97)).Force()
		assert.Equal(t, permissionError(ctx, operationOutput, permissionTypeStream, s), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a text stream", func(t *testing.T) {
		s := NewVariable()
		env := NewEnv().
			bind(s, &Stream{sourceSink: os.Stdout, mode: ioModeAppend})
		ctx := withEnv(context.Background(), env)

		ok, err := PutByte(ctx, s, Integer(97)).Force()
		assert.Equal(t, permissionError(ctx, operationOutput, permissionTypeTextStream, s), err)
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
		}, char: NewAtom("a"), err: InstantiationError(context.Background())},
		{title: "b", streamOrAlias: func() (Term, func(*testing.T)) {
			return NewOutputTextStream(nil), nil
		}, char: NewVariable(), err: InstantiationError(context.Background())},
		{title: "b: atom but not one-char", streamOrAlias: func() (Term, func(*testing.T)) {
			return NewOutputTextStream(nil), nil
		}, char: NewAtom("foo"), err: typeError(context.Background(), validTypeCharacter, NewAtom("foo"))},
		{title: "b: not even atom", streamOrAlias: func() (Term, func(*testing.T)) {
			return NewOutputTextStream(nil), nil
		}, char: Integer(1), err: typeError(context.Background(), validTypeCharacter, Integer(1))},
		{title: "f", streamOrAlias: func() (Term, func(*testing.T)) {
			return Integer(1), nil
		}, char: NewAtom("a"), err: domainError(context.Background(), validDomainStreamOrAlias, Integer(1))},
		{title: "g", streamOrAlias: func() (Term, func(*testing.T)) {
			return NewAtom("foo"), nil
		}, char: NewAtom("a"), err: existenceError(context.Background(), objectTypeStream, NewAtom("foo"))},
		{title: "h", streamOrAlias: func() (Term, func(*testing.T)) {
			return NewInputTextStream(nil), nil
		}, char: NewAtom("a"), err: permissionError(context.Background(), operationOutput, permissionTypeStream, NewInputTextStream(nil))},
		{title: "i", streamOrAlias: func() (Term, func(*testing.T)) {
			return NewOutputBinaryStream(nil), nil
		}, char: NewAtom("a"), err: permissionError(context.Background(), operationOutput, permissionTypeBinaryStream, NewOutputBinaryStream(nil))},

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

			ok, err := PutChar(context.Background(), sOrA, tt.char).Force()
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
		ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
			assert.Equal(t, NewAtom("foo"), Resolve(ctx, v))
			return Bool(true)
		})
		ok, err := ReadTerm(ctx, s, v, List()).Force()
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
		ctx := withVM(context.Background(), &vm)
		ctx = WithCont(ctx, func(ctx context.Context) *Promise {
			assert.Equal(t, NewAtom("foo"), Resolve(ctx, v))
			return Bool(true)
		})
		ok, err := ReadTerm(ctx, NewAtom("foo"), v, List()).Force()
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

		ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
			c, ok := Resolve(ctx, v).(*compound)
			assert.True(t, ok)
			assert.Equal(t, NewAtom("f"), c.functor)
			assert.Len(t, c.args, 3)

			x, ok := c.args[0].(Variable)
			assert.True(t, ok)
			assert.Equal(t, x, c.args[1])

			y, ok := c.args[2].(Variable)
			assert.True(t, ok)

			assert.Equal(t, List(y), Resolve(ctx, singletons))

			return Bool(true)
		})
		ok, err := ReadTerm(ctx, s, v, List(atomSingletons.Apply(singletons))).Force()
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

		ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
			c, ok := Resolve(ctx, v).(*compound)
			assert.True(t, ok)
			assert.Equal(t, NewAtom("f"), c.functor)
			assert.Len(t, c.args, 3)

			x, ok := c.args[0].(Variable)
			assert.True(t, ok)
			assert.Equal(t, x, c.args[1])

			y, ok := c.args[2].(Variable)
			assert.True(t, ok)

			assert.Equal(t, List(x, y), Resolve(ctx, variables))

			return Bool(true)
		})
		ok, err := ReadTerm(ctx, s, v, List(atomVariables.Apply(variables))).Force()
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

		ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
			c, ok := Resolve(ctx, v).(*compound)
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
			), Resolve(ctx, variableNames))

			return Bool(true)
		})
		ok, err := ReadTerm(ctx, s, v, List(atomVariableNames.Apply(variableNames))).Force()
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

		ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
			assert.Equal(t, NewAtom("foo").Apply(NewAtom("a")), Resolve(ctx, v))
			return Bool(true)
		})
		ok, err := ReadTerm(ctx, s, v, List()).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ctx = WithCont(context.Background(), func(ctx context.Context) *Promise {
			assert.Equal(t, NewAtom("foo").Apply(NewAtom("b")), Resolve(ctx, v))
			return Bool(true)
		})
		ok, err = ReadTerm(ctx, s, v, List()).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ctx = WithCont(context.Background(), func(ctx context.Context) *Promise {
			assert.Equal(t, &compound{functor: NewAtom("foo"), args: []Term{NewAtom("c")}}, Resolve(ctx, v))
			return Bool(true)
		})
		ok, err = ReadTerm(ctx, s, v, List()).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		ok, err := ReadTerm(context.Background(), NewVariable(), NewVariable(), List()).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("options is a partial list or a list with an element which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			ok, err := ReadTerm(context.Background(), &Stream{sourceSink: os.Stdin}, NewVariable(), PartialList(NewVariable(),
				atomVariables.Apply(NewVariable()),
			)).Force()
			assert.Equal(t, InstantiationError(context.Background()), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			ok, err := ReadTerm(context.Background(), &Stream{sourceSink: os.Stdin}, NewVariable(), List(NewVariable(), atomVariables.Apply(NewVariable()))).Force()
			assert.Equal(t, InstantiationError(context.Background()), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		ok, err := ReadTerm(context.Background(), Integer(0), NewVariable(), List()).Force()
		assert.Equal(t, domainError(context.Background(), validDomainStreamOrAlias, Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		ok, err := ReadTerm(context.Background(), &Stream{sourceSink: os.Stdin}, NewVariable(), NewAtom("options")).Force()
		assert.Equal(t, typeError(context.Background(), validTypeList, NewAtom("options")), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the Options list is neither a variable nor a valid read-option", func(t *testing.T) {
		for _, term := range []Term{atomUnknown, atomUnknown.Apply(NewAtom("option")), atomUnknown.Apply(NewAtom("option"), Integer(0))} {
			ok, err := ReadTerm(context.Background(), &Stream{sourceSink: os.Stdin}, NewVariable(), List(term)).Force()
			assert.Equal(t, domainError(context.Background(), validDomainReadOption, term), err)
			assert.False(t, ok)
		}
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		ok, err := ReadTerm(context.Background(), NewAtom("foo"), NewVariable(), List()).Force()
		assert.Equal(t, existenceError(context.Background(), objectTypeStream, NewAtom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		s := NewVariable()
		env := NewEnv().
			bind(s, &Stream{sourceSink: os.Stdout, mode: ioModeAppend})
		ctx := withEnv(context.Background(), env)

		ok, err := ReadTerm(ctx, s, NewVariable(), List()).Force()
		assert.Equal(t, permissionError(context.Background(), operationInput, permissionTypeStream, s), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		stream := &Stream{sourceSink: os.Stdin}
		stream.streamType = streamTypeBinary

		s := NewVariable()
		env := NewEnv().
			bind(s, stream)
		ctx := withEnv(context.Background(), env)

		ok, err := ReadTerm(ctx, s, NewVariable(), List()).Force()
		assert.Equal(t, permissionError(context.Background(), operationInput, permissionTypeBinaryStream, s), err)
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
		ctx := withEnv(context.Background(), env)

		ok, err := ReadTerm(ctx, s, NewVariable(), List()).Force()
		assert.Equal(t, permissionError(context.Background(), operationInput, permissionTypePastEndOfStream, s), err)
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

			ok, err := ReadTerm(context.Background(), s, NewVariable(), List()).Force()
			assert.Equal(t, syntaxError(context.Background(), unexpectedTokenError{actual: Token{kind: tokenLetterDigit, val: "bar"}}), err)
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
			ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
				assert.Equal(t, atomEndOfFile, Resolve(ctx, out))
				return Bool(true)
			})
			ok, err := ReadTerm(ctx, s, out, List()).Force()
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

		ok, err := ReadTerm(context.Background(), s, NewVariable(), List()).Force()
		assert.Equal(t, syntaxError(context.Background(), unexpectedTokenError{actual: Token{kind: tokenGraphic, val: "="}}), err)
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

		ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
			assert.Equal(t, Integer(97), Resolve(ctx, v))
			return Bool(true)
		})
		ok, err := GetByte(ctx, s, v).Force()
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
		ctx := withVM(context.Background(), &vm)
		ctx = WithCont(ctx, func(ctx context.Context) *Promise {
			assert.Equal(t, Integer(97), Resolve(ctx, v))
			return Bool(true)
		})
		ok, err := GetByte(ctx, foo, v).Force()
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

		ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
			assert.Equal(t, Integer(-1), Resolve(ctx, v))
			return Bool(true)
		})
		ok, err := GetByte(ctx, s, v).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("error", func(t *testing.T) {
		var m mockReader
		m.On("Read", mock.Anything).Return(0, errors.New("failed")).Twice()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m, mode: ioModeRead, streamType: streamTypeBinary}

		v := NewVariable()
		_, err := GetByte(context.Background(), s, v).Force()
		assert.Error(t, err)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		ok, err := GetByte(context.Background(), NewVariable(), NewVariable()).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("inByte is neither a variable nor an in-byte", func(t *testing.T) {
		s := &Stream{sourceSink: os.Stdin}
		s.streamType = streamTypeBinary

		t.Run("not even an integer", func(t *testing.T) {
			ok, err := GetByte(context.Background(), s, NewAtom("inByte")).Force()
			assert.Equal(t, typeError(context.Background(), validTypeInByte, NewAtom("inByte")), err)
			assert.False(t, ok)
		})

		t.Run("integer", func(t *testing.T) {
			ok, err := GetByte(context.Background(), s, Integer(256)).Force()
			assert.Equal(t, typeError(context.Background(), validTypeInByte, Integer(256)), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream-term or alias", func(t *testing.T) {
		ok, err := GetByte(context.Background(), Integer(0), NewVariable()).Force()
		assert.Equal(t, domainError(context.Background(), validDomainStreamOrAlias, Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		ok, err := GetByte(context.Background(), NewAtom("foo"), NewVariable()).Force()
		assert.Equal(t, existenceError(context.Background(), objectTypeStream, NewAtom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &Stream{sourceSink: os.Stdout, mode: ioModeAppend})
		ctx := withEnv(context.Background(), env)

		ok, err := GetByte(ctx, streamOrAlias, NewVariable()).Force()
		assert.Equal(t, permissionError(ctx, operationInput, permissionTypeStream, streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a text stream", func(t *testing.T) {
		streamOrAlias := NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &Stream{sourceSink: os.Stdin})
		ctx := withEnv(context.Background(), env)

		ok, err := GetByte(ctx, streamOrAlias, NewVariable()).Force()
		assert.Equal(t, permissionError(ctx, operationInput, permissionTypeTextStream, streamOrAlias), err)
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
		ctx := withEnv(context.Background(), env)

		ok, err := GetByte(ctx, streamOrAlias, NewVariable()).Force()
		assert.Equal(t, permissionError(context.Background(), operationInput, permissionTypePastEndOfStream, streamOrAlias), err)
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

		ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
			assert.Equal(t, NewAtom("😀"), Resolve(ctx, v))
			return Bool(true)
		})
		ok, err := GetChar(ctx, s, v).Force()
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
		ctx := withVM(context.Background(), &vm)
		ctx = WithCont(ctx, func(ctx context.Context) *Promise {
			assert.Equal(t, NewAtom("😀"), Resolve(ctx, v))
			return Bool(true)
		})
		ok, err := GetChar(ctx, foo, v).Force()
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

		ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
			assert.Equal(t, atomEndOfFile, Resolve(ctx, v))
			return Bool(true)
		})
		ok, err := GetChar(ctx, s, v).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("error", func(t *testing.T) {
		var m mockReader
		m.On("Read", mock.Anything).Return(0, errors.New("failed")).Times(2)
		defer m.AssertExpectations(t)

		v := NewVariable()

		ok, err := GetChar(context.Background(), &Stream{sourceSink: &m, mode: ioModeRead}, v).Force()
		assert.Equal(t, errors.New("failed"), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		ok, err := GetChar(context.Background(), NewVariable(), NewVariable()).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("char is neither a variable nor an in-character", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			ok, err := GetChar(context.Background(), &Stream{sourceSink: os.Stdin}, Integer(0)).Force()
			assert.Equal(t, typeError(context.Background(), validTypeInCharacter, Integer(0)), err)
			assert.False(t, ok)
		})

		t.Run("atom", func(t *testing.T) {
			ok, err := GetChar(context.Background(), &Stream{sourceSink: os.Stdin}, NewAtom("ab")).Force()
			assert.Equal(t, typeError(context.Background(), validTypeInCharacter, NewAtom("ab")), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		ok, err := GetChar(context.Background(), Integer(0), NewVariable()).Force()
		assert.Equal(t, domainError(context.Background(), validDomainStreamOrAlias, Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &Stream{sourceSink: os.Stdout, mode: ioModeAppend})
		ctx := withEnv(context.Background(), env)

		ok, err := GetChar(ctx, streamOrAlias, NewVariable()).Force()
		assert.Equal(t, permissionError(context.Background(), operationInput, permissionTypeStream, streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		s := &Stream{sourceSink: os.Stdin}
		s.streamType = streamTypeBinary

		streamOrAlias := NewVariable()
		env := NewEnv().
			bind(streamOrAlias, s)
		ctx := withEnv(context.Background(), env)

		ok, err := GetChar(ctx, streamOrAlias, NewVariable()).Force()
		assert.Equal(t, permissionError(context.Background(), operationInput, permissionTypeBinaryStream, streamOrAlias), err)
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
		ctx := withEnv(context.Background(), env)

		ok, err := GetChar(ctx, streamOrAlias, NewVariable()).Force()
		assert.Equal(t, permissionError(context.Background(), operationInput, permissionTypePastEndOfStream, streamOrAlias), err)
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
		ctx := withEnv(context.Background(), env)

		ok, err := GetChar(ctx, streamOrAlias, NewVariable()).Force()
		assert.Equal(t, representationError(context.Background(), flagCharacter), err)
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

		ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
			assert.Equal(t, Integer(97), Resolve(ctx, v))
			return Bool(true)
		})
		ok, err := PeekByte(ctx, s, v).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = PeekByte(ctx, s, v).Force() // 'a' again
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
		ctx := withVM(context.Background(), &vm)
		ctx = WithCont(ctx, func(ctx context.Context) *Promise {
			assert.Equal(t, Integer(97), Resolve(ctx, v))
			return Bool(true)
		})
		ok, err := PeekByte(ctx, NewAtom("foo"), v).Force()
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

		ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
			assert.Equal(t, Integer(-1), Resolve(ctx, v))
			return Bool(true)
		})
		ok, err := PeekByte(ctx, s, v).Force()
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

		ok, err := PeekByte(context.Background(), s, v).Force()
		assert.Equal(t, errors.New("failed"), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		ok, err := PeekByte(context.Background(), NewVariable(), NewVariable()).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("inByte is neither a variable nor an in-byte", func(t *testing.T) {
		s := &Stream{sourceSink: os.Stdin}
		s.streamType = streamTypeBinary

		t.Run("not even an integer", func(t *testing.T) {
			ok, err := PeekByte(context.Background(), s, NewAtom("byte")).Force()
			assert.Equal(t, typeError(context.Background(), validTypeInByte, NewAtom("byte")), err)
			assert.False(t, ok)
		})

		t.Run("integer", func(t *testing.T) {
			ok, err := PeekByte(context.Background(), s, Integer(256)).Force()
			assert.Equal(t, typeError(context.Background(), validTypeInByte, Integer(256)), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		ok, err := PeekByte(context.Background(), Integer(0), NewVariable()).Force()
		assert.Equal(t, domainError(context.Background(), validDomainStreamOrAlias, Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &Stream{sourceSink: os.Stdout, mode: ioModeAppend})
		ctx := withEnv(context.Background(), env)

		ok, err := PeekByte(ctx, streamOrAlias, NewVariable()).Force()
		assert.Equal(t, permissionError(context.Background(), operationInput, permissionTypeStream, streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a text stream", func(t *testing.T) {
		streamOrAlias := NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &Stream{sourceSink: os.Stdin})
		ctx := withEnv(context.Background(), env)

		ok, err := PeekByte(ctx, streamOrAlias, NewVariable()).Force()
		assert.Equal(t, permissionError(context.Background(), operationInput, permissionTypeTextStream, streamOrAlias), err)
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
		ctx := withEnv(context.Background(), env)

		ok, err := PeekByte(ctx, streamOrAlias, NewVariable()).Force()
		assert.Equal(t, permissionError(ctx, operationInput, permissionTypePastEndOfStream, streamOrAlias), err)
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

		ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
			assert.Equal(t, NewAtom("😀"), Resolve(ctx, v))
			return Bool(true)
		})
		ok, err := PeekChar(ctx, s, v).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ctx = WithCont(context.Background(), func(ctx context.Context) *Promise {
			assert.Equal(t, NewAtom("😀"), Resolve(ctx, v)) // '😀' again
			return Bool(true)
		})
		ok, err = PeekChar(ctx, s, v).Force()
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
		ctx := withVM(context.Background(), &vm)
		ctx = WithCont(ctx, func(ctx context.Context) *Promise {
			assert.Equal(t, NewAtom("😀"), Resolve(ctx, v))
			return Bool(true)
		})
		ok, err := PeekChar(ctx, foo, v).Force()
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

		ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
			assert.Equal(t, atomEndOfFile, Resolve(ctx, v))
			return Bool(true)
		})
		ok, err := PeekChar(ctx, s, v).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("error", func(t *testing.T) {
		var m mockReader
		m.On("Read", mock.Anything).Return(0, errors.New("failed")).Twice()
		defer m.AssertExpectations(t)

		v := NewVariable()

		ok, err := PeekChar(context.Background(), &Stream{sourceSink: &m, mode: ioModeRead}, v).Force()
		assert.Equal(t, errors.New("failed"), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		ok, err := PeekChar(context.Background(), NewVariable(), NewVariable()).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("char is neither a variable nor an in-character", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			ok, err := PeekChar(context.Background(), &Stream{sourceSink: os.Stdin}, Integer(0)).Force()
			assert.Equal(t, typeError(context.Background(), validTypeInCharacter, Integer(0)), err)
			assert.False(t, ok)
		})

		t.Run("atom", func(t *testing.T) {
			ok, err := PeekChar(context.Background(), &Stream{sourceSink: os.Stdin}, NewAtom("ab")).Force()
			assert.Equal(t, typeError(context.Background(), validTypeInCharacter, NewAtom("ab")), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		ok, err := PeekChar(context.Background(), Integer(0), NewVariable()).Force()
		assert.Equal(t, domainError(context.Background(), validDomainStreamOrAlias, Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &Stream{sourceSink: os.Stdout, mode: ioModeAppend})
		ctx := withEnv(context.Background(), env)

		ok, err := PeekChar(ctx, streamOrAlias, NewVariable()).Force()
		assert.Equal(t, permissionError(context.Background(), operationInput, permissionTypeStream, streamOrAlias), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		s := &Stream{sourceSink: os.Stdin}
		s.streamType = streamTypeBinary

		streamOrAlias := NewVariable()
		env := NewEnv().
			bind(streamOrAlias, s)
		ctx := withEnv(context.Background(), env)

		ok, err := PeekChar(ctx, streamOrAlias, NewVariable()).Force()
		assert.Equal(t, permissionError(context.Background(), operationInput, permissionTypeBinaryStream, streamOrAlias), err)
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
		ctx := withEnv(context.Background(), env)

		ok, err := PeekChar(ctx, streamOrAlias, NewVariable()).Force()
		assert.Equal(t, permissionError(context.Background(), operationInput, permissionTypePastEndOfStream, streamOrAlias), err)
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
		ctx := withEnv(context.Background(), env)

		ok, err := PeekChar(ctx, streamOrAlias, NewVariable()).Force()
		assert.Equal(t, representationError(context.Background(), flagCharacter), err)
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

		ok, err := Halt(context.Background(), Integer(2)).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.True(t, exitCalled)
	})

	t.Run("n is a variable", func(t *testing.T) {
		n := NewVariable()

		ok, err := Halt(context.Background(), n).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("n is neither a variable nor an integer", func(t *testing.T) {
		ok, err := Halt(context.Background(), NewAtom("foo")).Force()
		assert.Equal(t, typeError(context.Background(), validTypeInteger, NewAtom("foo")), err)
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
		ctx := withVM(context.Background(), &vm)
		ctx = WithCont(ctx, func(ctx context.Context) *Promise {
			switch c {
			case 0:
				b, ok := Resolve(ctx, body).(*compound)
				assert.True(t, ok)
				assert.Equal(t, NewAtom("moldy"), b.functor)
				assert.Len(t, b.args, 1)
			case 1:
				assert.Equal(t, NewAtom("kermit"), Resolve(ctx, what))
				assert.Equal(t, atomTrue, Resolve(ctx, body))
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return Bool(false)
		})
		ok, err := Clause(ctx, NewAtom("green").Apply(what), body).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("not found", func(t *testing.T) {
		var vm VM
		ctx := withVM(context.Background(), &vm)
		ok, err := Clause(ctx, NewAtom("foo"), atomTrue).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("head is a variable", func(t *testing.T) {
		ok, err := Clause(context.Background(), NewVariable(), atomTrue).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("head is neither a variable nor a predication", func(t *testing.T) {
		ok, err := Clause(context.Background(), Integer(0), atomTrue).Force()
		assert.Equal(t, typeError(context.Background(), validTypeCallable, Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("the predicate indicator Pred of Head is that of a private (ie. Not public) procedure", func(t *testing.T) {
		what, body := NewVariable(), NewVariable()

		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: NewAtom("green"), arity: 1}: Predicate1(func(ctx context.Context, t Term) *Promise {
					return Bool(true)
				}),
			},
		}
		ctx := withVM(context.Background(), &vm)
		ok, err := Clause(ctx, NewAtom("green").Apply(what), body).Force()
		assert.Equal(t, permissionError(context.Background(), operationAccess, permissionTypePrivateProcedure, atomSlash.Apply(NewAtom("green"), Integer(1))), err)
		assert.False(t, ok)
	})

	t.Run("body is neither a variable nor a callable term", func(t *testing.T) {
		ok, err := Clause(context.Background(), NewAtom("foo"), Integer(0)).Force()
		assert.Equal(t, typeError(context.Background(), validTypeCallable, Integer(0)), err)
		assert.False(t, ok)
	})
}

func TestAtomLength(t *testing.T) {
	n := NewVariable()

	tests := []struct {
		title        string
		atom, length Term
		ok           bool
		err          error
		env          map[Variable]Term
	}{
		// 8.16.1.4 Examples
		{title: "atom_length('enchanted evening', N).", atom: NewAtom("enchanted evening"), length: n, ok: true, env: map[Variable]Term{
			n: Integer(17),
		}},
		{title: `atom_length('enchanted\
 evening', N).`, atom: NewAtom("enchanted evening"), length: n, ok: true, env: map[Variable]Term{
			n: Integer(17),
		}},
		{title: "atom_length('', N).", atom: NewAtom(""), length: n, ok: true, env: map[Variable]Term{
			n: Integer(0),
		}},
		{title: "atom_length('scarlet', 5).", atom: NewAtom("scarlet"), length: Integer(5), ok: false},
		{title: "atom_length(Atom, 4).", atom: NewVariable(), length: Integer(4), err: InstantiationError(context.Background())},
		{title: "atom_length(1.23, 4).", atom: Float(1.23), length: Integer(4), err: typeError(context.Background(), validTypeAtom, Float(1.23))},
		{title: "atom_length(atom, '4').", atom: NewAtom("atom"), length: NewAtom("4"), err: typeError(context.Background(), validTypeInteger, NewAtom("4"))},

		// 8.16.1.3 Errors
		{title: "d", atom: NewAtom("atom"), length: Integer(-1), err: domainError(context.Background(), validDomainNotLessThanZero, Integer(-1))},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
				env := env(ctx)
				for k, v := range tt.env {
					_, ok := env.Unify(k, v)
					assert.True(t, ok)
				}
				return Bool(true)
			})
			ok, err := AtomLength(ctx, tt.atom, tt.length).Force()
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestAtomConcat(t *testing.T) {
	t.Run("atom3 is a variable", func(t *testing.T) {
		atom3 := NewVariable()

		ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
			assert.Equal(t, NewAtom("foobar"), Resolve(ctx, atom3))
			return Bool(true)
		})
		ok, err := AtomConcat(ctx, NewAtom("foo"), NewAtom("bar"), atom3).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("atom3 is an atom", func(t *testing.T) {
		var c int
		v1, v2 := NewVariable(), NewVariable()
		ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
			switch c {
			case 0:
				assert.Equal(t, NewAtom(""), Resolve(ctx, v1))
				assert.Equal(t, NewAtom("foo"), Resolve(ctx, v2))
			case 1:
				assert.Equal(t, NewAtom("f"), Resolve(ctx, v1))
				assert.Equal(t, NewAtom("oo"), Resolve(ctx, v2))
			case 2:
				assert.Equal(t, NewAtom("fo"), Resolve(ctx, v1))
				assert.Equal(t, NewAtom("o"), Resolve(ctx, v2))
			case 3:
				assert.Equal(t, NewAtom("foo"), Resolve(ctx, v1))
				assert.Equal(t, NewAtom(""), Resolve(ctx, v2))
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return Bool(false)
		})
		ok, err := AtomConcat(ctx, v1, v2, NewAtom("foo")).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("atom1 and atom3 are variables", func(t *testing.T) {
		atom1, atom3 := NewVariable(), NewVariable()

		ok, err := AtomConcat(context.Background(), atom1, NewAtom("bar"), atom3).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("atom2 and atom3 are variables", func(t *testing.T) {
		atom2, atom3 := NewVariable(), NewVariable()

		ok, err := AtomConcat(context.Background(), NewAtom("foo"), atom2, atom3).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("atom1 is neither a variable nor an atom", func(t *testing.T) {
		t.Run("atom3 is a variable", func(t *testing.T) {
			ok, err := AtomConcat(context.Background(), Integer(1), NewAtom("bar"), NewVariable()).Force()
			assert.Equal(t, typeError(context.Background(), validTypeAtom, Integer(1)), err)
			assert.False(t, ok)
		})

		t.Run("atom3 is an atom", func(t *testing.T) {
			ok, err := AtomConcat(context.Background(), Integer(1), NewAtom("bar"), NewAtom("foobar")).Force()
			assert.Equal(t, typeError(context.Background(), validTypeAtom, Integer(1)), err)
			assert.False(t, ok)
		})
	})

	t.Run("atom2 is neither a variable nor an atom", func(t *testing.T) {
		t.Run("atom3 is a variable", func(t *testing.T) {
			ok, err := AtomConcat(context.Background(), NewAtom("foo"), Integer(2), NewVariable()).Force()
			assert.Equal(t, typeError(context.Background(), validTypeAtom, Integer(2)), err)
			assert.False(t, ok)
		})

		t.Run("atom3 is an atom", func(t *testing.T) {
			ok, err := AtomConcat(context.Background(), NewAtom("foo"), Integer(2), NewAtom("foobar")).Force()
			assert.Equal(t, typeError(context.Background(), validTypeAtom, Integer(2)), err)
			assert.False(t, ok)
		})
	})

	t.Run("atom3 is neither a variable nor an atom", func(t *testing.T) {
		ok, err := AtomConcat(context.Background(), NewAtom("foo"), NewAtom("bar"), Integer(3)).Force()
		assert.Equal(t, typeError(context.Background(), validTypeAtom, Integer(3)), err)
		assert.False(t, ok)
	})
}

func TestSubAtom(t *testing.T) {
	t.Run("multiple solutions", func(t *testing.T) {
		before, length, after := NewVariable(), NewVariable(), NewVariable()
		var c int
		ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
			switch c {
			case 0:
				assert.Equal(t, Integer(1), Resolve(ctx, before))
				assert.Equal(t, Integer(4), Resolve(ctx, length))
				assert.Equal(t, Integer(14), Resolve(ctx, after))
			case 1:
				assert.Equal(t, Integer(4), Resolve(ctx, before))
				assert.Equal(t, Integer(4), Resolve(ctx, length))
				assert.Equal(t, Integer(11), Resolve(ctx, after))
			case 2:
				assert.Equal(t, Integer(9), Resolve(ctx, before))
				assert.Equal(t, Integer(4), Resolve(ctx, length))
				assert.Equal(t, Integer(6), Resolve(ctx, after))
			case 3:
				assert.Equal(t, Integer(14), Resolve(ctx, before))
				assert.Equal(t, Integer(4), Resolve(ctx, length))
				assert.Equal(t, Integer(1), Resolve(ctx, after))
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return Bool(false)
		})
		ok, err := SubAtom(ctx, NewAtom("xATGATGAxATGAxATGAx"), before, length, after, NewAtom("ATGA")).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("get the first char", func(t *testing.T) {
		char := NewVariable()
		ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
			assert.Equal(t, NewAtom("a"), Resolve(ctx, char))
			return Bool(true)
		})
		ok, err := SubAtom(ctx, NewAtom("a"), Integer(0), Integer(1), Integer(0), char).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("atom is a variable", func(t *testing.T) {
		ok, err := SubAtom(context.Background(), NewVariable(), NewVariable(), NewVariable(), NewVariable(), NewVariable()).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("atom is neither a variable nor an atom", func(t *testing.T) {
		ok, err := SubAtom(context.Background(), Integer(0), NewVariable(), NewVariable(), NewVariable(), NewVariable()).Force()
		assert.Equal(t, typeError(context.Background(), validTypeAtom, Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("subAtom is neither a variable nor an atom", func(t *testing.T) {
		ok, err := SubAtom(context.Background(), NewAtom("foo"), NewVariable(), NewVariable(), NewVariable(), Integer(0)).Force()
		assert.Equal(t, typeError(context.Background(), validTypeAtom, Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("before is neither a variable nor an integer", func(t *testing.T) {
		ok, err := SubAtom(context.Background(), NewAtom("foo"), NewAtom("before"), NewVariable(), NewVariable(), NewVariable()).Force()
		assert.Equal(t, typeError(context.Background(), validTypeInteger, NewAtom("before")), err)
		assert.False(t, ok)
	})

	t.Run("length is neither a variable nor an integer", func(t *testing.T) {
		ok, err := SubAtom(context.Background(), NewAtom("foo"), NewVariable(), NewAtom("length"), NewVariable(), NewVariable()).Force()
		assert.Equal(t, typeError(context.Background(), validTypeInteger, NewAtom("length")), err)
		assert.False(t, ok)
	})

	t.Run("after is neither a variable nor an integer", func(t *testing.T) {
		ok, err := SubAtom(context.Background(), NewAtom("foo"), NewVariable(), NewVariable(), NewAtom("after"), NewVariable()).Force()
		assert.Equal(t, typeError(context.Background(), validTypeInteger, NewAtom("after")), err)
		assert.False(t, ok)
	})

	t.Run("before is an integer less than zero", func(t *testing.T) {
		ok, err := SubAtom(context.Background(), NewAtom("foo"), Integer(-1), NewVariable(), NewVariable(), NewVariable()).Force()
		assert.Equal(t, domainError(context.Background(), validDomainNotLessThanZero, Integer(-1)), err)
		assert.False(t, ok)
	})

	t.Run("length is an integer less than zero", func(t *testing.T) {
		ok, err := SubAtom(context.Background(), NewAtom("foo"), NewVariable(), Integer(-1), NewVariable(), NewVariable()).Force()
		assert.Equal(t, domainError(context.Background(), validDomainNotLessThanZero, Integer(-1)), err)
		assert.False(t, ok)
	})

	t.Run("after is an integer less than zero", func(t *testing.T) {
		ok, err := SubAtom(context.Background(), NewAtom("foo"), NewVariable(), NewVariable(), Integer(-1), NewVariable()).Force()
		assert.Equal(t, domainError(context.Background(), validDomainNotLessThanZero, Integer(-1)), err)
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
		{title: "atom_chars(X, Y).", atom: x, list: y, err: InstantiationError(context.Background())},

		// 8.16.4.3 Errors
		{title: "a", atom: x, list: PartialList(y, NewAtom("a")), err: InstantiationError(context.Background())},
		{title: "b", atom: Integer(0), list: List(NewAtom("a"), NewAtom("b"), NewAtom("c")), err: typeError(context.Background(), validTypeAtom, Integer(0))},
		{title: "c: atom is a variable", atom: x, list: Integer(0), err: typeError(context.Background(), validTypeList, Integer(0))},
		{title: "c: atom is an atom", atom: NewAtom("a"), list: Integer(0), err: typeError(context.Background(), validTypeList, Integer(0))},
		{title: "d", atom: x, list: List(y, NewAtom("a")), err: InstantiationError(context.Background())},
		{title: "e: atom is a variable, more than one char", atom: x, list: List(NewAtom("abc")), err: typeError(context.Background(), validTypeCharacter, NewAtom("abc"))},
		{title: "e: atom is a variable, not an atom", atom: x, list: List(Integer(0)), err: typeError(context.Background(), validTypeCharacter, Integer(0))},
		{title: "e: atom is an atom, more than one char", atom: NewAtom("abc"), list: List(NewAtom("ab"), NewAtom("c")), err: typeError(context.Background(), validTypeCharacter, NewAtom("ab"))},
		{title: "e: atom is an atom, not an atom", atom: NewAtom("abc"), list: List(Integer('a'), NewAtom("b"), NewAtom("c")), err: typeError(context.Background(), validTypeCharacter, Integer('a'))},

		{title: "atom_chars('ant', ['a', X, 't']).", atom: NewAtom("ant"), list: List(NewAtom("a"), x, NewAtom("t")), ok: true, env: map[Variable]Term{
			x: NewAtom("n"),
		}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
				env := env(ctx)
				for k, v := range tt.env {
					_, ok := env.Unify(k, v)
					assert.True(t, ok)
				}
				return Bool(true)
			})
			ok, err := AtomChars(ctx, tt.atom, tt.list).Force()
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
		{title: "atom_codes(X, Y).", atom: x, list: y, err: InstantiationError(context.Background())},

		// 8.16.5.3 Errors
		{title: "a", atom: x, list: PartialList(y, Integer(0)), err: InstantiationError(context.Background())},
		{title: "b", atom: Integer(0), list: l, err: typeError(context.Background(), validTypeAtom, Integer(0))},
		{title: "c: atom is a variable", atom: x, list: Integer(0), err: typeError(context.Background(), validTypeList, Integer(0))},
		{title: "c: atom is an atom", atom: NewAtom("abc"), list: Integer(0), err: typeError(context.Background(), validTypeList, Integer(0))},
		{title: "d", atom: x, list: List(y, Integer('b'), Integer('c')), err: InstantiationError(context.Background())},
		{title: "e: atom is a variable", atom: x, list: List(NewAtom("a"), Integer('b'), Integer('c')), err: typeError(context.Background(), validTypeInteger, NewAtom("a"))},
		{title: "e: atom is an atom", atom: NewAtom("abc"), list: List(NewAtom("a"), Integer('b'), Integer('c')), err: typeError(context.Background(), validTypeInteger, NewAtom("a"))},
		{title: "f: atom is a variable", atom: x, list: List(Integer(-1), Integer('b'), Integer('c')), err: representationError(context.Background(), flagCharacterCode)},
		{title: "f: atom is an atom", atom: NewAtom("abc"), list: List(Integer(-1), Integer('b'), Integer('c')), err: representationError(context.Background(), flagCharacterCode)},

		{title: "atom_codes('ant', [0'a, X, 0't]).", atom: NewAtom("ant"), list: List(Integer('a'), x, Integer('t')), ok: true, env: map[Variable]Term{
			x: Integer('n'),
		}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
				env := env(ctx)
				for k, v := range tt.env {
					_, ok := env.Unify(k, v)
					assert.True(t, ok)
				}
				return Bool(true)
			})
			ok, err := AtomCodes(ctx, tt.atom, tt.list).Force()
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestNumberChars(t *testing.T) {
	t.Run("number to chars", func(t *testing.T) {
		t.Run("chars is a partial list", func(t *testing.T) {
			chars := NewVariable()
			ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
				assert.Equal(t, List(NewAtom("2"), NewAtom("3"), atomDot, NewAtom("4")), Resolve(ctx, chars))
				return Bool(true)
			})
			ok, err := NumberChars(ctx, Float(23.4), chars).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("chars is a list with variables", func(t *testing.T) {
			char := NewVariable()
			ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
				assert.Equal(t, NewAtom("2"), Resolve(ctx, char))
				return Bool(true)
			})
			ok, err := NumberChars(ctx, Float(23.4), List(char, NewAtom("3"), atomDot, NewAtom("4"))).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("chars to number", func(t *testing.T) {
		num := NewVariable()
		ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
			assert.Equal(t, Float(23.4), Resolve(ctx, num))
			return Bool(true)
		})
		ok, err := NumberChars(ctx, num, List(NewAtom("2"), NewAtom("3"), atomDot, NewAtom("4"))).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("both provided", func(t *testing.T) {
		t.Run("3.3", func(t *testing.T) {
			ok, err := NumberChars(context.Background(), Float(3.3), List(NewAtom("3"), atomDot, NewAtom("3"))).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("3.3E+0", func(t *testing.T) {
			ok, err := NumberChars(context.Background(), Float(3.3), List(NewAtom("3"), atomDot, NewAtom("3"), NewAtom("E"), atomPlus, NewAtom("0"))).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("num is a variable and chars is a partial list", func(t *testing.T) {
		chars := PartialList(NewVariable(),
			NewAtom("2"), NewAtom("3"), atomDot, NewAtom("4"),
		)

		ok, err := NumberChars(context.Background(), NewVariable(), chars).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("num is neither a variable nor a number", func(t *testing.T) {
		t.Run("chars is a list of one-char atoms", func(t *testing.T) {
			ok, err := NumberChars(context.Background(), NewAtom("23.4"), List(NewAtom("2"), NewAtom("3"), atomDot, NewAtom("4"))).Force()
			assert.Equal(t, typeError(context.Background(), validTypeNumber, NewAtom("23.4")), err)
			assert.False(t, ok)
		})

		t.Run("chars is not a list of one-char atoms", func(t *testing.T) {
			ok, err := NumberChars(context.Background(), NewAtom("23.4"), NewVariable()).Force()
			assert.Equal(t, typeError(context.Background(), validTypeNumber, NewAtom("23.4")), err)
			assert.False(t, ok)
		})
	})

	t.Run("chars is neither a partial list nor a list", func(t *testing.T) {
		t.Run("not even list-ish", func(t *testing.T) {
			ok, err := NumberChars(context.Background(), NewVariable(), NewAtom("foo")).Force()
			assert.Equal(t, typeError(context.Background(), validTypeList, NewAtom("foo")), err)
			assert.False(t, ok)
		})

		t.Run("list-ish", func(t *testing.T) {
			_, err := NumberChars(context.Background(), Integer(0), PartialList(NewAtom("b"), NewVariable())).Force()
			_, ok := NewEnv().Unify(err.(Exception).Term(), typeError(context.Background(), validTypeList, PartialList(NewAtom("b"), NewVariable())).Term())
			assert.True(t, ok)
		})
	})

	t.Run("num is a variable and an element of a list prefix of chars is a variable", func(t *testing.T) {
		ok, err := NumberChars(context.Background(), NewVariable(), List(NewAtom("1"), NewVariable())).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("chars is a list of one-char atoms but is not parsable as a number", func(t *testing.T) {
		t.Run("not a number", func(t *testing.T) {
			ok, err := NumberChars(context.Background(), NewVariable(), List(NewAtom("f"), NewAtom("o"), NewAtom("o"))).Force()
			assert.Equal(t, syntaxError(context.Background(), errNotANumber), err)
			assert.False(t, ok)
		})

		t.Run("unexpected token", func(t *testing.T) {
			ok, err := NumberChars(context.Background(), NewVariable(), List(NewAtom("1"), atomDot)).Force()
			assert.Equal(t, syntaxError(context.Background(), errNotANumber), err)
			assert.False(t, ok)
		})
	})

	t.Run("an element E of a list prefix of chars is neither a variable nor a one-char atom", func(t *testing.T) {
		t.Run("chars contains a variable", func(t *testing.T) {
			t.Run("not even an atom", func(t *testing.T) {
				ok, err := NumberChars(context.Background(), Integer(100), List(NewVariable(), NewAtom("0"), Integer(0))).Force()
				assert.Equal(t, typeError(context.Background(), validTypeCharacter, Integer(0)), err)
				assert.False(t, ok)
			})

			t.Run("atom", func(t *testing.T) {
				ok, err := NumberChars(context.Background(), Integer(100), List(NewVariable(), NewAtom("00"))).Force()
				assert.Equal(t, typeError(context.Background(), validTypeCharacter, NewAtom("00")), err)
				assert.False(t, ok)
			})
		})

		t.Run("chars does not contain a variable", func(t *testing.T) {
			t.Run("not even an atom", func(t *testing.T) {
				ok, err := NumberChars(context.Background(), Integer(100), List(NewAtom("1"), NewAtom("0"), Integer(0))).Force()
				assert.Equal(t, typeError(context.Background(), validTypeCharacter, Integer(0)), err)
				assert.False(t, ok)
			})

			t.Run("atom", func(t *testing.T) {
				ok, err := NumberChars(context.Background(), Integer(100), List(NewAtom("1"), NewAtom("00"))).Force()
				assert.Equal(t, typeError(context.Background(), validTypeCharacter, NewAtom("00")), err)
				assert.False(t, ok)
			})
		})
	})
}

func TestNumberCodes(t *testing.T) {
	a, l := NewVariable(), NewVariable()

	tests := []struct {
		title        string
		number, list Term
		ok           bool
		err          error
		env          map[Variable]Term
	}{
		// 8.16.8.4 Examples
		{title: "number_codes(33, L).", number: Integer(33), list: l, ok: true, env: map[Variable]Term{
			l: List(Integer('3'), Integer('3')),
		}},
		{title: "number_codes(33, [0'3, 0'3]).", number: Integer(33), list: List(Integer('3'), Integer('3')), ok: true},
		{title: "number_codes(33.0, L).", number: Float(33.0), list: l, ok: true, env: map[Variable]Term{
			l: List(Integer('3'), Integer('3'), Integer('.'), Integer('0')),
		}},
		{title: "number_codes(33.0, [0'3, 0'., 0'3, 0'E, 0'+, 0'0, 0'1]).", number: Float(33.0), list: List(Integer('3'), Integer('.'), Integer('3'), Integer('E'), Integer('+'), Integer('0'), Integer('1')), ok: true},
		{title: "number_codes(A, [0'-, 0'2, 0'5]).", number: a, list: List(Integer('-'), Integer('2'), Integer('5')), ok: true, env: map[Variable]Term{
			a: Integer(-25),
		}},
		{title: "number_codes(A, [0' , 0'3]).", number: a, list: List(Integer(' '), Integer('3')), ok: true, env: map[Variable]Term{
			a: Integer(3),
		}},
		{title: "number_codes(A, [0'0, 0'x, 0'f]).", number: a, list: List(Integer('0'), Integer('x'), Integer('f')), ok: true, env: map[Variable]Term{
			a: Integer(15),
		}},
		{title: "number_codes(A, [0'0, 0''', 0'a]).", number: a, list: List(Integer('0'), Integer('\''), Integer('a')), ok: true, env: map[Variable]Term{
			a: Integer('a'),
		}},
		{title: "number_codes(A, [0'4, 0'., 0'2]).", number: a, list: List(Integer('4'), Integer('.'), Integer('2')), ok: true, env: map[Variable]Term{
			a: Float(4.2),
		}},
		{title: "number_codes(A, [0'4, 0'2, 0'., 0'0, 0'e, 0'-, 0'1]).", number: a, list: List(Integer('4'), Integer('2'), Integer('.'), Integer('0'), Integer('e'), Integer('-'), Integer('1')), ok: true, env: map[Variable]Term{
			a: Float(4.2),
		}},

		// 8.16.8.3 Errors
		{title: "a", number: a, list: l, err: InstantiationError(context.Background())},
		{title: "b: no variables in the list", number: NewAtom("foo"), list: List(Integer('0')), err: typeError(context.Background(), validTypeNumber, NewAtom("foo"))},
		{title: "b: variables in the list", number: NewAtom("foo"), list: List(NewVariable(), Integer('0')), err: typeError(context.Background(), validTypeNumber, NewAtom("foo"))},
		{title: "c: without a variable element", number: Integer(0), list: NewAtom("foo"), err: typeError(context.Background(), validTypeList, NewAtom("foo"))},
		{title: "c: with a variable element", number: Integer(0), list: PartialList(NewAtom("foo"), NewVariable()), err: typeError(context.Background(), validTypeList, PartialList(NewAtom("foo"), NewVariable()))},
		{title: "d", number: a, list: List(NewVariable()), err: InstantiationError(context.Background())},
		{title: "e", number: a, list: List(Integer('f'), Integer('o'), Integer('o')), err: syntaxError(context.Background(), errNotANumber)},
		{title: "f: without a variable element", number: Integer(0), list: List(NewAtom("foo")), err: typeError(context.Background(), validTypeInteger, NewAtom("foo"))},
		{title: "f: with a variable element", number: Integer(0), list: List(NewVariable(), NewAtom("foo")), err: typeError(context.Background(), validTypeInteger, NewAtom("foo"))},
		{title: "g: without a variable element", number: Integer(0), list: List(Integer(utf8.MaxRune + 1)), err: representationError(context.Background(), flagCharacterCode)},
		{title: "g: with a variable element", number: Integer(0), list: List(NewVariable(), Integer(utf8.MaxRune+1)), err: representationError(context.Background(), flagCharacterCode)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
				env := env(ctx)
				for k, v := range tt.env {
					_, ok := env.Unify(k, v)
					assert.True(t, ok)
				}
				return Bool(true)
			})
			ok, err := NumberCodes(ctx, tt.number, tt.list).Force()
			if tt.err == nil {
				assert.NoError(t, err)
			} else if te, ok := tt.err.(Exception); ok {
				_, ok := NewEnv().Unify(te.term, err.(Exception).term)
				assert.True(t, ok)
			}
			assert.Equal(t, tt.ok, ok)
		})
	}
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
		{title: "b", stream: Integer(0), property: p, err: domainError(context.Background(), validDomainStream, Integer(0))},
		{title: "c: unknown atom", stream: s, property: NewAtom("foo"), err: domainError(context.Background(), validDomainStreamProperty, NewAtom("foo"))},
		{title: "c: compound with multiple args", stream: s, property: NewAtom("f").Apply(NewAtom("a"), NewAtom("b")), err: domainError(context.Background(), validDomainStreamProperty, NewAtom("f").Apply(NewAtom("a"), NewAtom("b")))},
		{title: "c: compound with an unexpected integer arg", stream: s, property: atomAlias.Apply(Integer(0)), err: domainError(context.Background(), validDomainStreamProperty, atomAlias.Apply(Integer(0)))},
		{title: "c: compound with an unexpected atom arg", stream: s, property: atomPosition.Apply(NewAtom("foo")), err: domainError(context.Background(), validDomainStreamProperty, atomPosition.Apply(NewAtom("foo")))},
		{title: "c: unknown compound", stream: s, property: NewAtom("foo").Apply(NewAtom("bar")), err: domainError(context.Background(), validDomainStreamProperty, NewAtom("foo").Apply(NewAtom("bar")))},
		{title: "c: unexpected arg", stream: s, property: Integer(0), err: domainError(context.Background(), validDomainStreamProperty, Integer(0))},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
				env := env(ctx)
				for k, v := range tt.env[0] {
					_, ok := env.Unify(k, v)
					assert.True(t, ok)
				}
				tt.env = tt.env[1:]
				return Bool(len(tt.env) == 0)
			})
			ok, err := StreamProperty(ctx, tt.stream, tt.property).Force()
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

		ok, err := SetStreamPosition(context.Background(), s, Integer(0)).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("seek failed", func(t *testing.T) {
		var m mockFile
		m.On("Seek", mock.Anything, mock.Anything).Return(int64(0), errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m, mode: ioModeRead, reposition: true}

		ok, err := SetStreamPosition(context.Background(), s, Integer(0)).Force()
		assert.Error(t, err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		ok, err := SetStreamPosition(context.Background(), NewVariable(), Integer(0)).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("position is a variable", func(t *testing.T) {
		f, err := os.Open("testdata/empty.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &Stream{sourceSink: f, mode: ioModeRead, reposition: true}

		ok, err := SetStreamPosition(context.Background(), s, NewVariable()).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		ok, err := SetStreamPosition(context.Background(), Integer(2), Integer(0)).Force()
		assert.Equal(t, domainError(context.Background(), validDomainStreamOrAlias, Integer(2)), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		ok, err := SetStreamPosition(context.Background(), NewAtom("foo"), Integer(0)).Force()
		assert.Equal(t, existenceError(context.Background(), objectTypeStream, NewAtom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream property reposition(false)", func(t *testing.T) {
		stream := &Stream{sourceSink: os.Stdin}

		assert.False(t, stream.reposition)

		s := NewVariable()
		env := NewEnv().
			bind(s, stream)
		ctx := withEnv(context.Background(), env)

		ok, err := SetStreamPosition(ctx, s, Integer(0)).Force()
		assert.Equal(t, permissionError(context.Background(), operationReposition, permissionTypeStream, s), err)
		assert.False(t, ok)
	})
}

func TestCharConversion(t *testing.T) {
	t.Run("register", func(t *testing.T) {
		var vm VM
		ctx := withVM(context.Background(), &vm)
		ok, err := CharConversion(ctx, NewAtom("a"), NewAtom("b")).Force()
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
		ctx := withVM(context.Background(), &vm)
		ok, err := CharConversion(ctx, NewAtom("a"), NewAtom("a")).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		_, ok = vm.charConversions['a']
		assert.False(t, ok)
	})

	t.Run("inChar is a variable", func(t *testing.T) {
		ok, err := CharConversion(context.Background(), NewVariable(), NewAtom("a")).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("outChar is a variable", func(t *testing.T) {
		ok, err := CharConversion(context.Background(), NewAtom("a"), NewVariable()).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("inChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			ok, err := CharConversion(context.Background(), Integer(0), NewAtom("a")).Force()
			assert.Equal(t, representationError(context.Background(), flagCharacter), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			ok, err := CharConversion(context.Background(), NewAtom("foo"), NewAtom("a")).Force()
			assert.Equal(t, representationError(context.Background(), flagCharacter), err)
			assert.False(t, ok)
		})
	})

	t.Run("outChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			ok, err := CharConversion(context.Background(), NewAtom("a"), Integer(0)).Force()
			assert.Equal(t, representationError(context.Background(), flagCharacter), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			ok, err := CharConversion(context.Background(), NewAtom("a"), NewAtom("foo")).Force()
			assert.Equal(t, representationError(context.Background(), flagCharacter), err)
			assert.False(t, ok)
		})
	})
}

func TestCurrentCharConversion(t *testing.T) {
	t.Run("specified", func(t *testing.T) {
		t.Run("as is", func(t *testing.T) {
			var vm VM
			ctx := withVM(context.Background(), &vm)
			ok, err := CurrentCharConversion(ctx, NewAtom("a"), NewAtom("a")).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("converted", func(t *testing.T) {
			vm := VM{
				charConversions: map[rune]rune{
					'a': 'b',
				},
			}
			ctx := withVM(context.Background(), &vm)
			ok, err := CurrentCharConversion(ctx, NewAtom("a"), NewAtom("b")).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("not specified", func(t *testing.T) {
		x, y := NewVariable(), NewVariable()

		var r rune
		var vm VM
		ctx := withVM(context.Background(), &vm)
		ctx = WithCont(ctx, func(ctx context.Context) *Promise {
			env := env(ctx)
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
		})
		ok, err := CurrentCharConversion(ctx, x, y).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
		assert.Equal(t, rune(256), r)
	})

	t.Run("inChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var vm VM
			ctx := withVM(context.Background(), &vm)
			ok, err := CurrentCharConversion(ctx, Integer(0), NewAtom("b")).Force()
			assert.Equal(t, representationError(context.Background(), flagCharacter), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var vm VM
			ctx := withVM(context.Background(), &vm)
			ok, err := CurrentCharConversion(ctx, NewAtom("foo"), NewAtom("b")).Force()
			assert.Equal(t, representationError(context.Background(), flagCharacter), err)
			assert.False(t, ok)
		})
	})

	t.Run("outChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var vm VM
			ctx := withVM(context.Background(), &vm)
			ok, err := CurrentCharConversion(ctx, NewAtom("a"), Integer(0)).Force()
			assert.Equal(t, representationError(context.Background(), flagCharacter), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var vm VM
			ctx := withVM(context.Background(), &vm)
			ok, err := CurrentCharConversion(ctx, NewAtom("a"), NewAtom("bar")).Force()
			assert.Equal(t, representationError(context.Background(), flagCharacter), err)
			assert.False(t, ok)
		})
	})
}

func TestSetPrologFlag(t *testing.T) {
	t.Run("bounded", func(t *testing.T) {
		var vm VM
		ctx := withVM(context.Background(), &vm)
		ok, err := SetPrologFlag(ctx, atomBounded, NewVariable()).Force()
		assert.Equal(t, permissionError(context.Background(), operationModify, permissionTypeFlag, atomBounded), err)
		assert.False(t, ok)
	})

	t.Run("max_integer", func(t *testing.T) {
		var vm VM
		ctx := withVM(context.Background(), &vm)
		ok, err := SetPrologFlag(ctx, atomMaxInteger, NewVariable()).Force()
		assert.Equal(t, permissionError(context.Background(), operationModify, permissionTypeFlag, atomMaxInteger), err)
		assert.False(t, ok)
	})

	t.Run("min_integer", func(t *testing.T) {
		var vm VM
		ctx := withVM(context.Background(), &vm)
		ok, err := SetPrologFlag(ctx, atomMinInteger, NewVariable()).Force()
		assert.Equal(t, permissionError(context.Background(), operationModify, permissionTypeFlag, atomMinInteger), err)
		assert.False(t, ok)
	})

	t.Run("integer_rounding_function", func(t *testing.T) {
		var vm VM
		ctx := withVM(context.Background(), &vm)
		ok, err := SetPrologFlag(ctx, atomIntegerRoundingFunction, NewVariable()).Force()
		assert.Equal(t, permissionError(context.Background(), operationModify, permissionTypeFlag, atomIntegerRoundingFunction), err)
		assert.False(t, ok)
	})

	t.Run("char_conversion", func(t *testing.T) {
		t.Run("on", func(t *testing.T) {
			var vm VM
			ctx := withVM(context.Background(), &vm)
			ok, err := SetPrologFlag(ctx, atomCharConversion, atomOn).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.True(t, vm.charConvEnabled)
		})

		t.Run("off", func(t *testing.T) {
			vm := VM{charConvEnabled: true}
			ctx := withVM(context.Background(), &vm)
			ok, err := SetPrologFlag(ctx, atomCharConversion, atomOff).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.False(t, vm.charConvEnabled)
		})

		t.Run("unknown", func(t *testing.T) {
			vm := VM{charConvEnabled: true}
			ctx := withVM(context.Background(), &vm)
			ok, err := SetPrologFlag(ctx, atomCharConversion, NewAtom("foo")).Force()
			assert.Error(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("debug", func(t *testing.T) {
		t.Run("on", func(t *testing.T) {
			var vm VM
			ctx := withVM(context.Background(), &vm)
			ok, err := SetPrologFlag(ctx, atomDebug, atomOn).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.True(t, vm.debug)
		})

		t.Run("off", func(t *testing.T) {
			vm := VM{debug: true}
			ctx := withVM(context.Background(), &vm)
			ok, err := SetPrologFlag(ctx, atomDebug, atomOff).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.False(t, vm.debug)
		})

		t.Run("unknown", func(t *testing.T) {
			vm := VM{debug: true}
			ctx := withVM(context.Background(), &vm)
			ok, err := SetPrologFlag(ctx, atomDebug, NewAtom("foo")).Force()
			assert.Error(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("max_arity", func(t *testing.T) {
		var vm VM
		ctx := withVM(context.Background(), &vm)
		ok, err := SetPrologFlag(ctx, atomMaxArity, NewVariable()).Force()
		assert.Equal(t, permissionError(context.Background(), operationModify, permissionTypeFlag, atomMaxArity), err)
		assert.False(t, ok)
	})

	t.Run("unknown", func(t *testing.T) {
		t.Run("error", func(t *testing.T) {
			vm := VM{unknown: unknownFail}
			ctx := withVM(context.Background(), &vm)
			ok, err := SetPrologFlag(ctx, atomUnknown, atomError).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, unknownError, vm.unknown)
		})

		t.Run("warning", func(t *testing.T) {
			var vm VM
			ctx := withVM(context.Background(), &vm)
			ok, err := SetPrologFlag(ctx, atomUnknown, atomWarning).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, unknownWarning, vm.unknown)
		})

		t.Run("fail", func(t *testing.T) {
			var vm VM
			ctx := withVM(context.Background(), &vm)
			ok, err := SetPrologFlag(ctx, atomUnknown, atomFail).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, unknownFail, vm.unknown)
		})

		t.Run("fail", func(t *testing.T) {
			var vm VM
			ctx := withVM(context.Background(), &vm)
			ok, err := SetPrologFlag(ctx, atomUnknown, NewAtom("foo")).Force()
			assert.Error(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("double_quotes", func(t *testing.T) {
		t.Run("codes", func(t *testing.T) {
			var vm VM
			ctx := withVM(context.Background(), &vm)
			ok, err := SetPrologFlag(ctx, atomDoubleQuotes, atomCodes).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, doubleQuotesCodes, vm.doubleQuotes)
		})

		t.Run("chars", func(t *testing.T) {
			var vm VM
			ctx := withVM(context.Background(), &vm)
			ok, err := SetPrologFlag(ctx, atomDoubleQuotes, atomChars).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, doubleQuotesChars, vm.doubleQuotes)
		})

		t.Run("atom", func(t *testing.T) {
			var vm VM
			ctx := withVM(context.Background(), &vm)
			ok, err := SetPrologFlag(ctx, atomDoubleQuotes, atomAtom).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, doubleQuotesAtom, vm.doubleQuotes)
		})

		t.Run("unknown", func(t *testing.T) {
			var vm VM
			ctx := withVM(context.Background(), &vm)
			ok, err := SetPrologFlag(ctx, atomDoubleQuotes, NewAtom("foo")).Force()
			assert.Error(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("flag is a variable", func(t *testing.T) {
		var vm VM
		ctx := withVM(context.Background(), &vm)
		ok, err := SetPrologFlag(ctx, NewVariable(), atomFail).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("value is a variable", func(t *testing.T) {
		var vm VM
		ctx := withVM(context.Background(), &vm)
		ok, err := SetPrologFlag(ctx, atomUnknown, NewVariable()).Force()
		assert.Equal(t, InstantiationError(context.Background()), err)
		assert.False(t, ok)
	})

	t.Run("flag is neither a variable nor an atom", func(t *testing.T) {
		var vm VM
		ctx := withVM(context.Background(), &vm)
		ok, err := SetPrologFlag(ctx, Integer(0), atomFail).Force()
		assert.Equal(t, typeError(context.Background(), validTypeAtom, Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("flag is an atom but an invalid flag for the processor", func(t *testing.T) {
		var vm VM
		ctx := withVM(context.Background(), &vm)
		ok, err := SetPrologFlag(ctx, NewAtom("foo"), atomFail).Force()
		assert.Equal(t, domainError(context.Background(), validDomainPrologFlag, NewAtom("foo")), err)
		assert.False(t, ok)
	})

	t.Run("value is inadmissible for flag", func(t *testing.T) {
		var vm VM
		ctx := withVM(context.Background(), &vm)
		ok, err := SetPrologFlag(ctx, atomUnknown, Integer(0)).Force()
		assert.Equal(t, domainError(context.Background(), validDomainFlagValue, atomPlus.Apply(atomUnknown, Integer(0))), err)
		assert.False(t, ok)
	})

	t.Run("value is admissible for flag but the flag is not modifiable", func(t *testing.T) {
		var vm VM
		ctx := withVM(context.Background(), &vm)
		ok, err := SetPrologFlag(ctx, atomBounded, atomTrue).Force()
		assert.Equal(t, permissionError(context.Background(), operationModify, permissionTypeFlag, atomBounded), err)
		assert.False(t, ok)
	})
}

func TestCurrentPrologFlag(t *testing.T) {
	var vm VM
	ctx := withVM(context.Background(), &vm)

	t.Run("specified", func(t *testing.T) {
		ok, err := CurrentPrologFlag(ctx, atomBounded, atomTrue).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = CurrentPrologFlag(ctx, atomMaxInteger, Integer(math.MaxInt64)).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = CurrentPrologFlag(ctx, atomMinInteger, Integer(math.MinInt64)).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = CurrentPrologFlag(ctx, atomIntegerRoundingFunction, atomTowardZero).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = CurrentPrologFlag(ctx, atomCharConversion, atomOff).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = CurrentPrologFlag(ctx, atomDebug, atomOff).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = CurrentPrologFlag(ctx, atomMaxArity, atomUnbounded).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = CurrentPrologFlag(ctx, atomUnknown, atomError).Force()
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not specified", func(t *testing.T) {
		flag, value := NewVariable(), NewVariable()
		var c int
		ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
			switch c {
			case 0:
				assert.Equal(t, atomBounded, Resolve(ctx, flag))
				assert.Equal(t, atomTrue, Resolve(ctx, value))
			case 1:
				assert.Equal(t, atomMaxInteger, Resolve(ctx, flag))
				assert.Equal(t, Integer(math.MaxInt64), Resolve(ctx, value))
			case 2:
				assert.Equal(t, atomMinInteger, Resolve(ctx, flag))
				assert.Equal(t, Integer(math.MinInt64), Resolve(ctx, value))
			case 3:
				assert.Equal(t, atomIntegerRoundingFunction, Resolve(ctx, flag))
				assert.Equal(t, atomTowardZero, Resolve(ctx, value))
			case 4:
				assert.Equal(t, atomCharConversion, Resolve(ctx, flag))
				assert.Equal(t, atomOff, Resolve(ctx, value))
			case 5:
				assert.Equal(t, atomDebug, Resolve(ctx, flag))
				assert.Equal(t, atomOff, Resolve(ctx, value))
			case 6:
				assert.Equal(t, atomMaxArity, Resolve(ctx, flag))
				assert.Equal(t, atomUnbounded, Resolve(ctx, value))
			case 7:
				assert.Equal(t, atomUnknown, Resolve(ctx, flag))
				assert.Equal(t, NewAtom(vm.unknown.String()), Resolve(ctx, value))
			case 8:
				assert.Equal(t, atomDoubleQuotes, Resolve(ctx, flag))
				assert.Equal(t, NewAtom(vm.doubleQuotes.String()), Resolve(ctx, value))
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return Bool(false)
		})
		ok, err := CurrentPrologFlag(ctx, flag, value).Force()
		assert.NoError(t, err)
		assert.False(t, ok)
		assert.Equal(t, 9, c)
	})

	t.Run("flag is neither a variable nor an atom", func(t *testing.T) {
		ok, err := CurrentPrologFlag(context.Background(), Integer(0), atomError).Force()
		assert.Equal(t, typeError(context.Background(), validTypeAtom, Integer(0)), err)
		assert.False(t, ok)
	})

	t.Run("flag is an atom but an invalid flag for the processor", func(t *testing.T) {
		ok, err := CurrentPrologFlag(context.Background(), NewAtom("foo"), atomError).Force()
		assert.Equal(t, domainError(context.Background(), validDomainPrologFlag, NewAtom("foo")), err)
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
	ctx := withVM(context.Background(), &vm)

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
			ok, err := ExpandTerm(ctx, tt.in, tt.out()).Force()
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
			ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
				results = append(results, pair.Apply(Resolve(ctx, n), Resolve(ctx, elem)))
				return Bool(false)
			})
			ok, err := Nth0(ctx, n, List(NewAtom("a"), NewAtom("b"), NewAtom("c")), elem).Force()
			assert.NoError(t, err)
			assert.False(t, ok)

			assert.Equal(t, []Term{
				pair.Apply(Integer(0), NewAtom("a")),
				pair.Apply(Integer(1), NewAtom("b")),
				pair.Apply(Integer(2), NewAtom("c")),
			}, results)
		})

		t.Run("list is an improper list", func(t *testing.T) {
			ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
				return Bool(false)
			})
			_, err := Nth0(ctx, NewVariable(), PartialList(NewVariable(), NewAtom("a")), NewVariable()).Force()
			assert.Equal(t, InstantiationError(context.Background()), err)
		})
	})

	t.Run("n is an integer", func(t *testing.T) {
		t.Run("list is a proper list", func(t *testing.T) {
			t.Run("n is a valid index", func(t *testing.T) {
				ok, err := Nth0(context.Background(), Integer(1), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewAtom("b")).Force()
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("n is too small for an index", func(t *testing.T) {
				ok, err := Nth0(context.Background(), Integer(-1), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewVariable()).Force()
				assert.NoError(t, err)
				assert.False(t, ok)
			})

			t.Run("n is too big for an index", func(t *testing.T) {
				ok, err := Nth0(context.Background(), Integer(3), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewVariable()).Force()
				assert.NoError(t, err)
				assert.False(t, ok)
			})
		})

		t.Run("list is an improper list", func(t *testing.T) {
			_, err := Nth0(context.Background(), Integer(1), PartialList(NewVariable(), NewAtom("a")), NewVariable()).Force()
			assert.Equal(t, InstantiationError(context.Background()), err)
		})
	})

	t.Run("n is neither a variable nor an integer", func(t *testing.T) {
		_, err := Nth0(context.Background(), NewAtom("foo"), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewVariable()).Force()
		assert.Equal(t, typeError(context.Background(), validTypeInteger, NewAtom("foo")), err)
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
			ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
				results = append(results, pair.Apply(Resolve(ctx, n), Resolve(ctx, elem)))
				return Bool(false)
			})
			ok, err := Nth1(ctx, n, List(NewAtom("a"), NewAtom("b"), NewAtom("c")), elem).Force()
			assert.NoError(t, err)
			assert.False(t, ok)

			assert.Equal(t, []Term{
				pair.Apply(Integer(1), NewAtom("a")),
				pair.Apply(Integer(2), NewAtom("b")),
				pair.Apply(Integer(3), NewAtom("c")),
			}, results)
		})

		t.Run("list is an improper list", func(t *testing.T) {
			ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
				return Bool(false)
			})
			_, err := Nth1(ctx, NewVariable(), PartialList(NewVariable(), NewAtom("a")), NewVariable()).Force()
			assert.Equal(t, InstantiationError(context.Background()), err)
		})
	})

	t.Run("n is an integer", func(t *testing.T) {
		t.Run("list is a proper list", func(t *testing.T) {
			t.Run("n is a valid index", func(t *testing.T) {
				ok, err := Nth1(context.Background(), Integer(2), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewAtom("b")).Force()
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("n is too small for an index", func(t *testing.T) {
				ok, err := Nth1(context.Background(), Integer(0), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewVariable()).Force()
				assert.NoError(t, err)
				assert.False(t, ok)
			})

			t.Run("n is too big for an index", func(t *testing.T) {
				ok, err := Nth1(context.Background(), Integer(4), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewVariable()).Force()
				assert.NoError(t, err)
				assert.False(t, ok)
			})
		})

		t.Run("list is an improper list", func(t *testing.T) {
			_, err := Nth1(context.Background(), Integer(2), PartialList(NewVariable(), NewAtom("a")), NewVariable()).Force()
			assert.Equal(t, InstantiationError(context.Background()), err)
		})
	})

	t.Run("n is neither a variable nor an integer", func(t *testing.T) {
		_, err := Nth1(context.Background(), NewAtom("foo"), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewVariable()).Force()
		assert.Equal(t, typeError(context.Background(), validTypeInteger, NewAtom("foo")), err)
	})
}

func TestSucc(t *testing.T) {
	t.Run("x is a variable", func(t *testing.T) {
		t.Run("s is a variable", func(t *testing.T) {
			_, err := Succ(context.Background(), NewVariable(), NewVariable()).Force()
			assert.Equal(t, InstantiationError(context.Background()), err)
		})

		t.Run("s is an integer", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				x := NewVariable()
				ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
					assert.Equal(t, Integer(0), Resolve(ctx, x))
					return Bool(true)
				})
				ok, err := Succ(ctx, x, Integer(1)).Force()
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("s < 0", func(t *testing.T) {
				_, err := Succ(context.Background(), NewVariable(), Integer(-1)).Force()
				assert.Equal(t, domainError(context.Background(), validDomainNotLessThanZero, Integer(-1)), err)
			})

			t.Run("s = 0", func(t *testing.T) {
				ok, err := Succ(context.Background(), NewVariable(), Integer(0)).Force()
				assert.NoError(t, err)
				assert.False(t, ok)
			})
		})

		t.Run("s is neither a variable nor an integer", func(t *testing.T) {
			_, err := Succ(context.Background(), NewVariable(), Float(1)).Force()
			assert.Equal(t, typeError(context.Background(), validTypeInteger, Float(1)), err)
		})
	})

	t.Run("x is an integer", func(t *testing.T) {
		t.Run("s is a variable", func(t *testing.T) {
			s := NewVariable()
			ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
				assert.Equal(t, Integer(1), Resolve(ctx, s))
				return Bool(true)
			})
			ok, err := Succ(ctx, Integer(0), s).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("s is an integer", func(t *testing.T) {
			ok, err := Succ(context.Background(), Integer(0), Integer(1)).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("s is neither a variable nor an integer", func(t *testing.T) {
			_, err := Succ(context.Background(), Integer(0), Float(1)).Force()
			assert.Equal(t, typeError(context.Background(), validTypeInteger, Float(1)), err)
		})

		t.Run("x is negative", func(t *testing.T) {
			_, err := Succ(context.Background(), Integer(-1), Integer(0)).Force()
			assert.Equal(t, domainError(context.Background(), validDomainNotLessThanZero, Integer(-1)), err)
		})

		t.Run("x is math.MaxInt64", func(t *testing.T) {
			_, err := Succ(context.Background(), Integer(math.MaxInt64), Integer(0)).Force()
			assert.Equal(t, evaluationError(context.Background(), exceptionalValueIntOverflow), err)
		})

		t.Run("s is negative", func(t *testing.T) {
			_, err := Succ(context.Background(), Integer(0), Integer(-1)).Force()
			assert.Equal(t, domainError(context.Background(), validDomainNotLessThanZero, Integer(-1)), err)
		})
	})

	t.Run("x is neither a variable nor an integer", func(t *testing.T) {
		_, err := Succ(context.Background(), Float(0), NewVariable()).Force()
		assert.Equal(t, typeError(context.Background(), validTypeInteger, Float(0)), err)
	})
}

func TestLength(t *testing.T) {
	t.Run("list is a list", func(t *testing.T) {
		t.Run("length is a variable", func(t *testing.T) {
			n := NewVariable()
			ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
				assert.Equal(t, Integer(3), Resolve(ctx, n))
				return Bool(true)
			})
			ok, err := Length(ctx, List(NewAtom("a"), NewAtom("b"), NewAtom("c")), n).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("length is an integer", func(t *testing.T) {
			t.Run("length is the exact length of list", func(t *testing.T) {
				ok, err := Length(context.Background(), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), Integer(3)).Force()
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("length is smaller than the length fo list", func(t *testing.T) {
				ok, err := Length(context.Background(), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), Integer(2)).Force()
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
				ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {

					var ret []Variable
					iter := ListIterator{List: l}
					for iter.Next(ctx) {
						ret = append(ret, Resolve(ctx, iter.Current()).(Variable))
					}
					assert.NoError(t, iter.Err())

					switch count {
					case 0:
						assert.Len(t, ret, 0)
						assert.Equal(t, Integer(2), Resolve(ctx, n))
					case 1:
						assert.Len(t, ret, 1)
						assert.Equal(t, Integer(3), Resolve(ctx, n))
					case 2:
						assert.Len(t, ret, 2)
						assert.Equal(t, Integer(4), Resolve(ctx, n))
					default:
						return Bool(true)
					}

					count++
					return Bool(false)
				})
				ok, err := Length(ctx, PartialList(l, NewAtom("a"), NewAtom("b")), n).Force()
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("length and the suffix of list are the same", func(t *testing.T) {
				l := NewVariable()
				_, err := Length(context.Background(), PartialList(l, NewAtom("a"), NewAtom("b")), l).Force()
				assert.Equal(t, resourceError(context.Background(), resourceFiniteMemory), err)
			})
		})

		t.Run("length is an integer", func(t *testing.T) {
			t.Run("small", func(t *testing.T) {
				l := NewVariable()
				ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
					iter := ListIterator{List: l}
					assert.True(t, iter.Next(ctx))
					assert.False(t, iter.Next(ctx))
					return Bool(true)
				})
				ok, err := Length(ctx, PartialList(l, NewAtom("a"), NewAtom("b")), Integer(3)).Force()
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("large", func(t *testing.T) {
				l := NewVariable()
				_, err := Length(context.Background(), PartialList(l, NewAtom("a"), NewAtom("b")), Integer(math.MaxInt64)).Force()
				assert.Equal(t, resourceError(context.Background(), resourceMemory), err)
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
				_, err := Length(context.Background(), PartialList(l, NewAtom("a"), NewAtom("b")), Integer(100*1024*1024)).Force()
				assert.Equal(t, resourceError(context.Background(), resourceMemory), err)
			})
		})
	})

	t.Run("list is neither a list nor a partial list", func(t *testing.T) {
		t.Run("the suffix is an atom", func(t *testing.T) {
			ok, err := Length(context.Background(), NewAtom("foo"), Integer(3)).Force()
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("the suffix is a compound", func(t *testing.T) {
			ok, err := Length(context.Background(), NewAtom("foo").Apply(NewAtom("bar")), Integer(3)).Force()
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("the suffix is neither an atom nor a compound", func(t *testing.T) {
			ok, err := Length(context.Background(), Integer(0), Integer(3)).Force()
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("length is neither a variable nor an integer", func(t *testing.T) {
		_, err := Length(context.Background(), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewAtom("three")).Force()
		assert.Equal(t, typeError(context.Background(), validTypeInteger, NewAtom("three")), err)
	})

	t.Run("length is an integer that is less than zero", func(t *testing.T) {
		_, err := Length(context.Background(), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), Integer(-3)).Force()
		assert.Equal(t, domainError(context.Background(), validDomainNotLessThanZero, Integer(-3)), err)
	})

	t.Run("list is so long that an integer cannot represent its length", func(t *testing.T) {
		maxInt = 2
		defer func() {
			maxInt = math.MaxInt64
		}()

		t.Run("list is a list", func(t *testing.T) {
			_, err := Length(context.Background(), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), NewVariable()).Force()
			assert.Equal(t, resourceError(context.Background(), resourceFiniteMemory), err)
		})

		t.Run("list is a partial list", func(t *testing.T) {
			ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
				return Bool(false)
			})
			_, err := Length(ctx, NewVariable(), NewVariable()).Force()
			assert.Equal(t, representationError(context.Background(), flagMaxInteger), err)
		})
	})
}

func TestSkipMaxList(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		t.Run("without max", func(t *testing.T) {
			ok, err := SkipMaxList(context.Background(), Integer(3), NewVariable(), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), atomEmptyList).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("with max", func(t *testing.T) {
			ok, err := SkipMaxList(context.Background(), Integer(2), Integer(2), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), List(NewAtom("c"))).Force()
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("max is neither a variable nor an integer", func(t *testing.T) {
		_, err := SkipMaxList(context.Background(), Integer(3), NewAtom("foo"), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), atomEmptyList).Force()
		assert.Equal(t, typeError(context.Background(), validTypeInteger, NewAtom("foo")), err)
	})

	t.Run("max is negative", func(t *testing.T) {
		_, err := SkipMaxList(context.Background(), Integer(3), Integer(-1), List(NewAtom("a"), NewAtom("b"), NewAtom("c")), atomEmptyList).Force()
		assert.Equal(t, domainError(context.Background(), validDomainNotLessThanZero, Integer(-1)), err)
	})
}

func TestRepeat(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	c := 0
	ctx = WithCont(ctx, func(ctx context.Context) *Promise {
		c++
		cancel()
		return Bool(true)
	})
	_, err := Repeat(ctx).Force()
	assert.Equal(t, context.Canceled, err)

	assert.Equal(t, 1, c)
}

func TestNegation(t *testing.T) {
	e := errors.New("failed")

	var vm VM
	vm.Register0(atomTrue, func(ctx context.Context) *Promise {
		return Continue(ctx)
	})
	vm.Register0(atomFalse, func(context.Context) *Promise {
		return Bool(false)
	})
	vm.Register0(atomError, func(context.Context) *Promise {
		return Error(e)
	})
	ctx := withVM(context.Background(), &vm)

	ok, err := Negate(ctx, atomTrue).Force()
	assert.NoError(t, err)
	assert.False(t, ok)

	ok, err = Negate(ctx, atomFalse).Force()
	assert.NoError(t, err)
	assert.True(t, ok)

	_, err = Negate(ctx, atomError).Force()
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
			ctx := WithCont(context.Background(), func(ctx context.Context) *Promise {
				env := env(ctx)
				for k, v := range tt.env[0] {
					_, ok := env.Unify(k, v)
					assert.True(t, ok)
				}
				tt.env = tt.env[1:]
				return Bool(len(tt.env) == 0)
			})
			ok, err := Append(ctx, tt.xs, tt.ys, tt.zs).Force()
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

func setMemFree(n int64) func() {
	if n <= 0 {
		return func() {}
	}

	orig := memFree
	memFree = func() int64 {
		return n
	}
	return func() {
		memFree = orig
	}
}
