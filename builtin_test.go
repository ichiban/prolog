package prolog

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"github.com/ichiban/prolog/internal"
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
	var vm internal.VM
	m := vm.TypeInModule()
	m.SetPredicate0("fail", func(_ *internal.VM, f internal.Cont, env *internal.Env) *internal.Promise {
		return internal.Bool(false)
	})
	m.SetPredicate0("do_not_call", func(*internal.VM, internal.Cont, *internal.Env) *internal.Promise {
		panic("told you")
	})
	m.SetPredicate0("lazy_do_not_call", func(*internal.VM, internal.Cont, *internal.Env) *internal.Promise {
		return internal.Delay(func(context.Context) *internal.Promise {
			panic("told you")
		})
	})
	_, err := vm.Compile(context.Background(), `
foo.
foo(_, _).
f(g([a, [b, c|X]])).
`)
	assert.NoError(t, err)

	tests := []struct {
		title string
		goal  Term
		ok    bool
		err   error

		mem int64
	}{
		// TODO: redo test cases based on 7.8.3.4 Examples
		{title: `undefined atom`, goal: internal.NewAtom("bar"), ok: false, err: existenceError(objectTypeProcedure, atomSlash.Apply(internal.NewAtom("bar"), Integer(0)), nil)},
		{title: `defined atom`, goal: internal.NewAtom("foo"), ok: true},
		{title: `undefined compound`, goal: internal.NewAtom("bar").Apply(internal.NewVariable(), internal.NewVariable()), ok: false, err: existenceError(objectTypeProcedure, atomSlash.Apply(internal.NewAtom("bar"), Integer(2)), nil)},
		{title: `defined compound`, goal: internal.NewAtom("foo").Apply(internal.NewVariable(), internal.NewVariable()), ok: true},
		{title: `variable: single predicate`, goal: internal.NewVariable(), ok: false, err: InstantiationError(nil)},
		{title: `variable: multiple predicates`, goal: atomComma.Apply(atomFail, internal.NewVariable()), ok: false},
		{title: `not callable: single predicate`, goal: Integer(0), ok: false, err: typeError(validTypeCallable, Integer(0), nil)},
		{title: `not callable: conjunction`, goal: atomComma.Apply(atomTrue, Integer(0)), ok: false, err: typeError(validTypeCallable, atomComma.Apply(atomTrue, Integer(0)), nil)},
		{title: `not callable: disjunction`, goal: atomSemiColon.Apply(Integer(1), atomTrue), ok: false, err: typeError(validTypeCallable, atomSemiColon.Apply(Integer(1), atomTrue), nil)},

		{title: `cover all`, goal: atomComma.Apply(atomCut, internal.NewAtom("f").Apply(internal.NewAtom("g").Apply(List(internal.NewAtom("a"), PartialList(internal.NewVariable(), internal.NewAtom("b"), internal.NewAtom("c")))))), ok: true},
		{title: `out of memory`, goal: internal.NewAtom("foo").Apply(internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable()), err: resourceError(resourceMemory, nil), mem: 1},
		{title: `panic`, goal: internal.NewAtom("do_not_call"), err: errors.New("panic: told you")},
		{title: `panic (lazy)`, goal: internal.NewAtom("lazy_do_not_call"), err: errors.New("panic: told you")},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			defer setMemFree(tt.mem)()

			ok, err := Call(&vm, tt.goal, Success, nil).Force(context.Background())
			assert.Equal(t, tt.err, err)
			assert.Equal(t, tt.ok, ok)
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
		{title: "ok", closure: internal.NewAtom("p").Apply(internal.NewAtom("a")), additional: [1]Term{internal.NewAtom("b")}, ok: true},
		{title: "closure is a variable", closure: internal.NewVariable(), additional: [1]Term{internal.NewAtom("b")}, err: InstantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [1]Term{internal.NewAtom("b")}, err: typeError(validTypeCallable, Integer(3), nil)},
		{title: "out of memory", closure: internal.NewAtom("p").Apply(internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a")), additional: [1]Term{internal.NewAtom("b")}, err: resourceError(resourceMemory, nil), mem: 1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			defer setMemFree(tt.mem)()

			vm := internal.VM{
				typeIn: atomUser,
				modules: map[internal.Atom]*internal.module{
					atomUser: {
						procedures: map[predicateIndicator]internal.procedureEntry{
							{name: internal.NewAtom("p"), arity: 2}: {procedure: Predicate2(func(_ *internal.VM, _, _ Term, k internal.Cont, env *internal.Env) *internal.Promise {
								return k(env)
							})},
						},
					},
				},
			}
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
		{title: "ok", closure: internal.NewAtom("p").Apply(internal.NewAtom("a")), additional: [2]Term{internal.NewAtom("b"), internal.NewAtom("c")}, ok: true},
		{title: "closure is a variable", closure: internal.NewVariable(), additional: [2]Term{internal.NewAtom("b"), internal.NewAtom("c")}, err: InstantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [2]Term{internal.NewAtom("b"), internal.NewAtom("c")}, err: typeError(validTypeCallable, Integer(3), nil)},
		{title: "out of memory", closure: internal.NewAtom("p").Apply(internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a")), additional: [2]Term{internal.NewAtom("b"), internal.NewAtom("c")}, err: resourceError(resourceMemory, nil), mem: 1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			defer setMemFree(tt.mem)()

			vm := internal.VM{
				typeIn: atomUser,
				modules: map[internal.Atom]*internal.module{
					atomUser: {
						procedures: map[predicateIndicator]internal.procedureEntry{
							{name: internal.NewAtom("p"), arity: 3}: {procedure: Predicate3(func(_ *internal.VM, _, _, _ Term, k internal.Cont, env *internal.Env) *internal.Promise {
								return k(env)
							})},
						},
					},
				},
			}
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
		{title: "ok", closure: internal.NewAtom("p").Apply(internal.NewAtom("a")), additional: [3]Term{internal.NewAtom("b"), internal.NewAtom("c"), internal.NewAtom("d")}, ok: true},
		{title: "closure is a variable", closure: internal.NewVariable(), additional: [3]Term{internal.NewAtom("b"), internal.NewAtom("c"), internal.NewAtom("d")}, err: InstantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [3]Term{internal.NewAtom("b"), internal.NewAtom("c"), internal.NewAtom("d")}, err: typeError(validTypeCallable, Integer(3), nil)},
		{title: "out of memory", closure: internal.NewAtom("p").Apply(internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a")), additional: [3]Term{internal.NewAtom("b"), internal.NewAtom("c"), internal.NewAtom("d")}, err: resourceError(resourceMemory, nil), mem: 1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			defer setMemFree(tt.mem)()

			vm := internal.VM{
				typeIn: atomUser,
				modules: map[internal.Atom]*internal.module{
					atomUser: {
						procedures: map[predicateIndicator]internal.procedureEntry{
							{name: internal.NewAtom("p"), arity: 4}: {procedure: Predicate4(func(_ *internal.VM, _, _, _, _ Term, k internal.Cont, env *internal.Env) *internal.Promise {
								return k(env)
							})},
						},
					},
				},
			}
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
		{title: "ok", closure: internal.NewAtom("p").Apply(internal.NewAtom("a")), additional: [4]Term{internal.NewAtom("b"), internal.NewAtom("c"), internal.NewAtom("d"), internal.NewAtom("e")}, ok: true},
		{title: "closure is a variable", closure: internal.NewVariable(), additional: [4]Term{internal.NewAtom("b"), internal.NewAtom("c"), internal.NewAtom("d"), internal.NewAtom("e")}, err: InstantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [4]Term{internal.NewAtom("b"), internal.NewAtom("c"), internal.NewAtom("d"), internal.NewAtom("e")}, err: typeError(validTypeCallable, Integer(3), nil)},
		{title: "out of memory", closure: internal.NewAtom("p").Apply(internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a")), additional: [4]Term{internal.NewAtom("b"), internal.NewAtom("c"), internal.NewAtom("d"), internal.NewAtom("e")}, err: resourceError(resourceMemory, nil), mem: 1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			defer setMemFree(tt.mem)()

			vm := internal.VM{
				typeIn: atomUser,
				modules: map[internal.Atom]*internal.module{
					atomUser: {
						procedures: map[predicateIndicator]internal.procedureEntry{
							{name: internal.NewAtom("p"), arity: 5}: {procedure: Predicate5(func(_ *internal.VM, _, _, _, _, _ Term, k internal.Cont, env *internal.Env) *internal.Promise {
								return k(env)
							})},
						},
					},
				},
			}
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
		{title: "ok", closure: internal.NewAtom("p").Apply(internal.NewAtom("a")), additional: [5]Term{internal.NewAtom("b"), internal.NewAtom("c"), internal.NewAtom("d"), internal.NewAtom("e"), internal.NewAtom("f")}, ok: true},
		{title: "closure is a variable", closure: internal.NewVariable(), additional: [5]Term{internal.NewAtom("b"), internal.NewAtom("c"), internal.NewAtom("d"), internal.NewAtom("e"), internal.NewAtom("f")}, err: InstantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [5]Term{internal.NewAtom("b"), internal.NewAtom("c"), internal.NewAtom("d"), internal.NewAtom("e"), internal.NewAtom("f")}, err: typeError(validTypeCallable, Integer(3), nil)},
		{title: "out of memory", closure: internal.NewAtom("p").Apply(internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a")), additional: [5]Term{internal.NewAtom("b"), internal.NewAtom("c"), internal.NewAtom("d"), internal.NewAtom("e"), internal.NewAtom("f")}, err: resourceError(resourceMemory, nil), mem: 1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			defer setMemFree(tt.mem)()

			vm := internal.VM{
				typeIn: atomUser,
				modules: map[internal.Atom]*internal.module{
					atomUser: {
						procedures: map[predicateIndicator]internal.procedureEntry{
							{name: internal.NewAtom("p"), arity: 6}: {procedure: Predicate6(func(_ *internal.VM, _, _, _, _, _, _ Term, k internal.Cont, env *internal.Env) *internal.Promise {
								return k(env)
							})},
						},
					},
				},
			}
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
		{title: "ok", closure: internal.NewAtom("p").Apply(internal.NewAtom("a")), additional: [6]Term{internal.NewAtom("b"), internal.NewAtom("c"), internal.NewAtom("d"), internal.NewAtom("e"), internal.NewAtom("f"), internal.NewAtom("g")}, ok: true},
		{title: "closure is a variable", closure: internal.NewVariable(), additional: [6]Term{internal.NewAtom("b"), internal.NewAtom("c"), internal.NewAtom("d"), internal.NewAtom("e"), internal.NewAtom("f"), internal.NewAtom("g")}, err: InstantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [6]Term{internal.NewAtom("b"), internal.NewAtom("c"), internal.NewAtom("d"), internal.NewAtom("e"), internal.NewAtom("f"), internal.NewAtom("g")}, err: typeError(validTypeCallable, Integer(3), nil)},
		{title: "out of memory", closure: internal.NewAtom("p").Apply(internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a")), additional: [6]Term{internal.NewAtom("b"), internal.NewAtom("c"), internal.NewAtom("d"), internal.NewAtom("e"), internal.NewAtom("f"), internal.NewAtom("g")}, err: resourceError(resourceMemory, nil), mem: 1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			defer setMemFree(tt.mem)()

			vm := internal.VM{
				typeIn: atomUser,
				modules: map[internal.Atom]*internal.module{
					atomUser: {
						procedures: map[predicateIndicator]internal.procedureEntry{
							{name: internal.NewAtom("p"), arity: 7}: {procedure: Predicate7(func(_ *internal.VM, _, _, _, _, _, _, _ Term, k internal.Cont, env *internal.Env) *internal.Promise {
								return k(env)
							})},
						},
					},
				},
			}
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
		{title: "ok", closure: internal.NewAtom("p").Apply(internal.NewAtom("a")), additional: [7]Term{internal.NewAtom("b"), internal.NewAtom("c"), internal.NewAtom("d"), internal.NewAtom("e"), internal.NewAtom("f"), internal.NewAtom("g"), internal.NewAtom("h")}, ok: true},
		{title: "closure is a variable", closure: internal.NewVariable(), additional: [7]Term{internal.NewAtom("b"), internal.NewAtom("c"), internal.NewAtom("d"), internal.NewAtom("e"), internal.NewAtom("f"), internal.NewAtom("g"), internal.NewAtom("h")}, err: InstantiationError(nil)},
		{title: "closure is neither a variable nor a callable term", closure: Integer(3), additional: [7]Term{internal.NewAtom("b"), internal.NewAtom("c"), internal.NewAtom("d"), internal.NewAtom("e"), internal.NewAtom("f"), internal.NewAtom("g"), internal.NewAtom("h")}, err: typeError(validTypeCallable, Integer(3), nil)},
		{title: "out of memory", closure: internal.NewAtom("p").Apply(internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a"), internal.NewAtom("a")), additional: [7]Term{internal.NewAtom("b"), internal.NewAtom("c"), internal.NewAtom("d"), internal.NewAtom("e"), internal.NewAtom("f"), internal.NewAtom("g"), internal.NewAtom("h")}, err: resourceError(resourceMemory, nil), mem: 1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			defer setMemFree(tt.mem)()

			vm := internal.VM{
				typeIn: atomUser,
				modules: map[internal.Atom]*internal.module{
					atomUser: {
						procedures: map[predicateIndicator]internal.procedureEntry{
							{name: internal.NewAtom("p"), arity: 8}: {procedure: Predicate8(func(_ *internal.VM, _, _, _, _, _, _, _, _ Term, k internal.Cont, env *internal.Env) *internal.Promise {
								return k(env)
							})},
						},
					},
				},
			}
			ok, err := Call7(&vm, tt.closure, tt.additional[0], tt.additional[1], tt.additional[2], tt.additional[3], tt.additional[4], tt.additional[5], tt.additional[6], Success, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestCallNth(t *testing.T) {
	vm := internal.VM{
		typeIn: atomUser,
		modules: map[internal.Atom]*internal.module{
			atomUser: {
				procedures: map[predicateIndicator]internal.procedureEntry{
					{name: internal.NewAtom("foo"), arity: 0}: {procedure: Predicate0(func(_ *internal.VM, k internal.Cont, env *internal.Env) *internal.Promise {
						return internal.Delay(func(context.Context) *internal.Promise {
							return k(env)
						}, func(context.Context) *internal.Promise {
							return k(env)
						}, func(context.Context) *internal.Promise {
							return internal.Error(errors.New("three"))
						})
					})},
				},
			},
		},
	}

	t.Run("ok", func(t *testing.T) {
		t.Run("nth is a variable", func(t *testing.T) {
			nth := internal.NewVariable()

			var ns []Integer
			ok, err := CallNth(&vm, internal.NewAtom("foo"), nth, func(env *internal.Env) *internal.Promise {
				n, ok := env.Resolve(nth).(Integer)
				assert.True(t, ok)
				ns = append(ns, n)
				switch n {
				case Integer(1):
					return internal.Bool(false)
				case Integer(2):
					return internal.Bool(true)
				default:
					return internal.Error(errors.New("unreachable"))
				}
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)

			assert.Equal(t, []Integer{1, 2}, ns)
		})

		t.Run("nth is an integer", func(t *testing.T) {
			ok, err := CallNth(&vm, internal.NewAtom("foo"), Integer(2), Failure, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("nth is 0", func(t *testing.T) {
		ok, err := CallNth(&vm, internal.NewAtom("foo"), Integer(0), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("goal is a variable and nth is not zero", func(t *testing.T) {
		_, err := CallNth(&vm, internal.NewVariable(), Integer(3), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
	})

	t.Run("goal is neither a variable nor a callable term", func(t *testing.T) {
		_, err := CallNth(&vm, Integer(0), Integer(3), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeCallable, Integer(0), nil), err)
	})

	t.Run("nth is neither a variable nor an integer", func(t *testing.T) {
		_, err := CallNth(&vm, internal.NewAtom("foo"), internal.NewAtom("bar"), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, internal.NewAtom("bar"), nil), err)
	})

	t.Run("nth is an integer which is less than zero", func(t *testing.T) {
		_, err := CallNth(&vm, internal.NewAtom("foo"), Integer(-1), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainNotLessThanZero, Integer(-1), nil), err)
	})

	t.Run("n+1 is larger than max_integer", func(t *testing.T) {
		internal.maxInt = 0
		defer func() {
			internal.maxInt = math.MaxInt64
		}()
		_, err := CallNth(&vm, internal.NewAtom("foo"), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, representationError(flagMaxInteger, nil), err)
	})
}

func TestUnify(t *testing.T) {
	x, y := internal.NewVariable(), internal.NewVariable()
	tests := []struct {
		title   string
		premise *internal.Env
		x, y    Term
		ok      bool
		err     error
		env     map[internal.Variable]Term
	}{
		// 8.2.1.4 Examples
		{title: `'='(1, 1).`, x: Integer(1), y: Integer(1), ok: true},
		{title: `'='(X, 1).`, x: x, y: Integer(1), ok: true, env: map[internal.Variable]Term{
			x: Integer(1),
		}},
		{title: `'='(X, Y).`, x: x, y: y, ok: true, env: map[internal.Variable]Term{
			x: y,
		}},
		{title: `'='(_, _).`, x: internal.NewVariable(), y: internal.NewVariable(), ok: true},
		{title: `'='(X, Y), '='(X, abc).`, premise: NewEnv().bind(x, y), x: x, y: internal.NewAtom("abc"), ok: true, env: map[internal.Variable]Term{
			x: internal.NewAtom("abc"),
			y: internal.NewAtom("abc"),
		}},
		{title: `'='(f(X, def), f(def, Y)).`, x: internal.NewAtom("f").Apply(x, internal.NewAtom("def")), y: internal.NewAtom("f").Apply(internal.NewAtom("def"), y), ok: true, env: map[internal.Variable]Term{
			x: internal.NewAtom("def"),
			y: internal.NewAtom("def"),
		}},
		{title: `'='(1, 2).`, x: Integer(1), y: Integer(2), ok: false},
		{title: `'='(1, 1.0).`, x: Integer(1), y: Float(1), ok: false},
		{title: `'='(g(X), f(f(X))).`, x: internal.NewAtom("g").Apply(x), y: internal.NewAtom("f").Apply(internal.NewAtom("f").Apply(x)), ok: false},
		{title: `'='(f(X, 1), f(a(X))).`, x: internal.NewAtom("f").Apply(x, Integer(1)), y: internal.NewAtom("f").Apply(internal.NewAtom("a").Apply(x)), ok: false},
		{title: `'='(f(X, Y, X), f(a(X), a(Y), Y, 2)).`, x: internal.NewAtom("f").Apply(x, y, x), y: internal.NewAtom("f").Apply(internal.NewAtom("a").Apply(x), internal.NewAtom("a").Apply(y), y, Integer(2)), ok: false},
		{title: `'='(X, a(X)).`, x: x, y: internal.NewAtom("a").Apply(x), ok: true},
		{title: `'='(f(X, 1), f(a(X), 2)).`, x: internal.NewAtom("f").Apply(x, Integer(1)), y: internal.NewAtom("f").Apply(internal.NewAtom("a").Apply(x), Integer(2)), ok: false},
		{title: `'='(f(1, X, 1), f(2, a(X), 2)).`, x: internal.NewAtom("f").Apply(Integer(1), x, Integer(1)), y: internal.NewAtom("f").Apply(Integer(2), internal.NewAtom("a").Apply(x), Integer(2)), ok: false},
		{title: `'='(f(1, X), f(2, a(X))).`, x: internal.NewAtom("f").Apply(Integer(1), x), y: internal.NewAtom("f").Apply(Integer(2), internal.NewAtom("a").Apply(x)), ok: false},
		// {title: `'='(f(X, Y, X, 1), f(a(X), a(Y), Y, 2)).`, x: NewAtom("f").Apply(x, y, x, Integer(1)), y: NewAtom("f").Apply(NewAtom("a").Apply(x), NewAtom("a").Apply(y), y, Integer(2)), ok: false},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := Unify(nil, tt.x, tt.y, func(env *internal.Env) *internal.Promise {
				for k, v := range tt.env {
					_, ok := env.Unify(k, v)
					assert.True(t, ok)
				}
				return internal.Bool(true)
			}, tt.premise).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestUnifyWithOccursCheck(t *testing.T) {
	x, y := internal.NewVariable(), internal.NewVariable()
	tests := []struct {
		title   string
		premise *internal.Env
		x, y    Term
		ok      bool
		err     error
		env     map[internal.Variable]Term
	}{
		// 8.2.2.4 Examples
		{title: `unify_with_occurs_check(1, 1).`, x: Integer(1), y: Integer(1), ok: true},
		{title: `unify_with_occurs_check(X, 1).`, x: x, y: Integer(1), ok: true, env: map[internal.Variable]Term{
			x: Integer(1),
		}},
		{title: `unify_with_occurs_check(X, Y).`, x: x, y: y, ok: true, env: map[internal.Variable]Term{
			x: y,
		}},
		{title: `unify_with_occurs_check(_, _).`, x: internal.NewVariable(), y: internal.NewVariable(), ok: true},
		{title: `unify_with_occurs_check(X, Y), unify_with_occurs_check(X, abc).`, premise: NewEnv().bind(x, y), x: x, y: internal.NewAtom("abc"), ok: true, env: map[internal.Variable]Term{
			x: internal.NewAtom("abc"),
			y: internal.NewAtom("abc"),
		}},
		{title: `unify_with_occurs_check(f(X, def), f(def, Y)).`, x: internal.NewAtom("f").Apply(x, internal.NewAtom("def")), y: internal.NewAtom("f").Apply(internal.NewAtom("def"), y), ok: true, env: map[internal.Variable]Term{
			x: internal.NewAtom("def"),
			y: internal.NewAtom("def"),
		}},
		{title: `unify_with_occurs_check(1, 2).`, x: Integer(1), y: Integer(2), ok: false},
		{title: `unify_with_occurs_check(1, 1.0).`, x: Integer(1), y: Float(1), ok: false},
		{title: `unify_with_occurs_check(g(X), f(f(X))).`, x: internal.NewAtom("g").Apply(x), y: internal.NewAtom("f").Apply(internal.NewAtom("f").Apply(x)), ok: false},
		{title: `unify_with_occurs_check(f(X, 1), f(a(X))).`, x: internal.NewAtom("f").Apply(x, Integer(1)), y: internal.NewAtom("f").Apply(internal.NewAtom("a").Apply(x)), ok: false},
		{title: `unify_with_occurs_check(f(X, Y, X), f(a(X), a(Y), Y, 2)).`, x: internal.NewAtom("f").Apply(x, y, x), y: internal.NewAtom("f").Apply(internal.NewAtom("a").Apply(x), internal.NewAtom("a").Apply(y), y, Integer(2)), ok: false},
		{title: `unify_with_occurs_check(X, a(X)).`, x: x, y: internal.NewAtom("a").Apply(x), ok: false},
		{title: `unify_with_occurs_check(f(X, 1), f(a(X), 2)).`, x: internal.NewAtom("f").Apply(x, Integer(1)), y: internal.NewAtom("f").Apply(internal.NewAtom("a").Apply(x), Integer(2)), ok: false},
		{title: `unify_with_occurs_check(f(1, X, 1), f(2, a(X), 2)).`, x: internal.NewAtom("f").Apply(Integer(1), x, Integer(1)), y: internal.NewAtom("f").Apply(Integer(2), internal.NewAtom("a").Apply(x), Integer(2)), ok: false},
		{title: `unify_with_occurs_check(f(1, X), f(2, a(X))).`, x: internal.NewAtom("f").Apply(Integer(1), x), y: internal.NewAtom("f").Apply(Integer(2), internal.NewAtom("a").Apply(x)), ok: false},
		{title: `unify_with_occurs_check(f(X, Y, X, 1), f(a(X), a(Y), Y, 2)).`, x: internal.NewAtom("f").Apply(x, y, x, Integer(1)), y: internal.NewAtom("f").Apply(internal.NewAtom("a").Apply(x), internal.NewAtom("a").Apply(y), y, Integer(2)), ok: false},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := UnifyWithOccursCheck(nil, tt.x, tt.y, func(env *internal.Env) *internal.Promise {
				for k, v := range tt.env {
					_, ok := env.Unify(k, v)
					assert.True(t, ok)
				}
				return internal.Bool(true)
			}, tt.premise).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestSubsumesTerm(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		ok, err := SubsumesTerm(nil, internal.NewVariable(), internal.NewAtom("a"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not unifiable", func(t *testing.T) {
		ok, err := SubsumesTerm(nil, internal.NewAtom("a"), internal.NewAtom("b"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("specific-general", func(t *testing.T) {
		ok, err := SubsumesTerm(nil, internal.NewAtom("a"), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeVar(t *testing.T) {
	t.Run("var", func(t *testing.T) {
		ok, err := TypeVar(nil, internal.NewVariable(), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not var", func(t *testing.T) {
		ok, err := TypeVar(nil, internal.NewAtom("foo"), Success, nil).Force(context.Background())
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
		ok, err := TypeFloat(nil, internal.NewAtom("foo"), Success, nil).Force(context.Background())
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
		ok, err := TypeInteger(nil, internal.NewAtom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestTypeAtom(t *testing.T) {
	t.Run("atom", func(t *testing.T) {
		ok, err := TypeAtom(nil, internal.NewAtom("foo"), Success, nil).Force(context.Background())
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
			functor: internal.NewAtom("foo"),
			args:    []Term{internal.NewAtom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not compound", func(t *testing.T) {
		ok, err := TypeCompound(nil, internal.NewAtom("foo"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestAcyclicTerm(t *testing.T) {
	t.Run("atomic", func(t *testing.T) {
		ok, err := AcyclicTerm(nil, internal.NewAtom("a"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("compound", func(t *testing.T) {
		t.Run("cyclic", func(t *testing.T) {
			var c = compound{
				functor: internal.NewAtom("f"),
				args: []Term{
					internal.NewAtom("a"),
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
	x, y := internal.NewVariable(), internal.NewVariable()
	a, b := internal.NewVariable(), internal.NewVariable()
	n := internal.NewVariable()
	f := internal.NewVariable()

	tests := []struct {
		title             string
		term, name, arity Term
		ok                bool
		err               error
		env               map[internal.Variable]Term
	}{
		// 8.5.1.4 Examples
		{title: `functor(foo(a, b, c), foo, 3).`, term: internal.NewAtom("foo").Apply(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), name: internal.NewAtom("foo"), arity: Integer(3), ok: true},
		{title: `functor(foo(a, b, c), X, Y).`, term: internal.NewAtom("foo").Apply(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), name: x, arity: y, ok: true, env: map[internal.Variable]Term{
			x: internal.NewAtom("foo"),
			y: Integer(3),
		}},
		{title: `functor(X, foo, 3).`, term: x, name: internal.NewAtom("foo"), arity: Integer(3), ok: true, env: map[internal.Variable]Term{
			x: internal.NewAtom("foo").Apply(internal.NewVariable(), internal.NewVariable(), internal.NewVariable()),
		}},
		{title: `functor(X, foo, 0).`, term: x, name: internal.NewAtom("foo"), arity: Integer(0), ok: true, env: map[internal.Variable]Term{
			x: internal.NewAtom("foo"),
		}},
		{title: `functor(mats(A, B), A, B).`, term: internal.NewAtom("mats").Apply(a, b), name: a, arity: b, ok: true, env: map[internal.Variable]Term{
			a: internal.NewAtom("mats"),
			b: Integer(2),
		}},
		{title: `functor(foo(a), foo, 2).`, term: internal.NewAtom("foo").Apply(internal.NewAtom("a")), name: internal.NewAtom("foo"), arity: Integer(2), ok: false},
		{title: `functor(foo(a), fo, 1).`, term: internal.NewAtom("foo").Apply(internal.NewAtom("a")), name: internal.NewAtom("fo"), arity: Integer(1), ok: false},
		{title: `functor(1, X, Y).`, term: Integer(1), name: x, arity: y, ok: true, env: map[internal.Variable]Term{
			x: Integer(1),
			y: Integer(0),
		}},
		{title: `functor(X, 1.1, 0).`, term: x, name: Float(1.1), arity: Integer(0), ok: true, env: map[internal.Variable]Term{
			x: Float(1.1),
		}},
		{title: `functor([_|_], '.', 2).`, term: Cons(internal.NewVariable(), internal.NewVariable()), name: atomDot, arity: Integer(2), ok: true},
		{title: `functor([], [], 0).`, term: internal.atomEmptyList, name: internal.atomEmptyList, arity: Integer(0), ok: true},
		{title: `functor(X, Y, 3).`, term: x, name: y, arity: Integer(3), err: InstantiationError(nil)},
		{title: `functor(X, foo, N).`, term: x, name: internal.NewAtom("foo"), arity: n, err: InstantiationError(nil)},
		{title: `functor(X, foo, a).`, term: x, name: internal.NewAtom("foo"), arity: internal.NewAtom("a"), err: typeError(validTypeInteger, internal.NewAtom("a"), nil)},
		{title: `functor(F, 1.5, 1).`, term: f, name: Float(1.5), arity: Integer(1), err: typeError(validTypeAtom, Float(1.5), nil)},
		{title: `functor(F, foo(a), 1).`, term: f, name: internal.NewAtom("foo").Apply(internal.NewAtom("a")), arity: Integer(1), err: typeError(validTypeAtomic, internal.NewAtom("foo").Apply(internal.NewAtom("a")), nil)},
		// {title: `current_prolog_flag(max_arity, A), X is A + 1, functor(T, foo, X).`}
		{title: `Minus_1 is 0 - 1, functor(F, foo, Minus_1).`, term: f, name: internal.NewAtom("foo"), arity: Integer(-1), err: domainError(validDomainNotLessThanZero, Integer(-1), nil)},

		// https://github.com/ichiban/prolog/issues/247
		{title: `functor(X, Y, 0).`, term: x, name: y, arity: Integer(0), err: InstantiationError(nil)},

		// https://github.com/ichiban/prolog/issues/226
		{title: `functor(F, f, max_int).`, term: f, name: internal.NewAtom("f"), arity: internal.maxInt, err: resourceError(resourceMemory, nil)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := internal.Functor(nil, tt.term, tt.name, tt.arity, func(env *internal.Env) *internal.Promise {
				for k, v := range tt.env {
					_, ok := env.Unify(k, v)
					assert.True(t, ok)
				}
				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestArg(t *testing.T) {
	t.Run("term is a variable", func(t *testing.T) {
		v := internal.NewVariable()
		ok, err := Arg(nil, internal.NewVariable(), v, internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("term is not a compound", func(t *testing.T) {
		ok, err := Arg(nil, internal.NewVariable(), internal.NewAtom("foo"), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeCompound, internal.NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("nth is a variable", func(t *testing.T) {
		nth := internal.NewVariable()
		_, err := Arg(nil, nth, &compound{
			functor: internal.NewAtom("f"),
			args:    []Term{internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("a")},
		}, internal.NewAtom("a"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
	})

	t.Run("nth is an integer", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			ok, err := Arg(nil, Integer(1), &compound{
				functor: internal.NewAtom("f"),
				args:    []Term{internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")},
			}, internal.NewAtom("a"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)

			ok, err = Arg(nil, Integer(2), &compound{
				functor: internal.NewAtom("f"),
				args:    []Term{internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")},
			}, internal.NewAtom("b"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)

			ok, err = Arg(nil, Integer(3), &compound{
				functor: internal.NewAtom("f"),
				args:    []Term{internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")},
			}, internal.NewAtom("c"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("ng", func(t *testing.T) {
			ok, err := Arg(nil, Integer(0), &compound{
				functor: internal.NewAtom("f"),
				args:    []Term{internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")},
			}, internal.NewVariable(), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)

			ok, err = Arg(nil, Integer(4), &compound{
				functor: internal.NewAtom("f"),
				args:    []Term{internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")},
			}, internal.NewVariable(), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("negative", func(t *testing.T) {
			ok, err := Arg(nil, Integer(-2), &compound{
				functor: internal.NewAtom("f"),
				args:    []Term{internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")},
			}, internal.NewAtom("b"), Success, nil).Force(context.Background())
			assert.Equal(t, domainError(validDomainNotLessThanZero, Integer(-2), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("nth is neither a variable nor an integer", func(t *testing.T) {
		ok, err := Arg(nil, internal.NewAtom("foo"), &compound{
			functor: internal.NewAtom("f"),
			args:    []Term{internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")},
		}, internal.NewAtom("b"), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, internal.NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})
}

func TestUniv(t *testing.T) {
	x, y := internal.NewVariable(), internal.NewVariable()
	l := internal.NewVariable()
	a, as := internal.NewVariable(), internal.NewVariable()
	foo := internal.NewVariable()

	tests := []struct {
		title      string
		term, list Term
		ok         bool
		err        error
		env        map[internal.Variable]Term
	}{
		// 8.5.3.4 Examples
		{title: "1", term: internal.NewAtom("foo").Apply(internal.NewAtom("a"), internal.NewAtom("b")), list: List(internal.NewAtom("foo"), internal.NewAtom("a"), internal.NewAtom("b")), ok: true},
		{title: "2", term: x, list: List(internal.NewAtom("foo"), internal.NewAtom("a"), internal.NewAtom("b")), ok: true, env: map[internal.Variable]Term{
			x: internal.NewAtom("foo").Apply(internal.NewAtom("a"), internal.NewAtom("b")),
		}},
		{title: "3", term: internal.NewAtom("foo").Apply(internal.NewAtom("a"), internal.NewAtom("b")), list: l, ok: true, env: map[internal.Variable]Term{
			l: List(internal.NewAtom("foo"), internal.NewAtom("a"), internal.NewAtom("b")),
		}},
		{title: "4", term: internal.NewAtom("foo").Apply(x, internal.NewAtom("b")), list: List(internal.NewAtom("foo"), internal.NewAtom("a"), y), ok: true, env: map[internal.Variable]Term{
			x: internal.NewAtom("a"),
			y: internal.NewAtom("b"),
		}},
		{title: "5", term: Integer(1), list: List(Integer(1)), ok: true},
		{title: "6", term: internal.NewAtom("foo").Apply(internal.NewAtom("a"), internal.NewAtom("b")), list: List(internal.NewAtom("foo"), internal.NewAtom("b"), internal.NewAtom("a")), ok: false},
		{title: "7", term: x, list: y, err: InstantiationError(nil)},
		{title: "8", term: x, list: PartialList(y, internal.NewAtom("foo"), internal.NewAtom("a")), err: InstantiationError(nil)},
		{title: "9", term: x, list: PartialList(internal.NewAtom("bar"), internal.NewAtom("foo")), err: typeError(validTypeList, PartialList(internal.NewAtom("bar"), internal.NewAtom("foo")), nil)},
		{title: "10", term: x, list: List(foo, internal.NewAtom("bar")), err: InstantiationError(nil)},
		{title: "11", term: x, list: List(Integer(3), Integer(1)), err: typeError(validTypeAtom, Integer(3), nil)},
		{title: "12", term: x, list: List(Float(1.1), internal.NewAtom("foo")), err: typeError(validTypeAtom, Float(1.1), nil)},
		{title: "13", term: x, list: List(internal.NewAtom("a").Apply(internal.NewAtom("b")), Integer(1)), err: typeError(validTypeAtom, internal.NewAtom("a").Apply(internal.NewAtom("b")), nil)},
		{title: "14", term: x, list: Integer(4), err: typeError(validTypeList, Integer(4), nil)},
		{title: "15", term: internal.NewAtom("f").Apply(x), list: List(internal.NewAtom("f"), internal.NewAtom("u").Apply(x)), ok: true, env: map[internal.Variable]Term{
			x: internal.NewAtom("u").Apply(x),
		}},

		// 8.5.3.3 Errors
		{title: "b: term is a compound", term: internal.NewAtom("f").Apply(internal.NewAtom("a")), list: PartialList(internal.NewAtom("a"), internal.NewAtom("f")), err: typeError(validTypeList, PartialList(internal.NewAtom("a"), internal.NewAtom("f")), nil)},
		{title: "b: term is an atomic", term: Integer(1), list: PartialList(internal.NewAtom("a"), internal.NewAtom("f")), err: typeError(validTypeList, PartialList(internal.NewAtom("a"), internal.NewAtom("f")), nil)},
		{title: "c", term: x, list: List(y), err: InstantiationError(nil)},
		{title: "e", term: x, list: List(internal.NewAtom("f").Apply(internal.NewAtom("a"))), err: typeError(validTypeAtomic, internal.NewAtom("f").Apply(internal.NewAtom("a")), nil)},
		{title: "f", term: x, list: List(), err: domainError(validDomainNonEmptyList, List(), nil)},

		{title: "term is a variable, list has exactly one member which is an atomic", term: x, list: List(Integer(1)), ok: true, env: map[internal.Variable]Term{
			x: Integer(1),
		}},
		{title: "term is an atomic, the length of list is not 1", term: Integer(1), list: List(), ok: false},

		// https://github.com/ichiban/prolog/issues/244
		{title: "term is atomic", term: internal.NewAtom("c"), list: PartialList(as, a), ok: true, env: map[internal.Variable]Term{
			a:  internal.NewAtom("c"),
			as: List(),
		}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := Univ(nil, tt.term, tt.list, func(env *internal.Env) *internal.Promise {
				for k, v := range tt.env {
					assert.Equal(t, v, env.Resolve(k))
				}
				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestCopyTerm(t *testing.T) {
	x, y := internal.NewVariable(), internal.NewVariable()
	a, b := internal.NewVariable(), internal.NewVariable()

	tests := []struct {
		title   string
		in, out Term
		ok      bool
		err     error
		env     map[internal.Variable]Term
		mem     int64
	}{
		// 8.5.4.4 Examples
		{title: "copy_term(X, Y).", in: x, out: y, ok: true},
		{title: "copy_term(X, 3).", in: x, out: Integer(3), ok: true},
		{title: "copy_term(_, a).", in: internal.NewVariable(), out: internal.NewAtom("a"), ok: true},
		{title: "copy_term(_, _).", in: internal.NewVariable(), out: internal.NewVariable(), ok: true},
		{title: "copy_term(X+X+Y, A+B+B).", in: atomPlus.Apply(atomPlus.Apply(x, x), y), out: atomPlus.Apply(atomPlus.Apply(a, b), b), ok: true, env: map[internal.Variable]Term{
			a: b,
		}},
		{title: "copy_term(a, b).", in: internal.NewAtom("a"), out: internal.NewAtom("b"), ok: false},
		// copy_term(a+X, X+b), copy_term(a+X, X+b).
		{title: "copy_term(demoen(X, X), demoen(Y, f(Y))).", in: internal.NewAtom("demoen").Apply(x, x), out: internal.NewAtom("demoen").Apply(y, internal.NewAtom("f").Apply(y)), ok: true, env: map[internal.Variable]Term{
			y: internal.NewAtom("f").Apply(y),
		}},

		{title: "charList", in: CharList("foo"), out: CharList("foo"), ok: true},
		{title: "codeList", in: CodeList("foo"), out: CodeList("foo"), ok: true},
		{title: "list", in: List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), out: List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), ok: true},
		{title: "partial", in: PartialList(x, internal.NewAtom("a"), internal.NewAtom("b")), out: PartialList(x, internal.NewAtom("a"), internal.NewAtom("b")), ok: true},

		{title: "out of memory: list", in: List(List(internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable())), out: internal.NewVariable(), mem: 1, err: resourceError(resourceMemory, nil)},
		{title: "out of memory: partial", in: PartialList(PartialList(internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable()), internal.NewVariable()), out: internal.NewVariable(), mem: 1, err: resourceError(resourceMemory, nil)},
		{title: "out of memory: compound", in: internal.NewAtom("f").Apply(internal.NewAtom("f").Apply(internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable())), out: internal.NewVariable(), mem: 1, err: resourceError(resourceMemory, nil)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			defer setMemFree(tt.mem)()

			ok, err := CopyTerm(nil, tt.in, tt.out, func(env *internal.Env) *internal.Promise {
				for k, v := range tt.env {
					assert.Equal(t, v, env.Resolve(k))
				}
				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestTermVariables(t *testing.T) {
	vars := internal.NewVariable()
	vs, vt := internal.NewVariable(), internal.NewVariable()
	a, b, c, d := internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable()

	tests := []struct {
		title      string
		term, vars Term
		ok         bool
		err        error
		env        map[internal.Variable]Term
		mem        int64
	}{
		// 8.5.5.4 Examples
		{title: "1", term: internal.NewAtom("t"), vars: vars, ok: true, env: map[internal.Variable]Term{
			vars: List(),
		}},
		{title: "2", term: atomMinus.Apply(
			atomPlus.Apply(
				a,
				atomSlash.Apply(
					internal.NewAtom("*").Apply(b, c),
					c,
				),
			),
			d,
		), vars: vars, ok: true, env: map[internal.Variable]Term{
			vars: List(a, b, c, d),
		}},
		{title: "3", term: internal.NewAtom("t"), vars: PartialList(internal.NewAtom("a"), internal.NewAtom("x"), internal.NewAtom("y")), err: typeError(validTypeList, PartialList(internal.NewAtom("a"), internal.NewAtom("x"), internal.NewAtom("y")), nil)},
		{title: "4, 5", term: vs, vars: vars, ok: true, env: map[internal.Variable]Term{
			vars: List(b, a),
			vs:   atomPlus.Apply(b, vt),
			vt:   internal.NewAtom("*").Apply(a, b),
		}},
		{title: "6", term: atomPlus.Apply(atomPlus.Apply(a, b), b), vars: PartialList(vars, b), ok: true, env: map[internal.Variable]Term{
			b:    a,
			vars: List(b),
		}},

		{title: "out of memory", term: internal.NewAtom("f").Apply(internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable()), vars: vars, ok: false, err: resourceError(resourceMemory, nil), mem: 1},
	}

	env := NewEnv().
		bind(vs, atomPlus.Apply(b, vt)).
		bind(vt, internal.NewAtom("*").Apply(a, b))
	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			defer setMemFree(tt.mem)()

			ok, err := TermVariables(nil, tt.term, tt.vars, func(env *internal.Env) *internal.Promise {
				for k, v := range tt.env {
					assert.Equal(t, v, env.Resolve(k))
				}
				return internal.Bool(true)
			}, env).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestOp(t *testing.T) {
	t.Run("insert", func(t *testing.T) {
		t.Run("atom", func(t *testing.T) {
			var ops internal.operators
			ops.define(900, internal.operatorSpecifierXFX, internal.NewAtom(`+++`))
			ops.define(1100, internal.operatorSpecifierXFX, internal.NewAtom(`+`))

			vm := internal.VM{
				typeIn: atomUser,
				modules: map[internal.Atom]*internal.module{
					atomUser: {
						operators: ops,
					},
				},
			}

			ok, err := Op(&vm, Integer(1000), atomXFX, internal.NewAtom("++"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)

			assert.Equal(t, internal.operators{
				internal.NewAtom(`+++`): {
					internal.operatorClassInfix: {
						priority:  900,
						specifier: internal.operatorSpecifierXFX,
						name:      internal.NewAtom("+++"),
					},
				},
				internal.NewAtom(`++`): {
					internal.operatorClassInfix: {
						priority:  1000,
						specifier: internal.operatorSpecifierXFX,
						name:      internal.NewAtom("++"),
					},
				},
				internal.NewAtom(`+`): {
					internal.operatorClassInfix: {
						priority:  1100,
						specifier: internal.operatorSpecifierXFX,
						name:      atomPlus,
					},
				},
			}, vm.TypeInModule().operators)
		})

		t.Run("list", func(t *testing.T) {
			vm := internal.VM{
				typeIn: atomUser,
				modules: map[internal.Atom]*internal.module{
					atomUser: {
						operators: internal.operators{
							internal.NewAtom(`+++`): {
								internal.operatorClassInfix: {
									priority:  900,
									specifier: internal.operatorSpecifierXFX,
									name:      internal.NewAtom("+++"),
								},
							},
							internal.NewAtom(`+`): {
								internal.operatorClassInfix: {
									priority:  1100,
									specifier: internal.operatorSpecifierXFX,
									name:      atomPlus,
								},
							},
						},
					},
				},
			}
			ok, err := Op(&vm, Integer(1000), atomXFX, List(internal.NewAtom("++"), internal.NewAtom("++")), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)

			assert.Equal(t, internal.operators{
				internal.NewAtom(`+++`): {
					internal.operatorClassInfix: {
						priority:  900,
						specifier: internal.operatorSpecifierXFX,
						name:      internal.NewAtom("+++"),
					},
				},
				internal.NewAtom(`++`): {
					internal.operatorClassInfix: {
						priority:  1000,
						specifier: internal.operatorSpecifierXFX,
						name:      internal.NewAtom("++"),
					},
				},
				internal.NewAtom(`+`): {
					internal.operatorClassInfix: {
						priority:  1100,
						specifier: internal.operatorSpecifierXFX,
						name:      atomPlus,
					},
				},
			}, vm.TypeInModule().operators)
		})
	})

	t.Run("remove", func(t *testing.T) {
		vm := internal.VM{
			typeIn: atomUser,
			modules: map[internal.Atom]*internal.module{
				atomUser: {
					operators: internal.operators{
						internal.NewAtom(`+++`): {
							internal.operatorClassInfix: {
								priority:  900,
								specifier: internal.operatorSpecifierXFX,
								name:      internal.NewAtom("+++"),
							},
						},
						internal.NewAtom(`++`): {
							internal.operatorClassInfix: {
								priority:  1000,
								specifier: internal.operatorSpecifierXFX,
								name:      internal.NewAtom("++"),
							},
						},
						internal.NewAtom(`+`): {
							internal.operatorClassInfix: {
								priority:  1100,
								specifier: internal.operatorSpecifierXFX,
								name:      atomPlus,
							},
						},
					},
				},
			},
		}
		ok, err := Op(&vm, Integer(0), atomXFX, internal.NewAtom("++"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, internal.operators{
			internal.NewAtom(`+++`): {
				internal.operatorClassInfix: {
					priority:  900,
					specifier: internal.operatorSpecifierXFX,
					name:      internal.NewAtom("+++"),
				},
			},
			internal.NewAtom(`+`): {
				internal.operatorClassInfix: {
					priority:  1100,
					specifier: internal.operatorSpecifierXFX,
					name:      atomPlus,
				},
			},
		}, vm.TypeInModule().operators)
	})

	t.Run("priority is a variable", func(t *testing.T) {
		var vm internal.VM
		ok, err := Op(&vm, internal.NewVariable(), atomXFX, atomPlus, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("specifier is a variable", func(t *testing.T) {
		var vm internal.VM
		ok, err := Op(&vm, Integer(1000), internal.NewVariable(), atomPlus, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("priority is neither a variable nor an integer", func(t *testing.T) {
		var vm internal.VM
		ok, err := Op(&vm, internal.NewAtom("foo"), atomXFX, atomPlus, Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, internal.NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("specifier is neither a variable nor an atom", func(t *testing.T) {
		var vm internal.VM
		ok, err := Op(&vm, Integer(1000), Integer(0), atomPlus, Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("operator is neither a partial list nor a list nor an atom", func(t *testing.T) {
		var vm internal.VM
		ok, err := Op(&vm, Integer(1000), atomXFX, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeList, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the operator list is neither a variable nor an atom", func(t *testing.T) {
		var vm internal.VM
		ok, err := Op(&vm, Integer(1000), atomXFX, List(Integer(0)), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("priority is not between 0 and 1200 inclusive", func(t *testing.T) {
		t.Run("priority is negative", func(t *testing.T) {
			var vm internal.VM
			ok, err := Op(&vm, Integer(-1), atomXFX, atomPlus, Success, nil).Force(context.Background())
			assert.Equal(t, domainError(validDomainOperatorPriority, Integer(-1), nil), err)
			assert.False(t, ok)
		})

		t.Run("priority is more than 1200", func(t *testing.T) {
			var vm internal.VM
			ok, err := Op(&vm, Integer(1201), atomXFX, atomPlus, Success, nil).Force(context.Background())
			assert.Equal(t, domainError(validDomainOperatorPriority, Integer(1201), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("specifier is not a valid operator specifier", func(t *testing.T) {
		var vm internal.VM
		ok, err := Op(&vm, Integer(1000), internal.NewAtom("foo"), atomPlus, Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainOperatorSpecifier, internal.NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("operator is ','", func(t *testing.T) {
		var ops internal.operators
		ops.define(1000, internal.operatorSpecifierXFY, internal.NewAtom(`,`))
		vm := internal.VM{
			typeIn: atomUser,
			modules: map[internal.Atom]*internal.module{
				atomUser: {
					operators: ops,
				},
			},
		}
		ok, err := Op(&vm, Integer(1000), atomXFY, atomComma, Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationModify, permissionTypeOperator, atomComma, nil), err)
		assert.False(t, ok)
	})

	t.Run("an element of the operator list is ','", func(t *testing.T) {
		var ops internal.operators
		ops.define(1000, internal.operatorSpecifierXFY, internal.NewAtom(`,`))
		vm := internal.VM{
			typeIn: atomUser,
			modules: map[internal.Atom]*internal.module{
				atomUser: {
					operators: ops,
				},
			},
		}
		ok, err := Op(&vm, Integer(1000), atomXFY, List(atomComma), Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationModify, permissionTypeOperator, atomComma, nil), err)
		assert.False(t, ok)
	})

	t.Run("operator is an atom, priority is a priority, and specifier is a specifier such that operator would have an invalid set of priorities and specifiers", func(t *testing.T) {
		t.Run("empty list", func(t *testing.T) {
			var vm internal.VM
			ok, err := Op(&vm, Integer(1000), atomXFY, internal.atomEmptyList, Success, nil).Force(context.Background())
			assert.Equal(t, permissionError(operationCreate, permissionTypeOperator, internal.atomEmptyList, nil), err)
			assert.False(t, ok)
		})

		t.Run("empty curly brackets", func(t *testing.T) {
			var vm internal.VM
			ok, err := Op(&vm, Integer(1000), atomXFY, internal.atomEmptyBlock, Success, nil).Force(context.Background())
			assert.Equal(t, permissionError(operationCreate, permissionTypeOperator, internal.atomEmptyBlock, nil), err)
			assert.False(t, ok)
		})

		t.Run("bar", func(t *testing.T) {
			t.Run("create", func(t *testing.T) {
				var ops internal.operators
				vm := internal.VM{
					typeIn: atomUser,
					modules: map[internal.Atom]*internal.module{
						atomUser: {
							operators: ops,
						},
					},
				}
				ok, err := Op(&vm, Integer(1000), atomXFY, atomBar, Success, nil).Force(context.Background())
				assert.Equal(t, permissionError(operationCreate, permissionTypeOperator, atomBar, nil), err)
				assert.False(t, ok)
			})

			t.Run("modify", func(t *testing.T) {
				var ops internal.operators
				ops.define(1001, internal.operatorSpecifierXFY, internal.NewAtom(`|`))
				vm := internal.VM{
					typeIn: atomUser,
					modules: map[internal.Atom]*internal.module{
						atomUser: {
							operators: ops,
						},
					},
				}
				ok, err := Op(&vm, Integer(1000), atomXFY, atomBar, Success, nil).Force(context.Background())
				assert.Equal(t, permissionError(operationModify, permissionTypeOperator, atomBar, nil), err)
				assert.False(t, ok)
			})
		})
	})

	t.Run("operator is a list, priority is a priority, and specifier is a specifier such that an element op of the list operator would have an invalid set of priorities and specifiers", func(t *testing.T) {
		t.Run("empty list", func(t *testing.T) {
			var vm internal.VM
			ok, err := Op(&vm, Integer(1000), atomXFY, List(internal.atomEmptyList), Success, nil).Force(context.Background())
			assert.Equal(t, permissionError(operationCreate, permissionTypeOperator, internal.atomEmptyList, nil), err)
			assert.False(t, ok)
		})

		t.Run("empty curly brackets", func(t *testing.T) {
			var vm internal.VM
			ok, err := Op(&vm, Integer(1000), atomXFY, List(internal.atomEmptyBlock), Success, nil).Force(context.Background())
			assert.Equal(t, permissionError(operationCreate, permissionTypeOperator, internal.atomEmptyBlock, nil), err)
			assert.False(t, ok)
		})

		t.Run("bar", func(t *testing.T) {
			t.Run("create", func(t *testing.T) {
				var ops internal.operators
				vm := internal.VM{
					typeIn: atomUser,
					modules: map[internal.Atom]*internal.module{
						atomUser: {
							operators: ops,
						},
					},
				}
				ok, err := Op(&vm, Integer(1000), atomXFY, List(atomBar), Success, nil).Force(context.Background())
				assert.Equal(t, permissionError(operationCreate, permissionTypeOperator, atomBar, nil), err)
				assert.False(t, ok)
			})

			t.Run("modify", func(t *testing.T) {
				var ops internal.operators
				ops.define(101, internal.operatorSpecifierXFY, internal.NewAtom(`|`))
				vm := internal.VM{
					typeIn: atomUser,
					modules: map[internal.Atom]*internal.module{
						atomUser: {
							operators: ops,
						},
					},
				}
				ok, err := Op(&vm, Integer(1000), atomXFY, List(atomBar), Success, nil).Force(context.Background())
				assert.Equal(t, permissionError(operationModify, permissionTypeOperator, atomBar, nil), err)
				assert.False(t, ok)
			})
		})
	})

	t.Run("There shall not be an infix and a postfix operator with the same name.", func(t *testing.T) {
		t.Run("infix", func(t *testing.T) {
			var ops internal.operators
			ops.define(200, internal.operatorSpecifierYF, internal.NewAtom(`+`))
			vm := internal.VM{
				typeIn: atomUser,
				modules: map[internal.Atom]*internal.module{
					atomUser: {
						operators: ops,
					},
				},
			}
			ok, err := Op(&vm, Integer(500), atomYFX, List(atomPlus), Success, nil).Force(context.Background())
			assert.Equal(t, permissionError(operationCreate, permissionTypeOperator, atomPlus, nil), err)
			assert.False(t, ok)
		})

		t.Run("postfix", func(t *testing.T) {
			var ops internal.operators
			ops.define(500, internal.operatorSpecifierYFX, internal.NewAtom(`+`))
			vm := internal.VM{
				typeIn: atomUser,
				modules: map[internal.Atom]*internal.module{
					atomUser: {
						operators: ops,
					},
				},
			}
			ok, err := Op(&vm, Integer(200), atomYF, List(atomPlus), Success, nil).Force(context.Background())
			assert.Equal(t, permissionError(operationCreate, permissionTypeOperator, atomPlus, nil), err)
			assert.False(t, ok)
		})
	})
}

func TestCurrentOp(t *testing.T) {
	var ops internal.operators
	ops.define(900, internal.operatorSpecifierXFX, internal.NewAtom(`+++`))
	ops.define(1000, internal.operatorSpecifierXFX, internal.NewAtom(`++`))
	ops.define(1100, internal.operatorSpecifierXFX, internal.NewAtom(`+`))
	vm := internal.VM{
		typeIn: atomUser,
		modules: map[internal.Atom]*internal.module{
			atomUser: {
				operators: ops,
			},
		},
	}

	t.Run("single solution", func(t *testing.T) {
		ok, err := CurrentOp(&vm, Integer(1100), atomXFX, atomPlus, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("multiple solutions", func(t *testing.T) {
		priority, specifier, operator := internal.NewVariable(), internal.NewVariable(), internal.NewVariable()
		ok, err := CurrentOp(&vm, priority, specifier, operator, func(env *internal.Env) *internal.Promise {
			switch env.Resolve(operator) {
			case internal.NewAtom("+++"):
				assert.Equal(t, Integer(900), env.Resolve(priority))
				assert.Equal(t, atomXFX, env.Resolve(specifier))
			case internal.NewAtom("++"):
				assert.Equal(t, Integer(1000), env.Resolve(priority))
				assert.Equal(t, atomXFX, env.Resolve(specifier))
			case atomPlus:
				assert.Equal(t, Integer(1100), env.Resolve(priority))
				assert.Equal(t, atomXFX, env.Resolve(specifier))
			default:
				assert.Fail(t, "unreachable")
			}
			return internal.Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("priority is not an operator priority", func(t *testing.T) {
		t.Run("priority is not an integer", func(t *testing.T) {
			ok, err := CurrentOp(&vm, internal.NewAtom("foo"), atomXFX, atomPlus, Success, nil).Force(context.Background())
			assert.Equal(t, domainError(validDomainOperatorPriority, internal.NewAtom("foo"), nil), err)
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
			ok, err := CurrentOp(&vm, Integer(1100), internal.NewAtom("foo"), atomPlus, Success, nil).Force(context.Background())
			assert.Equal(t, domainError(validDomainOperatorSpecifier, internal.NewAtom("foo"), nil), err)
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
	s := internal.NewVariable()
	x, y, z := internal.NewVariable(), internal.NewVariable(), internal.NewVariable()
	l := internal.NewVariable()

	tests := []struct {
		title                     string
		template, goal, instances Term
		err                       error
		env                       []map[internal.Variable]Term
		warning                   bool
		mem                       int64
	}{
		// 8.10.2.4 Examples
		{
			title:     "bagof(X, (X=1 ; X=2), S).",
			template:  x,
			goal:      atomSemiColon.Apply(atomEqual.Apply(x, Integer(1)), atomEqual.Apply(x, Integer(2))),
			instances: s,
			env: []map[internal.Variable]Term{
				{s: List(Integer(1), Integer(2))},
			},
		},
		{
			title:     "bagof(X, (X=1 ; X=2), X).",
			template:  x,
			goal:      atomSemiColon.Apply(atomEqual.Apply(x, Integer(1)), atomEqual.Apply(x, Integer(2))),
			instances: x,
			env: []map[internal.Variable]Term{
				{x: List(Integer(1), Integer(2))},
			},
		},
		{
			title:     "bagof(X, (X=Y ; X=Z), S).",
			template:  x,
			goal:      atomSemiColon.Apply(atomEqual.Apply(x, y), atomEqual.Apply(x, z)),
			instances: s,
			env: []map[internal.Variable]Term{
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
			env: []map[internal.Variable]Term{
				{l: List(Integer(1)), y: Integer(1)},
				{l: List(Integer(1)), y: Integer(2)},
			},
		},
		{
			title:     "bagof(f(X, Y), (X=a ; Y=b), L).",
			template:  internal.NewAtom("f").Apply(x, y),
			goal:      atomSemiColon.Apply(atomEqual.Apply(x, internal.NewAtom("a")), atomEqual.Apply(y, internal.NewAtom("b"))),
			instances: l,
			env: []map[internal.Variable]Term{
				{l: List(internal.NewAtom("f").Apply(internal.NewAtom("a"), internal.NewVariable()), internal.NewAtom("f").Apply(internal.NewVariable(), internal.NewAtom("b")))},
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
			env: []map[internal.Variable]Term{
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
			env: []map[internal.Variable]Term{
				{s: List(Integer(1), internal.NewVariable(), Integer(2))},
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
			env: []map[internal.Variable]Term{
				{s: List(Integer(3)), y: internal.NewVariable()},
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
			env: []map[internal.Variable]Term{
				{s: List(y, z)},
				{s: List(internal.NewVariable())},
			},
		},
		{
			title:     "bagof(X, a(X, Y), L).",
			template:  x,
			goal:      internal.NewAtom("a").Apply(x, y),
			instances: l,
			env: []map[internal.Variable]Term{
				{l: List(Integer(1), Integer(2)), y: internal.NewAtom("f").Apply(internal.NewVariable())},
			},
		},
		{
			title:     "bagof(X, b(X, Y), L).",
			template:  x,
			goal:      internal.NewAtom("b").Apply(x, y),
			instances: l,
			env: []map[internal.Variable]Term{
				{l: List(Integer(1), Integer(1), Integer(2)), y: Integer(1)},
				{l: List(Integer(1), Integer(2), Integer(2)), y: Integer(2)},
			},
		},
		{
			title:     "bagof(X, Y^Z, L).",
			template:  x,
			goal:      atomCaret.Apply(y, z),
			instances: l,
			err:       InstantiationError(nil),
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
			template:  internal.NewAtom("t"),
			goal:      atomTrue,
			instances: PartialList(Integer(1), internal.NewAtom("t")),
			err:       typeError(validTypeList, PartialList(Integer(1), internal.NewAtom("t")), nil),
		},

		{
			title:    "out of memory: goal",
			template: x,
			goal: seq(atomSemiColon,
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
			),
			instances: s,
			err:       resourceError(resourceMemory, nil),
			mem:       1,
		},
		{
			title:    "out of memory: free variables",
			template: x,
			goal: seq(atomSemiColon,
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
			),
			instances: s,
			err:       resourceError(resourceMemory, nil),
			mem:       1,
		},
	}

	m := internal.module{
		unknown: internal.unknownWarning,
	}
	m.SetPredicate2("=", Unify)
	m.SetPredicate2(",", func(vm *internal.VM, g1, g2 Term, k internal.Cont, env *internal.Env) *internal.Promise {
		return Call(vm, g1, func(env *internal.Env) *internal.Promise {
			return Call(vm, g2, k, env)
		}, env)
	})
	m.SetPredicate2(";", func(vm *internal.VM, g1, g2 Term, k internal.Cont, env *internal.Env) *internal.Promise {
		return internal.Delay(func(context.Context) *internal.Promise {
			return Call(vm, g1, k, env)
		}, func(context.Context) *internal.Promise {
			return Call(vm, g2, k, env)
		})
	})
	m.SetPredicate0("true", func(_ *internal.VM, k internal.Cont, env *internal.Env) *internal.Promise {
		return k(env)
	})
	m.SetPredicate0("fail", func(*internal.VM, internal.Cont, *internal.Env) *internal.Promise {
		return internal.Bool(false)
	})
	m.SetPredicate2("a", func(vm *internal.VM, x, y Term, k internal.Cont, env *internal.Env) *internal.Promise {
		a, f := internal.NewAtom("$a"), internal.NewAtom("f")
		return internal.Delay(func(context.Context) *internal.Promise {
			return Unify(vm, a.Apply(x, y), a.Apply(Integer(1), f.Apply(internal.NewVariable())), k, env)
		}, func(context.Context) *internal.Promise {
			return Unify(vm, a.Apply(x, y), a.Apply(Integer(2), f.Apply(internal.NewVariable())), k, env)
		})
	})
	m.SetPredicate2("b", func(vm *internal.VM, x, y Term, k internal.Cont, env *internal.Env) *internal.Promise {
		b := internal.NewAtom("$b")
		return internal.Delay(func(context.Context) *internal.Promise {
			return Unify(vm, b.Apply(x, y), b.Apply(Integer(1), Integer(1)), k, env)
		}, func(context.Context) *internal.Promise {
			return Unify(vm, b.Apply(x, y), b.Apply(Integer(1), Integer(1)), k, env)
		}, func(context.Context) *internal.Promise {
			return Unify(vm, b.Apply(x, y), b.Apply(Integer(1), Integer(2)), k, env)
		}, func(context.Context) *internal.Promise {
			return Unify(vm, b.Apply(x, y), b.Apply(Integer(2), Integer(1)), k, env)
		}, func(context.Context) *internal.Promise {
			return Unify(vm, b.Apply(x, y), b.Apply(Integer(2), Integer(2)), k, env)
		}, func(context.Context) *internal.Promise {
			return Unify(vm, b.Apply(x, y), b.Apply(Integer(2), Integer(2)), k, env)
		})
	})
	vm := internal.VM{
		typeIn: atomUser,
		modules: map[internal.Atom]*internal.module{
			atomUser: &m,
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			defer setMemFree(tt.mem)()

			vm.Unknown = func(internal.Atom, []Term, *internal.Env) {
				assert.True(t, tt.warning)
			}
			_, err := BagOf(&vm, tt.template, tt.goal, tt.instances, func(env *internal.Env) *internal.Promise {
				for k, v := range tt.env[0] {
					_, ok := env.Unify(v, k)
					assert.True(t, ok)
				}
				tt.env = tt.env[1:]
				return internal.Bool(false)
			}, nil).Force(context.Background())
			assert.Equal(t, tt.err, err)
			assert.Empty(t, tt.env)
		})
	}
}

func TestSetOf(t *testing.T) {
	s := internal.NewVariable()
	x, y, z := internal.NewVariable(), internal.NewVariable(), internal.NewVariable()
	xs := internal.NewVariable()
	l := internal.NewVariable()
	u, v := internal.NewVariable(), internal.NewVariable()
	tests := []struct {
		title                     string
		template, goal, instances Term
		err                       error
		env                       []map[internal.Variable]Term
		warning                   bool
		mem                       int64
	}{
		// 8.10.3.4 Examples
		{
			title:     "setof(X, (X=1; X=2), S).",
			template:  x,
			goal:      atomSemiColon.Apply(atomEqual.Apply(x, Integer(1)), atomEqual.Apply(x, Integer(2))),
			instances: s,
			env: []map[internal.Variable]Term{
				{s: List(Integer(1), Integer(2))},
			},
		},
		{
			title:     "setof(X, (X=1; X=2), X).",
			template:  x,
			goal:      atomSemiColon.Apply(atomEqual.Apply(x, Integer(1)), atomEqual.Apply(x, Integer(2))),
			instances: x,
			env: []map[internal.Variable]Term{
				{x: List(Integer(1), Integer(2))},
			},
		},
		{
			title:     "setof(X, (X=2; X=1), S).",
			template:  x,
			goal:      atomSemiColon.Apply(atomEqual.Apply(x, Integer(2)), atomEqual.Apply(x, Integer(1))),
			instances: s,
			env: []map[internal.Variable]Term{
				{s: List(Integer(1), Integer(2))},
			},
		},
		{
			title:     "setof(X, (X=2; X=2), S).",
			template:  x,
			goal:      atomSemiColon.Apply(atomEqual.Apply(x, Integer(2)), atomEqual.Apply(x, Integer(2))),
			instances: s,
			env: []map[internal.Variable]Term{
				{s: List(Integer(2))},
			},
		},
		{
			title:     "setof(X, (X=Y ; X=Z), S).",
			template:  x,
			goal:      atomSemiColon.Apply(atomEqual.Apply(x, y), atomEqual.Apply(x, z)),
			instances: s,
			env: []map[internal.Variable]Term{
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
			env: []map[internal.Variable]Term{
				{l: List(Integer(1)), y: Integer(2)},
				{l: List(Integer(1)), y: Integer(1)},
			},
		},
		{
			title:     "setof(f(X, Y), (X=a ; Y=b), L).",
			template:  internal.NewAtom("f").Apply(x, y),
			goal:      atomSemiColon.Apply(atomEqual.Apply(x, internal.NewAtom("a")), atomEqual.Apply(y, internal.NewAtom("b"))),
			instances: l,
			env: []map[internal.Variable]Term{
				{l: List(internal.NewAtom("f").Apply(internal.NewAtom("a"), internal.NewVariable()), internal.NewAtom("f").Apply(internal.NewVariable(), internal.NewAtom("b")))},
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
			env: []map[internal.Variable]Term{
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
			env: []map[internal.Variable]Term{
				{s: List(internal.NewVariable(), Integer(1), Integer(2))},
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
			env: []map[internal.Variable]Term{
				{s: List(Integer(3)), y: internal.NewVariable()},
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
			env: []map[internal.Variable]Term{
				{s: List(y, z)},
				{s: List(internal.NewVariable())},
			},
		},
		{
			title:     "setof(X, a(X, Y), L).",
			template:  x,
			goal:      internal.NewAtom("a").Apply(x, y),
			instances: l,
			env: []map[internal.Variable]Term{
				{l: List(Integer(1), Integer(2)), y: internal.NewAtom("f").Apply(internal.NewVariable())},
			},
		},
		{
			title:     "setof(X, member(X,[f(U,b),f(V,c)]), L).",
			template:  x,
			goal:      internal.NewAtom("member").Apply(x, List(internal.NewAtom("f").Apply(u, internal.NewAtom("b")), internal.NewAtom("f").Apply(v, internal.NewAtom("c")))),
			instances: l,
			env: []map[internal.Variable]Term{
				{l: List(internal.NewAtom("f").Apply(u, internal.NewAtom("b")), internal.NewAtom("f").Apply(v, internal.NewAtom("c")))},
			},
		},
		{
			title:     "setof(X, member(X,[f(U,b),f(V,c)]), [f(a,c),f(a,b)]).",
			template:  x,
			goal:      internal.NewAtom("member").Apply(x, List(internal.NewAtom("f").Apply(u, internal.NewAtom("b")), internal.NewAtom("f").Apply(v, internal.NewAtom("c")))),
			instances: List(internal.NewAtom("f").Apply(internal.NewAtom("a"), internal.NewAtom("c")), internal.NewAtom("f").Apply(internal.NewAtom("a"), internal.NewAtom("b"))),
			env:       nil,
		},
		{
			title:     "setof(X, member(X,[f(b,U),f(c,V)]), [f(b,a),f(c,a)]).",
			template:  x,
			goal:      internal.NewAtom("member").Apply(x, List(internal.NewAtom("f").Apply(internal.NewAtom("b"), u), internal.NewAtom("f").Apply(internal.NewAtom("c"), v))),
			instances: List(internal.NewAtom("f").Apply(internal.NewAtom("b"), internal.NewAtom("a")), internal.NewAtom("f").Apply(internal.NewAtom("c"), internal.NewAtom("a"))),
			env: []map[internal.Variable]Term{
				{u: internal.NewAtom("a"), v: internal.NewAtom("a")},
			},
		},
		{
			title:     "setof(X, member(X,[V,U,f(U),f(V)]), L).",
			template:  x,
			goal:      internal.NewAtom("member").Apply(x, List(v, u, internal.NewAtom("f").Apply(u), internal.NewAtom("f").Apply(v))),
			instances: l,
			env: []map[internal.Variable]Term{
				{l: List(u, v, internal.NewAtom("f").Apply(u), internal.NewAtom("f").Apply(v))},
			},
		},
		{
			title:     "setof(X, member(X,[V,U,f(U),f(V)]), [a,b,f(a),f(b)]).",
			template:  x,
			goal:      internal.NewAtom("member").Apply(x, List(v, u, internal.NewAtom("f").Apply(u), internal.NewAtom("f").Apply(v))),
			instances: List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("f").Apply(internal.NewAtom("a")), internal.NewAtom("f").Apply(internal.NewAtom("b"))),
			env: []map[internal.Variable]Term{
				{u: internal.NewAtom("a"), v: internal.NewAtom("b")},
			},
		},
		{
			title:     "setof(X, member(X,[V,U,f(U),f(V)]), [a,b,f(b),f(a)]).",
			template:  x,
			goal:      internal.NewAtom("member").Apply(x, List(v, u, internal.NewAtom("f").Apply(u), internal.NewAtom("f").Apply(v))),
			instances: List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("f").Apply(internal.NewAtom("b")), internal.NewAtom("f").Apply(internal.NewAtom("a"))),
			env:       nil,
		},
		{
			title:    "setof(X, (exists(U,V)^member(X,[V,U,f(U),f(V)])), [a,b,f(b),f(a)]).",
			template: x,
			goal: atomCaret.Apply(
				internal.NewAtom("exists").Apply(u, v),
				internal.NewAtom("member").Apply(x, List(v, u, internal.NewAtom("f").Apply(u), internal.NewAtom("f").Apply(v))),
			),
			instances: List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("f").Apply(internal.NewAtom("b")), internal.NewAtom("f").Apply(internal.NewAtom("a"))),
			env: []map[internal.Variable]Term{
				{},
			},
		},
		{
			title:     "setof(X, b(X, Y), L).",
			template:  x,
			goal:      internal.NewAtom("b").Apply(x, y),
			instances: l,
			env: []map[internal.Variable]Term{
				{l: List(Integer(1), Integer(2)), y: Integer(1)},
				{l: List(Integer(1), Integer(2)), y: Integer(2)},
			},
		},
		{
			title:    "setof(X-Xs, Y^setof(Y,b(X, Y),Xs), L).",
			template: atomMinus.Apply(x, xs),
			goal: atomCaret.Apply(
				y,
				internal.NewAtom("setof").Apply(
					y,
					internal.NewAtom("b").Apply(x, y),
					xs,
				),
			),
			instances: l,
			env: []map[internal.Variable]Term{
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
				internal.NewAtom("setof").Apply(
					y,
					internal.NewAtom("b").Apply(x, y),
					xs,
				),
			),
			instances: l,
			env: []map[internal.Variable]Term{
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
			goal: internal.NewAtom("bagof").Apply(
				y,
				internal.NewAtom("d").Apply(x, y),
				xs,
			),
			instances: l,
			env: []map[internal.Variable]Term{
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
			template:  internal.NewAtom("t"),
			goal:      atomTrue,
			instances: PartialList(Integer(1), internal.NewAtom("t")),
			err:       typeError(validTypeList, PartialList(Integer(1), internal.NewAtom("t")), nil),
		},

		{
			title:    "out of memory: goal",
			template: x,
			goal: seq(atomSemiColon,
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
			),
			instances: s,
			err:       resourceError(resourceMemory, nil),
			mem:       1,
		},
		{
			title:    "out of memory: free variables",
			template: x,
			goal: seq(atomSemiColon,
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
				atomEqual.Apply(x, internal.NewVariable()),
			),
			instances: s,
			err:       resourceError(resourceMemory, nil),
			mem:       1,
		},
	}

	m := internal.module{
		unknown: internal.unknownWarning,
	}
	m.SetPredicate2("=", Unify)
	m.SetPredicate2(",", func(vm *internal.VM, g1, g2 Term, k internal.Cont, env *internal.Env) *internal.Promise {
		return Call(vm, g1, func(env *internal.Env) *internal.Promise {
			return Call(vm, g2, k, env)
		}, env)
	})
	m.SetPredicate2(";", func(vm *internal.VM, g1, g2 Term, k internal.Cont, env *internal.Env) *internal.Promise {
		return internal.Delay(func(context.Context) *internal.Promise {
			return Call(vm, g1, k, env)
		}, func(context.Context) *internal.Promise {
			return Call(vm, g2, k, env)
		})
	})
	m.SetPredicate0("true", func(_ *internal.VM, k internal.Cont, env *internal.Env) *internal.Promise {
		return k(env)
	})
	m.SetPredicate0("fail", func(*internal.VM, internal.Cont, *internal.Env) *internal.Promise {
		return internal.Bool(false)
	})
	m.SetPredicate2("a", func(vm *internal.VM, x, y Term, k internal.Cont, env *internal.Env) *internal.Promise {
		a, f := internal.NewAtom("$a"), internal.NewAtom("f")
		return internal.Delay(func(context.Context) *internal.Promise {
			return Unify(vm, a.Apply(x, y), a.Apply(Integer(1), f.Apply(internal.NewVariable())), k, env)
		}, func(context.Context) *internal.Promise {
			return Unify(vm, a.Apply(x, y), a.Apply(Integer(2), f.Apply(internal.NewVariable())), k, env)
		})
	})
	m.SetPredicate2("b", func(vm *internal.VM, x, y Term, k internal.Cont, env *internal.Env) *internal.Promise {
		b := internal.NewAtom("$b")
		return internal.Delay(func(context.Context) *internal.Promise {
			return Unify(vm, b.Apply(x, y), b.Apply(Integer(1), Integer(1)), k, env)
		}, func(context.Context) *internal.Promise {
			return Unify(vm, b.Apply(x, y), b.Apply(Integer(1), Integer(1)), k, env)
		}, func(context.Context) *internal.Promise {
			return Unify(vm, b.Apply(x, y), b.Apply(Integer(1), Integer(2)), k, env)
		}, func(context.Context) *internal.Promise {
			return Unify(vm, b.Apply(x, y), b.Apply(Integer(2), Integer(1)), k, env)
		}, func(context.Context) *internal.Promise {
			return Unify(vm, b.Apply(x, y), b.Apply(Integer(2), Integer(2)), k, env)
		}, func(context.Context) *internal.Promise {
			return Unify(vm, b.Apply(x, y), b.Apply(Integer(2), Integer(2)), k, env)
		})
	})
	m.SetPredicate2("d", func(vm *internal.VM, x, y Term, k internal.Cont, env *internal.Env) *internal.Promise {
		d := internal.NewAtom("$d")
		return internal.Delay(func(context.Context) *internal.Promise {
			return Unify(vm, d.Apply(x, y), d.Apply(Integer(1), Integer(1)), k, env)
		}, func(context.Context) *internal.Promise {
			return Unify(vm, d.Apply(x, y), d.Apply(Integer(1), Integer(2)), k, env)
		}, func(context.Context) *internal.Promise {
			return Unify(vm, d.Apply(x, y), d.Apply(Integer(1), Integer(1)), k, env)
		}, func(context.Context) *internal.Promise {
			return Unify(vm, d.Apply(x, y), d.Apply(Integer(2), Integer(2)), k, env)
		}, func(context.Context) *internal.Promise {
			return Unify(vm, d.Apply(x, y), d.Apply(Integer(2), Integer(1)), k, env)
		}, func(context.Context) *internal.Promise {
			return Unify(vm, d.Apply(x, y), d.Apply(Integer(2), Integer(2)), k, env)
		})
	})
	m.SetPredicate2("member", func(vm *internal.VM, elem, list Term, k internal.Cont, env *internal.Env) *internal.Promise {
		var ks []func(context.Context) *internal.Promise
		iter := internal.ListIterator{List: list, Env: env, AllowPartial: true}
		for iter.Next() {
			e := iter.Current()
			ks = append(ks, func(context.Context) *internal.Promise {
				return Unify(vm, elem, e, k, env)
			})
		}
		if err := iter.Err(); err != nil {
			return internal.Error(err)
		}
		return internal.Delay(ks...)
	})
	m.SetPredicate3("setof", SetOf)
	m.SetPredicate3("bagof", BagOf)
	vm := internal.VM{
		typeIn: atomUser,
		modules: map[internal.Atom]*internal.module{
			atomUser: &m,
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			defer setMemFree(tt.mem)()

			vm.Unknown = func(internal.Atom, []Term, *internal.Env) {
				assert.True(t, tt.warning)
			}
			_, err := SetOf(&vm, tt.template, tt.goal, tt.instances, func(env *internal.Env) *internal.Promise {
				for k, v := range tt.env[0] {
					_, ok := env.Unify(v, k)
					assert.True(t, ok)
				}
				tt.env = tt.env[1:]
				return internal.Bool(false)
			}, nil).Force(context.Background())
			assert.Equal(t, tt.err, err)
			assert.Empty(t, tt.env)
		})
	}
}

func TestFindAll(t *testing.T) {
	x, y := internal.NewVariable(), internal.NewVariable()
	s := internal.NewVariable()
	l := internal.NewVariable()
	goal := internal.NewVariable()

	tests := []struct {
		title                     string
		template, goal, instances Term
		ok                        bool
		err                       error
		env                       map[internal.Variable]Term
		mem                       int64
	}{
		// 8.10.1.4 Examples
		{title: "1", template: x, goal: atomSemiColon.Apply(atomEqual.Apply(x, Integer(1)), atomEqual.Apply(x, Integer(2))), instances: s, ok: true, env: map[internal.Variable]Term{
			s: List(Integer(1), Integer(2)),
		}},
		{title: "2", template: atomPlus.Apply(x, y), goal: atomEqual.Apply(x, Integer(1)), instances: s, ok: true, env: map[internal.Variable]Term{
			s: List(atomPlus.Apply(Integer(1), internal.NewVariable())),
		}},
		{title: "3", template: x, goal: atomFail, instances: l, ok: true, env: map[internal.Variable]Term{
			l: List(),
		}},
		{title: "4", template: x, goal: atomSemiColon.Apply(atomEqual.Apply(x, Integer(1)), atomEqual.Apply(x, Integer(1))), instances: s, ok: true, env: map[internal.Variable]Term{
			s: List(Integer(1), Integer(1)),
		}},
		{title: "5", template: x, goal: atomSemiColon.Apply(atomEqual.Apply(x, Integer(2)), atomEqual.Apply(x, Integer(1))), instances: List(Integer(1), Integer(2)), ok: false},
		{title: "6", template: x, goal: goal, instances: s, err: InstantiationError(nil)},
		{title: "7", template: x, goal: Integer(4), instances: s, err: typeError(validTypeCallable, Integer(4), nil)},

		// 8.10.1.3 Errors
		{title: "c", template: x, goal: atomSemiColon.Apply(atomEqual.Apply(x, Integer(1)), atomEqual.Apply(x, Integer(2))), instances: internal.NewAtom("foo"), err: typeError(validTypeList, internal.NewAtom("foo"), nil)},

		{
			title:     "out of memory",
			template:  tuple(internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable()),
			goal:      atomEqual.Apply(x, Integer(1)),
			instances: s,
			err:       internal.Exception{term: atomError.Apply(atomResourceError.Apply(resourceMemory.Term()), atomSlash.Apply(atomEqual, Integer(2)))},
			mem:       1,
		},
	}

	var m internal.module
	m.SetPredicate2("=", Unify)
	m.SetPredicate2(";", func(vm *internal.VM, g1, g2 Term, k internal.Cont, env *internal.Env) *internal.Promise {
		return internal.Delay(func(context.Context) *internal.Promise {
			return Call(vm, g1, k, env)
		}, func(context.Context) *internal.Promise {
			return Call(vm, g2, k, env)
		})
	})
	m.SetPredicate0("fail", func(*internal.VM, internal.Cont, *internal.Env) *internal.Promise {
		return internal.Bool(false)
	})
	vm := internal.VM{
		typeIn: atomUser,
		modules: map[internal.Atom]*internal.module{
			atomUser: &m,
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			defer setMemFree(tt.mem)()

			ok, err := FindAll(&vm, tt.template, tt.goal, tt.instances, func(env *internal.Env) *internal.Promise {
				for k, v := range tt.env {
					_, ok := env.Unify(v, k)
					assert.True(t, ok)
				}
				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestCompare(t *testing.T) {
	order := internal.NewVariable()

	tests := []struct {
		title       string
		order, x, y Term
		ok          bool
		err         error
		env         map[internal.Variable]Term
	}{
		// 8.4.2.4 Examples
		{title: `compare(Order, 3, 5).`, order: order, x: Integer(3), y: Integer(5), ok: true, env: map[internal.Variable]Term{
			order: atomLessThan,
		}},
		{title: `compare(Order, d, d).`, order: order, x: internal.NewAtom("d"), y: internal.NewAtom("d"), ok: true, env: map[internal.Variable]Term{
			order: atomEqual,
		}},
		{title: `compare(Order, Order, <).`, order: order, x: order, y: atomLessThan, ok: true, env: map[internal.Variable]Term{
			order: atomLessThan,
		}},
		{title: `compare(<, <, <).`, order: atomLessThan, x: atomLessThan, y: atomLessThan, ok: false},
		{title: `compare(1+2, 3, 3.0).`, order: atomPlus.Apply(Integer(1), Integer(2)), x: Integer(3), y: Float(3.0), ok: false, err: typeError(validTypeAtom, atomPlus.Apply(Integer(1), Integer(2)), nil)},
		{title: `compare(>=, 3, 3.0).`, order: internal.NewAtom(">="), x: Integer(3), y: Float(3.0), ok: false, err: domainError(validDomainOrder, internal.NewAtom(">="), nil)},

		{title: `missing case for >`, order: atomGreaterThan, x: Integer(2), y: Integer(1), ok: true},
	}

	for _, tt := range tests {
		ok, err := Compare(nil, tt.order, tt.x, tt.y, func(env *internal.Env) *internal.Promise {
			for k, v := range tt.env {
				assert.Equal(t, v, env.Resolve(k))
			}
			return internal.Bool(true)
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
			value := internal.NewVariable()
			ok, err := Between(nil, Integer(1), Integer(1), value, func(env *internal.Env) *internal.Promise {
				assert.Equal(t, Integer(1), env.Resolve(value))
				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("lower and upper are MaxInt64", func(t *testing.T) {
			value := internal.NewVariable()
			ok, err := Between(nil, Integer(math.MaxInt64), Integer(math.MaxInt64), value, func(env *internal.Env) *internal.Promise {
				assert.Equal(t, Integer(math.MaxInt64), env.Resolve(value))
				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("multiple choice points", func(t *testing.T) {
			var n int
			value := internal.NewVariable()
			ok, err := Between(nil, Integer(0), Integer(3), value, func(env *internal.Env) *internal.Promise {
				assert.Equal(t, Integer(n), env.Resolve(value))
				n++
				return internal.Bool(false)
			}, nil).Force(context.Background())
			assert.Equal(t, n, 4)
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("lower > upper", func(t *testing.T) {
			value := internal.NewVariable()
			ok, err := Between(nil, Integer(3), Integer(0), value, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("lower is uninstantiated", func(t *testing.T) {
		_, err := Between(nil, internal.NewVariable(), Integer(2), Integer(1), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
	})

	t.Run("upper is uninstantiated", func(t *testing.T) {
		_, err := Between(nil, Integer(1), internal.NewVariable(), Integer(1), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
	})

	t.Run("lower is not an integer", func(t *testing.T) {
		_, err := Between(nil, internal.NewAtom("inf"), Integer(2), Integer(1), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, internal.NewAtom("inf"), nil), err)
	})

	t.Run("upper is not an integer", func(t *testing.T) {
		_, err := Between(nil, Integer(1), internal.NewAtom("inf"), Integer(1), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, internal.NewAtom("inf"), nil), err)
	})

	t.Run("value is not an integer or variable", func(t *testing.T) {
		_, err := Between(nil, Integer(1), Integer(1), internal.NewAtom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, internal.NewAtom("foo"), nil), err)
	})
}

func TestSort(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		t.Run("variable", func(t *testing.T) {
			sorted := internal.NewVariable()
			ok, err := Sort(nil, List(internal.NewAtom("a"), internal.NewAtom("c"), internal.NewAtom("b"), internal.NewAtom("a")), sorted, func(env *internal.Env) *internal.Promise {
				assert.Equal(t, List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), env.Resolve(sorted))
				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("list", func(t *testing.T) {
			ok, err := Sort(nil, List(internal.NewAtom("a"), internal.NewAtom("c"), internal.NewAtom("b"), internal.NewAtom("a")), List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("list is a partial list", func(t *testing.T) {
		_, err := Sort(nil, PartialList(internal.NewVariable(), internal.NewAtom("a"), internal.NewAtom("b")), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
	})

	t.Run("list is neither a partial list nor a list", func(t *testing.T) {
		_, err := Sort(nil, internal.NewAtom("a"), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeList, internal.NewAtom("a"), nil), err)
	})

	t.Run("sorted is neither a partial list nor a list", func(t *testing.T) {
		t.Run("obviously not a list", func(t *testing.T) {
			_, err := Sort(nil, List(internal.NewAtom("a")), internal.NewAtom("a"), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeList, internal.NewAtom("a"), nil), err)
		})

		t.Run("list-ish", func(t *testing.T) {
			_, err := Sort(nil, List(internal.NewAtom("a")), &compound{functor: atomDot, args: []Term{internal.NewAtom("a")}}, Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeList, &compound{functor: atomDot, args: []Term{internal.NewAtom("a")}}, nil), err)
		})
	})
}

func TestKeySort(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		t.Run("variable", func(t *testing.T) {
			sorted := internal.NewVariable()
			ok, err := KeySort(nil, List(
				pair(internal.NewAtom("c"), internal.NewAtom("4")),
				pair(internal.NewAtom("b"), internal.NewAtom("3")),
				pair(internal.NewAtom("a"), internal.NewAtom("1")),
				pair(internal.NewAtom("a"), internal.NewAtom("2")),
			), sorted, func(env *internal.Env) *internal.Promise {
				assert.Equal(t, List(
					pair(internal.NewAtom("a"), internal.NewAtom("1")),
					pair(internal.NewAtom("a"), internal.NewAtom("2")),
					pair(internal.NewAtom("b"), internal.NewAtom("3")),
					pair(internal.NewAtom("c"), internal.NewAtom("4")),
				), env.Resolve(sorted))
				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("list", func(t *testing.T) {
			second := internal.NewVariable()
			ok, err := KeySort(nil, List(
				pair(internal.NewAtom("c"), internal.NewAtom("4")),
				pair(internal.NewAtom("b"), internal.NewAtom("3")),
				pair(internal.NewAtom("a"), internal.NewAtom("1")),
				pair(internal.NewAtom("a"), internal.NewAtom("2")),
			), List(
				pair(internal.NewAtom("a"), internal.NewAtom("1")),
				second,
				pair(internal.NewAtom("b"), internal.NewAtom("3")),
				pair(internal.NewAtom("c"), internal.NewAtom("4")),
			), func(env *internal.Env) *internal.Promise {
				assert.Equal(t, pair(internal.NewAtom("a"), internal.NewAtom("2")), env.Resolve(second))
				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("pairs is a partial list", func(t *testing.T) {
		_, err := KeySort(nil, PartialList(internal.NewVariable(), pair(internal.NewAtom("a"), Integer(1))), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
	})

	t.Run("pairs is neither a partial list nor a list", func(t *testing.T) {
		_, err := KeySort(nil, internal.NewAtom("a"), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeList, internal.NewAtom("a"), nil), err)
	})

	t.Run("sorted is neither a partial list nor a list", func(t *testing.T) {
		_, err := KeySort(nil, List(), internal.NewAtom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeList, internal.NewAtom("foo"), nil), err)
	})

	t.Run("an element of a list prefix of pairs is a variable", func(t *testing.T) {
		_, err := KeySort(nil, List(internal.NewVariable()), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
	})

	t.Run("an element of a list prefix of pairs is neither a variable nor a compound term with principal functor (-)/2", func(t *testing.T) {
		t.Run("atomic", func(t *testing.T) {
			_, err := KeySort(nil, List(internal.NewAtom("foo")), internal.NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypePair, internal.NewAtom("foo"), nil), err)
		})

		t.Run("compound", func(t *testing.T) {
			_, err := KeySort(nil, List(internal.NewAtom("f").Apply(internal.NewAtom("a"))), internal.NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypePair, internal.NewAtom("f").Apply(internal.NewAtom("a")), nil), err)
		})
	})

	t.Run("an element of a list prefix of sorted is neither a variable nor a compound term with principal functor (-)/2", func(t *testing.T) {
		t.Run("atomic", func(t *testing.T) {
			_, err := KeySort(nil, List(), List(internal.NewAtom("foo")), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypePair, internal.NewAtom("foo"), nil), err)
		})

		t.Run("compound", func(t *testing.T) {
			_, err := KeySort(nil, List(), List(internal.NewAtom("f").Apply(internal.NewAtom("a"))), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypePair, internal.NewAtom("f").Apply(internal.NewAtom("a")), nil), err)
		})
	})
}

func TestThrow(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		ok, err := Throw(nil, internal.NewAtom("a"), Success, nil).Force(context.Background())
		assert.Equal(t, internal.Exception{term: internal.NewAtom("a")}, err)
		assert.False(t, ok)
	})

	t.Run("ball is a variable", func(t *testing.T) {
		ok, err := Throw(nil, internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})
}

func TestCatch(t *testing.T) {
	var m internal.module
	m.SetPredicate2("=", Unify)
	m.SetPredicate1("throw", Throw)
	m.SetPredicate0("true", func(_ *internal.VM, k internal.Cont, env *internal.Env) *internal.Promise {
		return k(env)
	})
	m.SetPredicate0("fail", func(*internal.VM, internal.Cont, *internal.Env) *internal.Promise {
		return internal.Bool(false)
	})
	vm := internal.VM{
		typeIn: atomUser,
		modules: map[internal.Atom]*internal.module{
			atomUser: &m,
		},
	}

	t.Run("match", func(t *testing.T) {
		v := internal.NewVariable()
		ok, err := Catch(&vm, &compound{
			functor: internal.NewAtom("throw"),
			args:    []Term{internal.NewAtom("a")},
		}, v, &compound{
			functor: atomEqual,
			args:    []Term{v, internal.NewAtom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("not match", func(t *testing.T) {
		ok, err := Catch(&vm, &compound{
			functor: internal.NewAtom("throw"),
			args:    []Term{internal.NewAtom("a")},
		}, internal.NewAtom("b"), atomFail, Success, nil).Force(context.Background())
		assert.False(t, ok)
		ex, ok := err.(internal.Exception)
		assert.True(t, ok)
		assert.Equal(t, internal.NewAtom("a"), ex.term)
	})

	t.Run("true", func(t *testing.T) {
		ok, err := Catch(&vm, atomTrue, internal.NewAtom("b"), atomFail, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("false", func(t *testing.T) {
		ok, err := Catch(&vm, atomFail, internal.NewAtom("b"), atomFail, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("non-exception error", func(t *testing.T) {
		ok, err := Catch(&vm, atomTrue, internal.NewVariable(), atomTrue, func(env *internal.Env) *internal.Promise {
			return internal.Error(errors.New("failed"))
		}, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestCurrentPredicate(t *testing.T) {
	t.Run("user defined predicate", func(t *testing.T) {
		vm := internal.VM{
			typeIn: atomUser,
			modules: map[internal.Atom]*internal.module{
				atomUser: {
					procedures: map[predicateIndicator]internal.procedureEntry{
						{name: internal.NewAtom("foo"), arity: 1}: {procedure: internal.clauses{}},
					},
				},
			},
		}
		ok, err := CurrentPredicate(&vm, &compound{
			functor: atomSlash,
			args: []Term{
				internal.NewAtom("foo"),
				Integer(1),
			},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("variable", func(t *testing.T) {
		var foo, bar, baz bool

		v := internal.NewVariable()

		vm := internal.VM{
			typeIn: atomUser,
			modules: map[internal.Atom]*internal.module{
				atomUser: {
					procedures: map[predicateIndicator]internal.procedureEntry{
						{name: internal.NewAtom("foo"), arity: 1}: {procedure: internal.clauses{}},
						{name: internal.NewAtom("bar"), arity: 1}: {procedure: internal.clauses{}},
						{name: internal.NewAtom("baz"), arity: 1}: {procedure: internal.clauses{}},
					},
				},
			},
		}
		ok, err := CurrentPredicate(&vm, v, func(env *internal.Env) *internal.Promise {
			c, ok := env.Resolve(v).(*compound)
			assert.True(t, ok)
			assert.Equal(t, atomSlash, c.Functor())
			assert.Equal(t, 2, c.Arity())
			assert.Equal(t, Integer(1), c.Arg(1))
			switch c.Arg(0) {
			case internal.NewAtom("foo"):
				foo = true
			case internal.NewAtom("bar"):
				bar = true
			case internal.NewAtom("baz"):
				baz = true
			default:
				assert.Fail(t, "unreachable")
			}
			return internal.Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)

		assert.True(t, foo)
		assert.True(t, bar)
		assert.True(t, baz)
	})

	t.Run("builtin predicate", func(t *testing.T) {
		vm := internal.VM{
			typeIn: atomUser,
			modules: map[internal.Atom]*internal.module{
				atomUser: {
					procedures: map[predicateIndicator]internal.procedureEntry{
						{name: atomEqual, arity: 2}: {procedure: Predicate2(Unify)},
					},
				},
			},
		}
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
			var vm internal.VM
			ok, err := CurrentPredicate(&vm, internal.NewAtom("foo"), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypePredicateIndicator, internal.NewAtom("foo"), nil), err)
			assert.False(t, ok)
		})

		t.Run("compound", func(t *testing.T) {
			t.Run("non slash", func(t *testing.T) {
				var vm internal.VM
				ok, err := CurrentPredicate(&vm, &compound{
					functor: internal.NewAtom("f"),
					args:    []Term{internal.NewAtom("a")},
				}, Success, nil).Force(context.Background())
				assert.Equal(t, typeError(validTypePredicateIndicator, &compound{
					functor: internal.NewAtom("f"),
					args:    []Term{internal.NewAtom("a")},
				}, nil), err)
				assert.False(t, ok)
			})

			t.Run("slash but number", func(t *testing.T) {
				var vm internal.VM
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
				var vm internal.VM
				ok, err := CurrentPredicate(&vm, &compound{
					functor: atomSlash,
					args:    []Term{internal.NewAtom("foo"), internal.NewAtom("bar")},
				}, Success, nil).Force(context.Background())
				assert.Equal(t, typeError(validTypePredicateIndicator, &compound{
					functor: atomSlash,
					args:    []Term{internal.NewAtom("foo"), internal.NewAtom("bar")},
				}, nil), err)
				assert.False(t, ok)
			})
		})
	})
}

func TestAssertz(t *testing.T) {
	t.Run("append", func(t *testing.T) {
		var vm internal.VM

		ok, err := Assertz(&vm, &compound{
			functor: internal.NewAtom("foo"),
			args:    []Term{internal.NewAtom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Assertz(&vm, &compound{
			functor: internal.NewAtom("foo"),
			args:    []Term{internal.NewAtom("b")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, internal.procedureEntry{dynamic: true, procedure: internal.clauses{
			{
				pi: predicateIndicator{
					name:  internal.NewAtom("foo"),
					arity: 1,
				},
				raw: &compound{
					functor: internal.NewAtom("foo"),
					args:    []Term{internal.NewAtom("a")},
				},
				bytecode: internal.bytecode{
					{opcode: internal.opGetConst, operand: internal.NewAtom("a")},
					{opcode: internal.opExit},
				},
			},
			{
				pi: predicateIndicator{
					name:  internal.NewAtom("foo"),
					arity: 1,
				},
				raw: &compound{
					functor: internal.NewAtom("foo"),
					args:    []Term{internal.NewAtom("b")},
				},
				bytecode: internal.bytecode{
					{opcode: internal.opGetConst, operand: internal.NewAtom("b")},
					{opcode: internal.opExit},
				},
			},
		}}, vm.TypeInModule().procedures[predicateIndicator{
			name:  internal.NewAtom("foo"),
			arity: 1,
		}])
	})

	t.Run("clause is a variable", func(t *testing.T) {
		var vm internal.VM
		ok, err := Assertz(&vm, internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("clause is neither a variable, nor callable", func(t *testing.T) {
		var vm internal.VM
		ok, err := Assertz(&vm, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("head is a variable", func(t *testing.T) {
		var vm internal.VM
		ok, err := Assertz(&vm, &compound{
			functor: atomColonMinus,
			args:    []Term{internal.NewVariable(), atomTrue},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("head is neither a variable, nor callable", func(t *testing.T) {
		var vm internal.VM
		ok, err := Assertz(&vm, &compound{
			functor: atomColonMinus,
			args:    []Term{Integer(0), atomTrue},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("body contains a term which is not callable", func(t *testing.T) {
		var vm internal.VM
		ok, err := Assertz(&vm, &compound{
			functor: atomColonMinus,
			args: []Term{
				internal.NewAtom("foo"),
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
		vm := internal.VM{
			typeIn: atomUser,
			modules: map[internal.Atom]*internal.module{
				atomUser: {
					procedures: map[predicateIndicator]internal.procedureEntry{
						{name: internal.NewAtom("foo"), arity: 0}: {dynamic: false},
					},
				},
			},
		}

		ok, err := Assertz(&vm, internal.NewAtom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationModify, permissionTypeStaticProcedure, &compound{
			functor: atomSlash,
			args: []Term{
				internal.NewAtom("foo"),
				Integer(0),
			},
		}, nil), err)
		assert.False(t, ok)
	})
}

func TestAsserta(t *testing.T) {
	t.Run("fact", func(t *testing.T) {
		var vm internal.VM
		ok, err := Asserta(&vm, &compound{
			functor: internal.NewAtom("foo"),
			args:    []Term{internal.NewAtom("a")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Asserta(&vm, &compound{
			functor: internal.NewAtom("foo"),
			args:    []Term{internal.NewAtom("b")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, internal.procedureEntry{dynamic: true, procedure: internal.clauses{
			{
				pi: predicateIndicator{name: internal.NewAtom("foo"), arity: 1},
				raw: &compound{
					functor: internal.NewAtom("foo"),
					args:    []Term{internal.NewAtom("b")},
				},
				bytecode: internal.bytecode{
					{opcode: internal.opGetConst, operand: internal.NewAtom("b")},
					{opcode: internal.opExit},
				},
			},
			{
				pi: predicateIndicator{name: internal.NewAtom("foo"), arity: 1},
				raw: &compound{
					functor: internal.NewAtom("foo"),
					args:    []Term{internal.NewAtom("a")},
				},
				bytecode: internal.bytecode{
					{opcode: internal.opGetConst, operand: internal.NewAtom("a")},
					{opcode: internal.opExit},
				},
			},
		}}, vm.TypeInModule().procedures[predicateIndicator{name: internal.NewAtom("foo"), arity: 1}])
	})

	t.Run("rule", func(t *testing.T) {
		var vm internal.VM
		vm.typeIn = atomUser
		ok, err := Asserta(&vm, &compound{
			functor: atomColonMinus,
			args: []Term{
				internal.NewAtom("foo"),
				&compound{
					functor: internal.NewAtom("p"),
					args:    []Term{internal.NewAtom("b")},
				},
			},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = Asserta(&vm, atomColonMinus.Apply(
			internal.NewAtom("foo"),
			atomComma.Apply(
				internal.NewAtom("p").Apply(internal.NewAtom("a")),
				atomCut,
			),
		), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, internal.procedureEntry{dynamic: true, procedure: internal.clauses{
			{
				pi: predicateIndicator{name: internal.NewAtom("foo"), arity: 0},
				raw: &compound{
					functor: atomColonMinus,
					args: []Term{
						internal.NewAtom("foo"),
						&compound{
							functor: atomComma,
							args: []Term{
								&compound{
									functor: internal.NewAtom("p"),
									args:    []Term{internal.NewAtom("a")},
								},
								atomCut,
							},
						},
					},
				},
				bytecode: internal.bytecode{
					{opcode: internal.opEnter},
					{opcode: internal.opPutConst, operand: internal.NewAtom("a")},
					{opcode: internal.opCall, operand: qualifiedPredicateIndicator{
						internal.module:    atomUser,
						predicateIndicator: predicateIndicator{name: internal.NewAtom("p"), arity: 1},
					}},
					{opcode: internal.opCut},
					{opcode: internal.opExit},
				},
			},
			{
				pi: predicateIndicator{name: internal.NewAtom("foo"), arity: 0},
				raw: &compound{
					functor: atomColonMinus,
					args: []Term{
						internal.NewAtom("foo"),
						&compound{
							functor: internal.NewAtom("p"),
							args:    []Term{internal.NewAtom("b")},
						},
					},
				},
				bytecode: internal.bytecode{
					{opcode: internal.opEnter},
					{opcode: internal.opPutConst, operand: internal.NewAtom("b")},
					{opcode: internal.opCall, operand: qualifiedPredicateIndicator{
						internal.module:    atomUser,
						predicateIndicator: predicateIndicator{name: internal.NewAtom("p"), arity: 1},
					}},
					{opcode: internal.opExit},
				},
			},
		}}, vm.TypeInModule().procedures[predicateIndicator{name: internal.NewAtom("foo"), arity: 0}])
	})

	t.Run("clause is a variable", func(t *testing.T) {
		var vm internal.VM
		ok, err := Asserta(&vm, internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("clause is neither a variable, nor callable", func(t *testing.T) {
		var vm internal.VM
		ok, err := Asserta(&vm, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("head is a variable", func(t *testing.T) {
		var vm internal.VM
		ok, err := Asserta(&vm, &compound{
			functor: atomColonMinus,
			args:    []Term{internal.NewVariable(), atomTrue},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("head is neither a variable, nor callable", func(t *testing.T) {
		var vm internal.VM
		ok, err := Asserta(&vm, &compound{
			functor: atomColonMinus,
			args:    []Term{Integer(0), atomTrue},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("body is not callable", func(t *testing.T) {
		var vm internal.VM
		ok, err := Asserta(&vm, &compound{
			functor: atomColonMinus,
			args:    []Term{internal.NewAtom("foo"), Integer(0)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("body contains a term which is not callable", func(t *testing.T) {
		var vm internal.VM
		ok, err := Asserta(&vm, &compound{
			functor: atomColonMinus,
			args: []Term{
				internal.NewAtom("foo"),
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
		vm := internal.VM{
			typeIn: atomUser,
			modules: map[internal.Atom]*internal.module{
				atomUser: {
					procedures: map[predicateIndicator]internal.procedureEntry{
						{name: internal.NewAtom("foo"), arity: 0}: {dynamic: false},
					},
				},
			},
		}

		ok, err := Asserta(&vm, internal.NewAtom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationModify, permissionTypeStaticProcedure, &compound{
			functor: atomSlash,
			args: []Term{
				internal.NewAtom("foo"),
				Integer(0),
			},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("cut", func(t *testing.T) {
		var vm internal.VM
		ok, err := Asserta(&vm, &compound{
			functor: atomColonMinus,
			args: []Term{
				internal.NewAtom("foo"),
				atomCut,
			},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})
}

func TestRetract(t *testing.T) {
	t.Run("retract the first one", func(t *testing.T) {
		vm := internal.VM{
			typeIn: atomUser,
			modules: map[internal.Atom]*internal.module{
				atomUser: {
					procedures: map[predicateIndicator]internal.procedureEntry{
						{name: internal.NewAtom("foo"), arity: 1}: {dynamic: true, procedure: internal.clauses{
							{raw: &compound{functor: internal.NewAtom("foo"), args: []Term{internal.NewAtom("a")}}},
							{raw: &compound{functor: internal.NewAtom("foo"), args: []Term{internal.NewAtom("b")}}},
							{raw: &compound{functor: internal.NewAtom("foo"), args: []Term{internal.NewAtom("c")}}},
						}},
					},
				},
			},
		}

		ok, err := Retract(&vm, &compound{
			functor: internal.NewAtom("foo"),
			args:    []Term{internal.NewVariable()},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, internal.procedureEntry{dynamic: true, procedure: internal.clauses{
			{raw: &compound{functor: internal.NewAtom("foo"), args: []Term{internal.NewAtom("b")}}},
			{raw: &compound{functor: internal.NewAtom("foo"), args: []Term{internal.NewAtom("c")}}},
		}}, vm.TypeInModule().procedures[predicateIndicator{name: internal.NewAtom("foo"), arity: 1}])
	})

	t.Run("retract the specific one", func(t *testing.T) {
		vm := internal.VM{
			typeIn: atomUser,
			modules: map[internal.Atom]*internal.module{
				atomUser: {
					procedures: map[predicateIndicator]internal.procedureEntry{
						{name: internal.NewAtom("foo"), arity: 1}: {dynamic: true, procedure: internal.clauses{
							{raw: &compound{functor: internal.NewAtom("foo"), args: []Term{internal.NewAtom("a")}}},
							{raw: &compound{functor: internal.NewAtom("foo"), args: []Term{internal.NewAtom("b")}}},
							{raw: &compound{functor: internal.NewAtom("foo"), args: []Term{internal.NewAtom("c")}}},
						}},
					},
				},
			},
		}

		ok, err := Retract(&vm, &compound{
			functor: internal.NewAtom("foo"),
			args:    []Term{internal.NewAtom("b")},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, internal.procedureEntry{dynamic: true, procedure: internal.clauses{
			{raw: &compound{functor: internal.NewAtom("foo"), args: []Term{internal.NewAtom("a")}}},
			{raw: &compound{functor: internal.NewAtom("foo"), args: []Term{internal.NewAtom("c")}}},
		}}, vm.TypeInModule().procedures[predicateIndicator{name: internal.NewAtom("foo"), arity: 1}])
	})

	t.Run("retract all", func(t *testing.T) {
		vm := internal.VM{
			typeIn: atomUser,
			modules: map[internal.Atom]*internal.module{
				atomUser: {
					procedures: map[predicateIndicator]internal.procedureEntry{
						{name: internal.NewAtom("foo"), arity: 1}: {dynamic: true, procedure: internal.clauses{
							{raw: &compound{functor: internal.NewAtom("foo"), args: []Term{internal.NewAtom("a")}}},
							{raw: &compound{functor: internal.NewAtom("foo"), args: []Term{internal.NewAtom("b")}}},
							{raw: &compound{functor: internal.NewAtom("foo"), args: []Term{internal.NewAtom("c")}}},
						}},
					},
				},
			},
		}

		ok, err := Retract(&vm, &compound{
			functor: internal.NewAtom("foo"),
			args:    []Term{internal.NewVariable()},
		}, Failure, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
		assert.Empty(t, vm.TypeInModule().procedures[predicateIndicator{name: internal.NewAtom("foo"), arity: 1}].procedure.(internal.clauses))
	})

	t.Run("variable", func(t *testing.T) {
		var vm internal.VM
		ok, err := Retract(&vm, internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("not callable", func(t *testing.T) {
		var vm internal.VM
		ok, err := Retract(&vm, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("no clause matches", func(t *testing.T) {
		var vm internal.VM

		ok, err := Retract(&vm, &compound{
			functor: internal.NewAtom("foo"),
			args:    []Term{internal.NewVariable()},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("static", func(t *testing.T) {
		vm := internal.VM{
			typeIn: atomUser,
			modules: map[internal.Atom]*internal.module{
				atomUser: {
					procedures: map[predicateIndicator]internal.procedureEntry{
						{name: internal.NewAtom("foo"), arity: 0}: {dynamic: false},
					},
				},
			},
		}

		ok, err := Retract(&vm, internal.NewAtom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationModify, permissionTypeStaticProcedure, &compound{
			functor: atomSlash,
			args:    []Term{internal.NewAtom("foo"), Integer(0)},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("exception in continuation", func(t *testing.T) {
		vm := internal.VM{
			typeIn: atomUser,
			modules: map[internal.Atom]*internal.module{
				atomUser: {
					procedures: map[predicateIndicator]internal.procedureEntry{
						{name: internal.NewAtom("foo"), arity: 1}: internal.procedureEntry{dynamic: true, procedure: internal.clauses{
							{raw: &compound{functor: internal.NewAtom("foo"), args: []Term{internal.NewAtom("a")}}},
						}},
					},
				},
			},
		}

		ok, err := Retract(&vm, &compound{
			functor: internal.NewAtom("foo"),
			args:    []Term{internal.NewVariable()},
		}, func(_ *internal.Env) *internal.Promise {
			return internal.Error(errors.New("failed"))
		}, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)

		// removed
		assert.Empty(t, vm.TypeInModule().procedures[predicateIndicator{name: internal.NewAtom("foo"), arity: 1}].procedure.(internal.clauses))
	})
}

func TestAbolish(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		vm := internal.VM{
			typeIn: atomUser,
			modules: map[internal.Atom]*internal.module{
				atomUser: {
					procedures: map[predicateIndicator]internal.procedureEntry{
						{name: internal.NewAtom("foo"), arity: 1}: internal.procedureEntry{dynamic: true, procedure: internal.clauses{
							{raw: &compound{functor: internal.NewAtom("foo"), args: []Term{internal.NewAtom("a")}}},
							{raw: &compound{functor: internal.NewAtom("foo"), args: []Term{internal.NewAtom("b")}}},
							{raw: &compound{functor: internal.NewAtom("foo"), args: []Term{internal.NewAtom("c")}}},
						}},
					},
				},
			},
		}

		ok, err := Abolish(&vm, &compound{
			functor: atomSlash,
			args:    []Term{internal.NewAtom("foo"), Integer(1)},
		}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		_, ok = vm.TypeInModule().procedures[predicateIndicator{name: internal.NewAtom("foo"), arity: 1}]
		assert.False(t, ok)
	})

	t.Run("pi is a variable", func(t *testing.T) {
		var vm internal.VM
		ok, err := Abolish(&vm, internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and either Name or Arity is a variable", func(t *testing.T) {
		t.Run("Name is a variable", func(t *testing.T) {
			var vm internal.VM
			ok, err := Abolish(&vm, &compound{
				functor: atomSlash,
				args:    []Term{internal.NewVariable(), Integer(2)},
			}, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		})

		t.Run("Arity is a variable", func(t *testing.T) {
			var vm internal.VM
			ok, err := Abolish(&vm, &compound{
				functor: atomSlash,
				args:    []Term{internal.NewAtom("foo"), internal.NewVariable()},
			}, Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("pi is neither a variable nor a predicate indicator", func(t *testing.T) {
		t.Run("compound", func(t *testing.T) {
			var vm internal.VM
			ok, err := Abolish(&vm, atomPlus.Apply(internal.NewAtom("foo"), Integer(1)), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypePredicateIndicator, atomPlus.Apply(internal.NewAtom("foo"), Integer(1)), nil), err)
			assert.False(t, ok)
		})

		t.Run("not a comnpound", func(t *testing.T) {
			var vm internal.VM
			ok, err := Abolish(&vm, Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypePredicateIndicator, Integer(0), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("pi is a term Name/Arity and Name is neither a variable nor an atom", func(t *testing.T) {
		var vm internal.VM
		ok, err := Abolish(&vm, &compound{
			functor: atomSlash,
			args:    []Term{Integer(0), Integer(2)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and Arity is neither a variable nor an integer", func(t *testing.T) {
		var vm internal.VM
		ok, err := Abolish(&vm, &compound{
			functor: atomSlash,
			args:    []Term{internal.NewAtom("foo"), internal.NewAtom("bar")},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, internal.NewAtom("bar"), nil), err)
		assert.False(t, ok)
	})

	t.Run("pi is a term Name/Arity and Arity is an integer less than zero", func(t *testing.T) {
		var vm internal.VM
		ok, err := Abolish(&vm, &compound{
			functor: atomSlash,
			args:    []Term{internal.NewAtom("foo"), Integer(-2)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainNotLessThanZero, Integer(-2), nil), err)
		assert.False(t, ok)
	})

	t.Run("The predicate indicator pi is that of a static procedure", func(t *testing.T) {
		vm := internal.VM{
			typeIn: atomUser,
			modules: map[internal.Atom]*internal.module{
				atomUser: {
					procedures: map[predicateIndicator]internal.procedureEntry{
						{name: internal.NewAtom("foo"), arity: 0}: internal.procedureEntry{dynamic: false},
					},
				},
			},
		}
		ok, err := Abolish(&vm, &compound{
			functor: atomSlash,
			args:    []Term{internal.NewAtom("foo"), Integer(0)},
		}, Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationModify, permissionTypeStaticProcedure, &compound{
			functor: atomSlash,
			args:    []Term{internal.NewAtom("foo"), Integer(0)},
		}, nil), err)
		assert.False(t, ok)
	})
}

func TestCurrentInput(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		var s internal.Stream
		vm := internal.VM{
			input: &s,
		}

		ok, err := CurrentInput(&vm, &s, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("stream is neither a variable nor a stream", func(t *testing.T) {
		var vm internal.VM
		ok, err := CurrentInput(&vm, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainStream, Integer(0), nil), err)
		assert.False(t, ok)
	})
}

func TestCurrentOutput(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		var s internal.Stream
		vm := internal.VM{
			output: &s,
		}

		ok, err := CurrentOutput(&vm, &s, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("stream is neither a variable nor a stream", func(t *testing.T) {
		var vm internal.VM
		ok, err := CurrentOutput(&vm, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainStream, Integer(0), nil), err)
		assert.False(t, ok)
	})
}

func TestSetInput(t *testing.T) {
	foo, bar := internal.NewAtom("foo"), internal.NewAtom("bar")
	input := internal.Stream{mode: internal.ioModeRead, alias: foo}
	output := internal.Stream{mode: internal.ioModeAppend}
	stream := internal.NewVariable()

	var vm internal.VM
	vm.streams.add(&input)

	tests := []struct {
		title         string
		streamOrAlias Term
		ok            bool
		err           error
		input         *internal.Stream
	}{
		{title: "stream", streamOrAlias: &input, ok: true, input: &input},
		{title: "alias", streamOrAlias: foo, ok: true, input: &input},

		// 8.11.3.3 Errors
		{title: "a", streamOrAlias: stream, err: InstantiationError(nil)},
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
	foo, bar := internal.NewAtom("foo"), internal.NewAtom("bar")
	input := internal.Stream{mode: internal.ioModeRead}
	output := internal.Stream{mode: internal.ioModeAppend, alias: foo}
	stream := internal.NewVariable()

	var vm internal.VM
	vm.streams.add(&output)

	tests := []struct {
		title         string
		streamOrAlias Term
		ok            bool
		err           error
		output        *internal.Stream
	}{
		{title: "stream", streamOrAlias: &output, ok: true, output: &output},
		{title: "alias", streamOrAlias: foo, ok: true, output: &output},

		// 8.11.4.3 Errors
		{title: "a", streamOrAlias: stream, err: InstantiationError(nil)},
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
	var vm internal.VM

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
			v := internal.NewVariable()
			ok, err := Open(&vm, internal.NewAtom(f.Name()), atomRead, v, List(
				atomAlias.Apply(atomInput),
			), func(env *internal.Env) *internal.Promise {
				ref, ok := env.lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*internal.Stream)
				assert.True(t, ok)

				l, ok := vm.streams.lookup(atomInput)
				assert.True(t, ok)
				assert.Equal(t, l, s)

				assert.NoError(t, s.initRead())
				b, err := io.ReadAll(s.buf)
				assert.NoError(t, err)
				assert.Equal(t, "test\n", string(b))

				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("type text", func(t *testing.T) {
			v := internal.NewVariable()
			ok, err := Open(&vm, internal.NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomType,
				args:    []Term{atomText},
			}), func(env *internal.Env) *internal.Promise {
				ref, ok := env.lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*internal.Stream)
				assert.True(t, ok)
				assert.Equal(t, internal.streamTypeText, s.streamType)
				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("type binary", func(t *testing.T) {
			v := internal.NewVariable()
			ok, err := Open(&vm, internal.NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomType,
				args:    []Term{atomBinary},
			}), func(env *internal.Env) *internal.Promise {
				ref, ok := env.lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*internal.Stream)
				assert.True(t, ok)
				assert.Equal(t, internal.streamTypeBinary, s.streamType)
				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("reposition true", func(t *testing.T) {
			v := internal.NewVariable()
			ok, err := Open(&vm, internal.NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomReposition,
				args:    []Term{atomTrue},
			}), func(env *internal.Env) *internal.Promise {
				ref, ok := env.lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*internal.Stream)
				assert.True(t, ok)
				assert.True(t, s.reposition)
				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("reposition true", func(t *testing.T) {
			v := internal.NewVariable()
			ok, err := Open(&vm, internal.NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomReposition,
				args:    []Term{atomFalse},
			}), func(env *internal.Env) *internal.Promise {
				ref, ok := env.lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*internal.Stream)
				assert.True(t, ok)
				assert.False(t, s.reposition)
				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("eof_action error", func(t *testing.T) {
			v := internal.NewVariable()
			ok, err := Open(&vm, internal.NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomEOFAction,
				args:    []Term{atomError},
			}), func(env *internal.Env) *internal.Promise {
				ref, ok := env.lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*internal.Stream)
				assert.True(t, ok)
				assert.Equal(t, internal.eofActionError, s.eofAction)
				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("eof_action eof_code", func(t *testing.T) {
			v := internal.NewVariable()
			ok, err := Open(&vm, internal.NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomEOFAction,
				args:    []Term{atomEOFCode},
			}), func(env *internal.Env) *internal.Promise {
				ref, ok := env.lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*internal.Stream)
				assert.True(t, ok)
				assert.Equal(t, internal.eofActionEOFCode, s.eofAction)
				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("eof_action reset", func(t *testing.T) {
			v := internal.NewVariable()
			ok, err := Open(&vm, internal.NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomEOFAction,
				args:    []Term{atomReset},
			}), func(env *internal.Env) *internal.Promise {
				ref, ok := env.lookup(v)
				assert.True(t, ok)
				s, ok := ref.(*internal.Stream)
				assert.True(t, ok)
				assert.Equal(t, internal.eofActionReset, s.eofAction)
				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("unknown option", func(t *testing.T) {
			v := internal.NewVariable()
			ok, err := Open(&vm, internal.NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomUnknown,
				args:    []Term{internal.NewAtom("option")},
			}), func(env *internal.Env) *internal.Promise {
				assert.Fail(t, "unreachable")
				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})

		t.Run("wrong arity", func(t *testing.T) {
			v := internal.NewVariable()
			ok, err := Open(&vm, internal.NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomType,
				args:    []Term{internal.NewAtom("a"), internal.NewAtom("b")},
			}), func(env *internal.Env) *internal.Promise {
				assert.Fail(t, "unreachable")
				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})

		t.Run("variable arg", func(t *testing.T) {
			v := internal.NewVariable()
			ok, err := Open(&vm, internal.NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomType,
				args:    []Term{internal.NewVariable()},
			}), func(env *internal.Env) *internal.Promise {
				assert.Fail(t, "unreachable")
				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})

		t.Run("non-atom arg", func(t *testing.T) {
			v := internal.NewVariable()
			ok, err := Open(&vm, internal.NewAtom(f.Name()), atomRead, v, List(&compound{
				functor: atomType,
				args:    []Term{Integer(0)},
			}), func(env *internal.Env) *internal.Promise {
				assert.Fail(t, "unreachable")
				return internal.Bool(true)
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

		v := internal.NewVariable()

		ok, err := Open(&vm, internal.NewAtom(n), atomWrite, v, List(&compound{
			functor: atomAlias,
			args:    []Term{atomOutput},
		}), func(env *internal.Env) *internal.Promise {
			ref, ok := env.lookup(v)
			assert.True(t, ok)
			s, ok := ref.(*internal.Stream)
			assert.True(t, ok)

			l, ok := vm.streams.lookup(atomOutput)
			assert.True(t, ok)
			assert.Equal(t, l, s)

			_, err := fmt.Fprintf(s.sink, "test\n")
			assert.NoError(t, err)

			f, err := os.Open(n)
			assert.NoError(t, err)
			defer func() {
				assert.NoError(t, f.Close())
			}()

			b, err := io.ReadAll(f)
			assert.NoError(t, err)
			assert.Equal(t, "test\n", string(b))

			return internal.Bool(true)
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

		v := internal.NewVariable()

		ok, err := Open(&vm, internal.NewAtom(f.Name()), atomAppend, v, List(&compound{
			functor: atomAlias,
			args:    []Term{atomAppend},
		}), func(env *internal.Env) *internal.Promise {
			ref, ok := env.lookup(v)
			assert.True(t, ok)
			s, ok := ref.(*internal.Stream)
			assert.True(t, ok)

			l, ok := vm.streams.lookup(atomAppend)
			assert.True(t, ok)
			assert.Equal(t, l, s)

			_, err = fmt.Fprintf(s.sink, "test\n")
			assert.NoError(t, err)

			f, err = os.Open(f.Name())
			assert.NoError(t, err)
			defer func() {
				assert.NoError(t, f.Close())
			}()

			b, err := io.ReadAll(f)
			assert.NoError(t, err)
			assert.Equal(t, "test\ntest\n", string(b))

			return internal.Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("sourceSink is a variable", func(t *testing.T) {
		var vm internal.VM
		ok, err := Open(&vm, internal.NewVariable(), atomRead, internal.NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("mode is a variable", func(t *testing.T) {
		var vm internal.VM
		ok, err := Open(&vm, internal.NewAtom("/dev/null"), internal.NewVariable(), internal.NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("options is a partial list or a list with an element E which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			var vm internal.VM
			ok, err := Open(&vm, internal.NewAtom("/dev/null"), atomRead, internal.NewVariable(), PartialList(internal.NewVariable(),
				atomType.Apply(atomText),
				atomAlias.Apply(internal.NewAtom("foo")),
			), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			var vm internal.VM
			ok, err := Open(&vm, internal.NewAtom("/dev/null"), atomRead, internal.NewVariable(), List(
				internal.NewVariable(),
				&compound{functor: atomType, args: []Term{atomText}},
				&compound{functor: atomAlias, args: []Term{internal.NewAtom("foo")}},
			), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("mode is neither a variable nor an atom", func(t *testing.T) {
		var vm internal.VM
		ok, err := Open(&vm, internal.NewAtom("/dev/null"), Integer(0), internal.NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		var vm internal.VM
		ok, err := Open(&vm, internal.NewAtom("/dev/null"), atomRead, internal.NewVariable(), internal.NewAtom("list"), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeList, internal.NewAtom("list"), nil), err)
		assert.False(t, ok)
	})

	t.Run("stream is not a variable", func(t *testing.T) {
		var vm internal.VM
		ok, err := Open(&vm, internal.NewAtom("/dev/null"), atomRead, internal.NewAtom("stream"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("sourceSink is neither a variable nor a source/sink", func(t *testing.T) {
		var vm internal.VM
		ok, err := Open(&vm, Integer(0), atomRead, internal.NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainSourceSink, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("mode is an atom but not an input/output mode", func(t *testing.T) {
		var vm internal.VM
		ok, err := Open(&vm, internal.NewAtom("/dev/null"), internal.NewAtom("foo"), internal.NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainIOMode, internal.NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the options list is neither a variable nor a stream-option", func(t *testing.T) {
		var vm internal.VM
		for _, o := range []Term{
			internal.NewAtom("foo"),
			&compound{functor: internal.NewAtom("foo"), args: []Term{internal.NewAtom("bar")}},
			&compound{functor: atomAlias, args: []Term{Integer(0)}},
			&compound{functor: atomType, args: []Term{Integer(0)}},
			&compound{functor: atomReposition, args: []Term{Integer(0)}},
			&compound{functor: atomEOFAction, args: []Term{Integer(0)}},
		} {
			ok, err := Open(&vm, internal.NewAtom("/dev/null"), atomRead, internal.NewVariable(), List(o), Success, nil).Force(context.Background())
			assert.Equal(t, domainError(validDomainStreamOption, o, nil), err)
			assert.False(t, ok)
		}
	})

	// Derived from 5.5.12 Options in Cor.3
	t.Run("a component of an element E of the options list is a variable", func(t *testing.T) {
		var vm internal.VM
		for _, o := range []Term{
			internal.NewVariable(),
			&compound{functor: atomAlias, args: []Term{internal.NewVariable()}},
			&compound{functor: atomType, args: []Term{internal.NewVariable()}},
			&compound{functor: atomReposition, args: []Term{internal.NewVariable()}},
			&compound{functor: atomEOFAction, args: []Term{internal.NewVariable()}},
		} {
			ok, err := Open(&vm, internal.NewAtom("/dev/null"), atomRead, internal.NewVariable(), List(o), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		}
	})

	t.Run("the source/sink specified by sourceSink does not exist", func(t *testing.T) {
		f, err := os.CreateTemp("", "open_test_existence")
		assert.NoError(t, err)
		assert.NoError(t, os.Remove(f.Name()))

		var vm internal.VM
		ok, err := Open(&vm, internal.NewAtom(f.Name()), atomRead, internal.NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, existenceError(objectTypeSourceSink, internal.NewAtom(f.Name()), nil), err)
		assert.False(t, ok)
	})

	t.Run("the source/sink specified by sourceSink cannot be opened", func(t *testing.T) {
		f, err := os.CreateTemp("", "open_test_permission")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, os.Remove(f.Name()))
		}()

		assert.NoError(t, f.Chmod(0200))

		var vm internal.VM
		ok, err := Open(&vm, internal.NewAtom(f.Name()), atomRead, internal.NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationOpen, permissionTypeSourceSink, internal.NewAtom(f.Name()), nil), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the options list is alias and A is already associated with an open stream", func(t *testing.T) {
		f, err := os.CreateTemp("", "open_test_dup_alias")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, os.Remove(f.Name()))
		}()

		var vm internal.VM
		vm.streams.add(&internal.Stream{alias: internal.NewAtom("foo")})
		ok, err := Open(&vm, internal.NewAtom(f.Name()), atomRead, internal.NewVariable(), List(&compound{
			functor: atomAlias,
			args:    []Term{internal.NewAtom("foo")},
		}), Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationOpen, permissionTypeSourceSink, &compound{
			functor: atomAlias,
			args:    []Term{internal.NewAtom("foo")},
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

		var vm internal.VM
		_, err := Open(&vm, internal.NewAtom("foo"), atomRead, internal.NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, errors.New("failed"), err)
	})
}

func TestClose(t *testing.T) {
	t.Run("without options", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			t.Run("stream", func(t *testing.T) {
				var m struct {
					internal.mockReader
					internal.mockCloser
				}
				m.mockCloser.On("Close").Return(nil).Once()
				defer m.mockCloser.AssertExpectations(t)

				var vm internal.VM
				ok, err := Close(&vm, &internal.Stream{source: &m}, List(), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("alias", func(t *testing.T) {
				var m struct {
					mockWriter
					internal.mockCloser
				}
				m.mockCloser.On("Close").Return(nil).Once()
				defer m.mockCloser.AssertExpectations(t)

				foo := internal.NewAtom("foo")

				var vm internal.VM
				vm.streams.add(&internal.Stream{sink: &m, alias: foo})
				ok, err := Close(&vm, foo, List(), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.True(t, ok)
			})
		})

		t.Run("ng", func(t *testing.T) {
			var m struct {
				internal.mockReader
				internal.mockCloser
			}
			m.mockCloser.On("Close").Return(errors.New("failed")).Once()
			defer m.mockCloser.AssertExpectations(t)

			var vm internal.VM
			_, err := Close(&vm, &internal.Stream{source: &m}, List(), Success, nil).Force(context.Background())
			assert.Equal(t, errors.New("failed"), err)
		})
	})

	t.Run("force false", func(t *testing.T) {
		var m struct {
			mockWriter
			internal.mockCloser
		}
		m.mockCloser.On("Close").Return(errors.New("failed")).Once()
		defer m.mockCloser.AssertExpectations(t)

		var vm internal.VM
		_, err := Close(&vm, &internal.Stream{sink: &m}, List(atomForce.Apply(atomFalse)), Success, nil).Force(context.Background())
		assert.Equal(t, errors.New("failed"), err)
	})

	t.Run("force true", func(t *testing.T) {
		var m struct {
			internal.mockReader
			internal.mockCloser
		}
		m.mockCloser.On("Close").Return(errors.New("failed")).Once()
		defer m.mockCloser.AssertExpectations(t)

		var vm internal.VM
		ok, err := Close(&vm, &internal.Stream{source: &m}, List(atomForce.Apply(atomTrue)), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		var m struct {
			internal.mockReader
			internal.mockCloser
		}
		m.mockCloser.On("Close").Return(nil).Once()
		defer m.mockCloser.AssertExpectations(t)

		foo := internal.NewAtom("foo")
		s := &internal.Stream{source: &m, alias: foo}

		var vm internal.VM
		vm.streams.add(s)
		ok, err := Close(&vm, internal.NewAtom("foo"), List(), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias ia a variable", func(t *testing.T) {
		var vm internal.VM
		ok, err := Close(&vm, internal.NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("options is a partial list or a list with an element E which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			var vm internal.VM
			ok, err := Close(&vm, &internal.Stream{}, PartialList(internal.NewVariable(),
				atomForce.Apply(atomTrue),
			), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			var vm internal.VM
			ok, err := Close(&vm, &internal.Stream{}, List(internal.NewVariable(), atomForce.Apply(atomTrue)), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		var vm internal.VM
		ok, err := Close(&vm, &internal.Stream{}, internal.NewAtom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeList, internal.NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream-term or alias", func(t *testing.T) {
		var vm internal.VM
		ok, err := Close(&vm, Integer(0), List(), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the Options list is neither a variable nor a stream-option", func(t *testing.T) {
		t.Run("not a compound", func(t *testing.T) {
			var vm internal.VM
			ok, err := Close(&vm, &internal.Stream{}, List(internal.NewAtom("foo")), Success, nil).Force(context.Background())
			assert.Equal(t, domainError(validDomainStreamOption, internal.NewAtom("foo"), nil), err)
			assert.False(t, ok)
		})

		t.Run("compound", func(t *testing.T) {
			t.Run("force but arity is not 1", func(t *testing.T) {
				var vm internal.VM
				ok, err := Close(&vm, &internal.Stream{}, List(atomForce.Apply(internal.NewAtom("a"), internal.NewAtom("b"))), Success, nil).Force(context.Background())
				assert.Equal(t, domainError(validDomainStreamOption, atomForce.Apply(internal.NewAtom("a"), internal.NewAtom("b")), nil), err)
				assert.False(t, ok)
			})

			t.Run("force but the argument is a variable", func(t *testing.T) {
				var vm internal.VM
				_, err := Close(&vm, &internal.Stream{}, List(atomForce.Apply(internal.NewVariable())), Success, nil).Force(context.Background())
				_, ok := NewEnv().Unify(domainError(validDomainStreamOption, atomForce.Apply(internal.NewVariable()), nil).term, err.(internal.Exception).term)
				assert.True(t, ok)
			})

			t.Run("force but the argument is neither true nor false", func(t *testing.T) {
				var vm internal.VM
				ok, err := Close(&vm, &internal.Stream{}, List(atomForce.Apply(internal.NewAtom("meh"))), Success, nil).Force(context.Background())
				assert.Equal(t, domainError(validDomainStreamOption, atomForce.Apply(internal.NewAtom("meh")), nil), err)
				assert.False(t, ok)
			})
		})
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var vm internal.VM
		ok, err := Close(&vm, internal.NewAtom("foo"), List(), Success, nil).Force(context.Background())
		assert.Equal(t, existenceError(objectTypeStream, internal.NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})
}

func TestFlushOutput(t *testing.T) {
	f, err := os.CreateTemp("", "")
	assert.NoError(t, err)
	defer func() {
		assert.NoError(t, os.Remove(f.Name()))
	}()

	foo := internal.NewAtom("foo")
	s := &internal.Stream{sink: f, mode: internal.ioModeWrite, alias: foo}

	t.Run("ok", func(t *testing.T) {
		var vm internal.VM
		ok, err := FlushOutput(&vm, s, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("ng", func(t *testing.T) {
		var m struct {
			mockWriter
			internal.mockSyncer
		}
		m.mockSyncer.On("Sync").Return(errors.New("ng")).Once()
		defer m.mockSyncer.AssertExpectations(t)

		s := &internal.Stream{sink: &m, mode: internal.ioModeWrite}

		var vm internal.VM
		_, err := FlushOutput(&vm, s, Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		var vm internal.VM
		vm.streams.add(s)
		ok, err := FlushOutput(&vm, foo, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		var vm internal.VM
		ok, err := FlushOutput(&vm, internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream-term or alias", func(t *testing.T) {
		var vm internal.VM
		ok, err := FlushOutput(&vm, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var vm internal.VM
		ok, err := FlushOutput(&vm, internal.NewAtom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, existenceError(objectTypeStream, internal.NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("SorA is an input stream", func(t *testing.T) {
		s := &internal.Stream{source: os.Stdin}

		var vm internal.VM
		ok, err := FlushOutput(&vm, s, Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationOutput, permissionTypeStream, s, nil), err)
		assert.False(t, ok)
	})
}

func TestWriteTerm(t *testing.T) {
	var buf bytes.Buffer
	w := &internal.Stream{sink: &buf, mode: internal.ioModeWrite}
	r := &internal.Stream{sink: &buf, mode: internal.ioModeRead}
	b := &internal.Stream{sink: &buf, mode: internal.ioModeWrite, streamType: internal.streamTypeBinary}

	B := internal.NewVariable()
	s := internal.NewVariable()
	x := internal.NewVariable()
	l := internal.NewVariable()
	e := internal.NewVariable()
	n, v := internal.NewVariable(), internal.NewVariable()

	err := errors.New("failed")

	var m mockWriter
	m.On("Write", mock.Anything).Return(0, err)

	mw := &internal.Stream{sink: &m, mode: internal.ioModeWrite}

	tests := []struct {
		title               string
		sOrA, term, options Term
		env                 *internal.Env
		ok                  bool
		err                 error
		output              string
		outputPattern       *regexp.Regexp
	}{
		// 8.14.2.4 Examples
		{title: `write_term(S, [1,2,3], []).`, sOrA: w, term: List(Integer(1), Integer(2), Integer(3)), options: List(), ok: true, output: `[1,2,3]`},
		{title: `write_canonical([1,2,3]).`, sOrA: w, term: List(Integer(1), Integer(2), Integer(3)), options: List(atomQuoted.Apply(atomTrue), atomIgnoreOps.Apply(atomTrue)), ok: true, output: `'.'(1,'.'(2,'.'(3,[])))`},
		{title: `write_term(S, '1<2', []).`, sOrA: w, term: internal.NewAtom("1<2"), options: List(), ok: true, output: `1<2`},
		{title: `writeq(S, '1<2').`, sOrA: w, term: internal.NewAtom("1<2"), options: List(atomQuoted.Apply(atomTrue), atomNumberVars.Apply(atomTrue)), ok: true, output: `'1<2'`},
		{title: `writeq('$VAR'(0)).`, sOrA: w, term: atomVar.Apply(Integer(0)), options: List(atomQuoted.Apply(atomTrue), atomNumberVars.Apply(atomTrue)), ok: true, output: `A`},
		{title: `write_term(S, '$VAR'(1), [numbervars(false)]).`, sOrA: w, term: atomVar.Apply(Integer(1)), options: List(atomNumberVars.Apply(atomFalse)), ok: true, output: `$VAR(1)`},
		{title: `write_term(S, '$VAR'(51), [numbervars(true)]).`, sOrA: w, term: atomVar.Apply(Integer(51)), options: List(atomNumberVars.Apply(atomTrue)), ok: true, output: `Z1`},
		{title: `write_term(1, [quoted(non_boolean)]).`, sOrA: w, term: Integer(1), options: List(atomQuoted.Apply(internal.NewAtom("non_boolean"))), err: domainError(validDomainWriteOption, atomQuoted.Apply(internal.NewAtom("non_boolean")), nil)},
		{title: `write_term(1, [quoted(B)]).`, sOrA: w, term: Integer(1), options: List(atomQuoted.Apply(B)), err: InstantiationError(nil)},
		{title: `B = true, write_term(1, [quoted(B)]).`, sOrA: w, env: NewEnv().bind(B, atomTrue), term: Integer(1), options: List(atomQuoted.Apply(B)), ok: true, output: `1`},

		// 8.14.2.3 Errors
		{title: `a`, sOrA: s, term: internal.NewAtom("foo"), options: List(), err: InstantiationError(nil)},
		{title: `b: partial list`, sOrA: w, term: internal.NewAtom("foo"), options: PartialList(x, atomQuoted.Apply(atomTrue)), err: InstantiationError(nil)},
		{title: `b: variable element`, sOrA: w, term: internal.NewAtom("foo"), options: List(x), err: InstantiationError(nil)},
		{title: `b: variable component`, sOrA: w, term: internal.NewAtom("foo"), options: List(atomQuoted.Apply(x)), err: InstantiationError(nil)},
		{title: `b: variable_names, partial list`, sOrA: w, term: internal.NewAtom("foo"), options: List(atomVariableNames.Apply(l)), err: InstantiationError(nil)},
		{title: `b: variable_names, element`, sOrA: w, term: internal.NewAtom("foo"), options: List(atomVariableNames.Apply(List(e))), err: InstantiationError(nil)},
		{title: `b: variable_names, name`, sOrA: w, term: x, options: List(atomVariableNames.Apply(List(atomEqual.Apply(n, v)))), err: InstantiationError(nil)},
		{title: `c`, sOrA: w, term: internal.NewAtom("foo"), options: internal.NewAtom("options"), err: typeError(validTypeList, internal.NewAtom("options"), nil)},
		{title: `d`, sOrA: Integer(0), term: internal.NewAtom("foo"), options: List(), err: domainError(validDomainStreamOrAlias, Integer(0), nil)},
		{title: `e: not a compound`, sOrA: w, term: internal.NewAtom("foo"), options: List(internal.NewAtom("bar")), err: domainError(validDomainWriteOption, internal.NewAtom("bar"), nil)},
		{title: `e: arity is not 1`, sOrA: w, term: internal.NewAtom("foo"), options: List(internal.NewAtom("bar").Apply(internal.NewAtom("a"), internal.NewAtom("b"))), err: domainError(validDomainWriteOption, internal.NewAtom("bar").Apply(internal.NewAtom("a"), internal.NewAtom("b")), nil)},
		{title: `e: variable_names, not a list, atom`, sOrA: w, term: internal.NewAtom("foo"), options: List(atomVariableNames.Apply(internal.NewAtom("a"))), err: domainError(validDomainWriteOption, atomVariableNames.Apply(internal.NewAtom("a")), nil)},
		{title: `e: variable_names, not a list, atomic`, sOrA: w, term: internal.NewAtom("foo"), options: List(atomVariableNames.Apply(Integer(0))), err: domainError(validDomainWriteOption, atomVariableNames.Apply(Integer(0)), nil)},
		{title: `e: variable_names, element is not a pair, atomic`, sOrA: w, term: internal.NewAtom("foo"), options: List(atomVariableNames.Apply(List(internal.NewAtom("a")))), err: domainError(validDomainWriteOption, atomVariableNames.Apply(List(internal.NewAtom("a"))), nil)},
		{title: `e: variable_names, element is not a pair, compound`, sOrA: w, term: internal.NewAtom("foo"), options: List(atomVariableNames.Apply(List(internal.NewAtom("f").Apply(internal.NewAtom("a"))))), err: domainError(validDomainWriteOption, atomVariableNames.Apply(List(internal.NewAtom("f").Apply(internal.NewAtom("a")))), nil)},
		{title: `e: variable_names, name is not an atom`, sOrA: w, term: v, options: List(atomVariableNames.Apply(List(atomEqual.Apply(Integer(0), v)))), err: domainError(validDomainWriteOption, atomVariableNames.Apply(List(atomEqual.Apply(Integer(0), internal.NewVariable()))), nil)},
		{title: `e: boolean option, not an atom`, sOrA: w, term: internal.NewAtom("foo"), options: List(atomQuoted.Apply(Integer(0))), err: domainError(validDomainWriteOption, atomQuoted.Apply(Integer(0)), nil)},
		{title: `e: unknown functor`, sOrA: w, term: internal.NewAtom("foo"), options: List(internal.NewAtom("bar").Apply(atomTrue)), err: domainError(validDomainWriteOption, internal.NewAtom("bar").Apply(atomTrue), nil)},
		{title: `f`, sOrA: internal.NewAtom("stream"), term: internal.NewAtom("foo"), options: List(), err: existenceError(objectTypeStream, internal.NewAtom("stream"), nil)},
		{title: `g`, sOrA: r, term: internal.NewAtom("foo"), options: List(), err: permissionError(operationOutput, permissionTypeStream, r, nil)},
		{title: `h`, sOrA: b, term: internal.NewAtom("foo"), options: List(), err: permissionError(operationOutput, permissionTypeBinaryStream, b, nil)},

		// 7.10.5
		{title: `a`, sOrA: w, term: x, options: List(), ok: true, outputPattern: regexp.MustCompile(`_\d+`)},

		{title: `variable_names`, sOrA: w, term: v, options: List(atomVariableNames.Apply(List(
			atomEqual.Apply(internal.NewAtom("n"), v),                     // left-most is used
			atomEqual.Apply(internal.NewAtom("m"), v),                     // ignored
			atomEqual.Apply(internal.NewAtom("a"), internal.NewAtom("b")), // ignored
		))), ok: true, output: `n`},

		{title: `failure`, sOrA: mw, term: internal.NewAtom("foo"), options: List(), err: err},

		{title: `write_term(S, + +1, [max_depth(2)]).`, sOrA: w, term: atomPlus.Apply(atomPlus.Apply(Integer(1))), options: List(atomMaxDepth.Apply(Integer(2))), ok: true, output: `+ + ...`},
		{title: `write_term(S, 1- -, [max_depth(2)]).`, sOrA: w, term: atomMinus.Apply(atomMinus.Apply(Integer(1))), options: List(atomMaxDepth.Apply(Integer(2))), ok: true, output: `... - -`},
		{title: `write_term(S, 1+2+3, [max_depth(2)]).`, sOrA: w, term: atomPlus.Apply(atomPlus.Apply(Integer(1), Integer(2)), Integer(3)), options: List(atomMaxDepth.Apply(Integer(2))), ok: true, output: `... + ... +3`},
		{title: `write_term(S, [1,2,3], [max_depth(2)]).`, sOrA: w, term: List(Integer(1), Integer(2), Integer(3)), options: List(atomMaxDepth.Apply(Integer(2))), ok: true, output: `[1,2|...]`},
		{title: `write_term(S, s(s(0)), [max_depth(2)]).`, sOrA: w, term: internal.NewAtom("s").Apply(internal.NewAtom("s").Apply(Integer(0))), options: List(atomMaxDepth.Apply(Integer(2))), ok: true, output: `s(s(...))`},
		{title: `write_term(S, _, [max_depth(_)]).`, sOrA: w, term: internal.NewVariable(), options: List(atomMaxDepth.Apply(internal.NewVariable())), err: InstantiationError(nil)},
		{title: `write_term(S, _, [max_depth(foo)]).`, sOrA: w, term: internal.NewVariable(), options: List(atomMaxDepth.Apply(internal.NewAtom("foo"))), err: domainError(validDomainWriteOption, atomMaxDepth.Apply(internal.NewAtom("foo")), nil)},
		{title: `L = [a, b|L], write_term(S, L, [max_depth(9)]).`, sOrA: w, term: l, options: List(atomMaxDepth.Apply(Integer(9))), env: NewEnv().bind(l, PartialList(l, internal.NewAtom("a"), internal.NewAtom("b"))), ok: true, output: `[a,b,a,b,a,b,a,b,a|...]`}, // https://github.com/ichiban/prolog/issues/297#issuecomment-1646750461
	}

	var ops internal.operators
	ops.define(500, internal.operatorSpecifierYFX, atomPlus)
	ops.define(200, internal.operatorSpecifierFY, atomPlus)
	ops.define(200, internal.operatorSpecifierYF, atomMinus)
	vm := internal.VM{
		typeIn: atomUser,
		modules: map[internal.Atom]*internal.module{
			atomUser: {
				operators: ops,
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			buf.Reset()
			ok, err := WriteTerm(&vm, tt.sOrA, tt.term, tt.options, Success, tt.env).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			if tt.err == nil {
				assert.NoError(t, err)
			} else if te, ok := tt.err.(internal.Exception); ok {
				_, ok := NewEnv().Unify(te.term, err.(internal.Exception).term)
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
	internal.WriteOptions
}

func (m *mockTerm) WriteTerm(w io.Writer, opts *internal.WriteOptions, env *internal.Env) error {
	args := m.Called(w, opts, env)
	return args.Error(0)
}

func (m *mockTerm) Compare(t Term, env *internal.Env) int {
	args := m.Called(t, env)
	return args.Int(0)
}

func (m *mockTerm) String() string {
	args := m.Called()
	return args.String(0)
}

func TestCharCode(t *testing.T) {
	t.Run("ascii", func(t *testing.T) {
		ok, err := CharCode(nil, internal.NewAtom("a"), Integer(97), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("emoji", func(t *testing.T) {
		ok, err := CharCode(nil, internal.NewAtom("😀"), Integer(128512), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("query char", func(t *testing.T) {
		v := internal.NewVariable()

		ok, err := CharCode(nil, v, Integer(128512), func(env *internal.Env) *internal.Promise {
			assert.Equal(t, internal.NewAtom("😀"), env.Resolve(v))
			return internal.Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("query code", func(t *testing.T) {
		v := internal.NewVariable()
		ok, err := CharCode(nil, internal.NewAtom("😀"), v, func(env *internal.Env) *internal.Promise {
			assert.Equal(t, Integer(128512), env.Resolve(v))
			return internal.Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("char and code are variables", func(t *testing.T) {
		char, code := internal.NewVariable(), internal.NewVariable()

		ok, err := CharCode(nil, char, code, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("char is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("atom", func(t *testing.T) {
			ok, err := CharCode(nil, internal.NewAtom("foo"), internal.NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeCharacter, internal.NewAtom("foo"), nil), err)
			assert.False(t, ok)
		})

		t.Run("non-atom", func(t *testing.T) {
			ok, err := CharCode(nil, Integer(0), internal.NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeCharacter, Integer(0), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("code is neither a variable nor an integer", func(t *testing.T) {
		t.Run("char is variable", func(t *testing.T) {
			ok, err := CharCode(nil, internal.NewVariable(), internal.NewAtom("foo"), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeInteger, internal.NewAtom("foo"), nil), err)
			assert.False(t, ok)
		})

		t.Run("char is a character", func(t *testing.T) {
			ok, err := CharCode(nil, internal.NewAtom("a"), internal.NewAtom("x"), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeInteger, internal.NewAtom("x"), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("code is neither a variable nor a character-code", func(t *testing.T) {
		ok, err := CharCode(nil, internal.NewVariable(), Integer(-1), Success, nil).Force(context.Background())
		assert.Equal(t, representationError(flagCharacterCode, nil), err)
		assert.False(t, ok)
	})
}

func TestPutByte(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		var m mockWriter
		m.On("Write", []byte{97}).Return(1, nil).Once()
		defer m.AssertExpectations(t)

		s := &internal.Stream{sink: &m, mode: internal.ioModeWrite, streamType: internal.streamTypeBinary}

		var vm internal.VM
		ok, err := PutByte(&vm, s, Integer(97), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("ng", func(t *testing.T) {
		var m mockWriter
		m.On("Write", []byte{97}).Return(0, errors.New("")).Once()
		defer m.AssertExpectations(t)

		s := &internal.Stream{sink: &m, mode: internal.ioModeWrite, streamType: internal.streamTypeBinary}

		var vm internal.VM
		_, err := PutByte(&vm, s, Integer(97), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("valid stream alias", func(t *testing.T) {
		var m mockWriter
		m.On("Write", []byte{97}).Return(1, nil).Once()
		defer m.AssertExpectations(t)

		foo := internal.NewAtom("foo")
		s := &internal.Stream{sink: &m, mode: internal.ioModeWrite, streamType: internal.streamTypeBinary, alias: foo}

		var vm internal.VM
		vm.streams.add(s)
		ok, err := PutByte(&vm, internal.NewAtom("foo"), Integer(97), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		var vm internal.VM
		ok, err := PutByte(&vm, internal.NewVariable(), Integer(97), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("byt is a variable", func(t *testing.T) {
		s := &internal.Stream{sink: os.Stdout, mode: internal.ioModeAppend}
		s.streamType = internal.streamTypeBinary

		var vm internal.VM
		ok, err := PutByte(&vm, s, internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("byt is neither a variable nor an byte", func(t *testing.T) {
		s := &internal.Stream{sink: os.Stdout, mode: internal.ioModeAppend}
		s.streamType = internal.streamTypeBinary

		t.Run("not even an integer", func(t *testing.T) {
			var vm internal.VM
			ok, err := PutByte(&vm, s, internal.NewAtom("byte"), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeByte, internal.NewAtom("byte"), nil), err)
			assert.False(t, ok)
		})

		t.Run("integer", func(t *testing.T) {
			var vm internal.VM
			ok, err := PutByte(&vm, s, Integer(256), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeByte, Integer(256), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var vm internal.VM
		ok, err := PutByte(&vm, Integer(0), Integer(97), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an input stream", func(t *testing.T) {
		s := internal.NewVariable()
		env := NewEnv().
			bind(s, &internal.Stream{source: os.Stdin, mode: internal.ioModeRead, streamType: internal.streamTypeBinary})

		var vm internal.VM
		ok, err := PutByte(&vm, s, Integer(97), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationOutput, permissionTypeStream, s, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a text stream", func(t *testing.T) {
		s := internal.NewVariable()
		env := NewEnv().
			bind(s, &internal.Stream{sink: os.Stdout, mode: internal.ioModeAppend})

		var vm internal.VM
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
		}, char: internal.NewAtom("t"), ok: true},
		{title: "put_char(st_o, 'A')", streamOrAlias: func() (Term, func(*testing.T)) {
			var sb strings.Builder
			sb.WriteString("qwer")
			return NewOutputTextStream(&sb), func(t *testing.T) {
				assert.Equal(t, "qwerA", sb.String())
			}
		}, char: internal.NewAtom("A"), ok: true},

		// 8.12.3.3 Errors
		{title: "a", streamOrAlias: func() (Term, func(*testing.T)) {
			return internal.NewVariable(), nil
		}, char: internal.NewAtom("a"), err: InstantiationError(nil)},
		{title: "b", streamOrAlias: func() (Term, func(*testing.T)) {
			return NewOutputTextStream(nil), nil
		}, char: internal.NewVariable(), err: InstantiationError(nil)},
		{title: "b: atom but not one-char", streamOrAlias: func() (Term, func(*testing.T)) {
			return NewOutputTextStream(nil), nil
		}, char: internal.NewAtom("foo"), err: typeError(validTypeCharacter, internal.NewAtom("foo"), nil)},
		{title: "b: not even atom", streamOrAlias: func() (Term, func(*testing.T)) {
			return NewOutputTextStream(nil), nil
		}, char: Integer(1), err: typeError(validTypeCharacter, Integer(1), nil)},
		{title: "f", streamOrAlias: func() (Term, func(*testing.T)) {
			return Integer(1), nil
		}, char: internal.NewAtom("a"), err: domainError(validDomainStreamOrAlias, Integer(1), nil)},
		{title: "g", streamOrAlias: func() (Term, func(*testing.T)) {
			return internal.NewAtom("foo"), nil
		}, char: internal.NewAtom("a"), err: existenceError(objectTypeStream, internal.NewAtom("foo"), nil)},
		{title: "h", streamOrAlias: func() (Term, func(*testing.T)) {
			return NewInputTextStream(nil), nil
		}, char: internal.NewAtom("a"), err: permissionError(operationOutput, permissionTypeStream, NewInputTextStream(nil), nil)},
		{title: "i", streamOrAlias: func() (Term, func(*testing.T)) {
			return NewOutputBinaryStream(nil), nil
		}, char: internal.NewAtom("a"), err: permissionError(operationOutput, permissionTypeBinaryStream, NewOutputBinaryStream(nil), nil)},

		{title: "error on write", streamOrAlias: func() (Term, func(*testing.T)) {
			var m mockWriter
			m.On("Write", mock.Anything).Return(0, errors.New("failed"))
			return NewOutputTextStream(&m), nil
		}, char: internal.NewAtom("a"), err: errors.New("failed")},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			sOrA, test := tt.streamOrAlias()
			if test != nil {
				defer test(t)
			}

			var vm internal.VM
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

		s := &internal.Stream{source: f, mode: internal.ioModeRead}

		v := internal.NewVariable()

		var vm internal.VM
		ok, err := ReadTerm(&vm, s, v, List(), func(env *internal.Env) *internal.Promise {
			assert.Equal(t, internal.NewAtom("foo"), env.Resolve(v))
			return internal.Bool(true)
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

		foo := internal.NewAtom("foo")
		s := &internal.Stream{source: f, mode: internal.ioModeRead, alias: foo}

		v := internal.NewVariable()

		var vm internal.VM
		vm.streams.add(s)
		ok, err := ReadTerm(&vm, internal.NewAtom("foo"), v, List(), func(env *internal.Env) *internal.Promise {
			assert.Equal(t, internal.NewAtom("foo"), env.Resolve(v))
			return internal.Bool(true)
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

		s := &internal.Stream{source: f, mode: internal.ioModeRead}

		v, singletons := internal.NewVariable(), internal.NewVariable()

		var vm internal.VM
		ok, err := ReadTerm(&vm, s, v, List(&compound{
			functor: atomSingletons,
			args:    []Term{singletons},
		}), func(env *internal.Env) *internal.Promise {
			c, ok := env.Resolve(v).(*compound)
			assert.True(t, ok)
			assert.Equal(t, internal.NewAtom("f"), c.functor)
			assert.Len(t, c.args, 3)

			x, ok := c.args[0].(internal.Variable)
			assert.True(t, ok)
			assert.Equal(t, x, c.args[1])

			y, ok := c.args[2].(internal.Variable)
			assert.True(t, ok)

			assert.Equal(t, List(y), env.Resolve(singletons))

			return internal.Bool(true)
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

		s := &internal.Stream{source: f, mode: internal.ioModeRead}

		v, variables := internal.NewVariable(), internal.NewVariable()

		var vm internal.VM
		ok, err := ReadTerm(&vm, s, v, List(&compound{
			functor: atomVariables,
			args:    []Term{variables},
		}), func(env *internal.Env) *internal.Promise {
			c, ok := env.Resolve(v).(*compound)
			assert.True(t, ok)
			assert.Equal(t, internal.NewAtom("f"), c.functor)
			assert.Len(t, c.args, 3)

			x, ok := c.args[0].(internal.Variable)
			assert.True(t, ok)
			assert.Equal(t, x, c.args[1])

			y, ok := c.args[2].(internal.Variable)
			assert.True(t, ok)

			assert.Equal(t, List(x, y), env.Resolve(variables))

			return internal.Bool(true)
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

		s := &internal.Stream{source: f, mode: internal.ioModeRead}

		v, variableNames := internal.NewVariable(), internal.NewVariable()

		var vm internal.VM
		ok, err := ReadTerm(&vm, s, v, List(&compound{
			functor: atomVariableNames,
			args:    []Term{variableNames},
		}), func(env *internal.Env) *internal.Promise {
			c, ok := env.Resolve(v).(*compound)
			assert.True(t, ok)
			assert.Equal(t, internal.NewAtom("f"), c.functor)
			assert.Len(t, c.args, 3)

			x, ok := c.args[0].(internal.Variable)
			assert.True(t, ok)
			assert.Equal(t, x, c.args[1])

			y, ok := c.args[2].(internal.Variable)
			assert.True(t, ok)

			assert.Equal(t, List(
				&compound{
					functor: atomEqual,
					args:    []Term{internal.NewAtom("X"), x},
				},
				&compound{
					functor: atomEqual,
					args:    []Term{internal.NewAtom("Y"), y},
				},
			), env.Resolve(variableNames))

			return internal.Bool(true)
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

		s := &internal.Stream{source: f, mode: internal.ioModeRead}

		v := internal.NewVariable()

		var vm internal.VM

		ok, err := ReadTerm(&vm, s, v, List(), func(env *internal.Env) *internal.Promise {
			assert.Equal(t, &compound{functor: internal.NewAtom("foo"), args: []Term{internal.NewAtom("a")}}, env.Resolve(v))
			return internal.Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = ReadTerm(&vm, s, v, List(), func(env *internal.Env) *internal.Promise {
			assert.Equal(t, &compound{functor: internal.NewAtom("foo"), args: []Term{internal.NewAtom("b")}}, env.Resolve(v))
			return internal.Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = ReadTerm(&vm, s, v, List(), func(env *internal.Env) *internal.Promise {
			assert.Equal(t, &compound{functor: internal.NewAtom("foo"), args: []Term{internal.NewAtom("c")}}, env.Resolve(v))
			return internal.Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		var vm internal.VM
		ok, err := ReadTerm(&vm, internal.NewVariable(), internal.NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("options is a partial list or a list with an element which is a variable", func(t *testing.T) {
		t.Run("partial list", func(t *testing.T) {
			var vm internal.VM
			ok, err := ReadTerm(&vm, &internal.Stream{source: os.Stdin}, internal.NewVariable(), PartialList(internal.NewVariable(),
				atomVariables.Apply(internal.NewVariable()),
			), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		})

		t.Run("variable element", func(t *testing.T) {
			var vm internal.VM
			ok, err := ReadTerm(&vm, &internal.Stream{source: os.Stdin}, internal.NewVariable(), List(internal.NewVariable(), atomVariables.Apply(internal.NewVariable())), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var vm internal.VM
		ok, err := ReadTerm(&vm, Integer(0), internal.NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("options is neither a partial list nor a list", func(t *testing.T) {
		var vm internal.VM
		ok, err := ReadTerm(&vm, &internal.Stream{source: os.Stdin}, internal.NewVariable(), internal.NewAtom("options"), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeList, internal.NewAtom("options"), nil), err)
		assert.False(t, ok)
	})

	t.Run("an element E of the Options list is neither a variable nor a valid read-option", func(t *testing.T) {
		for _, term := range []Term{atomUnknown, atomUnknown.Apply(internal.NewAtom("option")), atomUnknown.Apply(internal.NewAtom("option"), Integer(0))} {
			var vm internal.VM
			ok, err := ReadTerm(&vm, &internal.Stream{source: os.Stdin}, internal.NewVariable(), List(term), Success, nil).Force(context.Background())
			assert.Equal(t, domainError(validDomainReadOption, term, nil), err)
			assert.False(t, ok)
		}
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var vm internal.VM
		ok, err := ReadTerm(&vm, internal.NewAtom("foo"), internal.NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, existenceError(objectTypeStream, internal.NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		s := internal.NewVariable()
		env := NewEnv().
			bind(s, &internal.Stream{sink: os.Stdout, mode: internal.ioModeAppend})

		var vm internal.VM
		ok, err := ReadTerm(&vm, s, internal.NewVariable(), List(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypeStream, s, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		stream := &internal.Stream{source: os.Stdin}
		stream.streamType = internal.streamTypeBinary

		s := internal.NewVariable()
		env := NewEnv().
			bind(s, stream)

		var vm internal.VM
		ok, err := ReadTerm(&vm, s, internal.NewVariable(), List(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypeBinaryStream, s, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		var m internal.mockReader
		defer m.AssertExpectations(t)

		s := internal.NewVariable()
		env := NewEnv().
			bind(s, &internal.Stream{
				source:      &m,
				mode:        internal.ioModeRead,
				eofAction:   internal.eofActionError,
				endOfStream: internal.endOfStreamPast,
			})

		var vm internal.VM
		ok, err := ReadTerm(&vm, s, internal.NewVariable(), List(), Success, env).Force(context.Background())
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

			s := &internal.Stream{source: f, mode: internal.ioModeRead}

			var vm internal.VM
			ok, err := ReadTerm(&vm, s, internal.NewVariable(), List(), Success, nil).Force(context.Background())
			assert.Equal(t, syntaxError(internal.unexpectedTokenError{actual: internal.Token{kind: internal.tokenLetterDigit, val: "bar"}}, nil), err)
			assert.False(t, ok)
		})

		t.Run("insufficient", func(t *testing.T) {
			f, err := os.Open("testdata/insufficient.txt")
			assert.NoError(t, err)
			defer func() {
				assert.NoError(t, f.Close())
			}()

			s := &internal.Stream{source: f, mode: internal.ioModeRead}

			out := internal.NewVariable()
			var vm internal.VM
			ok, err := ReadTerm(&vm, s, out, List(), func(env *internal.Env) *internal.Promise {
				assert.Equal(t, atomEndOfFile, env.Resolve(out))
				return internal.Bool(true)
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

		s := &internal.Stream{source: f, mode: internal.ioModeRead}

		var vm internal.VM
		ok, err := ReadTerm(&vm, s, internal.NewVariable(), List(), Success, nil).Force(context.Background())
		assert.Equal(t, syntaxError(internal.unexpectedTokenError{actual: internal.Token{kind: internal.tokenGraphic, val: "***"}}, nil), err)
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

		s := &internal.Stream{source: f, mode: internal.ioModeRead, streamType: internal.streamTypeBinary}

		v := internal.NewVariable()

		var vm internal.VM
		ok, err := GetByte(&vm, s, v, func(env *internal.Env) *internal.Promise {
			assert.Equal(t, Integer(97), env.Resolve(v))
			return internal.Bool(true)
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

		foo := internal.NewAtom("foo")
		s := &internal.Stream{source: f, mode: internal.ioModeRead, streamType: internal.streamTypeBinary, alias: foo}

		v := internal.NewVariable()

		var vm internal.VM
		vm.streams.add(s)
		ok, err := GetByte(&vm, foo, v, func(env *internal.Env) *internal.Promise {
			assert.Equal(t, Integer(97), env.Resolve(v))
			return internal.Bool(true)
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

		s := &internal.Stream{source: f, mode: internal.ioModeRead, streamType: internal.streamTypeBinary}

		v := internal.NewVariable()

		var vm internal.VM
		ok, err := GetByte(&vm, s, v, func(env *internal.Env) *internal.Promise {
			assert.Equal(t, Integer(-1), env.Resolve(v))
			return internal.Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("error", func(t *testing.T) {
		var m internal.mockReader
		m.On("Read", mock.Anything).Return(0, errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		s := &internal.Stream{source: &m, mode: internal.ioModeRead, streamType: internal.streamTypeBinary}

		var vm internal.VM

		v := internal.NewVariable()
		_, err := GetByte(&vm, s, v, Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		var vm internal.VM
		ok, err := GetByte(&vm, internal.NewVariable(), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("inByte is neither a variable nor an in-byte", func(t *testing.T) {
		s := &internal.Stream{source: os.Stdin}
		s.streamType = internal.streamTypeBinary

		t.Run("not even an integer", func(t *testing.T) {
			var vm internal.VM
			ok, err := GetByte(&vm, s, internal.NewAtom("inByte"), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeInByte, internal.NewAtom("inByte"), nil), err)
			assert.False(t, ok)
		})

		t.Run("integer", func(t *testing.T) {
			var vm internal.VM
			ok, err := GetByte(&vm, s, Integer(256), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeInByte, Integer(256), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream-term or alias", func(t *testing.T) {
		var vm internal.VM
		ok, err := GetByte(&vm, Integer(0), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var vm internal.VM
		ok, err := GetByte(&vm, internal.NewAtom("foo"), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, existenceError(objectTypeStream, internal.NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := internal.NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &internal.Stream{sink: os.Stdout, mode: internal.ioModeAppend})

		var vm internal.VM
		ok, err := GetByte(&vm, streamOrAlias, internal.NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypeStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a text stream", func(t *testing.T) {
		streamOrAlias := internal.NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &internal.Stream{source: os.Stdin})

		var vm internal.VM
		ok, err := GetByte(&vm, streamOrAlias, internal.NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypeTextStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		var m internal.mockReader
		defer m.AssertExpectations(t)

		streamOrAlias := internal.NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &internal.Stream{
				source:      &m,
				mode:        internal.ioModeRead,
				streamType:  internal.streamTypeBinary,
				eofAction:   internal.eofActionError,
				endOfStream: internal.endOfStreamPast,
			})

		var vm internal.VM
		ok, err := GetByte(&vm, streamOrAlias, internal.NewVariable(), Success, env).Force(context.Background())
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

		s := &internal.Stream{source: f, mode: internal.ioModeRead}

		v := internal.NewVariable()

		var vm internal.VM
		ok, err := GetChar(&vm, s, v, func(env *internal.Env) *internal.Promise {
			assert.Equal(t, internal.NewAtom("😀"), env.Resolve(v))
			return internal.Bool(true)
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

		v := internal.NewVariable()

		foo := internal.NewAtom("foo")
		var vm internal.VM
		vm.streams.add(&internal.Stream{source: f, mode: internal.ioModeRead, alias: foo})
		ok, err := GetChar(&vm, foo, v, func(env *internal.Env) *internal.Promise {
			assert.Equal(t, internal.NewAtom("😀"), env.Resolve(v))
			return internal.Bool(true)
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

		s := &internal.Stream{source: f, mode: internal.ioModeRead}

		v := internal.NewVariable()

		var vm internal.VM
		ok, err := GetChar(&vm, s, v, func(env *internal.Env) *internal.Promise {
			assert.Equal(t, atomEndOfFile, env.Resolve(v))
			return internal.Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("error", func(t *testing.T) {
		var m internal.mockReader
		m.On("Read", mock.Anything).Return(0, errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		v := internal.NewVariable()

		var vm internal.VM
		ok, err := GetChar(&vm, &internal.Stream{source: &m, mode: internal.ioModeRead}, v, Success, nil).Force(context.Background())
		assert.Equal(t, errors.New("failed"), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		var vm internal.VM
		ok, err := GetChar(&vm, internal.NewVariable(), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("char is neither a variable nor an in-character", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var vm internal.VM
			ok, err := GetChar(&vm, &internal.Stream{source: os.Stdin}, Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeInCharacter, Integer(0), nil), err)
			assert.False(t, ok)
		})

		t.Run("atom", func(t *testing.T) {
			var vm internal.VM
			ok, err := GetChar(&vm, &internal.Stream{source: os.Stdin}, internal.NewAtom("ab"), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeInCharacter, internal.NewAtom("ab"), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var vm internal.VM
		ok, err := GetChar(&vm, Integer(0), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := internal.NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &internal.Stream{sink: os.Stdout, mode: internal.ioModeAppend})

		var vm internal.VM
		ok, err := GetChar(&vm, streamOrAlias, internal.NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypeStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		s := &internal.Stream{source: os.Stdin}
		s.streamType = internal.streamTypeBinary

		streamOrAlias := internal.NewVariable()
		env := NewEnv().
			bind(streamOrAlias, s)

		var vm internal.VM
		ok, err := GetChar(&vm, streamOrAlias, internal.NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypeBinaryStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		var m internal.mockReader
		defer m.AssertExpectations(t)

		streamOrAlias := internal.NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &internal.Stream{
				source:      &m,
				mode:        internal.ioModeRead,
				eofAction:   internal.eofActionError,
				endOfStream: internal.endOfStreamPast,
			})

		var vm internal.VM
		ok, err := GetChar(&vm, streamOrAlias, internal.NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypePastEndOfStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("the entity input from the stream is not a character", func(t *testing.T) {
		f, err := os.Open("testdata/replacement.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		streamOrAlias := internal.NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &internal.Stream{source: f, mode: internal.ioModeRead})

		var vm internal.VM
		ok, err := GetChar(&vm, streamOrAlias, internal.NewVariable(), Success, env).Force(context.Background())
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

		s := &internal.Stream{source: f, mode: internal.ioModeRead, streamType: internal.streamTypeBinary}

		v := internal.NewVariable()

		var vm internal.VM
		ok, err := PeekByte(&vm, s, v, func(env *internal.Env) *internal.Promise {
			assert.Equal(t, Integer(97), env.Resolve(v))
			return internal.Bool(true)
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

		v := internal.NewVariable()

		foo := internal.NewAtom("foo")
		var vm internal.VM
		vm.streams.add(&internal.Stream{source: f, mode: internal.ioModeRead, streamType: internal.streamTypeBinary, alias: foo})
		ok, err := PeekByte(&vm, internal.NewAtom("foo"), v, func(env *internal.Env) *internal.Promise {
			assert.Equal(t, Integer(97), env.Resolve(v))
			return internal.Bool(true)
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

		s := &internal.Stream{source: f, mode: internal.ioModeRead, streamType: internal.streamTypeBinary}

		v := internal.NewVariable()

		var vm internal.VM
		ok, err := PeekByte(&vm, s, v, func(env *internal.Env) *internal.Promise {
			assert.Equal(t, Integer(-1), env.Resolve(v))
			return internal.Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("error", func(t *testing.T) {
		var m internal.mockReader
		m.On("Read", mock.Anything).Return(0, errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		s := &internal.Stream{source: &m, mode: internal.ioModeRead}
		s.streamType = internal.streamTypeBinary

		v := internal.NewVariable()

		var vm internal.VM
		ok, err := PeekByte(&vm, s, v, Success, nil).Force(context.Background())
		assert.Equal(t, errors.New("failed"), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		var vm internal.VM
		ok, err := PeekByte(&vm, internal.NewVariable(), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("inByte is neither a variable nor an in-byte", func(t *testing.T) {
		s := &internal.Stream{source: os.Stdin}
		s.streamType = internal.streamTypeBinary

		t.Run("not even an integer", func(t *testing.T) {
			var vm internal.VM
			ok, err := PeekByte(&vm, s, internal.NewAtom("byte"), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeInByte, internal.NewAtom("byte"), nil), err)
			assert.False(t, ok)
		})

		t.Run("integer", func(t *testing.T) {
			var vm internal.VM
			ok, err := PeekByte(&vm, s, Integer(256), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeInByte, Integer(256), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var vm internal.VM
		ok, err := PeekByte(&vm, Integer(0), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := internal.NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &internal.Stream{sink: os.Stdout, mode: internal.ioModeAppend})

		var vm internal.VM
		ok, err := PeekByte(&vm, streamOrAlias, internal.NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypeStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a text stream", func(t *testing.T) {
		streamOrAlias := internal.NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &internal.Stream{source: os.Stdin})

		var vm internal.VM
		ok, err := PeekByte(&vm, streamOrAlias, internal.NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypeTextStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		var m internal.mockReader
		defer m.AssertExpectations(t)

		streamOrAlias := internal.NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &internal.Stream{
				source:      &m,
				mode:        internal.ioModeRead,
				streamType:  internal.streamTypeBinary,
				eofAction:   internal.eofActionError,
				endOfStream: internal.endOfStreamPast,
			})

		var vm internal.VM
		ok, err := PeekByte(&vm, streamOrAlias, internal.NewVariable(), Success, env).Force(context.Background())
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

		s := &internal.Stream{source: f, mode: internal.ioModeRead}

		v := internal.NewVariable()

		var vm internal.VM
		ok, err := PeekChar(&vm, s, v, func(env *internal.Env) *internal.Promise {
			assert.Equal(t, internal.NewAtom("😀"), env.Resolve(v))
			return internal.Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = PeekChar(&vm, s, v, func(env *internal.Env) *internal.Promise {
			assert.Equal(t, internal.NewAtom("😀"), env.Resolve(v)) // '😀' again
			return internal.Bool(true)
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

		v := internal.NewVariable()

		foo := internal.NewAtom("foo")
		var vm internal.VM
		vm.streams.add(&internal.Stream{source: f, mode: internal.ioModeRead, alias: foo})
		ok, err := PeekChar(&vm, foo, v, func(env *internal.Env) *internal.Promise {
			assert.Equal(t, internal.NewAtom("😀"), env.Resolve(v))
			return internal.Bool(true)
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

		s := &internal.Stream{source: f, mode: internal.ioModeRead}

		v := internal.NewVariable()

		var vm internal.VM
		ok, err := PeekChar(&vm, s, v, func(env *internal.Env) *internal.Promise {
			assert.Equal(t, atomEndOfFile, env.Resolve(v))
			return internal.Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("error", func(t *testing.T) {
		var m internal.mockReader
		m.On("Read", mock.Anything).Return(0, errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		v := internal.NewVariable()

		var vm internal.VM
		ok, err := PeekChar(&vm, &internal.Stream{source: &m, mode: internal.ioModeRead}, v, Success, nil).Force(context.Background())
		assert.Equal(t, errors.New("failed"), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		var vm internal.VM
		ok, err := PeekChar(&vm, internal.NewVariable(), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("char is neither a variable nor an in-character", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var vm internal.VM
			ok, err := PeekChar(&vm, &internal.Stream{source: os.Stdin}, Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeInCharacter, Integer(0), nil), err)
			assert.False(t, ok)
		})

		t.Run("atom", func(t *testing.T) {
			var vm internal.VM
			ok, err := PeekChar(&vm, &internal.Stream{source: os.Stdin}, internal.NewAtom("ab"), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeInCharacter, internal.NewAtom("ab"), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var vm internal.VM
		ok, err := PeekChar(&vm, Integer(0), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainStreamOrAlias, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is an output stream", func(t *testing.T) {
		streamOrAlias := internal.NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &internal.Stream{sink: os.Stdout, mode: internal.ioModeAppend})

		var vm internal.VM
		ok, err := PeekChar(&vm, streamOrAlias, internal.NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypeStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is associated with a binary stream", func(t *testing.T) {
		s := &internal.Stream{source: os.Stdin}
		s.streamType = internal.streamTypeBinary

		streamOrAlias := internal.NewVariable()
		env := NewEnv().
			bind(streamOrAlias, s)

		var vm internal.VM
		ok, err := PeekChar(&vm, streamOrAlias, internal.NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypeBinaryStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream properties end_of_stream(past) and eof_action(error)", func(t *testing.T) {
		var m internal.mockReader
		defer m.AssertExpectations(t)

		streamOrAlias := internal.NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &internal.Stream{
				source:      &m,
				eofAction:   internal.eofActionError,
				endOfStream: internal.endOfStreamPast,
			})

		var vm internal.VM
		ok, err := PeekChar(&vm, streamOrAlias, internal.NewVariable(), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationInput, permissionTypePastEndOfStream, streamOrAlias, env), err)
		assert.False(t, ok)
	})

	t.Run("the entity input from the stream is not a character", func(t *testing.T) {
		f, err := os.Open("testdata/replacement.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		streamOrAlias := internal.NewVariable()
		env := NewEnv().
			bind(streamOrAlias, &internal.Stream{source: f, mode: internal.ioModeRead})

		var vm internal.VM
		ok, err := PeekChar(&vm, streamOrAlias, internal.NewVariable(), Success, env).Force(context.Background())
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
		n := internal.NewVariable()

		ok, err := Halt(nil, n, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("n is neither a variable nor an integer", func(t *testing.T) {
		ok, err := Halt(nil, internal.NewAtom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, internal.NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})
}

func TestClause(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		x := internal.NewVariable()
		what, body := internal.NewVariable(), internal.NewVariable()

		var c int

		vm := internal.VM{
			typeIn: atomUser,
			modules: map[internal.Atom]*internal.module{
				atomUser: {
					procedures: map[predicateIndicator]internal.procedureEntry{
						{name: internal.NewAtom("green"), arity: 1}: internal.procedureEntry{public: true, procedure: internal.clauses{
							{raw: &compound{
								functor: atomColonMinus, args: []Term{
									&compound{functor: internal.NewAtom("green"), args: []Term{x}},
									&compound{functor: internal.NewAtom("moldy"), args: []Term{x}},
								},
							}},
							{raw: &compound{functor: internal.NewAtom("green"), args: []Term{internal.NewAtom("kermit")}}},
						}},
					},
				},
			},
		}
		ok, err := Clause(&vm, &compound{
			functor: internal.NewAtom("green"),
			args:    []Term{what},
		}, body, func(env *internal.Env) *internal.Promise {
			switch c {
			case 0:
				b, ok := env.Resolve(body).(*compound)
				assert.True(t, ok)
				assert.Equal(t, internal.NewAtom("moldy"), b.functor)
				assert.Len(t, b.args, 1)
			case 1:
				assert.Equal(t, internal.NewAtom("kermit"), env.Resolve(what))
				assert.Equal(t, atomTrue, env.Resolve(body))
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return internal.Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("not found", func(t *testing.T) {
		var vm internal.VM
		ok, err := Clause(&vm, internal.NewAtom("foo"), atomTrue, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("head is a variable", func(t *testing.T) {
		var vm internal.VM
		ok, err := Clause(&vm, internal.NewVariable(), atomTrue, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("head is neither a variable nor a predication", func(t *testing.T) {
		var vm internal.VM
		ok, err := Clause(&vm, Integer(0), atomTrue, Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("the predicate indicator Pred of Head is that of a private (ie. Not public) procedure", func(t *testing.T) {
		what, body := internal.NewVariable(), internal.NewVariable()

		vm := internal.VM{
			typeIn: atomUser,
			modules: map[internal.Atom]*internal.module{
				atomUser: {
					procedures: map[predicateIndicator]internal.procedureEntry{
						{name: internal.NewAtom("green"), arity: 1}: {procedure: Predicate1(func(_ *internal.VM, t Term, f internal.Cont, env *internal.Env) *internal.Promise {
							return internal.Bool(true)
						})},
					},
				},
			},
		}
		ok, err := Clause(&vm, &compound{
			functor: internal.NewAtom("green"),
			args:    []Term{what},
		}, body, Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationAccess, permissionTypePrivateProcedure, &compound{
			functor: atomSlash,
			args:    []Term{internal.NewAtom("green"), Integer(1)},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("body is neither a variable nor a callable term", func(t *testing.T) {
		var vm internal.VM
		ok, err := Clause(&vm, internal.NewAtom("foo"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeCallable, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("out of memory", func(t *testing.T) {
		defer setMemFree(1)()

		vm := internal.VM{
			typeIn: atomUser,
			modules: map[internal.Atom]*internal.module{
				atomUser: {
					procedures: map[predicateIndicator]internal.procedureEntry{
						{name: internal.NewAtom("green"), arity: 1}: {public: true, procedure: internal.clauses{
							{raw: internal.NewAtom("green").Apply(internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable())},
						}},
					},
				},
			},
		}
		ok, err := Clause(&vm, internal.NewAtom("green").Apply(internal.NewVariable()), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, resourceError(resourceMemory, nil), err)
		assert.False(t, ok)
	})
}

func TestAtomLength(t *testing.T) {
	n := internal.NewVariable()

	tests := []struct {
		title        string
		atom, length Term
		ok           bool
		err          error
		env          map[internal.Variable]Term
	}{
		// 8.16.1.4 Examples
		{title: "atom_length('enchanted evening', N).", atom: internal.NewAtom("enchanted evening"), length: n, ok: true, env: map[internal.Variable]Term{
			n: Integer(17),
		}},
		{title: `atom_length('enchanted\
 evening', N).`, atom: internal.NewAtom("enchanted evening"), length: n, ok: true, env: map[internal.Variable]Term{
			n: Integer(17),
		}},
		{title: "atom_length('', N).", atom: internal.NewAtom(""), length: n, ok: true, env: map[internal.Variable]Term{
			n: Integer(0),
		}},
		{title: "atom_length('scarlet', 5).", atom: internal.NewAtom("scarlet"), length: Integer(5), ok: false},
		{title: "atom_length(Atom, 4).", atom: internal.NewVariable(), length: Integer(4), err: InstantiationError(nil)},
		{title: "atom_length(1.23, 4).", atom: Float(1.23), length: Integer(4), err: typeError(validTypeAtom, Float(1.23), nil)},
		{title: "atom_length(atom, '4').", atom: internal.NewAtom("atom"), length: internal.NewAtom("4"), err: typeError(validTypeInteger, internal.NewAtom("4"), nil)},

		// 8.16.1.3 Errors
		{title: "d", atom: internal.NewAtom("atom"), length: Integer(-1), err: domainError(validDomainNotLessThanZero, Integer(-1), nil)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			var vm internal.VM
			ok, err := AtomLength(&vm, tt.atom, tt.length, func(env *internal.Env) *internal.Promise {
				for k, v := range tt.env {
					_, ok := env.Unify(k, v)
					assert.True(t, ok)
				}
				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestAtomConcat(t *testing.T) {
	t.Run("atom3 is a variable", func(t *testing.T) {
		atom3 := internal.NewVariable()

		ok, err := AtomConcat(nil, internal.NewAtom("foo"), internal.NewAtom("bar"), atom3, func(env *internal.Env) *internal.Promise {
			assert.Equal(t, internal.NewAtom("foobar"), env.Resolve(atom3))
			return internal.Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("atom3 is an atom", func(t *testing.T) {
		var c int
		v1, v2 := internal.NewVariable(), internal.NewVariable()
		ok, err := AtomConcat(nil, v1, v2, internal.NewAtom("foo"), func(env *internal.Env) *internal.Promise {
			switch c {
			case 0:
				assert.Equal(t, internal.NewAtom(""), env.Resolve(v1))
				assert.Equal(t, internal.NewAtom("foo"), env.Resolve(v2))
			case 1:
				assert.Equal(t, internal.NewAtom("f"), env.Resolve(v1))
				assert.Equal(t, internal.NewAtom("oo"), env.Resolve(v2))
			case 2:
				assert.Equal(t, internal.NewAtom("fo"), env.Resolve(v1))
				assert.Equal(t, internal.NewAtom("o"), env.Resolve(v2))
			case 3:
				assert.Equal(t, internal.NewAtom("foo"), env.Resolve(v1))
				assert.Equal(t, internal.NewAtom(""), env.Resolve(v2))
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return internal.Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("atom1 and atom3 are variables", func(t *testing.T) {
		atom1, atom3 := internal.NewVariable(), internal.NewVariable()

		ok, err := AtomConcat(nil, atom1, internal.NewAtom("bar"), atom3, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("atom2 and atom3 are variables", func(t *testing.T) {
		atom2, atom3 := internal.NewVariable(), internal.NewVariable()

		ok, err := AtomConcat(nil, internal.NewAtom("foo"), atom2, atom3, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("atom1 is neither a variable nor an atom", func(t *testing.T) {
		t.Run("atom3 is a variable", func(t *testing.T) {
			ok, err := AtomConcat(nil, Integer(1), internal.NewAtom("bar"), internal.NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeAtom, Integer(1), nil), err)
			assert.False(t, ok)
		})

		t.Run("atom3 is an atom", func(t *testing.T) {
			ok, err := AtomConcat(nil, Integer(1), internal.NewAtom("bar"), internal.NewAtom("foobar"), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeAtom, Integer(1), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("atom2 is neither a variable nor an atom", func(t *testing.T) {
		t.Run("atom3 is a variable", func(t *testing.T) {
			ok, err := AtomConcat(nil, internal.NewAtom("foo"), Integer(2), internal.NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeAtom, Integer(2), nil), err)
			assert.False(t, ok)
		})

		t.Run("atom3 is an atom", func(t *testing.T) {
			ok, err := AtomConcat(nil, internal.NewAtom("foo"), Integer(2), internal.NewAtom("foobar"), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeAtom, Integer(2), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("atom3 is neither a variable nor an atom", func(t *testing.T) {
		ok, err := AtomConcat(nil, internal.NewAtom("foo"), internal.NewAtom("bar"), Integer(3), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeAtom, Integer(3), nil), err)
		assert.False(t, ok)
	})
}

func TestSubAtom(t *testing.T) {
	t.Run("multiple solutions", func(t *testing.T) {
		before, length, after := internal.NewVariable(), internal.NewVariable(), internal.NewVariable()
		var c int
		ok, err := SubAtom(nil, internal.NewAtom("xATGATGAxATGAxATGAx"), before, length, after, internal.NewAtom("ATGA"), func(env *internal.Env) *internal.Promise {
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
			return internal.Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("get the first char", func(t *testing.T) {
		char := internal.NewVariable()
		ok, err := SubAtom(nil, internal.NewAtom("a"), Integer(0), Integer(1), Integer(0), char, func(env *internal.Env) *internal.Promise {
			assert.Equal(t, internal.NewAtom("a"), env.Resolve(char))
			return internal.Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("atom is a variable", func(t *testing.T) {
		ok, err := SubAtom(nil, internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("atom is neither a variable nor an atom", func(t *testing.T) {
		ok, err := SubAtom(nil, Integer(0), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("subAtom is neither a variable nor an atom", func(t *testing.T) {
		ok, err := SubAtom(nil, internal.NewAtom("foo"), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("before is neither a variable nor an integer", func(t *testing.T) {
		ok, err := SubAtom(nil, internal.NewAtom("foo"), internal.NewAtom("before"), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, internal.NewAtom("before"), nil), err)
		assert.False(t, ok)
	})

	t.Run("length is neither a variable nor an integer", func(t *testing.T) {
		ok, err := SubAtom(nil, internal.NewAtom("foo"), internal.NewVariable(), internal.NewAtom("length"), internal.NewVariable(), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, internal.NewAtom("length"), nil), err)
		assert.False(t, ok)
	})

	t.Run("after is neither a variable nor an integer", func(t *testing.T) {
		ok, err := SubAtom(nil, internal.NewAtom("foo"), internal.NewVariable(), internal.NewVariable(), internal.NewAtom("after"), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, internal.NewAtom("after"), nil), err)
		assert.False(t, ok)
	})

	t.Run("before is an integer less than zero", func(t *testing.T) {
		ok, err := SubAtom(nil, internal.NewAtom("foo"), Integer(-1), internal.NewVariable(), internal.NewVariable(), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainNotLessThanZero, Integer(-1), nil), err)
		assert.False(t, ok)
	})

	t.Run("length is an integer less than zero", func(t *testing.T) {
		ok, err := SubAtom(nil, internal.NewAtom("foo"), internal.NewVariable(), Integer(-1), internal.NewVariable(), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainNotLessThanZero, Integer(-1), nil), err)
		assert.False(t, ok)
	})

	t.Run("after is an integer less than zero", func(t *testing.T) {
		ok, err := SubAtom(nil, internal.NewAtom("foo"), internal.NewVariable(), internal.NewVariable(), Integer(-1), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainNotLessThanZero, Integer(-1), nil), err)
		assert.False(t, ok)
	})
}

func TestAtomChars(t *testing.T) {
	l := internal.NewVariable()
	str := internal.NewVariable()
	x, y := internal.NewVariable(), internal.NewVariable()

	tests := []struct {
		title      string
		atom, list Term
		ok         bool
		err        error
		env        map[internal.Variable]Term
	}{
		// 8.16.4.4 Examples
		{title: "atom_chars('', L).", atom: internal.NewAtom(""), list: l, ok: true, env: map[internal.Variable]Term{
			l: List(),
		}},
		{title: "atom_chars([], L).", atom: internal.atomEmptyList, list: l, ok: true, env: map[internal.Variable]Term{
			l: List(internal.NewAtom("["), internal.NewAtom("]")),
		}},
		{title: "atom_chars('''', L).", atom: internal.NewAtom("'"), list: l, ok: true, env: map[internal.Variable]Term{
			l: List(internal.NewAtom("'")),
		}},
		{title: "atom_chars('ant', L).", atom: internal.NewAtom("ant"), list: l, ok: true, env: map[internal.Variable]Term{
			l: List(internal.NewAtom("a"), internal.NewAtom("n"), internal.NewAtom("t")),
		}},
		{title: "atom_chars(Str, ['s', 'o', 'p']).", atom: str, list: List(internal.NewAtom("s"), internal.NewAtom("o"), internal.NewAtom("p")), ok: true, env: map[internal.Variable]Term{
			str: internal.NewAtom("sop"),
		}},
		{title: "atom_chars('North', ['N' | X]).", atom: internal.NewAtom("North"), list: PartialList(x, internal.NewAtom("N")), ok: true, env: map[internal.Variable]Term{
			x: List(internal.NewAtom("o"), internal.NewAtom("r"), internal.NewAtom("t"), internal.NewAtom("h")),
		}},
		{title: "atom_chars('soap', ['s', 'o', 'p']).", atom: internal.NewAtom("soap"), list: List(internal.NewAtom("s"), internal.NewAtom("o"), internal.NewAtom("p")), ok: false},
		{title: "atom_chars(X, Y).", atom: x, list: y, err: InstantiationError(nil)},

		// 8.16.4.3 Errors
		{title: "a", atom: x, list: PartialList(y, internal.NewAtom("a")), err: InstantiationError(nil)},
		{title: "b", atom: Integer(0), list: List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), err: typeError(validTypeAtom, Integer(0), nil)},
		{title: "c: atom is a variable", atom: x, list: Integer(0), err: typeError(validTypeList, Integer(0), nil)},
		{title: "c: atom is an atom", atom: internal.NewAtom("a"), list: Integer(0), err: typeError(validTypeList, Integer(0), nil)},
		{title: "d", atom: x, list: List(y, internal.NewAtom("a")), err: InstantiationError(nil)},
		{title: "e: atom is a variable, more than one char", atom: x, list: List(internal.NewAtom("abc")), err: typeError(validTypeCharacter, internal.NewAtom("abc"), nil)},
		{title: "e: atom is a variable, not an atom", atom: x, list: List(Integer(0)), err: typeError(validTypeCharacter, Integer(0), nil)},
		{title: "e: atom is an atom, more than one char", atom: internal.NewAtom("abc"), list: List(internal.NewAtom("ab"), internal.NewAtom("c")), err: typeError(validTypeCharacter, internal.NewAtom("ab"), nil)},
		{title: "e: atom is an atom, not an atom", atom: internal.NewAtom("abc"), list: List(Integer('a'), internal.NewAtom("b"), internal.NewAtom("c")), err: typeError(validTypeCharacter, Integer('a'), nil)},

		{title: "atom_chars('ant', ['a', X, 't']).", atom: internal.NewAtom("ant"), list: List(internal.NewAtom("a"), x, internal.NewAtom("t")), ok: true, env: map[internal.Variable]Term{
			x: internal.NewAtom("n"),
		}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := AtomChars(nil, tt.atom, tt.list, func(env *internal.Env) *internal.Promise {
				for k, v := range tt.env {
					_, ok := env.Unify(k, v)
					assert.True(t, ok)
				}
				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestAtomCodes(t *testing.T) {
	l := internal.NewVariable()
	str := internal.NewVariable()
	x, y := internal.NewVariable(), internal.NewVariable()

	tests := []struct {
		title      string
		atom, list Term
		ok         bool
		err        error
		env        map[internal.Variable]Term
	}{
		// 8.16.5.4 Examples
		{title: "atom_codes('', L).", atom: internal.NewAtom(""), list: l, ok: true, env: map[internal.Variable]Term{
			l: List(),
		}},
		{title: "atom_codes([], L).", atom: internal.atomEmptyList, list: l, ok: true, env: map[internal.Variable]Term{
			l: List(Integer('['), Integer(']')),
		}},
		{title: "atom_codes('''', L).", atom: internal.NewAtom("'"), list: l, ok: true, env: map[internal.Variable]Term{
			l: List(Integer('\'')),
		}},
		{title: "atom_codes('ant', L).", atom: internal.NewAtom("ant"), list: l, ok: true, env: map[internal.Variable]Term{
			l: List(Integer('a'), Integer('n'), Integer('t')),
		}},
		{title: "atom_codes(Str, [0's, 0'o, 0'p]).", atom: str, list: List(Integer('s'), Integer('o'), Integer('p')), ok: true, env: map[internal.Variable]Term{
			str: internal.NewAtom("sop"),
		}},
		{title: "atom_codes('North', [0'N | X]).", atom: internal.NewAtom("North"), list: PartialList(x, Integer('N')), ok: true, env: map[internal.Variable]Term{
			x: List(Integer('o'), Integer('r'), Integer('t'), Integer('h')),
		}},
		{title: "atom_codes('soap', [0's, 0'o, 0'p]).", atom: internal.NewAtom("soap"), list: List(Integer('s'), Integer('o'), Integer('p')), ok: false},
		{title: "atom_codes(X, Y).", atom: x, list: y, err: InstantiationError(nil)},

		// 8.16.5.3 Errors
		{title: "a", atom: x, list: PartialList(y, Integer(0)), err: InstantiationError(nil)},
		{title: "b", atom: Integer(0), list: l, err: typeError(validTypeAtom, Integer(0), nil)},
		{title: "c: atom is a variable", atom: x, list: Integer(0), err: typeError(validTypeList, Integer(0), nil)},
		{title: "c: atom is an atom", atom: internal.NewAtom("abc"), list: Integer(0), err: typeError(validTypeList, Integer(0), nil)},
		{title: "d", atom: x, list: List(y, Integer('b'), Integer('c')), err: InstantiationError(nil)},
		{title: "e: atom is a variable", atom: x, list: List(internal.NewAtom("a"), Integer('b'), Integer('c')), err: typeError(validTypeInteger, internal.NewAtom("a"), nil)},
		{title: "e: atom is an atom", atom: internal.NewAtom("abc"), list: List(internal.NewAtom("a"), Integer('b'), Integer('c')), err: typeError(validTypeInteger, internal.NewAtom("a"), nil)},
		{title: "f: atom is a variable", atom: x, list: List(Integer(-1), Integer('b'), Integer('c')), err: representationError(flagCharacterCode, nil)},
		{title: "f: atom is an atom", atom: internal.NewAtom("abc"), list: List(Integer(-1), Integer('b'), Integer('c')), err: representationError(flagCharacterCode, nil)},

		{title: "atom_codes('ant', [0'a, X, 0't]).", atom: internal.NewAtom("ant"), list: List(Integer('a'), x, Integer('t')), ok: true, env: map[internal.Variable]Term{
			x: Integer('n'),
		}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := AtomCodes(nil, tt.atom, tt.list, func(env *internal.Env) *internal.Promise {
				for k, v := range tt.env {
					_, ok := env.Unify(k, v)
					assert.True(t, ok)
				}
				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func TestNumberChars(t *testing.T) {
	t.Run("number to chars", func(t *testing.T) {
		t.Run("chars is a partial list", func(t *testing.T) {
			chars := internal.NewVariable()

			ok, err := NumberChars(nil, Float(23.4), chars, func(env *internal.Env) *internal.Promise {
				assert.Equal(t, List(internal.NewAtom("2"), internal.NewAtom("3"), atomDot, internal.NewAtom("4")), env.Resolve(chars))
				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("chars is a list with variables", func(t *testing.T) {
			char := internal.NewVariable()

			ok, err := NumberChars(nil, Float(23.4), List(char, internal.NewAtom("3"), atomDot, internal.NewAtom("4")), func(env *internal.Env) *internal.Promise {
				assert.Equal(t, internal.NewAtom("2"), env.Resolve(char))
				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("chars to number", func(t *testing.T) {
		num := internal.NewVariable()

		ok, err := NumberChars(nil, num, List(internal.NewAtom("2"), internal.NewAtom("3"), atomDot, internal.NewAtom("4")), func(env *internal.Env) *internal.Promise {
			assert.Equal(t, Float(23.4), env.Resolve(num))
			return internal.Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("both provided", func(t *testing.T) {
		t.Run("3.3", func(t *testing.T) {
			ok, err := NumberChars(nil, Float(3.3), List(internal.NewAtom("3"), atomDot, internal.NewAtom("3")), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("3.3E+0", func(t *testing.T) {
			ok, err := NumberChars(nil, Float(3.3), List(internal.NewAtom("3"), atomDot, internal.NewAtom("3"), internal.NewAtom("E"), atomPlus, internal.NewAtom("0")), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("num is a variable and chars is a partial list", func(t *testing.T) {
		chars := PartialList(internal.NewVariable(),
			internal.NewAtom("2"), internal.NewAtom("3"), atomDot, internal.NewAtom("4"),
		)

		ok, err := NumberChars(nil, internal.NewVariable(), chars, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("num is neither a variable nor a number", func(t *testing.T) {
		t.Run("chars is a list of one-char atoms", func(t *testing.T) {
			ok, err := NumberChars(nil, internal.NewAtom("23.4"), List(internal.NewAtom("2"), internal.NewAtom("3"), atomDot, internal.NewAtom("4")), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeNumber, internal.NewAtom("23.4"), nil), err)
			assert.False(t, ok)
		})

		t.Run("chars is not a list of one-char atoms", func(t *testing.T) {
			ok, err := NumberChars(nil, internal.NewAtom("23.4"), internal.NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeNumber, internal.NewAtom("23.4"), nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("chars is neither a partial list nor a list", func(t *testing.T) {
		t.Run("not even list-ish", func(t *testing.T) {
			ok, err := NumberChars(nil, internal.NewVariable(), internal.NewAtom("foo"), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeList, internal.NewAtom("foo"), nil), err)
			assert.False(t, ok)
		})

		t.Run("list-ish", func(t *testing.T) {
			_, err := NumberChars(nil, Integer(0), PartialList(internal.NewAtom("b"), internal.NewVariable()), Success, nil).Force(context.Background())
			_, ok := NewEnv().Unify(err.(internal.Exception).Term(), typeError(validTypeList, PartialList(internal.NewAtom("b"), internal.NewVariable()), nil).Term())
			assert.True(t, ok)
		})
	})

	t.Run("num is a variable and an element of a list prefix of chars is a variable", func(t *testing.T) {
		ok, err := NumberChars(nil, internal.NewVariable(), List(internal.NewAtom("1"), internal.NewVariable()), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("chars is a list of one-char atoms but is not parsable as a number", func(t *testing.T) {
		t.Run("not a number", func(t *testing.T) {
			ok, err := NumberChars(nil, internal.NewVariable(), List(internal.NewAtom("f"), internal.NewAtom("o"), internal.NewAtom("o")), Success, nil).Force(context.Background())
			assert.Equal(t, syntaxError(internal.errNotANumber, nil), err)
			assert.False(t, ok)
		})

		t.Run("unexpected token", func(t *testing.T) {
			ok, err := NumberChars(nil, internal.NewVariable(), List(internal.NewAtom("1"), atomDot), Success, nil).Force(context.Background())
			assert.Equal(t, syntaxError(internal.errNotANumber, nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("an element E of a list prefix of chars is neither a variable nor a one-char atom", func(t *testing.T) {
		t.Run("chars contains a variable", func(t *testing.T) {
			t.Run("not even an atom", func(t *testing.T) {
				ok, err := NumberChars(nil, Integer(100), List(internal.NewVariable(), internal.NewAtom("0"), Integer(0)), Success, nil).Force(context.Background())
				assert.Equal(t, typeError(validTypeCharacter, Integer(0), nil), err)
				assert.False(t, ok)
			})

			t.Run("atom", func(t *testing.T) {
				ok, err := NumberChars(nil, Integer(100), List(internal.NewVariable(), internal.NewAtom("00")), Success, nil).Force(context.Background())
				assert.Equal(t, typeError(validTypeCharacter, internal.NewAtom("00"), nil), err)
				assert.False(t, ok)
			})
		})

		t.Run("chars does not contain a variable", func(t *testing.T) {
			t.Run("not even an atom", func(t *testing.T) {
				ok, err := NumberChars(nil, Integer(100), List(internal.NewAtom("1"), internal.NewAtom("0"), Integer(0)), Success, nil).Force(context.Background())
				assert.Equal(t, typeError(validTypeCharacter, Integer(0), nil), err)
				assert.False(t, ok)
			})

			t.Run("atom", func(t *testing.T) {
				ok, err := NumberChars(nil, Integer(100), List(internal.NewAtom("1"), internal.NewAtom("00")), Success, nil).Force(context.Background())
				assert.Equal(t, typeError(validTypeCharacter, internal.NewAtom("00"), nil), err)
				assert.False(t, ok)
			})
		})
	})
}

func TestNumberCodes(t *testing.T) {
	a, l := internal.NewVariable(), internal.NewVariable()

	tests := []struct {
		title        string
		number, list Term
		ok           bool
		err          error
		env          map[internal.Variable]Term
	}{
		// 8.16.8.4 Examples
		{title: "number_codes(33, L).", number: Integer(33), list: l, ok: true, env: map[internal.Variable]Term{
			l: List(Integer('3'), Integer('3')),
		}},
		{title: "number_codes(33, [0'3, 0'3]).", number: Integer(33), list: List(Integer('3'), Integer('3')), ok: true},
		{title: "number_codes(33.0, L).", number: Float(33.0), list: l, ok: true, env: map[internal.Variable]Term{
			l: List(Integer('3'), Integer('3'), Integer('.'), Integer('0')),
		}},
		{title: "number_codes(33.0, [0'3, 0'., 0'3, 0'E, 0'+, 0'0, 0'1]).", number: Float(33.0), list: List(Integer('3'), Integer('.'), Integer('3'), Integer('E'), Integer('+'), Integer('0'), Integer('1')), ok: true},
		{title: "number_codes(A, [0'-, 0'2, 0'5]).", number: a, list: List(Integer('-'), Integer('2'), Integer('5')), ok: true, env: map[internal.Variable]Term{
			a: Integer(-25),
		}},
		{title: "number_codes(A, [0' , 0'3]).", number: a, list: List(Integer(' '), Integer('3')), ok: true, env: map[internal.Variable]Term{
			a: Integer(3),
		}},
		{title: "number_codes(A, [0'0, 0'x, 0'f]).", number: a, list: List(Integer('0'), Integer('x'), Integer('f')), ok: true, env: map[internal.Variable]Term{
			a: Integer(15),
		}},
		{title: "number_codes(A, [0'0, 0''', 0'a]).", number: a, list: List(Integer('0'), Integer('\''), Integer('a')), ok: true, env: map[internal.Variable]Term{
			a: Integer('a'),
		}},
		{title: "number_codes(A, [0'4, 0'., 0'2]).", number: a, list: List(Integer('4'), Integer('.'), Integer('2')), ok: true, env: map[internal.Variable]Term{
			a: Float(4.2),
		}},
		{title: "number_codes(A, [0'4, 0'2, 0'., 0'0, 0'e, 0'-, 0'1]).", number: a, list: List(Integer('4'), Integer('2'), Integer('.'), Integer('0'), Integer('e'), Integer('-'), Integer('1')), ok: true, env: map[internal.Variable]Term{
			a: Float(4.2),
		}},

		// 8.16.8.3 Errors
		{title: "a", number: a, list: l, err: InstantiationError(nil)},
		{title: "b: no variables in the list", number: internal.NewAtom("foo"), list: List(Integer('0')), err: typeError(validTypeNumber, internal.NewAtom("foo"), nil)},
		{title: "b: variables in the list", number: internal.NewAtom("foo"), list: List(internal.NewVariable(), Integer('0')), err: typeError(validTypeNumber, internal.NewAtom("foo"), nil)},
		{title: "c: without a variable element", number: Integer(0), list: internal.NewAtom("foo"), err: typeError(validTypeList, internal.NewAtom("foo"), nil)},
		{title: "c: with a variable element", number: Integer(0), list: PartialList(internal.NewAtom("foo"), internal.NewVariable()), err: typeError(validTypeList, PartialList(internal.NewAtom("foo"), internal.NewVariable()), nil)},
		{title: "d", number: a, list: List(internal.NewVariable()), err: InstantiationError(nil)},
		{title: "e", number: a, list: List(Integer('f'), Integer('o'), Integer('o')), err: syntaxError(internal.errNotANumber, nil)},
		{title: "f: without a variable element", number: Integer(0), list: List(internal.NewAtom("foo")), err: typeError(validTypeInteger, internal.NewAtom("foo"), nil)},
		{title: "f: with a variable element", number: Integer(0), list: List(internal.NewVariable(), internal.NewAtom("foo")), err: typeError(validTypeInteger, internal.NewAtom("foo"), nil)},
		{title: "g: without a variable element", number: Integer(0), list: List(Integer(utf8.MaxRune + 1)), err: representationError(flagCharacterCode, nil)},
		{title: "g: with a variable element", number: Integer(0), list: List(internal.NewVariable(), Integer(utf8.MaxRune+1)), err: representationError(flagCharacterCode, nil)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := NumberCodes(nil, tt.number, tt.list, func(env *internal.Env) *internal.Promise {
				for k, v := range tt.env {
					_, ok := env.Unify(k, v)
					assert.True(t, ok)
				}
				return internal.Bool(true)
			}, nil).Force(context.Background())
			if tt.err == nil {
				assert.NoError(t, err)
			} else if te, ok := tt.err.(internal.Exception); ok {
				_, ok := NewEnv().Unify(te.term, err.(internal.Exception).term)
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

	ss := []*internal.Stream{
		{source: f, mode: internal.ioModeRead, alias: internal.NewAtom("a"), reposition: true},
		{sink: f, mode: internal.ioModeWrite, alias: internal.NewAtom("b"), reposition: false},
		{sink: f, mode: internal.ioModeAppend, alias: internal.NewAtom("c"), reposition: true},
	}

	var vm internal.VM
	for _, s := range ss {
		vm.streams.add(s)
	}

	p, s := internal.NewVariable(), internal.NewVariable()

	tests := []struct {
		title            string
		stream, property Term
		ok               bool
		err              error
		env              []map[internal.Variable]Term
	}{
		{
			title:    "stream",
			stream:   &internal.Stream{source: f, mode: internal.ioModeRead, alias: internal.NewAtom("null"), reposition: true},
			property: p,
			ok:       true,
			env: []map[internal.Variable]Term{
				{p: atomFileName.Apply(internal.NewAtom(f.Name()))},
				{p: atomMode.Apply(atomRead)},
				{p: atomInput},
				{p: atomAlias.Apply(internal.NewAtom("null"))},
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
			env: []map[internal.Variable]Term{
				{s: ss[1]},
				{s: ss[2]},
			},
		},
		{
			title:    "alias",
			stream:   s,
			property: atomAlias.Apply(internal.NewAtom("b")),
			ok:       true,
			env: []map[internal.Variable]Term{
				{s: ss[1]},
			},
		},
		{
			title:    "position",
			stream:   s,
			property: atomPosition.Apply(Integer(0)),
			ok:       true,
			env: []map[internal.Variable]Term{
				{s: ss[0]},
				{s: ss[1]},
				{s: ss[2]},
			},
		},

		// 8.11.8.3 Errors
		{title: "b", stream: Integer(0), property: p, err: domainError(validDomainStream, Integer(0), nil)},
		{title: "c: unknown atom", stream: s, property: internal.NewAtom("foo"), err: domainError(validDomainStreamProperty, internal.NewAtom("foo"), nil)},
		{title: "c: compound with multiple args", stream: s, property: internal.NewAtom("f").Apply(internal.NewAtom("a"), internal.NewAtom("b")), err: domainError(validDomainStreamProperty, internal.NewAtom("f").Apply(internal.NewAtom("a"), internal.NewAtom("b")), nil)},
		{title: "c: compound with an unexpected integer arg", stream: s, property: atomAlias.Apply(Integer(0)), err: domainError(validDomainStreamProperty, atomAlias.Apply(Integer(0)), nil)},
		{title: "c: compound with an unexpected atom arg", stream: s, property: atomPosition.Apply(internal.NewAtom("foo")), err: domainError(validDomainStreamProperty, atomPosition.Apply(internal.NewAtom("foo")), nil)},
		{title: "c: unknown compound", stream: s, property: internal.NewAtom("foo").Apply(internal.NewAtom("bar")), err: domainError(validDomainStreamProperty, internal.NewAtom("foo").Apply(internal.NewAtom("bar")), nil)},
		{title: "c: unexpected arg", stream: s, property: Integer(0), err: domainError(validDomainStreamProperty, Integer(0), nil)},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := StreamProperty(&vm, tt.stream, tt.property, func(env *internal.Env) *internal.Promise {
				for k, v := range tt.env[0] {
					_, ok := env.Unify(k, v)
					assert.True(t, ok)
				}
				tt.env = tt.env[1:]
				return internal.Bool(len(tt.env) == 0)
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

		s := &internal.Stream{source: f, mode: internal.ioModeRead, reposition: true}

		var vm internal.VM
		ok, err := SetStreamPosition(&vm, s, Integer(0), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("seek failed", func(t *testing.T) {
		var m internal.mockFile
		m.On("Seek", mock.Anything, mock.Anything).Return(int64(0), errors.New("failed")).Once()
		defer m.AssertExpectations(t)

		s := &internal.Stream{source: &m, mode: internal.ioModeRead, reposition: true}

		var vm internal.VM
		ok, err := SetStreamPosition(&vm, s, Integer(0), Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is a variable", func(t *testing.T) {
		var vm internal.VM
		ok, err := SetStreamPosition(&vm, internal.NewVariable(), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("position is a variable", func(t *testing.T) {
		f, err := os.Open("testdata/empty.txt")
		assert.NoError(t, err)
		defer func() {
			assert.NoError(t, f.Close())
		}()

		s := &internal.Stream{source: f, mode: internal.ioModeRead, reposition: true}

		var vm internal.VM
		ok, err := SetStreamPosition(&vm, s, internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is neither a variable nor a stream term or alias", func(t *testing.T) {
		var vm internal.VM
		ok, err := SetStreamPosition(&vm, Integer(2), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainStreamOrAlias, Integer(2), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias is not associated with an open stream", func(t *testing.T) {
		var vm internal.VM
		ok, err := SetStreamPosition(&vm, internal.NewAtom("foo"), Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, existenceError(objectTypeStream, internal.NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("streamOrAlias has stream property reposition(false)", func(t *testing.T) {
		stream := &internal.Stream{source: os.Stdin}

		assert.False(t, stream.reposition)

		s := internal.NewVariable()
		env := NewEnv().
			bind(s, stream)

		var vm internal.VM
		ok, err := SetStreamPosition(&vm, s, Integer(0), Success, env).Force(context.Background())
		assert.Equal(t, permissionError(operationReposition, permissionTypeStream, s, env), err)
		assert.False(t, ok)
	})
}

func TestCharConversion(t *testing.T) {
	t.Run("register", func(t *testing.T) {
		var vm internal.VM
		ok, err := CharConversion(&vm, internal.NewAtom("a"), internal.NewAtom("b"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, 'b', vm.TypeInModule().charConversions['a'])
	})

	t.Run("remove", func(t *testing.T) {
		vm := internal.VM{
			typeIn: atomUser,
			modules: map[internal.Atom]*internal.module{
				atomUser: {
					charConversions: map[rune]rune{
						'a': 'b',
					},
				},
			},
		}
		ok, err := CharConversion(&vm, internal.NewAtom("a"), internal.NewAtom("a"), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		_, ok = vm.TypeInModule().charConversions['a']
		assert.False(t, ok)
	})

	t.Run("inChar is a variable", func(t *testing.T) {
		var vm internal.VM
		ok, err := CharConversion(&vm, internal.NewVariable(), internal.NewAtom("a"), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("outChar is a variable", func(t *testing.T) {
		var vm internal.VM
		ok, err := CharConversion(&vm, internal.NewAtom("a"), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("inChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var vm internal.VM
			ok, err := CharConversion(&vm, Integer(0), internal.NewAtom("a"), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(flagCharacter, nil), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var vm internal.VM
			ok, err := CharConversion(&vm, internal.NewAtom("foo"), internal.NewAtom("a"), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(flagCharacter, nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("outChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var vm internal.VM
			ok, err := CharConversion(&vm, internal.NewAtom("a"), Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(flagCharacter, nil), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var vm internal.VM
			ok, err := CharConversion(&vm, internal.NewAtom("a"), internal.NewAtom("foo"), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(flagCharacter, nil), err)
			assert.False(t, ok)
		})
	})
}

func TestCurrentCharConversion(t *testing.T) {
	t.Run("specified", func(t *testing.T) {
		t.Run("as is", func(t *testing.T) {
			var vm internal.VM
			ok, err := CurrentCharConversion(&vm, internal.NewAtom("a"), internal.NewAtom("a"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("converted", func(t *testing.T) {
			vm := internal.VM{
				typeIn: atomUser,
				modules: map[internal.Atom]*internal.module{
					atomUser: {
						charConversions: map[rune]rune{
							'a': 'b',
						},
					},
				},
			}
			ok, err := CurrentCharConversion(&vm, internal.NewAtom("a"), internal.NewAtom("b"), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("not specified", func(t *testing.T) {
		x, y := internal.NewVariable(), internal.NewVariable()

		var r rune
		var vm internal.VM
		ok, err := CurrentCharConversion(&vm, x, y, func(env *internal.Env) *internal.Promise {
			ref, ok := env.lookup(x)
			assert.True(t, ok)
			x, ok := ref.(internal.Atom)
			assert.True(t, ok)
			assert.Len(t, []rune(x.String()), 1)

			ref, ok = env.lookup(y)
			assert.True(t, ok)
			y, ok := ref.(internal.Atom)
			assert.True(t, ok)
			assert.Len(t, []rune(y.String()), 1)

			assert.Equal(t, r, []rune(x.String())[0])
			assert.Equal(t, r, []rune(y.String())[0])
			r++
			return internal.Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
		assert.Equal(t, rune(256), r)
	})

	t.Run("inChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var vm internal.VM
			ok, err := CurrentCharConversion(&vm, Integer(0), internal.NewAtom("b"), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(flagCharacter, nil), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var vm internal.VM
			ok, err := CurrentCharConversion(&vm, internal.NewAtom("foo"), internal.NewAtom("b"), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(flagCharacter, nil), err)
			assert.False(t, ok)
		})
	})

	t.Run("outChar is neither a variable nor a one character atom", func(t *testing.T) {
		t.Run("not even an atom", func(t *testing.T) {
			var vm internal.VM
			ok, err := CurrentCharConversion(&vm, internal.NewAtom("a"), Integer(0), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(flagCharacter, nil), err)
			assert.False(t, ok)
		})

		t.Run("multi-character atom", func(t *testing.T) {
			var vm internal.VM
			ok, err := CurrentCharConversion(&vm, internal.NewAtom("a"), internal.NewAtom("bar"), Success, nil).Force(context.Background())
			assert.Equal(t, representationError(flagCharacter, nil), err)
			assert.False(t, ok)
		})
	})
}

func TestSetPrologFlag(t *testing.T) {
	t.Run("bounded", func(t *testing.T) {
		var vm internal.VM
		ok, err := SetPrologFlag(&vm, atomBounded, internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationModify, permissionTypeFlag, atomBounded, nil), err)
		assert.False(t, ok)
	})

	t.Run("max_integer", func(t *testing.T) {
		var vm internal.VM
		ok, err := SetPrologFlag(&vm, atomMaxInteger, internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationModify, permissionTypeFlag, atomMaxInteger, nil), err)
		assert.False(t, ok)
	})

	t.Run("min_integer", func(t *testing.T) {
		var vm internal.VM
		ok, err := SetPrologFlag(&vm, atomMinInteger, internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationModify, permissionTypeFlag, atomMinInteger, nil), err)
		assert.False(t, ok)
	})

	t.Run("integer_rounding_function", func(t *testing.T) {
		var vm internal.VM
		ok, err := SetPrologFlag(&vm, atomIntegerRoundingFunction, internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationModify, permissionTypeFlag, atomIntegerRoundingFunction, nil), err)
		assert.False(t, ok)
	})

	t.Run("char_conversion", func(t *testing.T) {
		t.Run("on", func(t *testing.T) {
			var vm internal.VM
			ok, err := SetPrologFlag(&vm, atomCharConversion, atomOn, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.True(t, vm.TypeInModule().charConvEnabled)
		})

		t.Run("off", func(t *testing.T) {
			vm := internal.VM{
				typeIn: atomUser,
				modules: map[internal.Atom]*internal.module{
					atomUser: {
						charConvEnabled: true,
					},
				},
			}
			ok, err := SetPrologFlag(&vm, atomCharConversion, atomOff, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.False(t, vm.TypeInModule().charConvEnabled)
		})

		t.Run("unknown", func(t *testing.T) {
			vm := internal.VM{
				typeIn: atomUser,
				modules: map[internal.Atom]*internal.module{
					atomUser: {
						charConvEnabled: true,
					},
				},
			}
			ok, err := SetPrologFlag(&vm, atomCharConversion, internal.NewAtom("foo"), Success, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("debug", func(t *testing.T) {
		t.Run("on", func(t *testing.T) {
			var vm internal.VM
			ok, err := SetPrologFlag(&vm, atomDebug, atomOn, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.True(t, vm.TypeInModule().debug)
		})

		t.Run("off", func(t *testing.T) {
			vm := internal.VM{
				typeIn: atomUser,
				modules: map[internal.Atom]*internal.module{
					atomUser: {
						debug: true,
					},
				},
			}
			ok, err := SetPrologFlag(&vm, atomDebug, atomOff, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.False(t, vm.TypeInModule().debug)
		})

		t.Run("unknown", func(t *testing.T) {
			vm := internal.VM{
				typeIn: atomUser,
				modules: map[internal.Atom]*internal.module{
					atomUser: {
						debug: true,
					},
				},
			}
			ok, err := SetPrologFlag(&vm, atomDebug, internal.NewAtom("foo"), Success, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("max_arity", func(t *testing.T) {
		var vm internal.VM
		ok, err := SetPrologFlag(&vm, atomMaxArity, internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationModify, permissionTypeFlag, atomMaxArity, nil), err)
		assert.False(t, ok)
	})

	t.Run("unknown", func(t *testing.T) {
		t.Run("error", func(t *testing.T) {
			vm := internal.VM{
				typeIn: atomUser,
				modules: map[internal.Atom]*internal.module{
					atomUser: {
						unknown: internal.unknownFail,
					},
				},
			}
			ok, err := SetPrologFlag(&vm, atomUnknown, atomError, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, internal.unknownError, vm.TypeInModule().unknown)
		})

		t.Run("warning", func(t *testing.T) {
			var vm internal.VM
			ok, err := SetPrologFlag(&vm, atomUnknown, atomWarning, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, internal.unknownWarning, vm.TypeInModule().unknown)
		})

		t.Run("fail", func(t *testing.T) {
			var vm internal.VM
			ok, err := SetPrologFlag(&vm, atomUnknown, atomFail, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, internal.unknownFail, vm.TypeInModule().unknown)
		})

		t.Run("fail", func(t *testing.T) {
			var vm internal.VM
			ok, err := SetPrologFlag(&vm, atomUnknown, internal.NewAtom("foo"), Success, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("double_quotes", func(t *testing.T) {
		t.Run("codes", func(t *testing.T) {
			var vm internal.VM
			ok, err := SetPrologFlag(&vm, atomDoubleQuotes, atomCodes, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, internal.doubleQuotesCodes, vm.TypeInModule().doubleQuotes)
		})

		t.Run("chars", func(t *testing.T) {
			var vm internal.VM
			ok, err := SetPrologFlag(&vm, atomDoubleQuotes, atomChars, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, internal.doubleQuotesChars, vm.TypeInModule().doubleQuotes)
		})

		t.Run("atom", func(t *testing.T) {
			var vm internal.VM
			ok, err := SetPrologFlag(&vm, atomDoubleQuotes, atomAtom, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
			assert.Equal(t, internal.doubleQuotesAtom, vm.TypeInModule().doubleQuotes)
		})

		t.Run("unknown", func(t *testing.T) {
			var vm internal.VM
			ok, err := SetPrologFlag(&vm, atomDoubleQuotes, internal.NewAtom("foo"), Success, nil).Force(context.Background())
			assert.Error(t, err)
			assert.False(t, ok)
		})
	})

	t.Run("flag is a variable", func(t *testing.T) {
		var vm internal.VM
		ok, err := SetPrologFlag(&vm, internal.NewVariable(), atomFail, Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("value is a variable", func(t *testing.T) {
		var vm internal.VM
		ok, err := SetPrologFlag(&vm, atomUnknown, internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, InstantiationError(nil), err)
		assert.False(t, ok)
	})

	t.Run("flag is neither a variable nor an atom", func(t *testing.T) {
		var vm internal.VM
		ok, err := SetPrologFlag(&vm, Integer(0), atomFail, Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("flag is an atom but an invalid flag for the processor", func(t *testing.T) {
		var vm internal.VM
		ok, err := SetPrologFlag(&vm, internal.NewAtom("foo"), atomFail, Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainPrologFlag, internal.NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})

	t.Run("value is inadmissible for flag", func(t *testing.T) {
		var vm internal.VM
		ok, err := SetPrologFlag(&vm, atomUnknown, Integer(0), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainFlagValue, &compound{
			functor: atomPlus,
			args:    []Term{atomUnknown, Integer(0)},
		}, nil), err)
		assert.False(t, ok)
	})

	t.Run("value is admissible for flag but the flag is not modifiable", func(t *testing.T) {
		var vm internal.VM
		ok, err := SetPrologFlag(&vm, atomBounded, atomTrue, Success, nil).Force(context.Background())
		assert.Equal(t, permissionError(operationModify, permissionTypeFlag, atomBounded, nil), err)
		assert.False(t, ok)
	})
}

func TestCurrentPrologFlag(t *testing.T) {
	var vm internal.VM

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
		flag, value := internal.NewVariable(), internal.NewVariable()
		var c int
		ok, err := CurrentPrologFlag(&vm, flag, value, func(env *internal.Env) *internal.Promise {
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
				assert.Equal(t, internal.NewAtom(vm.TypeInModule().unknown.String()), env.Resolve(value))
			case 8:
				assert.Equal(t, atomDoubleQuotes, env.Resolve(flag))
				assert.Equal(t, internal.NewAtom(vm.TypeInModule().doubleQuotes.String()), env.Resolve(value))
			default:
				assert.Fail(t, "unreachable")
			}
			c++
			return internal.Bool(false)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
		assert.Equal(t, 9, c)
	})

	t.Run("flag is neither a variable nor an atom", func(t *testing.T) {
		var vm internal.VM
		ok, err := CurrentPrologFlag(&vm, Integer(0), atomError, Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeAtom, Integer(0), nil), err)
		assert.False(t, ok)
	})

	t.Run("flag is an atom but an invalid flag for the processor", func(t *testing.T) {
		var vm internal.VM
		ok, err := CurrentPrologFlag(&vm, internal.NewAtom("foo"), atomError, Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainPrologFlag, internal.NewAtom("foo"), nil), err)
		assert.False(t, ok)
	})
}

func TestExpandTerm(t *testing.T) {
	f, g := internal.NewAtom("f"), internal.NewAtom("g")
	a, b, c := internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")
	s := internal.NewAtom("s")

	x := internal.NewVariable()

	var vm internal.VM
	_, err := vm.Compile(context.Background(), `
term_expansion(f(X), g(X)).
`)
	assert.NoError(t, err)

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
				return atomColonMinus.Apply(
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
				return atomColonMinus.Apply(
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
				return atomColonMinus.Apply(
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
				return atomColonMinus.Apply(
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
				return atomColonMinus.Apply(
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
				return atomColonMinus.Apply(
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
				return atomColonMinus.Apply(
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
			in:    atomArrow.Apply(s, internal.atomEmptyBlock.Apply(a)),
			out: func() Term {
				return atomColonMinus.Apply(
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
				return atomColonMinus.Apply(
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
				return atomColonMinus.Apply(
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
				return atomColonMinus.Apply(
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
				return atomColonMinus.Apply(
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
				return atomColonMinus.Apply(
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
			in:    atomArrow.Apply(atomComma.Apply(internal.NewAtom("phrase1"), List(internal.NewAtom("word"))), atomComma.Apply(internal.NewAtom("phrase2"), internal.NewAtom("phrase3"))),
			out: func() Term {
				return atomColonMinus.Apply(
					internal.NewAtom("phrase1").Apply(lastVariable()+1, lastVariable()+3),
					atomComma.Apply(
						atomComma.Apply(
							internal.NewAtom("phrase2").Apply(lastVariable()+1, lastVariable()+4),
							internal.NewAtom("phrase3").Apply(lastVariable()+4, lastVariable()+2),
						),
						atomEqual.Apply(lastVariable()+3, PartialList(lastVariable()+2, internal.NewAtom("word"))),
					),
				)
			},
			ok: true,
		},
		{
			title: "with semicontexts: head is not callable",
			in:    atomArrow.Apply(atomComma.Apply(Integer(0), List(internal.NewAtom("word"))), atomComma.Apply(internal.NewAtom("phrase2"), internal.NewAtom("phrase3"))),
			out: func() Term {
				return atomArrow.Apply(atomComma.Apply(Integer(0), List(internal.NewAtom("word"))), atomComma.Apply(internal.NewAtom("phrase2"), internal.NewAtom("phrase3")))
			},
			ok: true,
		},
		{
			title: "with semicontexts: semicontext is not callable",
			in:    atomArrow.Apply(atomComma.Apply(internal.NewAtom("phrase1"), Integer(0)), atomComma.Apply(internal.NewAtom("phrase2"), internal.NewAtom("phrase3"))),
			out: func() Term {
				return atomArrow.Apply(atomComma.Apply(internal.NewAtom("phrase1"), Integer(0)), atomComma.Apply(internal.NewAtom("phrase2"), internal.NewAtom("phrase3")))
			},
			ok: true,
		},
		{
			title: "with semicontexts: body is not callable",
			in:    atomArrow.Apply(atomComma.Apply(internal.NewAtom("phrase1"), List(internal.NewAtom("word"))), atomComma.Apply(Integer(0), internal.NewAtom("phrase3"))),
			out: func() Term {
				return atomArrow.Apply(atomComma.Apply(internal.NewAtom("phrase1"), List(internal.NewAtom("word"))), atomComma.Apply(Integer(0), internal.NewAtom("phrase3")))
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
				n       = internal.NewVariable()
				elem    = internal.NewVariable()
				results []Term
			)
			ok, err := Nth0(nil, n, List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), elem, func(env *internal.Env) *internal.Promise {
				results = append(results, pair.Apply(env.Resolve(n), env.Resolve(elem)))
				return internal.Bool(false)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)

			assert.Equal(t, []Term{
				pair.Apply(Integer(0), internal.NewAtom("a")),
				pair.Apply(Integer(1), internal.NewAtom("b")),
				pair.Apply(Integer(2), internal.NewAtom("c")),
			}, results)
		})

		t.Run("list is an improper list", func(t *testing.T) {
			_, err := Nth0(nil, internal.NewVariable(), PartialList(internal.NewVariable(), internal.NewAtom("a")), internal.NewVariable(), Failure, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
		})
	})

	t.Run("n is an integer", func(t *testing.T) {
		t.Run("list is a proper list", func(t *testing.T) {
			t.Run("n is a valid index", func(t *testing.T) {
				ok, err := Nth0(nil, Integer(1), List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), internal.NewAtom("b"), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("n is too small for an index", func(t *testing.T) {
				ok, err := Nth0(nil, Integer(-1), List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), internal.NewVariable(), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.False(t, ok)
			})

			t.Run("n is too big for an index", func(t *testing.T) {
				ok, err := Nth0(nil, Integer(3), List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), internal.NewVariable(), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.False(t, ok)
			})
		})

		t.Run("list is an improper list", func(t *testing.T) {
			_, err := Nth0(nil, Integer(1), PartialList(internal.NewVariable(), internal.NewAtom("a")), internal.NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
		})
	})

	t.Run("n is neither a variable nor an integer", func(t *testing.T) {
		_, err := Nth0(nil, internal.NewAtom("foo"), List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, internal.NewAtom("foo"), nil), err)
	})
}

func TestNth1(t *testing.T) {
	t.Run("n is a variable", func(t *testing.T) {
		t.Run("list is a proper list", func(t *testing.T) {
			pair := atomMinus
			var (
				n       = internal.NewVariable()
				elem    = internal.NewVariable()
				results []Term
			)
			ok, err := Nth1(nil, n, List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), elem, func(env *internal.Env) *internal.Promise {
				results = append(results, pair.Apply(env.Resolve(n), env.Resolve(elem)))
				return internal.Bool(false)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)

			assert.Equal(t, []Term{
				pair.Apply(Integer(1), internal.NewAtom("a")),
				pair.Apply(Integer(2), internal.NewAtom("b")),
				pair.Apply(Integer(3), internal.NewAtom("c")),
			}, results)
		})

		t.Run("list is an improper list", func(t *testing.T) {
			_, err := Nth1(nil, internal.NewVariable(), PartialList(internal.NewVariable(), internal.NewAtom("a")), internal.NewVariable(), Failure, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
		})
	})

	t.Run("n is an integer", func(t *testing.T) {
		t.Run("list is a proper list", func(t *testing.T) {
			t.Run("n is a valid index", func(t *testing.T) {
				ok, err := Nth1(nil, Integer(2), List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), internal.NewAtom("b"), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("n is too small for an index", func(t *testing.T) {
				ok, err := Nth1(nil, Integer(0), List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), internal.NewVariable(), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.False(t, ok)
			})

			t.Run("n is too big for an index", func(t *testing.T) {
				ok, err := Nth1(nil, Integer(4), List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), internal.NewVariable(), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.False(t, ok)
			})
		})

		t.Run("list is an improper list", func(t *testing.T) {
			_, err := Nth1(nil, Integer(2), PartialList(internal.NewVariable(), internal.NewAtom("a")), internal.NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
		})
	})

	t.Run("n is neither a variable nor an integer", func(t *testing.T) {
		_, err := Nth1(nil, internal.NewAtom("foo"), List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, internal.NewAtom("foo"), nil), err)
	})
}

func TestSucc(t *testing.T) {
	t.Run("x is a variable", func(t *testing.T) {
		t.Run("s is a variable", func(t *testing.T) {
			_, err := Succ(nil, internal.NewVariable(), internal.NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, InstantiationError(nil), err)
		})

		t.Run("s is an integer", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				x := internal.NewVariable()
				ok, err := Succ(nil, x, Integer(1), func(env *internal.Env) *internal.Promise {
					assert.Equal(t, Integer(0), env.Resolve(x))
					return internal.Bool(true)
				}, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("s < 0", func(t *testing.T) {
				_, err := Succ(nil, internal.NewVariable(), Integer(-1), Success, nil).Force(context.Background())
				assert.Equal(t, domainError(validDomainNotLessThanZero, Integer(-1), nil), err)
			})

			t.Run("s = 0", func(t *testing.T) {
				ok, err := Succ(nil, internal.NewVariable(), Integer(0), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.False(t, ok)
			})
		})

		t.Run("s is neither a variable nor an integer", func(t *testing.T) {
			_, err := Succ(nil, internal.NewVariable(), Float(1), Success, nil).Force(context.Background())
			assert.Equal(t, typeError(validTypeInteger, Float(1), nil), err)
		})
	})

	t.Run("x is an integer", func(t *testing.T) {
		t.Run("s is a variable", func(t *testing.T) {
			s := internal.NewVariable()
			ok, err := Succ(nil, Integer(0), s, func(env *internal.Env) *internal.Promise {
				assert.Equal(t, Integer(1), env.Resolve(s))
				return internal.Bool(true)
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
		_, err := Succ(nil, Float(0), internal.NewVariable(), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, Float(0), nil), err)
	})
}

func TestLength(t *testing.T) {
	t.Run("list is a list", func(t *testing.T) {
		t.Run("length is a variable", func(t *testing.T) {
			n := internal.NewVariable()
			ok, err := Length(nil, List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), n, func(env *internal.Env) *internal.Promise {
				assert.Equal(t, Integer(3), env.Resolve(n))
				return internal.Bool(true)
			}, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("length is an integer", func(t *testing.T) {
			t.Run("length is the exact length of list", func(t *testing.T) {
				ok, err := Length(nil, List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), Integer(3), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("length is smaller than the length fo list", func(t *testing.T) {
				ok, err := Length(nil, List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), Integer(2), Success, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.False(t, ok)
			})
		})
	})

	t.Run("list is a partial list", func(t *testing.T) {
		t.Run("length is a variable", func(t *testing.T) {
			t.Run("length and the suffix of list are different", func(t *testing.T) {
				l := internal.NewVariable()
				n := internal.NewVariable()
				var count int
				ok, err := Length(nil, PartialList(l, internal.NewAtom("a"), internal.NewAtom("b")), n, func(env *internal.Env) *internal.Promise {
					var ret []internal.Variable
					iter := internal.ListIterator{List: l, Env: env}
					for iter.Next() {
						ret = append(ret, env.Resolve(iter.Current()).(internal.Variable))
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
						return internal.Bool(true)
					}

					count++
					return internal.Bool(false)
				}, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("length and the suffix of list are the same", func(t *testing.T) {
				l := internal.NewVariable()
				_, err := Length(nil, PartialList(l, internal.NewAtom("a"), internal.NewAtom("b")), l, Success, nil).Force(context.Background())
				assert.Equal(t, resourceError(resourceFiniteMemory, nil), err)
			})
		})

		t.Run("length is an integer", func(t *testing.T) {
			t.Run("small", func(t *testing.T) {
				l := internal.NewVariable()
				ok, err := Length(nil, PartialList(l, internal.NewAtom("a"), internal.NewAtom("b")), Integer(3), func(env *internal.Env) *internal.Promise {
					iter := internal.ListIterator{List: l, Env: env}
					assert.True(t, iter.Next())
					assert.False(t, iter.Next())
					return internal.Bool(true)
				}, nil).Force(context.Background())
				assert.NoError(t, err)
				assert.True(t, ok)
			})

			t.Run("large", func(t *testing.T) {
				l := internal.NewVariable()
				_, err := Length(nil, PartialList(l, internal.NewAtom("a"), internal.NewAtom("b")), Integer(math.MaxInt64), Success, nil).Force(context.Background())
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

				l := internal.NewVariable()
				_, err := Length(nil, PartialList(l, internal.NewAtom("a"), internal.NewAtom("b")), Integer(100*1024*1024), Success, nil).Force(context.Background())
				assert.Equal(t, resourceError(resourceMemory, nil), err)
			})
		})
	})

	t.Run("list is neither a list nor a partial list", func(t *testing.T) {
		t.Run("the suffix is an atom", func(t *testing.T) {
			ok, err := Length(nil, internal.NewAtom("foo"), Integer(3), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})

		t.Run("the suffix is a compound", func(t *testing.T) {
			ok, err := Length(nil, internal.NewAtom("foo").Apply(internal.NewAtom("bar")), Integer(3), Success, nil).Force(context.Background())
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
		_, err := Length(nil, List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), internal.NewAtom("three"), Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, internal.NewAtom("three"), nil), err)
	})

	t.Run("length is an integer that is less than zero", func(t *testing.T) {
		_, err := Length(nil, List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), Integer(-3), Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainNotLessThanZero, Integer(-3), nil), err)
	})

	t.Run("list is so long that an integer cannot represent its length", func(t *testing.T) {
		internal.maxInt = 2
		defer func() {
			internal.maxInt = math.MaxInt64
		}()

		t.Run("list is a list", func(t *testing.T) {
			_, err := Length(nil, List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), internal.NewVariable(), Success, nil).Force(context.Background())
			assert.Equal(t, resourceError(resourceFiniteMemory, nil), err)
		})

		t.Run("list is a partial list", func(t *testing.T) {
			_, err := Length(nil, internal.NewVariable(), internal.NewVariable(), Failure, nil).Force(context.Background())
			assert.Equal(t, representationError(flagMaxInteger, nil), err)
		})
	})
}

func TestSkipMaxList(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		t.Run("without max", func(t *testing.T) {
			ok, err := SkipMaxList(nil, Integer(3), internal.NewVariable(), List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), internal.atomEmptyList, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("with max", func(t *testing.T) {
			ok, err := SkipMaxList(nil, Integer(2), Integer(2), List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), List(internal.NewAtom("c")), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("max is neither a variable nor an integer", func(t *testing.T) {
		_, err := SkipMaxList(nil, Integer(3), internal.NewAtom("foo"), List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), internal.atomEmptyList, Success, nil).Force(context.Background())
		assert.Equal(t, typeError(validTypeInteger, internal.NewAtom("foo"), nil), err)
	})

	t.Run("max is negative", func(t *testing.T) {
		_, err := SkipMaxList(nil, Integer(3), Integer(-1), List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), internal.atomEmptyList, Success, nil).Force(context.Background())
		assert.Equal(t, domainError(validDomainNotLessThanZero, Integer(-1), nil), err)
	})
}

func TestRepeat(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	c := 0

	_, err := Repeat(nil, func(*internal.Env) *internal.Promise {
		c++
		cancel()
		return internal.Bool(true)
	}, nil).Force(ctx)
	assert.Equal(t, context.Canceled, err)

	assert.Equal(t, 1, c)
}

func TestNot(t *testing.T) {
	e := errors.New("failed")

	var m internal.module
	m.SetPredicate0("true", func(_ *internal.VM, k internal.Cont, env *internal.Env) *internal.Promise {
		return k(env)
	})
	m.SetPredicate0("false", func(*internal.VM, internal.Cont, *internal.Env) *internal.Promise {
		return internal.Bool(false)
	})
	m.SetPredicate0("error", func(*internal.VM, internal.Cont, *internal.Env) *internal.Promise {
		return internal.Error(e)
	})
	vm := internal.VM{
		typeIn: atomUser,
		modules: map[internal.Atom]*internal.module{
			atomUser: &m,
		},
	}

	ok, err := Not(&vm, atomTrue, Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.False(t, ok)

	ok, err = Not(&vm, atomFalse, Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	_, err = Not(&vm, atomError, Success, nil).Force(context.Background())
	assert.Equal(t, e, err)
}

func TestAppend(t *testing.T) {
	xs, ys, zs := internal.NewVariable(), internal.NewVariable(), internal.NewVariable()
	tests := []struct {
		title      string
		xs, ys, zs Term
		ok         bool
		err        error
		env        []map[internal.Variable]Term
	}{
		// p.p.2.4 Examples
		{title: `append([a,b],[c,d], Xs).`, xs: List(internal.NewAtom("a"), internal.NewAtom("b")), ys: List(internal.NewAtom("c"), internal.NewAtom("d")), zs: xs, ok: true, env: []map[internal.Variable]Term{
			{xs: List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c"), internal.NewAtom("d"))},
		}},
		{title: `append([a], nonlist, Xs).`, xs: List(internal.NewAtom("a")), ys: internal.NewAtom("nonlist"), zs: xs, ok: true, env: []map[internal.Variable]Term{
			{xs: PartialList(internal.NewAtom("nonlist"), internal.NewAtom("a"))},
		}},
		{title: `append([a], Ys, Zs).`, xs: List(internal.NewAtom("a")), ys: ys, zs: zs, ok: true, env: []map[internal.Variable]Term{
			{zs: PartialList(ys, internal.NewAtom("a"))},
		}},
		{title: `append(Xs, Ys, [a,b,c]).`, xs: xs, ys: ys, zs: List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), ok: true, env: []map[internal.Variable]Term{
			{xs: List(), ys: List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c"))},
			{xs: List(internal.NewAtom("a")), ys: List(internal.NewAtom("b"), internal.NewAtom("c"))},
			{xs: List(internal.NewAtom("a"), internal.NewAtom("b")), ys: List(internal.NewAtom("c"))},
			{xs: List(internal.NewAtom("a"), internal.NewAtom("b"), internal.NewAtom("c")), ys: List()},
		}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := Append(nil, tt.xs, tt.ys, tt.zs, func(env *internal.Env) *internal.Promise {
				for k, v := range tt.env[0] {
					_, ok := env.Unify(k, v)
					assert.True(t, ok)
				}
				tt.env = tt.env[1:]
				return internal.Bool(len(tt.env) == 0)
			}, nil).Force(context.Background())
			assert.Equal(t, tt.ok, ok)
			assert.Equal(t, tt.err, err)
		})
	}
}

func Test_variant(t *testing.T) {
	f, g := internal.NewAtom("f"), internal.NewAtom("g")
	a, b := internal.NewVariable(), internal.NewVariable()
	x, y, z := internal.NewVariable(), internal.NewVariable(), internal.NewVariable()
	p, q := internal.NewVariable(), internal.NewVariable()

	tests := []struct {
		t1, t2 Term
		result bool
	}{
		{
			t1:     f.Apply(a, b, a),
			t2:     f.Apply(x, y, x),
			result: true,
		},
		{
			t1:     g.Apply(a, b),
			t2:     g.Apply(internal.NewVariable(), internal.NewVariable()),
			result: true,
		},
		{
			t1:     atomPlus.Apply(p, q),
			t2:     atomPlus.Apply(p, q),
			result: true,
		},
		{
			t1:     f.Apply(a, a),
			t2:     f.Apply(x, y),
			result: false,
		},
		{
			t1:     f.Apply(a, a),
			t2:     f.Apply(x, Integer(0)),
			result: false,
		},
		{
			t1:     f.Apply(a, b),
			t2:     g.Apply(x, y),
			result: false,
		},
		{
			t1:     f.Apply(a, b),
			t2:     f.Apply(x, y, z),
			result: false,
		},
		{
			t1:     f.Apply(a, b),
			t2:     Integer(0),
			result: false,
		},
		{
			t1:     Integer(1),
			t2:     Integer(0),
			result: false,
		},
	}

	for _, tt := range tests {
		assert.Equal(t, tt.result, variant(tt.t1, tt.t2, nil))
	}
}

func Test_iteratedGoalTerm(t *testing.T) {
	x := internal.NewVariable()

	tests := []struct {
		t, g Term
	}{
		{t: &compound{
			functor: atomCaret,
			args: []Term{
				x,
				&compound{
					functor: internal.NewAtom("foo"),
					args:    []Term{x},
				},
			},
		}, g: &compound{
			functor: internal.NewAtom("foo"),
			args:    []Term{x},
		}},
		{
			t: atomCaret.Apply(internal.NewVariable(), atomCaret.Apply(internal.NewVariable(), atomEqual.Apply(x, Integer(1)))),
			g: atomEqual.Apply(x, Integer(1)),
		},
	}

	for _, tt := range tests {
		assert.Equal(t, tt.g, iteratedGoalTerm(tt.t, nil))
	}
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
