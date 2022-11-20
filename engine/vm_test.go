package engine

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestVM_Register0(t *testing.T) {
	var vm VM
	vm.Register0("foo", func(k func(*Env) *Promise, env *Env) *Promise {
		return k(env)
	})
	p := vm.procedures[ProcedureIndicator{Name: NewAtom("foo"), Arity: 0}]

	t.Run("ok", func(t *testing.T) {
		ok, err := p.Call(&vm, []Term{}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.Call(&vm, []Term{NewAtom("a")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)

		assert.Equal(t, "wrong number of arguments: expected=0, actual=[a]", err.Error())
	})
}

func TestVM_Register1(t *testing.T) {
	var vm VM
	vm.Register1("foo", func(a Term, k func(*Env) *Promise, env *Env) *Promise {
		return k(env)
	})
	p := vm.procedures[ProcedureIndicator{Name: NewAtom("foo"), Arity: 1}]

	t.Run("ok", func(t *testing.T) {
		ok, err := p.Call(&vm, []Term{NewAtom("a")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.Call(&vm, []Term{NewAtom("a"), NewAtom("b")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestVM_Register2(t *testing.T) {
	var vm VM
	vm.Register2("foo", func(a, b Term, k func(*Env) *Promise, env *Env) *Promise {
		return k(env)
	})
	p := vm.procedures[ProcedureIndicator{Name: NewAtom("foo"), Arity: 2}]

	t.Run("ok", func(t *testing.T) {
		ok, err := p.Call(&vm, []Term{NewAtom("a"), NewAtom("b")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.Call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestVM_Register3(t *testing.T) {
	var vm VM
	vm.Register3("foo", func(a, b, c Term, k func(*Env) *Promise, env *Env) *Promise {
		return k(env)
	})
	p := vm.procedures[ProcedureIndicator{Name: NewAtom("foo"), Arity: 3}]

	t.Run("ok", func(t *testing.T) {
		ok, err := p.Call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.Call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestVM_Register4(t *testing.T) {
	var vm VM
	vm.Register4("foo", func(a, b, c, d Term, k func(*Env) *Promise, env *Env) *Promise {
		return k(env)
	})
	p := vm.procedures[ProcedureIndicator{Name: NewAtom("foo"), Arity: 4}]

	t.Run("ok", func(t *testing.T) {
		ok, err := p.Call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.Call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestVM_Register5(t *testing.T) {
	var vm VM
	vm.Register5("foo", func(a, b, c, d, e Term, k func(*Env) *Promise, env *Env) *Promise {
		return k(env)
	})
	p := vm.procedures[ProcedureIndicator{Name: NewAtom("foo"), Arity: 5}]

	t.Run("ok", func(t *testing.T) {
		ok, err := p.Call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.Call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestVM_Register6(t *testing.T) {
	var vm VM
	vm.Register6("foo", func(a, b, c, d, e, f Term, k func(*Env) *Promise, env *Env) *Promise {
		return k(env)
	})
	p := vm.procedures[ProcedureIndicator{Name: NewAtom("foo"), Arity: 6}]

	t.Run("ok", func(t *testing.T) {
		ok, err := p.Call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.Call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestVM_Register7(t *testing.T) {
	var vm VM
	vm.Register7("foo", func(a, b, c, d, e, f, g Term, k func(*Env) *Promise, env *Env) *Promise {
		return k(env)
	})
	p := vm.procedures[ProcedureIndicator{Name: NewAtom("foo"), Arity: 7}]

	t.Run("ok", func(t *testing.T) {
		ok, err := p.Call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.Call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g"), NewAtom("h")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestVM_Register8(t *testing.T) {
	var vm VM
	vm.Register8("foo", func(a, b, c, d, e, f, g, h Term, k func(*Env) *Promise, env *Env) *Promise {
		return k(env)
	})
	p := vm.procedures[ProcedureIndicator{Name: NewAtom("foo"), Arity: 8}]

	t.Run("ok", func(t *testing.T) {
		ok, err := p.Call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g"), NewAtom("h")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.Call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g"), NewAtom("h"), NewAtom("i")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestVM_Arrive(t *testing.T) {
	t.Run("success", func(t *testing.T) {
		vm := VM{
			procedures: map[ProcedureIndicator]procedure{
				{Name: NewAtom("foo"), Arity: 1}: predicate1(func(t Term, k func(*Env) *Promise, env *Env) *Promise {
					return k(env)
				}),
			},
		}
		ok, err := vm.Arrive(ProcedureIndicator{Name: NewAtom("foo"), Arity: 1}, []Term{NewAtom("a")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("unknown procedure", func(t *testing.T) {
		t.Run("error", func(t *testing.T) {
			vm := VM{
				unknown: unknownError,
			}
			ok, err := vm.Arrive(ProcedureIndicator{Name: NewAtom("foo"), Arity: 1}, []Term{NewAtom("a")}, Success, nil).Force(context.Background())
			assert.Equal(t, ExistenceError(ObjectTypeProcedure, &compound{
				functor: atomSlash,
				args:    []Term{NewAtom("foo"), Integer(1)},
			}, nil), err)
			assert.False(t, ok)
		})

		t.Run("warning", func(t *testing.T) {
			var warned bool
			vm := VM{
				unknown: unknownWarning,
				OnUnknown: func(pi ProcedureIndicator, args []Term, env *Env) {
					assert.Equal(t, ProcedureIndicator{Name: NewAtom("foo"), Arity: 1}, pi)
					assert.Equal(t, []Term{NewAtom("a")}, args)
					assert.Nil(t, env)
					warned = true
				},
			}
			ok, err := vm.Arrive(ProcedureIndicator{Name: NewAtom("foo"), Arity: 1}, []Term{NewAtom("a")}, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
			assert.True(t, warned)
		})

		t.Run("fail", func(t *testing.T) {
			vm := VM{
				unknown: unknownFail,
			}
			ok, err := vm.Arrive(ProcedureIndicator{Name: NewAtom("foo"), Arity: 1}, []Term{NewAtom("a")}, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})
}

func TestNewProcedureIndicator(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		pi, err := NewProcedureIndicator(&compound{
			functor: atomSlash,
			args:    []Term{NewAtom("foo"), Integer(2)},
		}, nil)
		assert.NoError(t, err)
		assert.Equal(t, ProcedureIndicator{Name: NewAtom("foo"), Arity: 2}, pi)
	})

	t.Run("variable", func(t *testing.T) {
		pi, err := NewProcedureIndicator(NewNamedVariable("PI"), nil)
		assert.Equal(t, InstantiationError(nil), err)
		assert.Zero(t, pi)
	})

	t.Run("atomic", func(t *testing.T) {
		pi, err := NewProcedureIndicator(NewAtom("foo"), nil)
		assert.Equal(t, TypeError(ValidTypePredicateIndicator, NewAtom("foo"), nil), err)
		assert.Zero(t, pi)
	})

	t.Run("non-PI compound", func(t *testing.T) {
		pi, err := NewProcedureIndicator(&compound{
			functor: NewAtom("foo"),
			args:    []Term{NewAtom("a"), NewAtom("b")},
		}, nil)
		assert.Equal(t, TypeError(ValidTypePredicateIndicator, &compound{
			functor: NewAtom("foo"),
			args:    []Term{NewAtom("a"), NewAtom("b")},
		}, nil), err)
		assert.Zero(t, pi)
	})

	t.Run("variable functor", func(t *testing.T) {
		pi, err := NewProcedureIndicator(&compound{
			functor: atomSlash,
			args:    []Term{NewNamedVariable("functor"), Integer(2)},
		}, nil)
		assert.Equal(t, InstantiationError(nil), err)
		assert.Zero(t, pi)
	})

	t.Run("non-atom functor", func(t *testing.T) {
		pi, err := NewProcedureIndicator(&compound{
			functor: atomSlash,
			args:    []Term{Integer(0), Integer(2)},
		}, nil)
		assert.Equal(t, TypeError(ValidTypePredicateIndicator, &compound{
			functor: atomSlash,
			args:    []Term{Integer(0), Integer(2)},
		}, nil), err)
		assert.Zero(t, pi)
	})

	t.Run("variable arity", func(t *testing.T) {
		pi, err := NewProcedureIndicator(&compound{
			functor: atomSlash,
			args:    []Term{NewAtom("foo"), NewNamedVariable("Arity")},
		}, nil)
		assert.Equal(t, InstantiationError(nil), err)
		assert.Zero(t, pi)
	})

	t.Run("non-integer arity", func(t *testing.T) {
		pi, err := NewProcedureIndicator(&compound{
			functor: atomSlash,
			args:    []Term{NewAtom("foo"), NewAtom("arity")},
		}, nil)
		assert.Equal(t, TypeError(ValidTypePredicateIndicator, &compound{
			functor: atomSlash,
			args:    []Term{NewAtom("foo"), NewAtom("arity")},
		}, nil), err)
		assert.Zero(t, pi)
	})
}

func TestProcedureIndicator_String(t *testing.T) {
	assert.Equal(t, `foo/2`, ProcedureIndicator{Name: NewAtom("foo"), Arity: 2}.String())
}

func TestProcedureIndicator_Term(t *testing.T) {
	assert.Equal(t, &compound{
		functor: atomSlash,
		args:    []Term{NewAtom("foo"), Integer(2)},
	}, ProcedureIndicator{Name: NewAtom("foo"), Arity: 2}.Term())
}

func TestProcedureIndicator_Apply(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		c, err := ProcedureIndicator{Name: NewAtom("foo"), Arity: 2}.Apply(NewAtom("a"), NewAtom("b"))
		assert.NoError(t, err)
		assert.Equal(t, &compound{
			functor: NewAtom("foo"),
			args:    []Term{NewAtom("a"), NewAtom("b")},
		}, c)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		c, err := ProcedureIndicator{Name: NewAtom("foo"), Arity: 2}.Apply(NewAtom("a"), NewAtom("b"), NewAtom("c"))
		assert.Error(t, err)
		assert.Nil(t, c)
	})
}
