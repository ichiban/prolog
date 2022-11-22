package engine

import (
	"context"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestVM_Register0(t *testing.T) {
	var vm VM
	vm.Register0(NewAtom("foo"), func(_ *VM, k func(*Env) *Promise, env *Env) *Promise {
		return k(env)
	})
	p := vm.procedures[procedureIndicator{name: NewAtom("foo"), arity: 0}]

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(&vm, []Term{}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(&vm, []Term{NewAtom("a")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)

		assert.Equal(t, "wrong number of arguments: expected=0, actual=[a]", err.Error())
	})
}

func TestVM_Register1(t *testing.T) {
	var vm VM
	vm.Register1(NewAtom("foo"), func(_ *VM, a Term, k func(*Env) *Promise, env *Env) *Promise {
		return k(env)
	})
	p := vm.procedures[procedureIndicator{name: NewAtom("foo"), arity: 1}]

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(&vm, []Term{NewAtom("a")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(&vm, []Term{NewAtom("a"), NewAtom("b")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestVM_Register2(t *testing.T) {
	var vm VM
	vm.Register2(NewAtom("foo"), func(_ *VM, a, b Term, k func(*Env) *Promise, env *Env) *Promise {
		return k(env)
	})
	p := vm.procedures[procedureIndicator{name: NewAtom("foo"), arity: 2}]

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(&vm, []Term{NewAtom("a"), NewAtom("b")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestVM_Register3(t *testing.T) {
	var vm VM
	vm.Register3(NewAtom("foo"), func(_ *VM, a, b, c Term, k func(*Env) *Promise, env *Env) *Promise {
		return k(env)
	})
	p := vm.procedures[procedureIndicator{name: NewAtom("foo"), arity: 3}]

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestVM_Register4(t *testing.T) {
	var vm VM
	vm.Register4(NewAtom("foo"), func(_ *VM, a, b, c, d Term, k func(*Env) *Promise, env *Env) *Promise {
		return k(env)
	})
	p := vm.procedures[procedureIndicator{name: NewAtom("foo"), arity: 4}]

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestVM_Register5(t *testing.T) {
	var vm VM
	vm.Register5(NewAtom("foo"), func(_ *VM, a, b, c, d, e Term, k func(*Env) *Promise, env *Env) *Promise {
		return k(env)
	})
	p := vm.procedures[procedureIndicator{name: NewAtom("foo"), arity: 5}]

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestVM_Register6(t *testing.T) {
	var vm VM
	vm.Register6(NewAtom("foo"), func(_ *VM, a, b, c, d, e, f Term, k func(*Env) *Promise, env *Env) *Promise {
		return k(env)
	})
	p := vm.procedures[procedureIndicator{name: NewAtom("foo"), arity: 6}]

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestVM_Register7(t *testing.T) {
	var vm VM
	vm.Register7(NewAtom("foo"), func(_ *VM, a, b, c, d, e, f, g Term, k func(*Env) *Promise, env *Env) *Promise {
		return k(env)
	})
	p := vm.procedures[procedureIndicator{name: NewAtom("foo"), arity: 7}]

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g"), NewAtom("h")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestVM_Register8(t *testing.T) {
	var vm VM
	vm.Register8(NewAtom("foo"), func(_ *VM, a, b, c, d, e, f, g, h Term, k func(*Env) *Promise, env *Env) *Promise {
		return k(env)
	})
	p := vm.procedures[procedureIndicator{name: NewAtom("foo"), arity: 8}]

	t.Run("ok", func(t *testing.T) {
		ok, err := p.call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g"), NewAtom("h")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		ok, err := p.call(&vm, []Term{NewAtom("a"), NewAtom("b"), NewAtom("c"), NewAtom("d"), NewAtom("e"), NewAtom("f"), NewAtom("g"), NewAtom("h"), NewAtom("i")}, Success, nil).Force(context.Background())
		assert.Error(t, err)
		assert.False(t, ok)
	})
}

func TestVM_Arrive(t *testing.T) {
	t.Run("success", func(t *testing.T) {
		vm := VM{
			procedures: map[procedureIndicator]procedure{
				{name: NewAtom("foo"), arity: 1}: Predicate1(func(_ *VM, t Term, k func(*Env) *Promise, env *Env) *Promise {
					return k(env)
				}),
			},
		}
		ok, err := vm.Arrive(NewAtom("foo"), []Term{NewAtom("a")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("unknown procedure", func(t *testing.T) {
		t.Run("error", func(t *testing.T) {
			vm := VM{
				unknown: unknownError,
			}
			ok, err := vm.Arrive(NewAtom("foo"), []Term{NewAtom("a")}, Success, nil).Force(context.Background())
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
				Unknown: func(name Atom, args []Term, env *Env) {
					assert.Equal(t, NewAtom("foo"), name)
					assert.Equal(t, []Term{NewAtom("a")}, args)
					assert.Nil(t, env)
					warned = true
				},
			}
			ok, err := vm.Arrive(NewAtom("foo"), []Term{NewAtom("a")}, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
			assert.True(t, warned)
		})

		t.Run("fail", func(t *testing.T) {
			vm := VM{
				unknown: unknownFail,
			}
			ok, err := vm.Arrive(NewAtom("foo"), []Term{NewAtom("a")}, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})
}

func TestVM_SetUserInput(t *testing.T) {
	t.Run("file", func(t *testing.T) {
		var vm VM
		vm.SetUserInput(NewInputTextStream(os.Stdin))

		s, ok := vm.streams.lookup(atomUserInput)
		assert.True(t, ok)
		assert.Equal(t, os.Stdin, s.sourceSink)
	})
}

func TestVM_SetUserOutput(t *testing.T) {
	t.Run("file", func(t *testing.T) {
		var vm VM
		vm.SetUserOutput(NewOutputTextStream(os.Stdout))

		s, ok := vm.streams.lookup(atomUserOutput)
		assert.True(t, ok)
		assert.Equal(t, os.Stdout, s.sourceSink)
	})
}

func TestProcedureIndicator_Apply(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		c, err := procedureIndicator{name: NewAtom("foo"), arity: 2}.Apply(NewAtom("a"), NewAtom("b"))
		assert.NoError(t, err)
		assert.Equal(t, &compound{
			functor: NewAtom("foo"),
			args:    []Term{NewAtom("a"), NewAtom("b")},
		}, c)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		c, err := procedureIndicator{name: NewAtom("foo"), arity: 2}.Apply(NewAtom("a"), NewAtom("b"), NewAtom("c"))
		assert.Error(t, err)
		assert.Nil(t, c)
	})
}
