package engine

import (
	"context"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestVM_Module(t *testing.T) {
	tests := []struct {
		title string
		vm    VM
		name  Atom
	}{
		{title: "already set", vm: VM{typeIn: &module{name: NewAtom("foo")}}, name: NewAtom("foo")},
		{title: "by name", vm: VM{modules: map[Atom]*module{atomUser: {name: atomUser}}}},
		{title: "creating", vm: VM{}},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			assert.Equal(t, tt.name, tt.vm.Module().name)
		})
	}
}

func TestVM_SetModule(t *testing.T) {
	tests := []struct {
		title string
		vm    VM
		name  Atom
	}{
		{title: "by name", vm: VM{modules: map[Atom]*module{atomUser: {name: atomUser}}}, name: atomUser},
		{title: "creating", vm: VM{}, name: atomUser},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			tt.vm.SetModule(tt.name)
			assert.Equal(t, tt.name, tt.vm.typeIn.name)
			_, ok := tt.vm.modules[tt.name]
			assert.True(t, ok)
		})
	}
}

func TestVM_Arrive(t *testing.T) {
	t.Run("success", func(t *testing.T) {
		vm := VM{
			typeIn: &module{
				procedures: map[procedureIndicator]procedureEntry{
					{name: NewAtom("foo"), arity: 1}: {procedure: Predicate1(func(_ *VM, t Term, k Cont, env *Env) *Promise {
						return k(env)
					})},
				},
			},
		}
		ok, err := vm.Arrive(NewAtom("foo"), []Term{NewAtom("a")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("unknown procedure", func(t *testing.T) {
		t.Run("error", func(t *testing.T) {
			vm := VM{
				typeIn: &module{
					unknown: unknownError,
				},
			}
			ok, err := vm.Arrive(NewAtom("foo"), []Term{NewAtom("a")}, Success, nil).Force(context.Background())
			assert.Equal(t, existenceError(objectTypeProcedure, &compound{
				functor: atomSlash,
				args:    []Term{NewAtom("foo"), Integer(1)},
			}, nil), err)
			assert.False(t, ok)
		})

		t.Run("warning", func(t *testing.T) {
			var warned bool
			vm := VM{
				typeIn: &module{
					unknown: unknownWarning,
				},
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
				typeIn: &module{
					unknown: unknownFail,
				},
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
		assert.Equal(t, os.Stdin, s.source)
	})
}

func TestVM_SetUserOutput(t *testing.T) {
	t.Run("file", func(t *testing.T) {
		var vm VM
		vm.SetUserOutput(NewOutputTextStream(os.Stdout))

		s, ok := vm.streams.lookup(atomUserOutput)
		assert.True(t, ok)
		assert.Equal(t, os.Stdout, s.sink)
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
