package engine

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestVM_Phrase(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		var called bool
		vm := VM{
			procedures: map[ProcedureIndicator]procedure{
				{Name: NewAtom("a"), Arity: 2}: predicate2(func(s0, s Term, k func(*Env) *Promise, env *Env) *Promise {
					called = true
					return k(env)
				}),
			},
		}

		s0, s := NewVariable(), NewVariable()
		ok, err := vm.Phrase(NewAtom("a"), s0, s, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.True(t, called)
	})

	t.Run("failed", func(t *testing.T) {
		s0, s := NewVariable(), NewVariable()
		var vm VM
		_, err := vm.Phrase(Integer(0), s0, s, Success, nil).Force(context.Background())
		assert.Error(t, err)
	})
}
