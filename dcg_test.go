package prolog

import (
	"context"
	"github.com/ichiban/prolog/internal"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestVM_Phrase(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		var called bool
		vm := internal.VM{
			typeIn: atomUser,
			modules: map[internal.Atom]*internal.module{
				atomUser: {
					procedures: map[predicateIndicator]internal.procedureEntry{
						{name: internal.NewAtom("a"), arity: 2}: {procedure: Predicate2(func(_ *internal.VM, s0, s Term, k internal.Cont, env *internal.Env) *internal.Promise {
							called = true
							return k(env)
						})},
					},
				},
			},
		}

		s0, s := internal.NewVariable(), internal.NewVariable()
		ok, err := Phrase(&vm, internal.NewAtom("a"), s0, s, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.True(t, called)
	})

	t.Run("failed", func(t *testing.T) {
		s0, s := internal.NewVariable(), internal.NewVariable()
		var vm internal.VM
		_, err := Phrase(&vm, Integer(0), s0, s, Success, nil).Force(context.Background())
		assert.Error(t, err)
	})
}
