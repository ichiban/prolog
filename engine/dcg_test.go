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
			procedures: map[procedureIndicator]procedure{
				{name: NewAtom("a"), arity: 2}: Predicate2(func(ctx context.Context, s0, s Term) *Promise {
					called = true
					return Continue(ctx)
				}),
			},
		}
		ctx := withVM(context.Background(), &vm)

		s0, s := NewVariable(), NewVariable()
		ok, err := Phrase(ctx, NewAtom("a"), s0, s).Force()
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.True(t, called)
	})

	t.Run("failed", func(t *testing.T) {
		s0, s := NewVariable(), NewVariable()
		var vm VM
		ctx := withVM(context.Background(), &vm)
		_, err := Phrase(ctx, Integer(0), s0, s).Force()
		assert.Error(t, err)
	})
}
