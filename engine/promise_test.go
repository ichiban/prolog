package engine

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestPromise_Force(t *testing.T) {
	var res []int
	k := Delay(func(context.Context) *Promise {
		res = append(res, 1)
		return Bool(false)
	}, func(context.Context) *Promise {
		res = append(res, 2)
		return Delay(func(context.Context) *Promise {
			res = append(res, 3)
			return Bool(false)
		}, func(context.Context) *Promise {
			res = append(res, 4)
			return Delay(func(context.Context) *Promise {
				res = append(res, 5)
				return Bool(false)
			}, func(context.Context) *Promise {
				res = append(res, 6)
				return Cut(nil, func(context.Context) *Promise {
					return Bool(true)
				})
			}, func(context.Context) *Promise {
				res = append(res, 7)
				return Bool(false)
			})
		}, func(context.Context) *Promise {
			res = append(res, 8)
			return Bool(false)
		})
	}, func(context.Context) *Promise {
		res = append(res, 9)
		return Bool(true)
	})

	t.Run("ok", func(t *testing.T) {
		res = nil
		ok, err := k.Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		assert.Equal(t, []int{1, 2, 3, 4, 5, 6}, res)
	})

	t.Run("canceled", func(t *testing.T) {
		res = nil
		ctx, cancel := context.WithCancel(context.Background())
		cancel()
		ok, err := k.Force(ctx)
		assert.Error(t, err)
		assert.False(t, ok)

		assert.Empty(t, res)
	})

	t.Run("repeat", func(t *testing.T) {
		count := 0
		k := repeat(func(context.Context) *Promise {
			count++
			return Bool(count >= 10)
		})

		ok, err := k.Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
		assert.Equal(t, 10, count)
	})
}
