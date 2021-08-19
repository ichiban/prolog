package nondet

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestPromise_Force(t *testing.T) {
	var res []int
	k := Delay(func() *Promise {
		res = append(res, 1)
		return Bool(false)
	}, func() *Promise {
		res = append(res, 2)
		return Delay(func() *Promise {
			res = append(res, 3)
			return Bool(false)
		}, func() *Promise {
			res = append(res, 4)
			return Delay(func() *Promise {
				res = append(res, 5)
				return Bool(false)
			}, func() *Promise {
				res = append(res, 6)
				return Bool(false)
			}, func() *Promise {
				res = append(res, 7)
				return Bool(false)
			})
		}, func() *Promise {
			res = append(res, 8)
			return Bool(false)
		})
	}, func() *Promise {
		res = append(res, 9)
		return Bool(true)
	})

	ok, err := k.Force()
	assert.NoError(t, err)
	assert.True(t, ok)

	assert.Equal(t, []int{1, 2, 3, 4, 5, 6, 7, 8, 9}, res)
}
