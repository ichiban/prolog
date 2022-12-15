package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestNew(t *testing.T) {
	t.Run("go_string", func(t *testing.T) {
		p := New(nil, nil)
		assert.NoError(t, p.QuerySolution(`go_string("foo", '"foo"').`).Err())
	})
}
