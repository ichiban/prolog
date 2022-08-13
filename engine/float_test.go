package engine

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFloat_Compare(t *testing.T) {
	var m mockTerm
	defer m.AssertExpectations(t)

	assert.Equal(t, int64(-1), Float(0).Compare(&m, nil))
	assert.Equal(t, int64(-1), Float(0).Compare(Integer(0), nil))
	assert.Equal(t, int64(-1), Float(0).Compare(Float(1), nil))
	assert.Equal(t, int64(0), Float(0).Compare(Float(0), nil))
	assert.Equal(t, int64(1), Float(1).Compare(Float(0), nil))
	assert.Equal(t, int64(1), Float(1).Compare(Variable("X"), nil))
}
