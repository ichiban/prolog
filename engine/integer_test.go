package engine

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestInteger_Compare(t *testing.T) {
	var m mockTerm
	defer m.AssertExpectations(t)

	assert.Equal(t, int64(-1), Integer(0).Compare(&m, nil))
	assert.Equal(t, int64(-1), Integer(0).Compare(Integer(1), nil))
	assert.Equal(t, int64(0), Integer(0).Compare(Integer(0), nil))
	assert.Equal(t, int64(1), Integer(1).Compare(Integer(0), nil))
	assert.Equal(t, int64(1), Integer(0).Compare(Float(0), nil))
	assert.Equal(t, int64(1), Integer(0).Compare(Variable("X"), nil))
}
