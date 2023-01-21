package engine

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestIntegerNumber(t *testing.T) {
	assert.Implements(t, (*Number)(nil), Integer(0))
}
