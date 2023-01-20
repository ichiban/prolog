package engine

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestFloatNumber(t *testing.T) {
	assert.Implements(t, (*Number)(nil), Float(0))
}
