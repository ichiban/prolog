package term

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFloat_String(t *testing.T) {
	assert.Equal(t, "33.0", Float(33.0).String())
}
