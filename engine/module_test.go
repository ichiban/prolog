package engine

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestRealFS_Open(t *testing.T) {
	var fs RealFS
	f, err := fs.Open("module.go")
	assert.NoError(t, err)
	assert.NotNil(t, f)
}
