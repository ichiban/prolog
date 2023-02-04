package engine

import (
	"errors"
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestErrWriter_Write(t *testing.T) {
	var failed = errors.New("failed")

	var m mockWriter
	m.On("Write", []byte("foo")).Return(0, failed).Once()
	defer m.AssertExpectations(t)

	ew := errWriter{w: &m}
	_, err := ew.Write([]byte("foo"))
	assert.NoError(t, err)
	_, err = ew.Write([]byte("bar"))
	assert.NoError(t, err)
	_, err = ew.Write([]byte("baz"))
	assert.NoError(t, err)
	assert.Equal(t, failed, ew.err)
}
