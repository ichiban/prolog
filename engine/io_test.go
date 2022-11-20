package engine

import (
	"github.com/stretchr/testify/assert"
	"os"
	"testing"
)

func TestIO_SetUserInput(t *testing.T) {
	t.Run("file", func(t *testing.T) {
		var i IO
		i.SetUserInput(os.Stdin)

		s, ok := i.streams.lookup(atomUserInput)
		assert.True(t, ok)
		assert.Equal(t, os.Stdin, s.sourceSink)
	})
}

func TestIO_SetUserOutput(t *testing.T) {
	t.Run("file", func(t *testing.T) {
		var i IO
		i.SetUserOutput(os.Stdout)

		s, ok := i.streams.lookup(atomUserOutput)
		assert.True(t, ok)
		assert.Equal(t, os.Stdout, s.sourceSink)
	})
}
