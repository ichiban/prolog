package engine

import (
	"errors"
	"github.com/stretchr/testify/assert"
	"io"
	"os"
	"testing"
)

func TestNewStream(t *testing.T) {
	f, err := os.Open("testdata/empty.txt")
	assert.NoError(t, err)
	defer func() {
		assert.NoError(t, f.Close())
	}()

	t.Run("without options", func(t *testing.T) {
		s := NewStream(f, StreamModeAppend)
		assert.Equal(t, f, s.file)
		assert.Equal(t, Atom(""), s.alias)
		assert.Equal(t, EOFActionEOFCode, s.eofAction)
		assert.Equal(t, StreamModeAppend, s.mode)
		assert.True(t, s.reposition)
		assert.Equal(t, StreamTypeText, s.streamType)
	})

	t.Run("with alias", func(t *testing.T) {
		var state State
		s := NewStream(f, StreamModeAppend, WithAlias(&state, "foo"))
		assert.Equal(t, f, s.file)
		assert.Equal(t, Atom("foo"), s.alias)
		assert.Equal(t, EOFActionEOFCode, s.eofAction)
		assert.Equal(t, StreamModeAppend, s.mode)
		assert.True(t, s.reposition)
		assert.Equal(t, StreamTypeText, s.streamType)

		assert.Equal(t, s, state.streams[Atom("foo")])
	})

	t.Run("with EOF action", func(t *testing.T) {
		s := NewStream(f, StreamModeAppend, WithEOFAction(EOFActionError))
		assert.Equal(t, f, s.file)
		assert.Equal(t, Atom(""), s.alias)
		assert.Equal(t, EOFActionError, s.eofAction)
		assert.Equal(t, StreamModeAppend, s.mode)
		assert.True(t, s.reposition)
		assert.Equal(t, StreamTypeText, s.streamType)
	})

	t.Run("with reposition", func(t *testing.T) {
		s := NewStream(f, StreamModeAppend, WithReposition(false))
		assert.Equal(t, f, s.file)
		assert.Equal(t, Atom(""), s.alias)
		assert.Equal(t, EOFActionEOFCode, s.eofAction)
		assert.Equal(t, StreamModeAppend, s.mode)
		assert.False(t, s.reposition)
		assert.Equal(t, StreamTypeText, s.streamType)
	})

	t.Run("with stream type", func(t *testing.T) {
		s := NewStream(f, StreamModeAppend, WithStreamType(StreamTypeBinary))
		assert.Equal(t, f, s.file)
		assert.Equal(t, Atom(""), s.alias)
		assert.Equal(t, EOFActionEOFCode, s.eofAction)
		assert.Equal(t, StreamModeAppend, s.mode)
		assert.True(t, s.reposition)
		assert.Equal(t, StreamTypeBinary, s.streamType)
	})
}

func TestOpen(t *testing.T) {
	f, err := os.Open("testdata/empty.txt")
	assert.NoError(t, err)
	defer func() {
		assert.NoError(t, f.Close())
	}()

	t.Run("ok", func(t *testing.T) {
		openFile = func(name string, flag int, perm os.FileMode) (*os.File, error) {
			return f, nil
		}
		defer func() {
			openFile = os.OpenFile
		}()

		s, err := Open("/this/file/exists", StreamModeRead)
		assert.NoError(t, err)
		assert.Equal(t, f, s.file)
	})

	t.Run("not exist", func(t *testing.T) {
		openFile = func(name string, flag int, perm os.FileMode) (*os.File, error) {
			return nil, os.ErrNotExist
		}
		defer func() {
			openFile = os.OpenFile
		}()

		s, err := Open("/this/file/does/not/exist", StreamModeRead)
		assert.Equal(t, ExistenceError(ObjectTypeSourceSink, Atom("/this/file/does/not/exist"), nil), err)
		assert.Nil(t, s)
	})

	t.Run("permission", func(t *testing.T) {
		openFile = func(name string, flag int, perm os.FileMode) (*os.File, error) {
			return nil, os.ErrPermission
		}
		defer func() {
			openFile = os.OpenFile
		}()

		s, err := Open("/this/file/is/protected", StreamModeRead)
		assert.Equal(t, PermissionError(OperationOpen, PermissionTypeSourceSink, Atom("/this/file/is/protected"), nil), err)
		assert.Nil(t, s)
	})

	t.Run("other error", func(t *testing.T) {
		errFailed := errors.New("failed")
		openFile = func(name string, flag int, perm os.FileMode) (*os.File, error) {
			return nil, errFailed
		}
		defer func() {
			openFile = os.OpenFile
		}()

		s, err := Open("/this/file/is/protected", StreamModeRead)
		assert.Equal(t, SystemError(errFailed), err)
		assert.Nil(t, s)
	})
}

func TestStream_Close(t *testing.T) {
	var f os.File
	s := Stream{
		file: &f,
	}
	var called bool
	closeFile = func(file io.Closer) error {
		assert.Equal(t, &f, file)
		called = true
		return nil
	}
	defer func() {
		closeFile = io.Closer.Close
	}()
	assert.NoError(t, s.Close())
	assert.True(t, called)
}
