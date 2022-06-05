package engine

import (
	"errors"
	"fmt"
	"io"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
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

func TestStream_Unify(t *testing.T) {
	t.Run("stream", func(t *testing.T) {
		t.Run("same", func(t *testing.T) {
			var s Stream
			env := NewEnv().Bind("Foo", Atom("foo"))
			e, ok := s.Unify(&s, false, env)
			assert.True(t, ok)
			assert.Equal(t, env, e)
		})

		t.Run("different", func(t *testing.T) {
			var s1, s2 Stream
			env := NewEnv().Bind("Foo", Atom("foo"))
			e, ok := s1.Unify(&s2, false, env)
			assert.False(t, ok)
			assert.Equal(t, env, e)
		})
	})

	t.Run("variable", func(t *testing.T) {
		t.Run("free", func(t *testing.T) {
			var s Stream
			env := NewEnv().Bind("Foo", Atom("foo"))
			env, ok := s.Unify(Variable("Bar"), false, env)
			assert.True(t, ok)
			assert.Equal(t, &s, env.Resolve(Variable("Bar")))
		})

		t.Run("bound", func(t *testing.T) {
			t.Run("same", func(t *testing.T) {
				var s Stream
				env := NewEnv().Bind("Foo", &s)
				env, ok := s.Unify(Variable("Foo"), false, env)
				assert.True(t, ok)
				assert.Equal(t, &s, env.Resolve(Variable("Foo")))
			})

			t.Run("different", func(t *testing.T) {
				var s Stream
				env := NewEnv().Bind("Foo", Atom("foo"))
				e, ok := s.Unify(Variable("Foo"), false, env)
				assert.False(t, ok)
				assert.Equal(t, e, env)
			})
		})
	})
}

func TestStream_Unparse(t *testing.T) {
	t.Run("aliased", func(t *testing.T) {
		s := Stream{alias: "foo"}

		var ret []Token
		s.Unparse(func(token Token) {
			ret = append(ret, token)
		}, nil)
		assert.Equal(t, []Token{
			{Kind: TokenLetterDigit, Val: "foo"},
		}, ret)
	})

	t.Run("not aliased", func(t *testing.T) {
		var s Stream

		var ret []Token
		s.Unparse(func(token Token) {
			ret = append(ret, token)
		}, nil)
		assert.Equal(t, []Token{
			{Kind: TokenGraphic, Val: fmt.Sprintf("<stream>(%p)", &s)},
		}, ret)
	})
}

func TestStream_Compare(t *testing.T) {
	var m mockTerm
	defer m.AssertExpectations(t)

	var s1, s2 Stream
	assert.Equal(t, int64(0), s1.Compare(&s1, nil))
	assert.Equal(t, int64(1), s1.Compare(&s2, nil))
	assert.Equal(t, int64(1), s2.Compare(&s1, nil))
	assert.Equal(t, int64(1), s1.Compare(&m, nil))
}
