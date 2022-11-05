package engine

import (
	"bytes"
	"errors"
	"io"
	"io/fs"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
)

type mockNamer struct {
	mock.Mock
}

func (m *mockNamer) Name() string {
	args := m.Called()
	return args.String(0)
}

func TestStream_Name(t *testing.T) {
	t.Run("namer", func(t *testing.T) {
		var m mockNamer
		m.On("Name").Return("name").Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m}
		assert.Equal(t, "name", s.Name())
	})

	t.Run("not namer", func(t *testing.T) {
		s := &Stream{sourceSink: "not namer"}
		assert.Equal(t, "", s.Name())
	})
}

type mockFileInfo struct {
	mock.Mock
}

func (m *mockFileInfo) Name() string {
	args := m.Called()
	return args.String(0)
}

func (m *mockFileInfo) Size() int64 {
	args := m.Called()
	return args.Get(0).(int64)
}

func (m *mockFileInfo) Mode() fs.FileMode {
	args := m.Called()
	return args.Get(0).(fs.FileMode)
}

func (m *mockFileInfo) ModTime() time.Time {
	args := m.Called()
	return args.Get(0).(time.Time)
}

func (m *mockFileInfo) IsDir() bool {
	args := m.Called()
	return args.Bool(0)
}

func (m *mockFileInfo) Sys() interface{} {
	args := m.Called()
	return args.Get(0)
}

type mockFile struct {
	mock.Mock
}

func (m *mockFile) Stat() (fs.FileInfo, error) {
	args := m.Called()
	return args.Get(0).(fs.FileInfo), args.Error(1)
}

func (m *mockFile) Read(p []byte) (int, error) {
	args := m.Called(p)
	return args.Int(0), args.Error(1)
}

func (m *mockFile) Close() error {
	args := m.Called()
	return args.Error(0)
}

func (m *mockFile) Seek(offset int64, whence int) (int64, error) {
	args := m.Called(offset, whence)
	return args.Get(0).(int64), args.Error(1)
}

func TestStream_Stat(t *testing.T) {
	t.Run("file", func(t *testing.T) {
		var mfi mockFileInfo
		defer mfi.AssertExpectations(t)

		var m mockFile
		m.On("Stat").Return(&mfi, nil).Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m}
		fi, err := s.Stat()
		assert.NoError(t, err)
		assert.Equal(t, &mfi, fi)
	})

	t.Run("not file", func(t *testing.T) {
		s := &Stream{sourceSink: "not file"}
		_, err := s.Stat()
		assert.Error(t, err)
	})
}

type mockCloser struct {
	mock.Mock
}

func (m *mockCloser) Close() error {
	args := m.Called()
	return args.Error(0)
}

func TestStream_Close(t *testing.T) {
	t.Run("closer", func(t *testing.T) {
		var m mockCloser
		m.On("Close").Return(nil).Once()
		defer m.AssertExpectations(t)

		s := Stream{
			sourceSink: &m,
		}
		assert.NoError(t, s.Close())
	})

	t.Run("not closer", func(t *testing.T) {
		s := Stream{
			sourceSink: "not closer",
		}
		assert.NoError(t, s.Close())
	})
}

type mockReader struct {
	mock.Mock
}

func (m *mockReader) Read(p []byte) (int, error) {
	args := m.Called(p)
	return args.Int(0), args.Error(1)
}

func TestStream_Read(t *testing.T) {
	t.Run("reader", func(t *testing.T) {
		var m mockReader
		m.On("Read", mock.Anything).Return(1, nil).Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m}
		n, err := s.Read(make([]byte, 1))
		assert.Equal(t, 1, n)
		assert.NoError(t, err)
	})

	t.Run("not reader", func(t *testing.T) {
		s := &Stream{sourceSink: "not reader"}
		_, err := s.Read(nil)
		assert.Error(t, err)
	})
}

func TestStream_ReadByte(t *testing.T) {
	t.Run("reader", func(t *testing.T) {
		s := &Stream{sourceSink: bytes.NewReader([]byte{1, 2, 3})}
		b, err := s.ReadByte()
		assert.Equal(t, byte(1), b)
		assert.NoError(t, err)
	})

	t.Run("not reader", func(t *testing.T) {
		s := &Stream{sourceSink: "not reader"}
		_, err := s.ReadByte()
		assert.Error(t, err)
	})
}

func TestStream_Peek(t *testing.T) {
	t.Run("reader", func(t *testing.T) {
		s := &Stream{sourceSink: bytes.NewReader([]byte{1, 2, 3})}
		b, err := s.Peek(2)
		assert.Equal(t, []byte{1, 2}, b)
		assert.NoError(t, err)
	})

	t.Run("not reader", func(t *testing.T) {
		s := &Stream{sourceSink: "not reader"}
		_, err := s.Peek(2)
		assert.Error(t, err)
	})
}

func TestStream_ReadRune(t *testing.T) {
	t.Run("reader", func(t *testing.T) {
		s := &Stream{sourceSink: bytes.NewReader([]byte("abc"))}
		r, n, err := s.ReadRune()
		assert.NoError(t, err)
		assert.Equal(t, 'a', r)
		assert.Equal(t, 1, n)
	})

	t.Run("not reader", func(t *testing.T) {
		s := &Stream{sourceSink: "not reader"}
		_, _, err := s.ReadRune()
		assert.Error(t, err)
	})
}

func TestStream_UnreadRune(t *testing.T) {
	t.Run("reader", func(t *testing.T) {
		s := &Stream{sourceSink: bytes.NewReader([]byte("abc"))}
		_, _, err := s.ReadRune()
		assert.NoError(t, err)

		assert.NoError(t, s.UnreadRune())
	})

	t.Run("not reader", func(t *testing.T) {
		s := &Stream{sourceSink: "not reader"}
		assert.Error(t, s.UnreadRune())
	})
}

type mockSeeker struct {
	mock.Mock
}

func (m *mockSeeker) Seek(offset int64, whence int) (int64, error) {
	args := m.Called(offset, whence)
	return args.Get(0).(int64), args.Error(1)
}

func TestStream_Seek(t *testing.T) {
	t.Run("seeker", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			var m mockSeeker
			m.On("Seek", int64(0), 0).Return(int64(0), nil).Once()
			defer m.AssertExpectations(t)

			s := &Stream{sourceSink: &m}
			_, err := s.Seek(0, 0)
			assert.NoError(t, err)
		})

		t.Run("ng", func(t *testing.T) {
			var m mockSeeker
			m.On("Seek", int64(0), 0).Return(int64(0), errors.New("failed")).Once()
			defer m.AssertExpectations(t)

			s := &Stream{sourceSink: &m}
			_, err := s.Seek(0, 0)
			assert.Error(t, err)
		})

		t.Run("reader", func(t *testing.T) {
			var m struct {
				mockSeeker
				io.Reader
			}
			m.On("Seek", int64(0), 0).Return(int64(0), nil).Once()
			defer m.AssertExpectations(t)
			m.Reader = bytes.NewReader([]byte("abc"))

			s := &Stream{sourceSink: &m}
			_, err := s.ReadByte()
			assert.NoError(t, err)
			_, err = s.Seek(0, 0)
			assert.NoError(t, err)
		})
	})

	t.Run("not seeker", func(t *testing.T) {
		s := &Stream{sourceSink: "not seeker"}
		_, err := s.Seek(0, 0)
		assert.Error(t, err)
	})
}

func TestStream_Write(t *testing.T) {
	t.Run("writer", func(t *testing.T) {
		var m mockWriter
		m.On("Write", []byte("abc")).Return(3, nil).Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m}
		n, err := s.Write([]byte("abc"))
		assert.NoError(t, err)
		assert.Equal(t, 3, n)
	})

	t.Run("not writer", func(t *testing.T) {
		s := &Stream{sourceSink: "not writer"}
		_, err := s.Write([]byte("abc"))
		assert.Error(t, err)
	})
}

type mockFlusher struct {
	mock.Mock
}

func (m *mockFlusher) Flush() error {
	args := m.Called()
	return args.Error(0)
}

type mockSyncer struct {
	mock.Mock
}

func (m *mockSyncer) Sync() error {
	args := m.Called()
	return args.Error(0)
}

func TestStream_Flush(t *testing.T) {
	t.Run("flusher", func(t *testing.T) {
		var m mockFlusher
		m.On("Flush").Return(nil).Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m}
		assert.NoError(t, s.Flush())
	})

	t.Run("syncer", func(t *testing.T) {
		var m mockSyncer
		m.On("Sync").Return(nil).Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m}
		assert.NoError(t, s.Flush())
	})

	t.Run("else", func(t *testing.T) {
		s := &Stream{sourceSink: "else"}
		assert.NoError(t, s.Flush())
	})
}
