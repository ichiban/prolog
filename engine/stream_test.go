package engine

import (
	"bytes"
	"errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
	"io"
	"io/fs"
	"testing"
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

type mockCloser struct {
	mock.Mock
}

func (m *mockCloser) Close() error {
	args := m.Called()
	return args.Error(0)
}

func TestStream_Close(t *testing.T) {
	var okCloser mockCloser
	okCloser.On("Close").Return(nil)

	var ngCloser mockCloser
	ngCloser.On("Close").Return(errors.New("ng"))

	var state State

	foo := NewAtom("foo")
	s := &Stream{state: &state, sourceSink: &okCloser, alias: foo}
	state.streams.add(s)

	bar := NewAtom("bar")
	state.streams.add(&Stream{state: &state, sourceSink: &okCloser, alias: bar})

	tests := []struct {
		title string
		s     *Stream
		err   error
	}{
		{title: "ok closer", s: &Stream{sourceSink: &okCloser}},
		{title: "not closer", s: &Stream{sourceSink: "not closer"}},
		{title: "alias", s: s},

		{title: "ng closer", s: &Stream{sourceSink: &ngCloser}, err: errors.New("ng")},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			assert.Equal(t, tt.err, tt.s.Close())
		})
	}
}

type mockReader struct {
	mock.Mock
}

func (m *mockReader) Read(p []byte) (int, error) {
	args := m.Called(p)
	return args.Int(0), args.Error(1)
}

func TestStream_ReadByte(t *testing.T) {
	tests := []struct {
		title string
		s     *Stream
		b     byte
		err   error
		pos   int64
		eos   endOfStream
	}{
		{
			title: "input binary: 3 bytes left",
			s:     &Stream{sourceSink: bytes.NewReader([]byte{1, 2, 3}), streamType: streamTypeBinary},
			b:     1,
			pos:   1,
			eos:   endOfStreamNot,
		},
		{
			title: "input binary: 2 bytes left",
			s:     &Stream{sourceSink: bytes.NewReader([]byte{2, 3}), streamType: streamTypeBinary, position: 1},
			b:     2,
			pos:   2,
			eos:   endOfStreamNot,
		},
		{
			title: "input binary: 1 byte left",
			s:     &Stream{sourceSink: bytes.NewReader([]byte{3}), streamType: streamTypeBinary, position: 2},
			b:     3,
			pos:   3,
			eos:   endOfStreamAt,
		},
		{
			title: "input binary: empty",
			s:     &Stream{sourceSink: bytes.NewReader([]byte{}), streamType: streamTypeBinary, position: 3},
			err:   io.EOF,
			pos:   3,
			eos:   endOfStreamPast,
		},
		{
			title: "end of stream past: error",
			s:     &Stream{sourceSink: bytes.NewReader([]byte{1, 2, 3}), streamType: streamTypeBinary, endOfStream: endOfStreamPast, eofAction: eofActionError},
			err:   errPastEndOfStream,
			pos:   0,
			eos:   endOfStreamPast,
		},
		{
			title: "end of stream past: reset",
			s:     &Stream{sourceSink: bytes.NewReader([]byte{1, 2, 3}), streamType: streamTypeBinary, endOfStream: endOfStreamPast, eofAction: eofActionReset, reposition: true},
			b:     1,
			pos:   1,
			eos:   endOfStreamNot,
		},
		{
			title: "input text",
			s:     &Stream{sourceSink: bytes.NewReader([]byte{1, 2, 3}), streamType: streamTypeText},
			err:   errWrongStreamType,
		},
		{
			title: "output",
			s:     &Stream{sourceSink: bytes.NewReader([]byte{1, 2, 3}), mode: ioModeAppend},
			err:   errWrongIOMode,
		},
		{
			title: "not reader",
			s:     &Stream{sourceSink: "not reader"},
			err:   errNotSupported,
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			b, err := tt.s.ReadByte()
			assert.Equal(t, tt.b, b)
			assert.Equal(t, tt.err, err)

			assert.Equal(t, tt.pos, tt.s.position)
			assert.Equal(t, tt.eos, tt.s.endOfStream)
		})
	}
}

func TestStream_ReadRune(t *testing.T) {
	tests := []struct {
		title string
		s     *Stream
		r     rune
		size  int
		err   error
		pos   int64
		eos   endOfStream
	}{
		{
			title: "input text: 3 runes left",
			s:     &Stream{sourceSink: bytes.NewReader([]byte("abc")), streamType: streamTypeText},
			r:     'a',
			size:  1,
			pos:   1,
			eos:   endOfStreamNot,
		},
		{
			title: "input text: 2 runes left",
			s:     &Stream{sourceSink: bytes.NewReader([]byte("bc")), streamType: streamTypeText, position: 1},
			r:     'b',
			size:  1,
			pos:   2,
			eos:   endOfStreamNot,
		},
		{
			title: "input text: 1 rune left",
			s:     &Stream{sourceSink: bytes.NewReader([]byte("c")), streamType: streamTypeText, position: 2},
			r:     'c',
			size:  1,
			pos:   3,
			eos:   endOfStreamAt,
		},
		{
			title: "input Text: empty",
			s:     &Stream{sourceSink: bytes.NewReader([]byte("")), streamType: streamTypeText, position: 3},
			err:   io.EOF,
			pos:   3,
			eos:   endOfStreamPast,
		},
		{
			title: "end of stream past: error",
			s:     &Stream{sourceSink: bytes.NewReader([]byte("abc")), streamType: streamTypeText, endOfStream: endOfStreamPast, eofAction: eofActionError},
			err:   errPastEndOfStream,
			pos:   0,
			eos:   endOfStreamPast,
		},
		{
			title: "end of stream past: reset",
			s:     &Stream{sourceSink: bytes.NewReader([]byte("abc")), streamType: streamTypeText, endOfStream: endOfStreamPast, eofAction: eofActionReset, reposition: true},
			r:     'a',
			size:  1,
			pos:   1,
			eos:   endOfStreamNot,
		},
		{
			title: "input binary",
			s:     &Stream{sourceSink: bytes.NewReader([]byte("abc")), streamType: streamTypeBinary},
			err:   errWrongStreamType,
		},
		{
			title: "output",
			s:     &Stream{sourceSink: bytes.NewReader([]byte("abc")), mode: ioModeAppend},
			err:   errWrongIOMode,
		},
		{
			title: "not reader",
			s:     &Stream{sourceSink: "not reader"},
			err:   errNotSupported,
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			r, size, err := tt.s.ReadRune()
			assert.Equal(t, tt.r, r)
			assert.Equal(t, tt.size, size)
			assert.Equal(t, tt.err, err)

			assert.Equal(t, tt.pos, tt.s.position)
			assert.Equal(t, tt.eos, tt.s.endOfStream)
		})
	}
}

type mockSeeker struct {
	mock.Mock
}

func (m *mockSeeker) Seek(offset int64, whence int) (int64, error) {
	args := m.Called(offset, whence)
	return args.Get(0).(int64), args.Error(1)
}

func TestStream_Seek(t *testing.T) {
	var okSeeker mockSeeker
	okSeeker.On("Seek", int64(0), 0).Return(int64(0), nil)

	var ngSeeker mockSeeker
	ngSeeker.On("Seek", mock.Anything, mock.Anything).Return(int64(0), errors.New("ng"))

	s := &Stream{sourceSink: bytes.NewReader([]byte("abc")), streamType: streamTypeBinary, reposition: true}
	_, err := s.ReadByte()
	assert.NoError(t, err)

	tests := []struct {
		title  string
		s      *Stream
		offset int64
		whence int
		pos    int64
		err    error
		eos    endOfStream
	}{
		{
			title:  "ok",
			s:      &Stream{sourceSink: &okSeeker, reposition: true},
			offset: 0,
			whence: 0,
		},
		{
			title:  "ng",
			s:      &Stream{sourceSink: &ngSeeker, reposition: true},
			offset: 0,
			whence: 0,
			err:    errors.New("ng"),
		},
		{title: "reader", s: s, offset: 0, whence: 0, pos: 0, eos: endOfStreamNot},
		{title: "reader", s: s, offset: 1, whence: 0, pos: 1, eos: endOfStreamNot},
		{title: "reader", s: s, offset: 2, whence: 0, pos: 2, eos: endOfStreamAt},
		{title: "reader", s: s, offset: 3, whence: 0, pos: 3, eos: endOfStreamPast},
		{
			title:  "not seeker",
			s:      &Stream{sourceSink: "not seeker", reposition: true, position: 123},
			offset: 0,
			whence: 0,
			pos:    123,
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			pos, err := tt.s.Seek(tt.offset, tt.whence)
			assert.Equal(t, tt.pos, pos)
			assert.Equal(t, tt.err, err)

			assert.Equal(t, tt.eos, tt.s.endOfStream)
		})
	}
}

func TestStream_Write(t *testing.T) {
	var m mockWriter
	m.On("Write", []byte("abc")).Return(3, nil).Once()
	defer m.AssertExpectations(t)

	tests := []struct {
		title string
		s     *Stream
		p     []byte
		n     int
		err   error
		pos   int64
	}{
		{
			title: "writer",
			s:     &Stream{sourceSink: &m, mode: ioModeAppend, streamType: streamTypeBinary},
			p:     []byte("abc"),
			n:     3,
			pos:   3,
		},
		{
			title: "not writer",
			s:     &Stream{sourceSink: "not writer", mode: ioModeAppend, streamType: streamTypeBinary},
			p:     []byte("abc"),
			err:   errNotSupported,
			pos:   0,
		},
		{
			title: "input",
			s:     &Stream{mode: ioModeRead, streamType: streamTypeBinary},
			p:     []byte("abc"),
			err:   errWrongIOMode,
			pos:   0,
		},
		{
			title: "text",
			s:     &Stream{mode: ioModeAppend, streamType: streamTypeText},
			p:     []byte("abc"),
			err:   errWrongStreamType,
			pos:   0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			n, err := tt.s.Write(tt.p)
			assert.Equal(t, tt.n, n)
			assert.Equal(t, tt.err, err)

			assert.Equal(t, tt.pos, tt.s.position)
		})
	}
}

func TestStream_WriteString(t *testing.T) {
	var m mockWriter
	m.On("Write", []byte("abc")).Return(3, nil).Once()
	defer m.AssertExpectations(t)

	tests := []struct {
		title string
		s     *Stream
		str   string
		n     int
		err   error
		pos   int64
	}{
		{
			title: "writer",
			s:     &Stream{sourceSink: &m, mode: ioModeAppend, streamType: streamTypeText},
			str:   "abc",
			n:     3,
			pos:   3,
		},
		{
			title: "not writer",
			s:     &Stream{sourceSink: "not writer", mode: ioModeAppend, streamType: streamTypeText},
			str:   "abc",
			err:   errNotSupported,
			pos:   0,
		},
		{
			title: "input",
			s:     &Stream{mode: ioModeRead, streamType: streamTypeText},
			str:   "abc",
			err:   errWrongIOMode,
			pos:   0,
		},
		{
			title: "binary",
			s:     &Stream{mode: ioModeAppend, streamType: streamTypeBinary},
			str:   "abc",
			err:   errWrongStreamType,
			pos:   0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			n, err := tt.s.WriteString(tt.str)
			assert.Equal(t, tt.n, n)
			assert.Equal(t, tt.err, err)

			assert.Equal(t, tt.pos, tt.s.position)
		})
	}
}

func TestStream_WriteByte(t *testing.T) {
	var m mockWriter
	m.On("Write", []byte("a")).Return(1, nil).Once()
	defer m.AssertExpectations(t)

	tests := []struct {
		title string
		s     *Stream
		c     byte
		err   error
		pos   int64
	}{
		{
			title: "writer",
			s:     &Stream{sourceSink: &m, mode: ioModeAppend, streamType: streamTypeBinary},
			c:     byte('a'),
			pos:   1,
		},
		{
			title: "not writer",
			s:     &Stream{sourceSink: "not writer", mode: ioModeAppend, streamType: streamTypeBinary},
			c:     byte('a'),
			err:   errNotSupported,
			pos:   0,
		},
		{
			title: "input",
			s:     &Stream{mode: ioModeRead, streamType: streamTypeBinary},
			c:     byte('a'),
			err:   errWrongIOMode,
			pos:   0,
		},
		{
			title: "text",
			s:     &Stream{mode: ioModeAppend, streamType: streamTypeText},
			c:     byte('a'),
			err:   errWrongStreamType,
			pos:   0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			err := tt.s.WriteByte(tt.c)
			assert.Equal(t, tt.err, err)

			assert.Equal(t, tt.pos, tt.s.position)
		})
	}
}

func TestStream_WriteRune(t *testing.T) {
	var m mockWriter
	m.On("Write", []byte("a")).Return(1, nil).Once()
	defer m.AssertExpectations(t)

	tests := []struct {
		title string
		s     *Stream
		r     rune
		n     int
		err   error
		pos   int64
	}{
		{
			title: "writer",
			s:     &Stream{sourceSink: &m, mode: ioModeAppend, streamType: streamTypeText},
			r:     'a',
			n:     1,
			pos:   1,
		},
		{
			title: "not writer",
			s:     &Stream{sourceSink: "not writer", mode: ioModeAppend, streamType: streamTypeText},
			r:     'a',
			err:   errNotSupported,
			pos:   0,
		},
		{
			title: "input",
			s:     &Stream{mode: ioModeRead, streamType: streamTypeText},
			r:     'a',
			err:   errWrongIOMode,
			pos:   0,
		},
		{
			title: "binary",
			s:     &Stream{mode: ioModeAppend, streamType: streamTypeBinary},
			r:     'a',
			err:   errWrongStreamType,
			pos:   0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			n, err := tt.s.WriteRune(tt.r)
			assert.Equal(t, tt.n, n)
			assert.Equal(t, tt.err, err)

			assert.Equal(t, tt.pos, tt.s.position)
		})
	}
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

		s := &Stream{sourceSink: &m, mode: ioModeAppend}
		assert.NoError(t, s.Flush())
	})

	t.Run("syncer", func(t *testing.T) {
		var m mockSyncer
		m.On("Sync").Return(nil).Once()
		defer m.AssertExpectations(t)

		s := &Stream{sourceSink: &m, mode: ioModeAppend}
		assert.NoError(t, s.Flush())
	})

	t.Run("else", func(t *testing.T) {
		s := &Stream{sourceSink: "else", mode: ioModeAppend}
		assert.NoError(t, s.Flush())
	})
}
