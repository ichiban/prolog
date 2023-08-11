package engine

import (
	"bytes"
	"errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
	"io"
	"io/fs"
	"os"
	"testing"
)

func TestNewInputTextStream(t *testing.T) {
	assert.Equal(t, &Stream{
		source:     os.Stdin,
		mode:       ioModeRead,
		eofAction:  eofActionReset,
		streamType: streamTypeText,
	}, NewInputTextStream(os.Stdin))
}

func TestNewInputBinaryStream(t *testing.T) {
	assert.Equal(t, &Stream{
		source:     os.Stdin,
		mode:       ioModeRead,
		eofAction:  eofActionReset,
		streamType: streamTypeBinary,
	}, NewInputBinaryStream(os.Stdin))
}

func TestNewOutputTextStream(t *testing.T) {
	assert.Equal(t, &Stream{
		sink:       os.Stdout,
		mode:       ioModeAppend,
		eofAction:  eofActionReset,
		streamType: streamTypeText,
	}, NewOutputTextStream(os.Stdout))
}

func TestNewOutputBinaryStream(t *testing.T) {
	assert.Equal(t, &Stream{
		sink:       os.Stdout,
		mode:       ioModeAppend,
		eofAction:  eofActionReset,
		streamType: streamTypeBinary,
	}, NewOutputBinaryStream(os.Stdout))
}

func TestStream_WriteTerm(t *testing.T) {
	tests := []struct {
		title  string
		s      *Stream
		opts   WriteOptions
		output string
	}{
		{title: "stream", s: &Stream{}, output: `<stream>\(0x[[:xdigit:]]+\)`},
	}

	var buf bytes.Buffer
	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			buf.Reset()
			assert.NoError(t, tt.s.WriteTerm(&buf, &tt.opts, nil))
			assert.Regexp(t, tt.output, buf.String())
		})
	}
}

func TestStream_Compare(t *testing.T) {
	x := NewVariable()
	var ss [3]Stream

	tests := []struct {
		title string
		s     *Stream
		t     Term
		o     int
	}{
		{title: `s > X`, s: &ss[1], t: x, o: 1},
		{title: `s > 1.0`, s: &ss[1], t: Float(1), o: 1},
		{title: `s > 1`, s: &ss[1], t: Integer(2), o: 1},
		{title: `s > a`, s: &ss[1], t: NewAtom("a"), o: 1},
		{title: `s > s`, s: &ss[1], t: &ss[0], o: 1},
		{title: `s = s`, s: &ss[1], t: &ss[1], o: 0},
		{title: `s < s`, s: &ss[1], t: &ss[2], o: -1},
		{title: `s < f(a)`, s: &ss[1], t: NewAtom("f").Apply(NewAtom("a")), o: -1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			assert.Equal(t, tt.o, tt.s.Compare(tt.t, nil))
		})
	}
}

type mockNamer struct {
	mock.Mock
}

func (m *mockNamer) Name() string {
	args := m.Called()
	return args.String(0)
}

func TestStream_Name(t *testing.T) {
	t.Run("namer", func(t *testing.T) {
		var m struct {
			mockReader
			mockNamer
		}
		m.mockNamer.On("Name").Return("name").Once()
		defer m.mockNamer.AssertExpectations(t)

		s := &Stream{source: &m}
		assert.Equal(t, "name", s.Name())
	})

	t.Run("not namer", func(t *testing.T) {
		var m mockWriter

		s := &Stream{sink: &m}
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
	var okCloser struct {
		mockReader
		mockCloser
	}
	okCloser.mockCloser.On("Close").Return(nil)

	var ngCloser struct {
		mockWriter
		mockCloser
	}
	ngCloser.mockCloser.On("Close").Return(errors.New("ng"))

	var vm VM

	foo := NewAtom("foo")
	s := &Stream{vm: &vm, source: &okCloser, alias: foo}
	vm.streams.add(s)

	bar := NewAtom("bar")
	vm.streams.add(&Stream{vm: &vm, source: &okCloser, alias: bar})

	tests := []struct {
		title string
		s     *Stream
		err   error
	}{
		{title: "ok closer", s: &Stream{source: &okCloser}},
		{title: "not closer", s: &Stream{source: &okCloser.mockReader}},
		{title: "alias", s: s},

		{title: "ng closer", s: &Stream{sink: &ngCloser}, err: errors.New("ng")},
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
			s:     &Stream{source: bytes.NewReader([]byte{1, 2, 3}), streamType: streamTypeBinary},
			b:     1,
			pos:   1,
			eos:   endOfStreamNot,
		},
		{
			title: "input binary: 2 bytes left",
			s:     &Stream{source: bytes.NewReader([]byte{2, 3}), streamType: streamTypeBinary, position: 1},
			b:     2,
			pos:   2,
			eos:   endOfStreamNot,
		},
		{
			title: "input binary: 1 byte left",
			s:     &Stream{source: bytes.NewReader([]byte{3}), streamType: streamTypeBinary, position: 2},
			b:     3,
			pos:   3,
			eos:   endOfStreamNot,
		},
		{
			title: "input binary: empty",
			s:     &Stream{source: bytes.NewReader([]byte{}), streamType: streamTypeBinary, position: 3},
			err:   io.EOF,
			pos:   3,
			eos:   endOfStreamPast,
		},
		{
			title: "end of stream past: error",
			s:     &Stream{source: bytes.NewReader([]byte{1, 2, 3}), streamType: streamTypeBinary, endOfStream: endOfStreamPast, eofAction: eofActionError},
			err:   errPastEndOfStream,
			pos:   0,
			eos:   endOfStreamPast,
		},
		{
			title: "end of stream past: reset",
			s:     &Stream{source: bytes.NewReader([]byte{1, 2, 3}), streamType: streamTypeBinary, endOfStream: endOfStreamPast, eofAction: eofActionReset, reposition: true},
			b:     1,
			pos:   1,
			eos:   endOfStreamNot,
		},
		{
			title: "input text",
			s:     &Stream{source: bytes.NewReader([]byte{1, 2, 3}), streamType: streamTypeText},
			err:   errWrongStreamType,
		},
		{
			title: "output",
			s:     &Stream{source: bytes.NewReader([]byte{1, 2, 3}), mode: ioModeAppend},
			err:   errWrongIOMode,
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
			s:     &Stream{source: bytes.NewReader([]byte("abc")), streamType: streamTypeText},
			r:     'a',
			size:  1,
			pos:   1,
			eos:   endOfStreamNot,
		},
		{
			title: "input text: 2 runes left",
			s:     &Stream{source: bytes.NewReader([]byte("bc")), streamType: streamTypeText, position: 1},
			r:     'b',
			size:  1,
			pos:   2,
			eos:   endOfStreamNot,
		},
		{
			title: "input text: 1 rune left, abrupt EOF",
			s:     &Stream{source: bytes.NewReader([]byte("c")), streamType: streamTypeText, position: 2},
			r:     'c',
			size:  1,
			pos:   3,
			eos:   endOfStreamNot,
		},
		{
			title: "input text: 1 rune left, non-abrupt EOF",
			s:     &Stream{source: newNonAbruptReader([]byte("c")), streamType: streamTypeText, position: 2},
			r:     'c',
			size:  1,
			pos:   3,
			eos:   endOfStreamAt,
		},
		{
			title: "input text: 1 rune left, file",
			s:     &Stream{source: mustOpen(testdata, "testdata/a.txt"), streamType: streamTypeText, position: 0},
			r:     'a',
			size:  1,
			pos:   1,
			eos:   endOfStreamAt,
		},
		{
			title: "input Text: empty",
			s:     &Stream{source: bytes.NewReader([]byte("")), streamType: streamTypeText, position: 3},
			err:   io.EOF,
			pos:   3,
			eos:   endOfStreamPast,
		},
		{
			title: "end of stream past: error",
			s:     &Stream{source: bytes.NewReader([]byte("abc")), streamType: streamTypeText, endOfStream: endOfStreamPast, eofAction: eofActionError},
			err:   errPastEndOfStream,
			pos:   0,
			eos:   endOfStreamPast,
		},
		{
			title: "end of stream past: reset",
			s:     &Stream{source: bytes.NewReader([]byte("abc")), streamType: streamTypeText, endOfStream: endOfStreamPast, eofAction: eofActionReset, reposition: true},
			r:     'a',
			size:  1,
			pos:   1,
			eos:   endOfStreamNot,
		},
		{
			title: "input binary",
			s:     &Stream{source: bytes.NewReader([]byte("abc")), streamType: streamTypeBinary},
			err:   errWrongStreamType,
		},
		{
			title: "output",
			s:     &Stream{source: bytes.NewReader([]byte("abc")), mode: ioModeAppend},
			err:   errWrongIOMode,
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
	var okSeeker struct {
		mockReader
		mockSeeker
	}
	okSeeker.mockSeeker.On("Seek", int64(0), 0).Return(int64(0), nil)

	var ngSeeker struct {
		mockWriter
		mockSeeker
	}
	ngSeeker.mockSeeker.On("Seek", mock.Anything, mock.Anything).Return(int64(0), errors.New("ng"))

	s := &Stream{source: bytes.NewReader([]byte("abc")), streamType: streamTypeBinary, reposition: true}
	_, err := s.ReadByte()
	assert.NoError(t, err)

	tests := []struct {
		title  string
		s      *Stream
		offset int64
		whence int
		pos    int64
		err    error
	}{
		{
			title:  "ok",
			s:      &Stream{source: &okSeeker, reposition: true},
			offset: 0,
			whence: 0,
		},
		{
			title:  "ng",
			s:      &Stream{sink: &ngSeeker, reposition: true},
			offset: 0,
			whence: 0,
			err:    errors.New("ng"),
		},
		{title: "reader", s: s, offset: 0, whence: 0, pos: 0},
		{title: "reader", s: s, offset: 1, whence: 0, pos: 1},
		{title: "reader", s: s, offset: 2, whence: 0, pos: 2},
		{title: "reader", s: s, offset: 3, whence: 0, pos: 3},
		{
			title:  "not seeker",
			s:      &Stream{source: &okSeeker.mockReader, reposition: true, position: 123},
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
			s:     &Stream{sink: &m, mode: ioModeAppend, streamType: streamTypeBinary},
			c:     byte('a'),
			pos:   1,
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
			s:     &Stream{sink: &m, mode: ioModeAppend, streamType: streamTypeText},
			r:     'a',
			n:     1,
			pos:   1,
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
		var m struct {
			mockWriter
			mockFlusher
		}
		m.mockFlusher.On("Flush").Return(nil).Once()
		defer m.mockFlusher.AssertExpectations(t)

		s := &Stream{sink: &m, mode: ioModeAppend}
		assert.NoError(t, s.Flush())
	})

	t.Run("syncer", func(t *testing.T) {
		var m struct {
			mockWriter
			mockSyncer
		}
		m.mockSyncer.On("Sync").Return(nil).Once()
		defer m.mockSyncer.AssertExpectations(t)

		s := &Stream{sink: &m, mode: ioModeAppend}
		assert.NoError(t, s.Flush())
	})

	t.Run("else", func(t *testing.T) {
		var m mockWriter
		defer m.AssertExpectations(t)

		s := &Stream{sink: &m, mode: ioModeAppend}
		assert.NoError(t, s.Flush())
	})
}

type nonAbruptReader struct {
	*bytes.Reader
}

func newNonAbruptReader(b []byte) nonAbruptReader {
	return nonAbruptReader{
		Reader: bytes.NewReader(b),
	}
}

func (r nonAbruptReader) Read(b []byte) (int, error) {
	n, err := r.Reader.Read(b)
	if err == nil && r.Reader.Len() == 0 {
		err = io.EOF
	}
	return n, err
}
