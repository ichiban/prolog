package internal

import (
	"bytes"
	"embed"
	_ "embed"
	"errors"
	"io"
	"io/fs"
	"testing"
	"time"
)

type mockNamer struct {
	name string
}

func (m *mockNamer) Name() string {
	return m.name
}

func TestStream_Name(t *testing.T) {
	t.Run("namer", func(t *testing.T) {
		var m struct {
			bytes.Buffer
			mockNamer
		}
		m.name = "Name"

		s := &Stream{source: &m}
		if s.Name() != "Name" {
			t.Error("unexpected Name")
		}
	})

	t.Run("not namer", func(t *testing.T) {
		var m bytes.Buffer

		s := &Stream{sink: &m}
		if s.Name() != "" {
			t.Error("unexpected Name")
		}
	})
}

type mockFile struct {
	stat  func() (fs.FileInfo, error)
	read  func(b []byte) (n int, err error)
	close func() error
	seek  func(offset int64, whence int) (int64, error)
}

func (m *mockFile) Stat() (fs.FileInfo, error) {
	return m.stat()
}

func (m *mockFile) Read(p []byte) (int, error) {
	return m.read(p)
}

func (m *mockFile) Close() error {
	return m.close()
}

func (m *mockFile) Seek(offset int64, whence int) (int64, error) {
	return m.seek(offset, whence)
}

type mockFileInfo struct {
	name     string
	size     int64
	fileMode fs.FileMode
	modTime  time.Time
	isDir    bool
	sys      any
}

func (m *mockFileInfo) Name() string {
	return m.name
}

func (m *mockFileInfo) Size() int64 {
	return m.size
}

func (m *mockFileInfo) Mode() fs.FileMode {
	return m.fileMode
}

func (m *mockFileInfo) ModTime() time.Time {
	return m.modTime
}

func (m *mockFileInfo) IsDir() bool {
	return m.isDir
}

func (m *mockFileInfo) Sys() any {
	return m.sys
}

type mockCloser struct {
	close func() error
}

func (m *mockCloser) Close() error {
	return m.close()
}

func TestStream_Close(t *testing.T) {
	var okCloser struct {
		bytes.Buffer
		mockCloser
	}
	okCloser.close = func() error {
		return nil
	}

	ng := errors.New("ng")

	var ngCloser struct {
		bytes.Buffer
		mockCloser
	}
	ngCloser.close = func() error {
		return ng
	}

	foo := NewAtom("foo")
	s := &Stream{source: &okCloser, alias: foo}

	tests := []struct {
		title string
		s     *Stream
		err   error
	}{
		{title: "ok closer", s: &Stream{source: &okCloser}},
		{title: "not closer", s: &Stream{source: bytes.NewBuffer(nil)}},
		{title: "alias", s: s},

		{title: "ng closer", s: &Stream{sink: &ngCloser}, err: ng},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			if !errors.Is(tt.s.Close(), tt.err) {
				t.Error("unexpected error")
			}
		})
	}
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
			if b != tt.b {
				t.Errorf("got %v, want %v", b, tt.b)
			}
			if !errors.Is(err, tt.err) {
				t.Errorf("got %v, want %v", err, tt.err)
			}

			if tt.s.position != tt.pos {
				t.Errorf("got %v, want %v", tt.s.position, tt.pos)
			}
			if tt.s.endOfStream != tt.eos {
				t.Errorf("got %v, want %v", tt.s.endOfStream, tt.eos)
			}
		})
	}
}

func TestStream_ReadRune(t *testing.T) {
	var m mockFile
	m.stat = func() (fs.FileInfo, error) {
		return &mockFileInfo{}, errors.New("failed")
	}
	m.read = func(p []byte) (n int, err error) {
		copy(p, "a")
		return 1, nil
	}

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
			s:     &Stream{source: must(testdata.Open("testdata/a.txt")), streamType: streamTypeText, position: 0},
			r:     'a',
			size:  1,
			pos:   1,
			eos:   endOfStreamAt,
		},
		{
			title: "input text: 1 rune left, file, failed to get file size",
			s:     &Stream{source: &m, streamType: streamTypeText, position: 0},
			r:     'a',
			size:  1,
			pos:   1,
			eos:   endOfStreamNot,
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
			if r != tt.r {
				t.Errorf("got %v, want %v", r, tt.r)
			}
			if size != tt.size {
				t.Errorf("got %v, want %v", size, tt.size)
			}
			if !errors.Is(err, tt.err) {
				t.Errorf("got %v, want %v", err, tt.err)
			}

			if tt.s.position != tt.pos {
				t.Errorf("got %v, want %v", tt.s.position, tt.pos)
			}
			if tt.s.endOfStream != tt.eos {
				t.Errorf("got %v, want %v", tt.s.endOfStream, tt.eos)
			}
		})
	}
}

var (
	//go:embed testdata
	testdata embed.FS
)

func must(fs fs.File, err error) fs.File {
	if err != nil {
		panic(err)
	}
	return fs
}

type mockSeeker struct {
	seek func(offset int64, whence int) (int64, error)
}

func (m *mockSeeker) Seek(offset int64, whence int) (int64, error) {
	return m.seek(offset, whence)
}

func TestStream_Seek(t *testing.T) {
	var okSeeker struct {
		bytes.Buffer
		mockSeeker
	}
	okSeeker.seek = func(offset int64, whence int) (int64, error) {
		return 0, nil
	}

	ng := errors.New("ng")

	var ngSeeker struct {
		bytes.Buffer
		mockSeeker
	}
	ngSeeker.seek = func(offset int64, whence int) (int64, error) {
		return 0, ng
	}

	s := &Stream{source: bytes.NewReader([]byte("abc")), streamType: streamTypeBinary, reposition: true}
	_, err := s.ReadByte()
	if err != nil {
		t.Error(err)
	}

	tests := []struct {
		title  string
		s      *Stream
		offset int64
		whence int
		pos    int64
		err    error
	}{
		{
			title:  "ok input",
			s:      &Stream{source: &okSeeker, mode: ioModeRead, reposition: true},
			offset: 0,
			whence: 0,
		},
		{
			title:  "ok output",
			s:      &Stream{sink: &okSeeker, mode: ioModeWrite, reposition: true},
			offset: 0,
			whence: 0,
		},
		{
			title:  "ng input",
			s:      &Stream{source: &ngSeeker, mode: ioModeRead, reposition: true},
			offset: 0,
			whence: 0,
			err:    ng,
		},
		{
			title:  "ng output",
			s:      &Stream{sink: &ngSeeker, mode: ioModeWrite, reposition: true},
			offset: 0,
			whence: 0,
			err:    ng,
		},
		{title: "reader", s: s, offset: 0, whence: 0, pos: 0},
		{title: "reader", s: s, offset: 1, whence: 0, pos: 1},
		{title: "reader", s: s, offset: 2, whence: 0, pos: 2},
		{title: "reader", s: s, offset: 3, whence: 0, pos: 3},
		{
			title:  "not seeker",
			s:      &Stream{source: &okSeeker.Buffer, reposition: true, position: 123},
			offset: 0,
			whence: 0,
			pos:    123,
		},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			pos, err := tt.s.Seek(tt.offset, tt.whence)
			if pos != tt.pos {
				t.Errorf("got %v, want %v", pos, tt.pos)
			}
			if !errors.Is(err, tt.err) {
				t.Errorf("got %v, want %v", err, tt.err)
			}
		})
	}
}

func TestStream_WriteByte(t *testing.T) {
	var m bytes.Buffer

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
			if !errors.Is(err, tt.err) {
				t.Errorf("got %v, want %v", err, tt.err)
			}

			if tt.s.position != tt.pos {
				t.Errorf("got %v, want %v", tt.s.position, tt.pos)
			}
		})
	}
}

func TestStream_WriteRune(t *testing.T) {
	var m bytes.Buffer

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
			if n != tt.n {
				t.Errorf("got %v, want %v", n, tt.n)
			}
			if !errors.Is(err, tt.err) {
				t.Errorf("got %v, want %v", err, tt.err)
			}

			if tt.s.position != tt.pos {
				t.Errorf("got %v, want %v", tt.s.position, tt.pos)
			}
		})
	}
}

type mockFlusher struct {
	flush func() error
}

func (m *mockFlusher) Flush() error {
	return m.flush()
}

type mockSyncer struct {
	sync func() error
}

func (m *mockSyncer) Sync() error {
	return m.sync()
}

func TestStream_Flush(t *testing.T) {
	t.Run("flusher", func(t *testing.T) {
		called := false
		var m struct {
			bytes.Buffer
			mockFlusher
		}
		m.flush = func() error {
			called = true
			return nil
		}

		s := &Stream{sink: &m, mode: ioModeAppend}
		if err := s.Flush(); err != nil {
			t.Error(err)
		}
		if !called {
			t.Error("expected flusher to be called")
		}
	})

	t.Run("syncer", func(t *testing.T) {
		called := false
		var m struct {
			bytes.Buffer
			mockSyncer
		}
		m.sync = func() error {
			called = true
			return nil
		}

		s := &Stream{sink: &m, mode: ioModeAppend}
		if err := s.Flush(); err != nil {
			t.Error(err)
		}
		if !called {
			t.Error("expected syncer to be called")
		}
	})

	t.Run("else", func(t *testing.T) {
		var m bytes.Buffer

		s := &Stream{sink: &m, mode: ioModeAppend}
		if err := s.Flush(); err != nil {
			t.Error(err)
		}
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
