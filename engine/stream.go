package engine

import (
	"bufio"
	"errors"
	"io"
	"io/fs"
	"os"
)

// ioMode describes what operations you can perform on the stream.
type ioMode int

const (
	// ioModeRead means you can read from the stream.
	ioModeRead = ioMode(os.O_RDONLY)
	// ioModeWrite means you can write to the stream.
	ioModeWrite = ioMode(os.O_CREATE | os.O_WRONLY)
	// ioModeAppend means you can append to the stream.
	ioModeAppend = ioMode(os.O_APPEND) | ioModeWrite
)

func (m ioMode) Term() Term {
	return [...]Term{
		ioModeRead:   atomRead,
		ioModeWrite:  atomWrite,
		ioModeAppend: atomAppend,
	}[m]
}

// eofAction describes what happens when you reached to the end of the stream.
type eofAction int

const (
	// eofActionEOFCode means either an atom `end_of_file`, or an integer `-1` will be returned.
	eofActionEOFCode eofAction = iota
	// eofActionError means an error will be raised.
	eofActionError
	// eofActionReset means another attempt will be made.
	eofActionReset
)

func (a eofAction) Term() Term {
	return [...]Term{
		eofActionError:   atomError,
		eofActionEOFCode: atomEOFCode,
		eofActionReset:   atomReset,
	}[a]
}

// streamType describes what will be transferred in the stream, either text or binary.
type streamType int

const (
	// streamTypeText means text.
	streamTypeText streamType = iota
	// streamTypeBinary means binary.
	streamTypeBinary
)

func (t streamType) Term() Term {
	return [...]Atom{
		streamTypeText:   atomText,
		streamTypeBinary: atomBinary,
	}[t]
}

var errNotSupported = errors.New("not supported")

// Stream is a prolog stream.
type Stream struct {
	sourceSink interface{} // Either io.Reader or io.Writer.
	buf        *bufio.Reader
	mode       ioMode
	alias      Atom
	eofAction  eofAction
	reposition bool
	streamType streamType
}

// Name returns the stream's name. If the underlying source/sink doesn't have a name, returns "".
func (s *Stream) Name() string {
	type namer interface {
		Name() string
	}
	f, ok := s.sourceSink.(namer)
	if !ok {
		return ""
	}

	return f.Name()
}

// Stat returns the underlying source/sink's fs.FileInfo.
func (s *Stream) Stat() (fs.FileInfo, error) {
	f, ok := s.sourceSink.(fs.File)
	if !ok {
		return nil, errNotSupported
	}

	return f.Stat()
}

// Read reads from the underlying source.
func (s *Stream) Read(p []byte) (int, error) {
	if err := s.initBuf(); err != nil {
		return 0, err
	}
	return s.buf.Read(p)
}

// ReadByte reads a byte from the underlying source.
func (s *Stream) ReadByte() (byte, error) {
	if err := s.initBuf(); err != nil {
		return 0, err
	}
	return s.buf.ReadByte()
}

// Peek peeks the next n bytes from the underlying source.
func (s *Stream) Peek(n int) ([]byte, error) {
	if err := s.initBuf(); err != nil {
		return nil, err
	}
	return s.buf.Peek(n)
}

// ReadRune reads the next rune from the underlying source.
func (s *Stream) ReadRune() (r rune, size int, err error) {
	if err := s.initBuf(); err != nil {
		return 0, 0, err
	}
	return s.buf.ReadRune()
}

func (s *Stream) UnreadRune() error {
	if err := s.initBuf(); err != nil {
		return err
	}
	return s.buf.UnreadRune()
}

// Seek sets the offset to the underlying source/sink.
func (s *Stream) Seek(offset int64, whence int) (int64, error) {
	sk, ok := s.sourceSink.(io.Seeker)
	if !ok {
		return 0, errNotSupported
	}

	n, err := sk.Seek(offset, whence)
	if err != nil {
		return n, err
	}

	if r, ok := sk.(io.Reader); ok && s.buf != nil {
		s.buf.Reset(r)
	}

	return n, nil
}

func (s *Stream) Write(p []byte) (int, error) {
	w, ok := s.sourceSink.(io.Writer)
	if !ok {
		return 0, errNotSupported
	}

	return w.Write(p)
}

// Flush flushes the buffered output to the sink.
func (s *Stream) Flush() error {
	// E.g. *bufio.Writer.
	type flusher interface {
		Flush() error
	}

	// E.g. *os.File.
	type syncer interface {
		Sync() error
	}

	switch f := s.sourceSink.(type) {
	case flusher:
		return f.Flush()
	case syncer:
		return f.Sync()
	default:
		return nil
	}
}

// Close closes the underlying file of the stream.
func (s *Stream) Close() error {
	if c, ok := s.sourceSink.(io.Closer); ok {
		return c.Close()
	}
	return nil
}

func (s *Stream) initBuf() error {
	if s.buf != nil {
		return nil
	}

	r, ok := s.sourceSink.(io.Reader)
	if !ok {
		return errNotSupported
	}
	s.buf = bufio.NewReader(r)
	return nil
}

func (s *Stream) properties() []Term {
	ps := []Term{
		atomMode.Apply(s.mode.Term()),
	}

	switch s.mode {
	case ioModeRead:
		ps = append(ps, atomInput)
	case ioModeWrite, ioModeAppend:
		ps = append(ps, atomOutput)
	}

	if s.alias != 0 {
		ps = append(ps, atomAlias.Apply(s.alias))
	}

	ps = append(ps, atomEOFAction.Apply(s.eofAction.Term()))

	if n := s.Name(); n != "" {
		ps = append(ps, atomFileName.Apply(NewAtom(n)))
	}

	if pos, err := s.Seek(0, 1); err == nil {
		ps = append(ps, atomPosition.Apply(Integer(pos)))

		if fi, err := s.Stat(); err == nil {
			size := fi.Size()
			eos := atomNot
			switch {
			case pos == size:
				eos = atomAt
			case pos > size:
				eos = atomPast
			}
			ps = append(ps, atomEndOfStream.Apply(eos))
		}
	}

	if s.reposition {
		ps = append(ps, atomReposition.Apply(atomTrue))
	} else {
		ps = append(ps, atomReposition.Apply(atomFalse))
	}

	ps = append(ps, atomType.Apply(s.streamType.Term()))

	return ps
}
