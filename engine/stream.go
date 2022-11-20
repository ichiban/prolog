package engine

import (
	"bufio"
	"errors"
	"io"
	"os"
)

var (
	errWrongIOMode     = errors.New("wrong i/o mode")
	errWrongStreamType = errors.New("wrong stream type")
	errPastEndOfStream = errors.New("past end of stream")
	errReposition      = errors.New("reposition")
	errNotSupported    = errors.New("not supported")
)

// Stream is a prolog stream.
type Stream struct {
	vm *VM

	sourceSink   interface{} // Either io.Reader or io.Writer.
	buf          *bufio.Reader
	lastRuneSize int

	mode        ioMode
	alias       Atom
	position    int64
	endOfStream endOfStream
	eofAction   eofAction
	reposition  bool
	streamType  streamType
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

// ReadByte reads a byte from the underlying source.
// It throws an error if the stream is not an input binary stream.
func (s *Stream) ReadByte() (byte, error) {
	if err := s.initRead(); err != nil {
		return 0, err
	}

	if s.streamType != streamTypeBinary {
		return 0, errWrongStreamType
	}

	// After reading a byte, we might be at the end of stream.
	bs, err := s.buf.Peek(2)

	b, err := s.buf.ReadByte()
	if err == nil {
		s.position += 1
	}
	switch len(bs) {
	case 2:
		s.endOfStream = endOfStreamNot
	case 1:
		s.endOfStream = endOfStreamAt
	case 0:
		s.endOfStream = endOfStreamPast
	}
	return b, err
}

func (s *Stream) UnreadByte() error {
	if err := s.initRead(); err != nil {
		return err
	}

	if s.streamType != streamTypeBinary {
		return errWrongStreamType
	}

	err := s.buf.UnreadByte()
	if err == nil {
		s.position -= 1
		s.endOfStream = endOfStreamNot
	}
	return err
}

// ReadRune reads the next rune from the underlying source.
// It throws an error if the stream is not an input text stream.
func (s *Stream) ReadRune() (r rune, size int, err error) {
	if err := s.initRead(); err != nil {
		return 0, 0, err
	}

	if s.streamType != streamTypeText {
		return 0, 0, errWrongStreamType
	}

	// After reading a rune, we might be at the end of stream.
	b, _ := s.buf.Peek(5) // A rune is 1~4 bytes.

	r, n, err := s.buf.ReadRune()
	s.position += int64(n)
	s.lastRuneSize = n
	switch {
	case n == 0:
		s.endOfStream = endOfStreamPast
	case n < len(b):
		s.endOfStream = endOfStreamNot
	case n == len(b):
		s.endOfStream = endOfStreamAt
	}
	return r, n, err
}

func (s *Stream) UnreadRune() error {
	if err := s.initRead(); err != nil {
		return err
	}

	if s.streamType != streamTypeText {
		return errWrongStreamType
	}

	err := s.buf.UnreadRune()
	if err == nil {
		s.position -= int64(s.lastRuneSize)
		s.endOfStream = endOfStreamNot
		s.lastRuneSize = 0
	}
	return err
}

// Seek sets the offset to the underlying source/sink.
func (s *Stream) Seek(offset int64, whence int) (int64, error) {
	if !s.reposition {
		return 0, errReposition
	}

	sk, ok := s.sourceSink.(io.Seeker)
	if !ok {
		return s.position, nil
	}

	n, err := sk.Seek(offset, whence)
	if err != nil {
		return n, err
	}

	s.position = n

	if r, ok := sk.(io.Reader); ok && s.buf != nil {
		s.buf.Reset(r)
		s.checkEOS()
	}

	return n, nil
}

// Write writes the contents of p to the underlying sink.
// It throws an error if the stream is not an output binary stream.
func (s *Stream) Write(p []byte) (int, error) {
	if s.mode != ioModeWrite && s.mode != ioModeAppend {
		return 0, errWrongIOMode
	}

	if s.streamType != streamTypeBinary {
		return 0, errWrongStreamType
	}

	w, ok := s.sourceSink.(io.Writer)
	if !ok {
		return 0, errNotSupported
	}

	n, err := w.Write(p)
	s.position += int64(n)
	return n, err
}

// WriteString writes the contents of str to the underlying sink.
// It throws an error if the stream is not an output text stream.
func (s *Stream) WriteString(str string) (int, error) {
	if s.mode != ioModeWrite && s.mode != ioModeAppend {
		return 0, errWrongIOMode
	}

	if s.streamType != streamTypeText {
		return 0, errWrongStreamType
	}

	w, ok := s.sourceSink.(io.Writer)
	if !ok {
		return 0, errNotSupported
	}

	n, err := w.Write([]byte(str))
	s.position += int64(n)
	return n, err
}

// WriteByte writes the byte c to the underlying sink.
// It throws an error if the stream is not an output binary stream,.
func (s *Stream) WriteByte(c byte) error {
	if s.mode != ioModeWrite && s.mode != ioModeAppend {
		return errWrongIOMode
	}

	if s.streamType != streamTypeBinary {
		return errWrongStreamType
	}

	w, ok := s.sourceSink.(io.Writer)
	if !ok {
		return errNotSupported
	}

	n, err := w.Write([]byte{c})
	s.position += int64(n)
	return err
}

// WriteRune writes the rune r to the underlying sink.
// It throws an error if the stream is not an output binary stream.
func (s *Stream) WriteRune(r rune) (size int, err error) {
	if s.mode != ioModeWrite && s.mode != ioModeAppend {
		return 0, errWrongIOMode
	}

	if s.streamType != streamTypeText {
		return 0, errWrongStreamType
	}

	w, ok := s.sourceSink.(io.Writer)
	if !ok {
		return 0, errNotSupported
	}

	n, err := w.Write([]byte(string(r)))
	s.position += int64(n)
	return n, err
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

	if s.mode != ioModeWrite && s.mode != ioModeAppend {
		return errWrongIOMode
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

// Close closes the underlying source/sink.
func (s *Stream) Close() error {
	if c, ok := s.sourceSink.(io.Closer); ok {
		if err := c.Close(); err != nil {
			return err
		}
	}

	if s.vm != nil {
		s.vm.streams.remove(s)
	}

	return nil
}

func (s *Stream) initRead() error {
	if s.buf == nil {
		r, ok := s.sourceSink.(io.Reader)
		if !ok {
			return errNotSupported
		}
		s.buf = bufio.NewReader(r)
	}

	if s.mode != ioModeRead {
		return errWrongIOMode
	}

	if s.endOfStream == endOfStreamPast {
		switch s.eofAction {
		case eofActionError:
			return errPastEndOfStream
		case eofActionReset:
			_, err := s.Seek(0, io.SeekStart)
			return err
		}
	}

	return nil
}

func (s *Stream) checkEOS() {
	b, _ := s.buf.Peek(2)
	switch len(b) {
	case 0:
		s.endOfStream = endOfStreamPast
	case 1:
		s.endOfStream = endOfStreamAt
	default:
		s.endOfStream = endOfStreamNot
	}
}

func (s *Stream) properties() []Term {
	ps := make([]Term, 0, 9)

	if n := s.Name(); n != "" {
		ps = append(ps, atomFileName.Apply(NewAtom(n)))
	}

	ps = append(ps, atomMode.Apply(s.mode.Term()))

	switch s.mode {
	case ioModeRead:
		ps = append(ps, atomInput)
	case ioModeWrite, ioModeAppend:
		ps = append(ps, atomOutput)
	}

	if s.alias != 0 {
		ps = append(ps, atomAlias.Apply(s.alias))
	}

	ps = append(ps,
		atomPosition.Apply(Integer(s.position)),
		atomEndOfStream.Apply(s.endOfStream.Term()),
		atomEOFAction.Apply(s.eofAction.Term()),
	)

	if s.reposition {
		ps = append(ps, atomReposition.Apply(atomTrue))
	} else {
		ps = append(ps, atomReposition.Apply(atomFalse))
	}

	ps = append(ps, atomType.Apply(s.streamType.Term()))

	return ps
}

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

type endOfStream uint8

const (
	endOfStreamNot endOfStream = iota
	endOfStreamAt
	endOfStreamPast
)

func (e endOfStream) Term() Term {
	return [...]Atom{
		endOfStreamNot:  atomNot,
		endOfStreamAt:   atomAt,
		endOfStreamPast: atomPast,
	}[e]
}

type streams struct {
	elems   []*Stream
	aliases map[Atom]*Stream
}

func (ss *streams) add(s *Stream) {
	if s.alias != 0 {
		if ss.aliases == nil {
			ss.aliases = map[Atom]*Stream{}
		}
		ss.aliases[s.alias] = s
	}

	ss.elems = append(ss.elems, s)
}

func (ss *streams) remove(s *Stream) {
	delete(ss.aliases, s.alias)
	for i, e := range ss.elems {
		if e == s {
			// Delete the i-th element.
			if i < len(ss.elems)-1 {
				copy(ss.elems[i:], ss.elems[i+1:])
			}
			ss.elems[len(ss.elems)-1] = nil
			ss.elems = ss.elems[:len(ss.elems)-1]
			return
		}
	}
}

func (ss *streams) lookup(a Atom) (*Stream, bool) {
	s, ok := ss.aliases[a]
	return s, ok
}
