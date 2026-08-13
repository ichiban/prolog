package term

import (
	"bufio"
	"errors"
	"io"
	"io/fs"
	"os"
)

var (
	ErrWrongIOMode     = errors.New("wrong i/o mode")
	errWrongStreamType = errors.New("wrong stream type")
	errPastEndOfStream = errors.New("past end of stream")
	ErrReposition      = errors.New("reposition")
)

// Mode describes what operations you can perform on the stream.
type Mode int

const (
	// Read means you can read from the stream.
	Read = Mode(os.O_RDONLY)
	// Write means you can write to the stream.
	Write = Mode(os.O_CREATE | os.O_WRONLY)
	// Append means you can append to the stream.
	Append = Mode(os.O_APPEND) | Write
)

var modeName = [...]string{
	Read:   "read",
	Write:  "write",
	Append: "append",
}

func (m Mode) String() string {
	return modeName[m]
}

type EndOfStream uint8

const (
	Not EndOfStream = iota
	At
	Past
)

var EndOfStreamNames = [...]string{
	Not:  "not",
	At:   "at",
	Past: "past",
}

func (e EndOfStream) String() string {
	return EndOfStreamNames[e]
}

// EOFAction describes what happens when you reached to the end of the stream.
type EOFAction int8

const (
	// EOFCode means either an atom `end_of_file`, or an integer `-1` will be returned.
	EOFCode EOFAction = iota
	// Error means an error will be raised.
	Error
	// Reset means another attempt will be made.
	Reset
)

var eofActionNames = [...]string{
	EOFCode: "eof_code",
	Error:   "error",
	Reset:   "reset",
}

func (a EOFAction) String() string {
	return eofActionNames[a]
}

// StreamType describes what will be transferred in the stream, either text or binary.
type StreamType int8

const (
	// Text means text.
	Text StreamType = iota
	// Binary means binary.
	Binary
)

var streamTypeNames = [...]string{
	Text:   "text",
	Binary: "binary",
}

func (t StreamType) String() string {
	return streamTypeNames[t]
}

// Stream is a prolog stream.
type Stream struct {
	Source       io.Reader
	Sink         io.Writer
	buf          bufReader
	lastRuneSize int

	Closed bool

	Mode        Mode
	Alias       Atom
	Position    int64
	EndOfStream EndOfStream
	EOFAction   EOFAction
	Reposition  bool
	StreamType  StreamType
}

// Name returns the stream's name. If the underlying source/sink doesn't have a name, returns "".
func (s *Stream) Name() string {
	type namer interface {
		Name() string
	}

	if f, ok := s.Source.(namer); ok {
		return f.Name()
	}

	if f, ok := s.Sink.(namer); ok {
		return f.Name()
	}

	return ""
}

// ReadByte reads a byte from the underlying Source.
// It throws an error if the stream is not an input binary stream.
func (s *Stream) ReadByte() (byte, error) {
	if err := s.InitRead(); err != nil {
		return 0, err
	}

	if s.StreamType != Binary {
		return 0, errWrongStreamType
	}

	b, err := s.buf.ReadByte()
	if err == nil {
		s.Position += 1
	}
	s.checkEOS(err)
	return b, err
}

func (s *Stream) UnreadByte() error {
	if err := s.InitRead(); err != nil {
		return err
	}

	if s.StreamType != Binary {
		return errWrongStreamType
	}

	err := s.buf.UnreadByte()
	if err == nil {
		s.Position -= 1
		s.EndOfStream = Not
	}
	return err
}

// ReadRune reads the next rune from the underlying Source.
// It throws an error if the stream is not an input text stream.
func (s *Stream) ReadRune() (r rune, size int, err error) {
	if err := s.InitRead(); err != nil {
		return 0, 0, err
	}

	if s.StreamType != Text {
		return 0, 0, errWrongStreamType
	}

	r, n, err := s.buf.ReadRune()
	s.Position += int64(n)
	s.lastRuneSize = n
	s.checkEOS(err)
	return r, n, err
}

func (s *Stream) UnreadRune() error {
	if err := s.InitRead(); err != nil {
		return err
	}

	if s.StreamType != Text {
		return errWrongStreamType
	}

	err := s.buf.UnreadRune()
	if err == nil {
		s.Position -= int64(s.lastRuneSize)
		s.EndOfStream = Not
		s.lastRuneSize = 0
	}
	return err
}

// Seek sets the offset to the underlying Source/Sink.
func (s *Stream) Seek(offset int64, whence int) (int64, error) {
	if !s.Reposition {
		return 0, ErrReposition
	}

	sk, ok := s.Source.(io.Seeker)
	if !ok {
		sk, ok = s.Sink.(io.Seeker)
		if !ok {
			return s.Position, nil
		}
	}

	n, err := sk.Seek(offset, whence)
	if err != nil {
		return n, err
	}

	s.Position = n
	s.reset()

	return n, nil
}

// WriteByte writes the byte c to the underlying Sink.
// It throws an error if the stream is not an output binary stream,.
func (s *Stream) WriteByte(c byte) error {
	b, err := s.binaryWriter()
	if err != nil {
		return err
	}
	_, err = b.Write([]byte{c})
	return err
}

// WriteRune writes the rune r to the underlying Sink.
// It throws an error if the stream is not an output binary stream.
func (s *Stream) WriteRune(r rune) (size int, err error) {
	t, err := s.textWriter()
	if err != nil {
		return 0, err
	}
	return t.Write([]byte(string(r)))
}

// Flush flushes the buffered output to the Sink.
func (s *Stream) Flush() error {
	// E.g. *bufio.Writer.
	type flusher interface {
		Flush() error
	}

	// E.g. *os.File.
	type syncer interface {
		Sync() error
	}

	if s.Mode != Write && s.Mode != Append {
		return ErrWrongIOMode
	}

	switch f := s.Sink.(type) {
	case flusher:
		return f.Flush()
	case syncer:
		return f.Sync()
	default:
		return nil
	}
}

// Close closes the underlying Source/Sink.
func (s *Stream) Close() error {
	if c, ok := s.Source.(io.Closer); ok {
		if err := c.Close(); err != nil {
			return err
		}
	}

	if c, ok := s.Sink.(io.Closer); ok {
		if err := c.Close(); err != nil {
			return err
		}
	}

	s.Closed = true

	return nil
}

func (s *Stream) InitRead() error {
	if s.Mode != Read {
		return ErrWrongIOMode
	}

	if s.buf == (bufReader{}) {
		s.buf = newBufReader(s.Source)
	}

	if s.EndOfStream == Past {
		switch s.EOFAction {
		case Error:
			return errPastEndOfStream
		case Reset:
			s.reset()
		}
	}

	return nil
}

func (s *Stream) reset() {
	if s.Mode != Read {
		return
	}

	s.buf = newBufReader(s.Source)
	s.EndOfStream = Not
}

func (s *Stream) checkEOS(err error) {
	// After reading, we might be at the end of stream.
	switch b := s.buf.Buffered(); {
	case errors.Is(err, io.EOF):
		s.EndOfStream = Past
	case b == 0 && errors.Is(s.buf.ReadErr(), io.EOF):
		// io.Reader may return io.EOF at the very last read with a non-zero number of bytes.
		// In that case, we can say we're at the end of stream after consuming all the buffered bytes.
		s.EndOfStream = At
	case b == 0 && s.Position == fileSize(s.Source):
		// If the position equals to the file size after consuming all the buffered bytes,
		// we can say we're at the end of stream.
		s.EndOfStream = At
	default:
		// At least one byte is buffered or the underlying io.Reader hasn't reported io.EOF yet.
		// io.Reader may surprise us with `0, io.EOF`. In that case, we fail to detect the end of stream.
		s.EndOfStream = Not
	}
}

func fileSize(r io.Reader) int64 {
	f, ok := r.(fs.File)
	if !ok {
		return -1
	}
	fi, err := f.Stat()
	if err != nil {
		return -1
	}
	return fi.Size()
}

func (s *Stream) textWriter() (textWriter, error) {
	if s.Mode != Write && s.Mode != Append {
		return textWriter{}, ErrWrongIOMode
	}

	if s.StreamType != Text {
		return textWriter{}, errWrongStreamType
	}

	return textWriter{stream: s}, nil
}

func (s *Stream) binaryWriter() (binaryWriter, error) {
	if s.Mode != Write && s.Mode != Append {
		return binaryWriter{}, ErrWrongIOMode
	}

	if s.StreamType != Binary {
		return binaryWriter{}, errWrongStreamType
	}

	return binaryWriter{stream: s}, nil
}

type textWriter struct {
	stream *Stream
}

// Write writes to the underlying Sink.
// It throws an error if the stream is not an output text stream.
func (t textWriter) Write(p []byte) (int, error) {
	s := t.stream
	n, err := s.Sink.Write(p)
	s.Position += int64(n)
	return n, err
}

type binaryWriter struct {
	stream *Stream
}

// Write writes the contents of p to the underlying Sink.
// It throws an error if the stream is not an output binary stream.
func (b binaryWriter) Write(p []byte) (int, error) {
	s := b.stream

	n, err := s.Sink.Write(p)
	s.Position += int64(n)
	return n, err
}

// bufReader is a wrapper around *bufio.Reader.
// *bufio.Reader doesn't tell us if the underlying io.Reader returned an error.
// We need to know this to determine end_of_stream.
type bufReader struct {
	*bufio.Reader
	er *errReader
}

func newBufReader(r io.Reader) bufReader {
	er := errReader{r: r}
	return bufReader{
		Reader: bufio.NewReader(&er),
		er:     &er,
	}
}

func (b bufReader) ReadErr() error {
	return b.er.err
}

type errReader struct {
	r   io.Reader
	err error
}

func (e *errReader) Read(p []byte) (n int, err error) {
	defer func() {
		e.err = err
	}()
	return e.r.Read(p)
}
