package internal

import (
	"bufio"
	"errors"
	"github.com/ichiban/prolog/internal/rbtree"
	"io"
	"io/fs"
	"os"
)

var (
	errTooManyStreams       = errors.New("too many streams")
	errTooManyStreamAliases = errors.New("too many streams aliases")
	errWrongIOMode          = errors.New("wrong i/o mode")
	errWrongStreamType      = errors.New("wrong stream type")
	errPastEndOfStream      = errors.New("past end of stream")
	errReposition           = errors.New("reposition")
)

// Stream is a prolog stream.
type Stream struct {
	source       io.Reader
	sink         io.Writer
	buf          bufReader
	lastRuneSize int

	mode        ioMode
	alias       Atom
	position    int64
	endOfStream endOfStream
	eofAction   eofAction
	reposition  bool
	streamType  streamType
}

// Name returns the stream's Name. If the underlying source/sink doesn't have a Name, returns "".
func (s *Stream) Name() string {
	type namer interface {
		Name() string
	}

	if f, ok := s.source.(namer); ok {
		return f.Name()
	}

	if f, ok := s.sink.(namer); ok {
		return f.Name()
	}

	return ""
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

	b, err := s.buf.ReadByte()
	if err == nil {
		s.position += 1
	}
	s.checkEOS(err)
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

	r, n, err := s.buf.ReadRune()
	s.position += int64(n)
	s.lastRuneSize = n
	s.checkEOS(err)
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

	sk, ok := s.source.(io.Seeker)
	if !ok {
		sk, ok = s.sink.(io.Seeker)
		if !ok {
			return s.position, nil
		}
	}

	n, err := sk.Seek(offset, whence)
	if err != nil {
		return n, err
	}

	s.position = n
	s.reset()

	return n, nil
}

// WriteByte writes the byte c to the underlying sink.
// It throws an error if the stream is not an output binary stream.
func (s *Stream) WriteByte(c byte) error {
	b, err := s.binaryWriter()
	if err != nil {
		return err
	}
	_, err = b.Write([]byte{c})
	return err
}

// WriteRune writes the rune r to the underlying sink.
// It throws an error if the stream is not an output binary stream.
func (s *Stream) WriteRune(r rune) (size int, err error) {
	t, err := s.textWriter()
	if err != nil {
		return 0, err
	}
	return t.Write([]byte(string(r)))
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

	switch f := s.sink.(type) {
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
	if c, ok := s.source.(io.Closer); ok {
		if err := c.Close(); err != nil {
			return err
		}
	}

	if c, ok := s.sink.(io.Closer); ok {
		if err := c.Close(); err != nil {
			return err
		}
	}

	return nil
}

func (s *Stream) initRead() error {
	if s.mode != ioModeRead {
		return errWrongIOMode
	}

	if s.buf == (bufReader{}) {
		s.buf = newBufReader(s.source)
	}

	if s.endOfStream == endOfStreamPast {
		switch s.eofAction {
		case eofActionError:
			return errPastEndOfStream
		case eofActionReset:
			s.reset()
		default:
			break
		}
	}

	return nil
}

func (s *Stream) reset() {
	if s.mode != ioModeRead {
		return
	}

	s.buf = newBufReader(s.source)
	s.endOfStream = endOfStreamNot
}

func (s *Stream) checkEOS(err error) {
	// After reading, we might be at the end of stream.
	switch b := s.buf.Buffered(); {
	case errors.Is(err, io.EOF):
		s.endOfStream = endOfStreamPast
	case b == 0 && errors.Is(s.buf.ReadErr(), io.EOF):
		// io.Reader may return io.EOF at the very last read with a non-zero number of bytes.
		// In that case, we can say we're at the end of stream after consuming all the buffered bytes.
		s.endOfStream = endOfStreamAt
	case b == 0 && s.position == fileSize(s.source):
		// If the position equals to the file size after consuming all the buffered bytes,
		// we can say we're at the end of stream.
		s.endOfStream = endOfStreamAt
	default:
		// At least one byte is buffered or the underlying io.Reader hasn't reported io.EOF yet.
		// io.Reader may surprise us with `0, io.EOF`. In that case, we fail to detect the end of stream.
		s.endOfStream = endOfStreamNot
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
	if s.mode != ioModeWrite && s.mode != ioModeAppend {
		return textWriter{}, errWrongIOMode
	}

	if s.streamType != streamTypeText {
		return textWriter{}, errWrongStreamType
	}

	return textWriter{stream: s}, nil
}

func (s *Stream) binaryWriter() (binaryWriter, error) {
	if s.mode != ioModeWrite && s.mode != ioModeAppend {
		return binaryWriter{}, errWrongIOMode
	}

	if s.streamType != streamTypeBinary {
		return binaryWriter{}, errWrongStreamType
	}

	return binaryWriter{stream: s}, nil
}

type textWriter struct {
	stream *Stream
}

// Write writes to the underlying sink.
// It throws an error if the stream is not an output text stream.
func (t textWriter) Write(p []byte) (int, error) {
	s := t.stream
	n, err := s.sink.Write(p)
	s.position += int64(n)
	return n, err
}

type binaryWriter struct {
	stream *Stream
}

// Write writes the contents of p to the underlying sink.
// It throws an error if the stream is not an output binary stream.
func (b binaryWriter) Write(p []byte) (int, error) {
	s := b.stream

	n, err := s.sink.Write(p)
	s.position += int64(n)
	return n, err
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

// streamType describes what will be transferred in the stream, either text or binary.
type streamType int

const (
	// streamTypeText means text.
	streamTypeText streamType = iota
	// streamTypeBinary means binary.
	streamTypeBinary
)

type endOfStream uint8

const (
	endOfStreamNot endOfStream = iota
	endOfStreamAt
	endOfStreamPast
)

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

type StreamID int32

type StreamPool struct {
	streams []Stream
	aliases rbtree.Map[Atom, StreamID]
}

type StreamOption func(*Stream)

func WithStreamTypeText() StreamOption {
	return func(s *Stream) {
		s.streamType = streamTypeText
	}
}

func WithStreamTypeBinary() StreamOption {
	return func(s *Stream) {
		s.streamType = streamTypeBinary
	}
}

func WithStreamAlias(alias Atom) StreamOption {
	return func(s *Stream) {
		s.alias = alias
	}
}

// PutInputStream creates a new input text stream backed by the given io.Reader.
func (s *StreamPool) PutInputStream(r io.Reader, opts ...StreamOption) (StreamID, error) {
	in := Stream{
		source:     r,
		mode:       ioModeRead,
		eofAction:  eofActionReset,
		reposition: false,
		streamType: streamTypeText,
	}
	for _, opt := range opts {
		opt(&in)
	}

	var ok bool
	s.streams, ok = cappend(s.streams, in)
	if !ok {
		return 0, errTooManyStreams
	}
	id := StreamID(len(s.streams) - 1)
	if in.alias != 0 {
		if !s.aliases.SafeSet(in.alias, id) {
			return 0, errTooManyStreamAliases
		}
	}
	return id, nil
}

// PutOutputStream creates a new output stream backed by the given io.Writer.
func (s *StreamPool) PutOutputStream(w io.Writer, opts ...StreamOption) (StreamID, error) {
	out := Stream{
		sink:       w,
		mode:       ioModeAppend,
		eofAction:  eofActionReset,
		reposition: false,
		streamType: streamTypeText,
	}
	for _, opt := range opts {
		opt(&out)
	}

	var ok bool
	s.streams, ok = cappend(s.streams, out)
	if !ok {
		return 0, errTooManyStreams
	}
	id := StreamID(len(s.streams) - 1)
	if out.alias != 0 {
		if !s.aliases.SafeSet(out.alias, id) {
			return 0, errTooManyStreamAliases
		}
	}
	return id, nil
}

func (s *StreamPool) GetByID(id StreamID) (*Stream, bool) {
	if id < 0 || id >= StreamID(len(s.streams)) {
		return nil, false
	}
	return &s.streams[id], true
}

func (s *StreamPool) GetByAlias(alias Atom) (*Stream, bool) {
	id, ok := s.aliases.Get(alias)
	if !ok {
		return nil, false
	}
	return s.GetByID(id)
}
