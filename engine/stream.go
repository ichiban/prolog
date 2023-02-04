package engine

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"os"
	"unsafe"
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

	source       io.Reader
	sink         io.Writer
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

// NewInputTextStream creates a new input text stream backed by the given io.Reader.
func NewInputTextStream(r io.Reader) *Stream {
	return &Stream{
		source:     r,
		mode:       ioModeRead,
		eofAction:  eofActionReset,
		reposition: false,
		streamType: streamTypeText,
	}
}

// NewInputBinaryStream creates a new input binary stream backed by the given io.Reader.
func NewInputBinaryStream(r io.Reader) *Stream {
	return &Stream{
		source:     r,
		mode:       ioModeRead,
		eofAction:  eofActionReset,
		reposition: false,
		streamType: streamTypeBinary,
	}
}

// NewOutputTextStream creates a new output text stream backed by the given io.Writer.
func NewOutputTextStream(w io.Writer) *Stream {
	return &Stream{
		sink:       w,
		mode:       ioModeAppend,
		eofAction:  eofActionReset,
		reposition: false,
		streamType: streamTypeText,
	}
}

// NewOutputBinaryStream creates a new output binary stream backed by the given io.Writer.
func NewOutputBinaryStream(w io.Writer) *Stream {
	return &Stream{
		sink:       w,
		mode:       ioModeAppend,
		eofAction:  eofActionReset,
		reposition: false,
		streamType: streamTypeBinary,
	}
}

func (s *Stream) WriteTerm(w io.Writer, _ *WriteOptions, _ *Env) error {
	_, err := fmt.Fprint(w, "<stream>")
	return err
}

func (s *Stream) Compare(t Term, env *Env) int {
	switch t := env.Resolve(t).(type) {
	case *Stream:
		switch x, y := uintptr(unsafe.Pointer(s)), uintptr(unsafe.Pointer(t)); {
		case x > y:
			return 1
		case x < y:
			return -1
		default:
			return 0
		}
	case Compound:
		return -1
	default:
		return 1
	}
}

// Name returns the stream's name. If the underlying source/sink doesn't have a name, returns "".
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

	if r, ok := sk.(io.Reader); ok && s.buf != nil {
		s.buf.Reset(r)
		s.checkEOS()
	}

	return n, nil
}

// WriteByte writes the byte c to the underlying sink.
// It throws an error if the stream is not an output binary stream,.
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

	if s.vm != nil {
		s.vm.streams.remove(s)
	}

	return nil
}

func (s *Stream) initRead() error {
	if s.buf == nil {
		s.buf = bufio.NewReader(s.source)
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
