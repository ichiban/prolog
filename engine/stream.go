package engine

import (
	"bufio"
	"fmt"
	"io"
	"io/fs"
	"os"
	"strings"
)

// StreamMode describes what operations you can perform on the stream.
type StreamMode int

const (
	// StreamModeRead means you can read from the stream.
	StreamModeRead = StreamMode(os.O_RDONLY)
	// StreamModeWrite means you can write to the stream.
	StreamModeWrite = StreamMode(os.O_CREATE | os.O_WRONLY)
	// StreamModeAppend means you can append to the stream.
	StreamModeAppend = StreamMode(os.O_APPEND) | StreamModeWrite
)

// EOFAction describes what happens when you reached to the end of the stream.
type EOFAction int

const (
	// EOFActionEOFCode means either an atom `end_of_file`, or an integer `-1` will be returned.
	EOFActionEOFCode EOFAction = iota
	// EOFActionError means an error will be raised.
	EOFActionError
	// EOFActionReset means another attempt will be made.
	EOFActionReset
)

// StreamType describes what will be transferred in the stream, either text or binary.
type StreamType int

const (
	// StreamTypeText means text.
	StreamTypeText StreamType = iota
	// StreamTypeBinary means binary.
	StreamTypeBinary
)

// Stream is a prolog stream.
type Stream struct {
	file       io.ReadWriteCloser
	buf        *bufio.Reader
	mode       StreamMode
	alias      Atom
	eofAction  EOFAction
	reposition bool
	streamType StreamType
}

// NewStream creates a new stream from an opened file.
func NewStream(f io.ReadWriteCloser, mode StreamMode, opts ...StreamOption) *Stream {
	s := Stream{
		file: f,
		mode: mode,
	}
	if f, ok := f.(*os.File); ok {
		if stat, err := f.Stat(); err == nil {
			s.reposition = stat.Mode()&fs.ModeType == 0
		}
	}
	for _, opt := range opts {
		opt(&s)
	}
	if a, ok := f.(*rwc); ok && a.r != nil {
		s.buf = bufio.NewReader(a.r)
	} else {
		s.buf = bufio.NewReader(f)
	}
	return &s
}

// StreamOption describes an option on stream creation.
type StreamOption func(*Stream)

// WithAlias sets an alias for the stream.
func WithAlias(state *State, alias Atom) StreamOption {
	return func(s *Stream) {
		s.alias = alias
		if state.streams == nil {
			state.streams = map[Term]*Stream{}
		}
		state.streams[alias] = s
	}
}

// WithEOFAction sets eof_action for the stream.
func WithEOFAction(eofAction EOFAction) StreamOption {
	return func(s *Stream) {
		s.eofAction = eofAction
	}
}

// WithReposition sets if the stream is random access.
func WithReposition(b bool) StreamOption {
	return func(s *Stream) {
		s.reposition = b
	}
}

// WithStreamType sets type of the stream.
func WithStreamType(streamType StreamType) StreamOption {
	return func(s *Stream) {
		s.streamType = streamType
	}
}

var openFile = os.OpenFile

// Open opens a file and creates a new stream out of it.
func Open(name Atom, mode StreamMode, opts ...StreamOption) (*Stream, error) {
	f, err := openFile(string(name), int(mode), 0644)
	if err != nil {
		switch {
		case os.IsNotExist(err):
			return nil, existenceErrorSourceSink(name)
		case os.IsPermission(err):
			return nil, PermissionError("open", "source_sink", name, "%s cannot be opened.", name)
		default:
			return nil, SystemError(err)
		}
	}

	return NewStream(f, mode, opts...), nil
}

var closeFile = io.Closer.Close

// Close closes the underlying file of the stream.
func (s *Stream) Close() error {
	return closeFile(s.file)
}

func (s *Stream) String() string {
	var sb strings.Builder
	_ = Write(&sb, s, defaultWriteTermOptions, nil)
	return sb.String()
}

// Unify unifies the stream with t.
func (s *Stream) Unify(t Term, occursCheck bool, env *Env) (*Env, bool) {
	switch t := env.Resolve(t).(type) {
	case *Stream:
		return env, s == t
	case Variable:
		return t.Unify(s, occursCheck, env)
	default:
		return env, false
	}
}

// Unparse emits tokens that represent the stream.
func (s *Stream) Unparse(emit func(Token), _ WriteTermOptions, _ *Env) {
	if s.alias != "" {
		emit(Token{Kind: TokenIdent, Val: string(s.alias)})
		return
	}
	emit(Token{Kind: TokenIdent, Val: fmt.Sprintf("<stream>(%p)", s)}) // TODO: special token kind?
}
