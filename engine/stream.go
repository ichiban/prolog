package engine

import (
	"bufio"
	"fmt"
	"io"
	"io/fs"
	"os"
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

func (m StreamMode) String() string {
	return [...]string{
		StreamModeRead:   "read",
		StreamModeWrite:  "write",
		StreamModeAppend: "append",
	}[m]
}

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

func (a EOFAction) String() string {
	return [...]string{
		EOFActionError:   "error",
		EOFActionEOFCode: "eof_code",
		EOFActionReset:   "reset",
	}[a]
}

// StreamType describes what will be transferred in the stream, either text or binary.
type StreamType int

const (
	// StreamTypeText means text.
	StreamTypeText StreamType = iota
	// StreamTypeBinary means binary.
	StreamTypeBinary
)

func (t StreamType) String() string {
	return [...]string{
		StreamTypeText:   "text",
		StreamTypeBinary: "false",
	}[t]
}

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
			return nil, PermissionError("open", "source_sink", name)
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
func (s *Stream) Unparse(emit func(Token), _ *Env, _ ...WriteOption) {
	if s.alias != "" {
		emit(Token{Kind: TokenIdent, Val: string(s.alias)})
		return
	}
	emit(Token{Kind: TokenIdent, Val: fmt.Sprintf("<stream>(%p)", s)}) // TODO: special token kind?
}

// Compare compares the stream to another term.
func (s *Stream) Compare(t Term, env *Env) int64 {
	switch t := env.Resolve(t).(type) {
	case *Stream:
		if s == t {
			return 0
		}
		return 1
	default:
		return 1
	}
}

var fileStat = (*os.File).Stat

func (s *Stream) properties() ([]Term, error) {
	var properties []Term

	properties = append(properties, &Compound{Functor: "mode", Args: []Term{Atom(s.mode.String())}})

	switch s.mode {
	case StreamModeRead:
		properties = append(properties, Atom("input"))
	case StreamModeWrite, StreamModeAppend:
		properties = append(properties, Atom("output"))
	}

	if s.alias != "" {
		properties = append(properties, &Compound{Functor: "alias", Args: []Term{s.alias}})
	}

	properties = append(properties, &Compound{Functor: "eof_action", Args: []Term{Atom(s.eofAction.String())}})

	if f, ok := s.file.(*os.File); ok {
		pos, err := seek(f, 0, 1)
		if err != nil {
			return nil, err
		}
		pos -= int64(s.buf.Buffered())

		fi, err := fileStat(f)
		if err != nil {
			return nil, err
		}

		eos := "not"
		switch {
		case pos == fi.Size():
			eos = "at"
		case pos > fi.Size():
			eos = "past"
		}

		properties = append(properties,
			&Compound{Functor: "file_name", Args: []Term{Atom(f.Name())}},
			&Compound{Functor: "position", Args: []Term{Integer(pos)}},
			&Compound{Functor: "end_of_stream", Args: []Term{Atom(eos)}},
		)
	}

	if s.reposition {
		properties = append(properties, &Compound{Functor: "reposition", Args: []Term{Atom("true")}})
	} else {
		properties = append(properties, &Compound{Functor: "reposition", Args: []Term{Atom("false")}})
	}

	properties = append(properties, &Compound{Functor: "type", Args: []Term{Atom(s.streamType.String())}})

	return properties, nil
}
