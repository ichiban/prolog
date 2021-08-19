package term

import (
	"bytes"
	"fmt"
	"io"
)

type StreamMode int

const (
	StreamModeRead StreamMode = iota
	StreamModeWrite
	StreamModeAppend
)

type EofAction int

const (
	EofActionEOFCode EofAction = iota
	EofActionError
	EofActionReset
)

type StreamType int

const (
	StreamTypeText StreamType = iota
	StreamTypeBinary
)

// Stream is a prolog stream.
type Stream struct {
	Source io.Reader
	Sink   io.Writer
	Closer io.Closer

	Mode       StreamMode
	Alias      Atom
	EofAction  EofAction
	Reposition bool
	StreamType StreamType
}

func (s *Stream) String() string {
	var buf bytes.Buffer
	_ = s.WriteTerm(&buf, DefaultWriteTermOptions, nil)
	return buf.String()
}

// WriteTerm writes the stream into w.
func (s *Stream) WriteTerm(w io.Writer, _ WriteTermOptions, _ Env) error {
	if s.Alias != "" {
		_, err := fmt.Fprintf(w, "<stream>(%s)", s.Alias)
		return err
	}
	_, err := fmt.Fprintf(w, "<stream>(%p)", s)
	return err
}

// Unify unifies the stream with t.
func (s *Stream) Unify(t Interface, occursCheck bool, env *Env) bool {
	switch t := t.(type) {
	case *Stream:
		return s == t
	case Variable:
		return t.Unify(s, occursCheck, env)
	default:
		return false
	}
}
