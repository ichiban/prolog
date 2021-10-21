package term

import (
	"fmt"
	"io"
	"strings"

	"github.com/ichiban/prolog/syntax"
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
	var sb strings.Builder
	_ = Write(&sb, s, defaultWriteTermOptions, nil)
	return sb.String()
}

// Unify unifies the stream with t.
func (s *Stream) Unify(t Interface, occursCheck bool, env *Env) (*Env, bool) {
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
func (s *Stream) Unparse(emit func(syntax.Token), _ WriteTermOptions, _ *Env) {
	if s.Alias != "" {
		emit(syntax.Token{Kind: syntax.TokenIdent, Val: string(s.Alias)})
		return
	}
	emit(syntax.Token{Kind: syntax.TokenIdent, Val: fmt.Sprintf("<stream>(%p)", s)}) // TODO: special token kind?
}
