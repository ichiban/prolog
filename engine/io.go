package engine

import (
	"io"
)

type IO struct {
	// Internal/external expression
	operators       operators
	charConversions map[rune]rune
	charConvEnabled bool
	doubleQuotes    doubleQuotes

	// I/O
	streams       streams
	input, output *Stream
}

// SetUserInput sets the given reader as a stream with an alias of user_input.
func (i *IO) SetUserInput(r io.Reader) {
	s := Stream{
		io:         i,
		sourceSink: r,
		mode:       ioModeRead,
		alias:      atomUserInput,
		eofAction:  eofActionReset,
		reposition: false,
		streamType: streamTypeText,
	}
	i.streams.add(&s)
	i.input = &s
}

// SetUserOutput sets the given writer as a stream with an alias of user_output.
func (i *IO) SetUserOutput(w io.Writer) {
	s := Stream{
		io:         i,
		sourceSink: w,
		mode:       ioModeAppend,
		alias:      atomUserOutput,
		eofAction:  eofActionReset,
		reposition: false,
		streamType: streamTypeText,
	}
	i.streams.add(&s)
	i.output = &s
}

// Parser creates a new parser from the current State and io.Reader.
// If non-nil, vars will hold the information on variables it parses.
func (i *IO) Parser(r io.RuneReader, vars *[]ParsedVariable) *Parser {
	if i.operators == nil {
		i.operators = operators{}
	}
	return newParser(r,
		withCharConversions(i.charConversions),
		withOperators(i.operators),
		withDoubleQuotes(i.doubleQuotes),
		withParsedVars(vars),
	)
}

// Write outputs term to the writer.
func (i *IO) Write(w io.StringWriter, t Term, opts *WriteOptions, env *Env) error {
	opts.ops = i.operators
	opts.priority = 1200
	return writeTerm(w, t, opts, env)
}

// Stream returns a stream represented by streamOrAlias.
func (i *IO) Stream(streamOrAlias Term, env *Env) (*Stream, error) {
	switch s := env.Resolve(streamOrAlias).(type) {
	case Variable:
		return nil, InstantiationError(env)
	case Atom:
		v, ok := i.streams.lookup(s)
		if !ok {
			return nil, ExistenceError(ObjectTypeStream, streamOrAlias, env)
		}
		return v, nil
	case *Stream:
		return s, nil
	default:
		return nil, DomainError(ValidDomainStreamOrAlias, streamOrAlias, env)
	}
}
