package prolog

import (
	"context"
	"errors"
	"iter"
)

type unknownAction int

const (
	unknownError unknownAction = iota
	unknownFail
	unknownWarning
)

var unknownActionNames = [...]string{
	unknownError:   "error",
	unknownFail:    "fail",
	unknownWarning: "warning",
}

func (u unknownAction) String() string {
	return unknownActionNames[u]
}

type Module struct {
	name       Atom
	procedures map[Functor]procedureEntry
	unknown    unknownAction
	initGoals  []Term

	// Compiled code
	code      []instruction
	constants []Term
	builtins  []func(ctx context.Context, e *Engine) iter.Seq[Success]

	// Internal/external expression
	operators       Operators
	charConversions func(rune) rune
	charConvEnabled bool
	doubleQuotes    doubleQuotes

	// Misc
	debug bool
}

type procedureEntry struct {
	module        *Module
	dynamic       bool
	public        bool
	builtIn       bool
	multifile     bool
	exported      bool
	metaPredicate []Term
	importedFrom  string
	definedIn     string
	discontiguous bool
	offset        int
}

type Procedure interface {
	Call(ctx context.Context, engine *Engine, args []Term) iter.Seq[error]
}

var ErrInvalidArguments = errors.New("invalid arguments")

type ProcedureFunc0 func(ctx context.Context, engine *Engine) iter.Seq[error]

func (p ProcedureFunc0) Call(ctx context.Context, engine *Engine, args []Term) iter.Seq[error] {
	if len(args) != 0 {
		return func(yield func(error) bool) {
			_ = yield(ErrInvalidArguments)
		}
	}
	return p(ctx, engine)
}

type Success struct {
	Last bool
}
