package prolog

import (
	"context"
	"errors"
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

type ModuleSystem struct {
	modules map[string]*Module
	system  *Module
	typeIn  *Module
}

func (ms *ModuleSystem) SetModule(name string) {
	if _, ok := ms.modules[name]; !ok {
		if ms.modules == nil {
			ms.modules = map[string]*Module{}
		}
		ms.modules[name] = &Module{}
	}
	ms.typeIn = ms.modules[name]
}

type Module struct {
	procedures map[Functor]procedureEntry
	unknown    unknownAction
	initGoals  []Term

	// Compiled code
	code      []instruction
	constants []Term

	// Internal/external expression
	operators       Operators
	charConversions func(rune) rune
	charConvEnabled bool
	doubleQuotes    doubleQuotes

	// Misc
	debug bool
}

type procedureEntry struct {
	dynamic       bool
	public        bool
	builtIn       bool
	multifile     bool
	exported      bool
	metaPredicate []Term
	importedFrom  string
	definedIn     string
	discontiguous bool
	procedure     Procedure
}

type Procedure interface {
	Call(ctx context.Context, proc *Processor, args []Term, cont Promise) Promise
}

var ErrInvalidArguments = errors.New("invalid arguments")

type ProcedureFunc0 func(ctx context.Context, proc *Processor, cont Promise) Promise

func (p ProcedureFunc0) Call(ctx context.Context, proc *Processor, args []Term, cont Promise) Promise {
	if len(args) != 0 {
		return Eager(false, ErrInvalidArguments)
	}
	return p(ctx, proc, cont)
}
