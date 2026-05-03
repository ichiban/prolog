package builtin

import (
	"context"

	"github.com/ichiban/prolog/v2/internal/term"
)

type Registry struct {
	entries []Entry
	index   map[term.Functor]int
}

type Entry struct {
	PI     term.Functor
	InHead bool
	Func   func(ctx context.Context, args []term.Handle) error // FIXME:
}
