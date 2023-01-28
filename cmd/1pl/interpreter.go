package main

import (
	"context"
	"fmt"
	"github.com/ichiban/prolog"
	"github.com/ichiban/prolog/engine"
	"io"
)

// New creates a prolog.Interpreter with some helper predicates.
func New(r io.Reader, w io.Writer) *prolog.Interpreter {
	i := prolog.New(r, w)
	i.Register4(engine.NewAtom("skip_max_list"), engine.SkipMaxList)
	i.Register2(engine.NewAtom("go_string"), func(ctx context.Context, term, s engine.Term) *engine.Promise {
		return engine.Unify(ctx, s, engine.NewAtom(fmt.Sprintf("%#v", term)))
	})
	return i
}
