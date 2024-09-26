package prolog

import (
	"context"
	"github.com/ichiban/prolog/internal"
)

type Promise = internal.Promise

var Failure = internal.Bool(false)

func Continue(ctx context.Context) Promise {
	return internal.Continue(ctx)
}

func Error(err error) Promise {
	return internal.Error(err)
}

func Delay(delayed func(yield func(thunk func() Promise))) Promise {
	return internal.Delay(delayed)
}
