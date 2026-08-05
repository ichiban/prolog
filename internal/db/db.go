package db

import (
	"context"
	"iter"

	"github.com/ichiban/prolog/v2/internal/term"
	"github.com/ichiban/prolog/v2/internal/wam"
)

type Record struct {
	ID         int
	Head, Body term.Handle
	CreatedAt  wam.LogicalTime
	DeletedAt  wam.LogicalTime
}

// DB stores dynamic predicates.
type DB interface {
	Insert(ctx context.Context, arena *term.Arena, record Record) error
	Select(ctx context.Context, arena *term.Arena, pi term.Functor, aliveAt wam.LogicalTime) iter.Seq2[Record, error]
	Delete(ctx context.Context, id int, deletedAt wam.LogicalTime) error
}
