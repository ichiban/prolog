package runtime

import (
	"context"
	"iter"

	"github.com/ichiban/prolog/v2/internal/term"
)

type (
	ClauseID int64
	Revision int64
)

type Record struct {
	ID      ClauseID
	Payload []byte
}

// DB stores the clauses of dynamic predicates. Every write happens at a
// revision strictly greater than any Revision has returned.
type DB interface {
	// Revision returns the current revision.
	Revision() Revision

	// Select yields the clauses as of revision, in clause order.
	Select(ctx context.Context, module, name term.Atom, arity int, revision Revision) iter.Seq2[Record, error]

	// InsertBefore stores clause ahead of the existing clauses.
	InsertBefore(ctx context.Context, module, name term.Atom, arity int, payload []byte) error

	// InsertAfter stores clause after the existing clauses.
	InsertAfter(ctx context.Context, module, name term.Atom, arity int, payload []byte) error

	// Delete deletes the clause.
	Delete(ctx context.Context, module, name term.Atom, arity int, id ClauseID) error
}
