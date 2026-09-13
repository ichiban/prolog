package prolog

import (
	"context"
	"iter"

	"github.com/ichiban/prolog/v2/internal/runtime"
	"github.com/ichiban/prolog/v2/internal/term"
)

type (
	// ClauseID identifies a stored clause within module/name/arity.
	ClauseID int64

	// Revision is a point in a [DB]'s history. Only comparisons are meaningful.
	Revision int64
)

// Record is a stored clause. Payload is opaque; the interpreter copies what it
// needs.
type Record struct {
	ID      ClauseID
	Payload []byte
}

// DB stores the clauses of dynamic predicates. Every write happens at a
// revision strictly greater than any [DB.Revision] has returned, so a Select
// under way never sees a later one.
type DB interface {
	// Revision returns the current revision.
	Revision() Revision

	// Select yields the clauses of module/name/arity as of revision -- inserted
	// at or before it, not deleted as of it -- in clause order.
	Select(ctx context.Context, module, name Atom, arity int, revision Revision) iter.Seq2[Record, error]

	// InsertBefore stores payload ahead of the other clauses of
	// module/name/arity. It may keep the slice.
	InsertBefore(ctx context.Context, module, name Atom, arity int, payload []byte) error

	// InsertAfter stores payload after the other clauses of module/name/arity.
	// It may keep the slice.
	InsertAfter(ctx context.Context, module, name Atom, arity int, payload []byte) error

	// Delete removes the clause. Select still yields it at earlier revisions.
	Delete(ctx context.Context, module, name Atom, arity int, id ClauseID) error
}

type adapter struct {
	db DB
}

func (d adapter) Revision() runtime.Revision {
	return runtime.Revision(d.db.Revision())
}

func (d adapter) Select(ctx context.Context, module, name term.Atom, arity int, revision runtime.Revision) iter.Seq2[runtime.Record, error] {
	return func(yield func(runtime.Record, error) bool) {
		for r, err := range d.db.Select(ctx, Atom(module.String()), Atom(name.String()), arity, Revision(revision)) {
			if err != nil {
				_ = yield(runtime.Record{}, err)
				return
			}
			if !yield(runtime.Record{ID: runtime.ClauseID(r.ID), Payload: r.Payload}, nil) {
				return
			}
		}
	}
}

func (d adapter) InsertBefore(ctx context.Context, module, name term.Atom, arity int, payload []byte) error {
	return d.db.InsertBefore(ctx, Atom(module.String()), Atom(name.String()), arity, payload)
}

func (d adapter) InsertAfter(ctx context.Context, module, name term.Atom, arity int, payload []byte) error {
	return d.db.InsertAfter(ctx, Atom(module.String()), Atom(name.String()), arity, payload)
}

func (d adapter) Delete(ctx context.Context, module, name term.Atom, arity int, id runtime.ClauseID) error {
	return d.db.Delete(ctx, Atom(module.String()), Atom(name.String()), arity, ClauseID(id))
}
