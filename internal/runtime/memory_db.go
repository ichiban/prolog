package runtime

import (
	"context"
	"errors"
	"iter"
	"slices"

	"github.com/ichiban/prolog/v2/internal/term"
)

type memoryDBRecord struct {
	ID      ClauseID
	Module  term.Atom
	Name    term.Atom
	Arity   int
	Payload []byte

	// CreatedAt is the revision the clause was inserted at. DeletedAt is the
	// revision it was deleted at, or 0 while it is still there: no write ever
	// happens at revision 0, so the zero value reads as "not deleted".
	CreatedAt Revision
	DeletedAt Revision
}

func (r memoryDBRecord) isOf(module, name term.Atom, arity int) bool {
	return r.Module == module && r.Name == name && r.Arity == arity
}

// MemoryDB is the default [DB]. It keeps every clause ever asserted, marking
// the retracted ones deleted rather than dropping them, so a Select that is
// still running sees the clauses that existed when it started.
type MemoryDB struct {
	revision Revision
	nextID   ClauseID
	records  []memoryDBRecord
}

func (db *MemoryDB) Revision() Revision {
	return db.revision
}

func (db *MemoryDB) Select(ctx context.Context, module, name term.Atom, arity int, revision Revision) iter.Seq2[Record, error] {
	return func(yield func(Record, error) bool) {
		for _, r := range db.records {
			if r.CreatedAt > revision || r.DeletedAt != 0 && r.DeletedAt <= revision {
				continue
			}
			if !r.isOf(module, name, arity) {
				continue
			}
			if !yield(Record{ID: r.ID, Payload: r.Payload}, nil) {
				return
			}
		}
	}
}

func (db *MemoryDB) InsertBefore(ctx context.Context, module, name term.Atom, arity int, payload []byte) error {
	r := db.record(module, name, arity, payload)
	// Ahead of the existing clauses of this predicate, which is where the
	// first one of them sits.
	i := slices.IndexFunc(db.records, func(o memoryDBRecord) bool {
		return o.isOf(module, name, arity) && o.DeletedAt == 0
	})
	if i < 0 {
		i = len(db.records)
	}
	db.records = slices.Insert(db.records, i, r)
	return nil
}

func (db *MemoryDB) InsertAfter(ctx context.Context, module, name term.Atom, arity int, payload []byte) error {
	r := db.record(module, name, arity, payload)
	db.records = append(db.records, r)
	return nil
}

func (db *MemoryDB) Delete(ctx context.Context, module, name term.Atom, arity int, id ClauseID) error {
	i := slices.IndexFunc(db.records, func(o memoryDBRecord) bool {
		return o.ID == id
	})
	if i < 0 {
		return errors.New("delete: no such clause")
	}
	if db.records[i].DeletedAt != 0 {
		return nil
	}
	db.revision++
	db.records[i].DeletedAt = db.revision
	return nil
}

func (db *MemoryDB) record(module, name term.Atom, arity int, payload []byte) memoryDBRecord {
	db.revision++
	db.nextID++
	return memoryDBRecord{
		ID:        db.nextID,
		Module:    module,
		Name:      name,
		Arity:     arity,
		Payload:   slices.Clone(payload),
		CreatedAt: db.revision,
	}
}
