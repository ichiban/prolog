package db

import (
	"context"
	"iter"

	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
	"github.com/ichiban/prolog/v2/internal/wam"
)

type memoryDBRecord struct {
	ID         int
	Head, Body syntax.Serialized
	CreatedAt  wam.LogicalTime
	DeletedAt  wam.LogicalTime
}

type MemoryDB struct {
	records []memoryDBRecord
}

func (db *MemoryDB) Insert(ctx context.Context, arena *term.Arena, record Record) error {
	record.ID = len(db.records)
	db.records = append(db.records, memoryDBRecord{
		Head:      syntax.Serialize(arena, record.Head),
		Body:      syntax.Serialize(arena, record.Body),
		CreatedAt: record.CreatedAt,
	})
	return nil
}

func (db *MemoryDB) Select(ctx context.Context, arena *term.Arena, pi term.Functor, aliveAt wam.LogicalTime) iter.Seq2[Record, error] {
	return func(yield func(Record, error) bool) {
		for _, record := range db.records {
			if record.CreatedAt > aliveAt || record.DeletedAt != 0 && record.DeletedAt <= aliveAt {
				continue
			}
			h, err := syntax.Deserialize(arena, record.Head)
			if err != nil {
				_ = yield(Record{}, err)
				return
			}
			if f, _ := arena.Functor(h, term.AllowAtom(true)); f != pi {
				continue
			}
			b, err := syntax.Deserialize(arena, record.Body)
			if err != nil {
				_ = yield(Record{}, err)
				return
			}
			if !yield(Record{
				ID:        record.ID,
				Head:      h,
				Body:      b,
				CreatedAt: record.CreatedAt,
				DeletedAt: record.DeletedAt,
			}, nil) {
				return
			}
		}
	}
}

func (db *MemoryDB) Delete(ctx context.Context, id int, deletedAt wam.LogicalTime) error {
	db.records[id].DeletedAt = deletedAt
	return nil
}
