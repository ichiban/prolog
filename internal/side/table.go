package side

import "iter"

type Table[T any] struct {
	entries []tableEntry[T]
	free    int
}

type tableEntry[T any] struct {
	value T
	live  bool
	free  int
}

func (t *Table[T]) Get(id int) T {
	return t.entries[id].value
}

// Set replaces the value of an entry that's already in the table.
func (t *Table[T]) Set(id int, value T) {
	t.entries[id].value = value
}

func (t *Table[T]) All() iter.Seq2[int, T] {
	return func(yield func(int, T) bool) {
		for i, e := range t.entries {
			// live is the mark bit, which is only set between Mark and Sweep.
			// Occupancy is what the free list says.
			if e.free != inUse {
				continue
			}

			if !yield(i, e.value) {
				return
			}
		}
	}
}

func (t *Table[T]) Add(value T) int {
	if len(t.entries) == 0 {
		t.free = -1
	}

	// Recycle an entry in free list.
	if t.free >= 0 {
		id := t.free
		e := &t.entries[id]
		e.value = value
		t.free, e.free = e.free, inUse
		return id
	}

	id := len(t.entries)
	t.entries = append(t.entries, tableEntry[T]{
		value: value,
		free:  inUse,
	})

	return id
}

func (t *Table[T]) Mark(id int) {
	e := &t.entries[id]
	e.live = true
}

func (t *Table[T]) Sweep() {
	for i := range t.entries {
		e := &t.entries[i]

		if e.live {
			e.live = false
			continue
		}

		// Already swept by an earlier Sweep. Freeing it twice would put it on
		// the free list twice and hand the same id out for two values.
		if e.free != inUse {
			continue
		}

		var zero T
		e.value = zero

		e.free, t.free = t.free, i
	}
}
