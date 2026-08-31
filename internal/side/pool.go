package side

const (
	inUse = -2
)

type Pool[T comparable] struct {
	entries []poolEntry[T]
	ids     map[T]int
	free    int
}

type poolEntry[T comparable] struct {
	value T
	live  bool
	free  int
}

func (p *Pool[T]) Get(id int) T {
	return p.entries[id].value
}

func (p *Pool[T]) Add(value T) int {
	if id, ok := p.ids[value]; ok {
		return id
	}

	if p.ids == nil {
		p.ids = map[T]int{}
		p.free = -1
	}

	// Recycle an entry in free list.
	if p.free >= 0 {
		id := p.free
		p.ids[value] = id
		e := &p.entries[id]
		e.value = value
		p.free, e.free = e.free, inUse
		return id
	}

	id := len(p.entries)
	p.ids[value] = id
	p.entries = append(p.entries, poolEntry[T]{
		value: value,
		free:  inUse,
	})

	return id
}

func (p *Pool[T]) Mark(id int) {
	e := &p.entries[id]
	e.live = true
}

func (p *Pool[T]) Sweep() {
	for i := range p.entries {
		e := &p.entries[i]

		if e.live {
			e.live = false
			continue
		}

		// Already swept by an earlier Sweep. Its value has been zeroed, so
		// deleting it again would drop a live entry that holds the zero value.
		if e.free != inUse {
			continue
		}

		delete(p.ids, e.value)

		var zero T
		e.value = zero

		e.free, p.free = p.free, i
	}
}
