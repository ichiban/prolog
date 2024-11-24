package ring

// Buffer is a ring buffer.
type Buffer[E any] struct {
	elems      []E
	start, end int
}

// NewBuffer returns a new Buffer with size.
func NewBuffer[E any](size int) *Buffer[E] {
	return &Buffer[E]{elems: make([]E, size)}
}

// Put puts an element to the buffer.
func (b *Buffer[E]) Put(elem E) {
	b.elems[b.end] = elem
	b.end++
	b.end %= len(b.elems)
}

// Get gets an element from the buffer.
func (b *Buffer[E]) Get() E {
	e := b.elems[b.start]
	b.start++
	b.start %= len(b.elems)
	return e
}

// Current returns the current element of the buffer.
func (b *Buffer[E]) Current() E {
	return b.elems[b.start]
}

// Empty returns if the buffer is empty.
func (b *Buffer[E]) Empty() bool {
	return b.start == b.end
}

// Backup undoes the previous Get operation.
func (b *Buffer[E]) Backup() {
	b.start--
	b.start %= len(b.elems)
	if b.start < 0 {
		b.start += len(b.elems)
	}
}
