package bit

import "math/bits"

// Set is a bitset.
type Set struct {
	bits []uint64

	// sums is the number of elements below the i-th word of bits.
	sums  []uint32
	stale bool
}

// NewSet creates a bitset that can contain items in [0, n).
func NewSet(n int) *Set {
	words := n/64 + 1
	return &Set{
		bits: make([]uint64, words),
		sums: make([]uint32, words),
	}
}

// Add adds an element.
func (s *Set) Add(n int) {
	i, j := n/64, n%64
	s.bits[i] |= 1 << uint64(j)
	s.stale = true
}

// Exists checks if the given element is in the set.
func (s *Set) Exists(n int) bool {
	i, j := n/64, n%64
	return s.bits[i]&(1<<uint64(j)) != 0
}

// Rank counts the number of elements up to and including the given number.
func (s *Set) Rank(n int) int {
	if s.stale {
		s.summarize()
	}
	i, j := n/64, n%64
	// Shift out the elements above the n-th.
	return int(s.sums[i]) + bits.OnesCount64(s.bits[i]<<(63-j))
}

// summarize recomputes the per-word prefix sums Rank reads.
func (s *Set) summarize() {
	var c uint32
	for i, w := range s.bits {
		s.sums[i] = c
		c += uint32(bits.OnesCount64(w))
	}
	s.stale = false
}

// Clear clears the bitset.
func (s *Set) Clear() {
	clear(s.bits)
	clear(s.sums)
	s.stale = false
}
