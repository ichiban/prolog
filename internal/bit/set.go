package bit

import "math/bits"

// Set is a bitset.
type Set struct {
	bits []uint64
}

// NewSet creates a bitset that can contain items in [0, n).
func NewSet(n int) *Set {
	return &Set{
		bits: make([]uint64, n/64+1),
	}
}

// Add adds an element.
func (s *Set) Add(n int) {
	i, j := n/64, n%64
	s.bits[i] |= 1 << uint64(j)
}

// Exists checks if the given element is in the set.
func (s *Set) Exists(n int) bool {
	i, j := n/64, n%64
	return s.bits[i]&(1<<uint64(j)) != 0
}

// Rank counts the number of elements up to and including the given number.
func (s *Set) Rank(n int) int {
	i, j := n/64, n%64
	var c int
	for k := range i {
		c += bits.OnesCount64(s.bits[k]) // TODO: Memoize?
	}
	c += bits.OnesCount64(s.bits[i] << (63 - j)) // Shift out the elements above the n-th.
	return c
}

func (s *Set) Clear() {
	for i := range s.bits {
		s.bits[i] = 0
	}
}
