package rbtree

import (
	"cmp"
	"slices"
)

type color int8

const (
	red color = iota
	black
)

// Map is a map backed by a binary search tree.
// Each tree node is stored in an internal slice.
// This implementation is based on red-black tree from Purely Functional Data Structures by Okasaki.
type Map[K cmp.Ordered, V any] struct {
	nodes []Node[K, V]
	root  int
}

// Len returns the node slice's length.
func (t *Map[E, K]) Len() int {
	return len(t.nodes)
}

// Cap returns the node slice's capacity.
func (t *Map[E, K]) Cap() int {
	return cap(t.nodes)
}

// Grow increases the node slice's capacity.
func (t *Map[E, K]) Grow(n int) {
	t.nodes = slices.Grow(t.nodes, n)
}

// Set stores a pair of key and value.
func (t *Map[K, V]) Set(key K, value V) {
	elem := elem[K, V]{key: key, value: value}
	id, _ := insert(t, t.root, elem, true)

	if n := t.nodes[id]; n.color == red {
		id, _ = addNode(t, Node[K, V]{
			color: black,
			left:  n.left,
			elem:  n.elem,
			right: n.right,
		}, true)
	}

	t.root = id
}

// SafeSet stores a pair of key and value only if it doesn't cause a growth of the internal Node slice.
// It returns true if it successfully stored the pair. Otherwise, it returns false.
func (t *Map[K, V]) SafeSet(key K, value V) (ok bool) {
	snapshot := *t
	defer func() {
		if !ok {
			*t = snapshot
		}
	}()

	elem := elem[K, V]{key: key, value: value}
	id, ok := insert(t, t.root, elem, false)
	if !ok {
		return false
	}

	if n := t.nodes[id]; n.color == red {
		id, ok = addNode(t, Node[K, V]{
			color: black,
			left:  n.left,
			elem:  n.elem,
			right: n.right,
		}, false)
		if !ok {
			return false
		}
	}

	t.root = id
	return true
}

// Get returns the associated value for a key.
func (t *Map[K, V]) Get(key K) (V, bool) {
	var zero V

	if len(t.nodes) == 0 {
		return zero, false
	}

	id := t.root
	for {
		if id < 0 {
			return zero, false
		}

		var (
			n = t.nodes[id]
			e = n.elem
		)
		switch {
		case key < e.key:
			id = n.left
		case key > e.key:
			id = n.right
		default:
			return e.value, true
		}
	}
}

// Node is a node of binary search tree.
// It resides in Map as an element of the Node slice.
// Exposed just for size reference.
type Node[K cmp.Ordered, V any] struct {
	color       color
	left, right int
	elem        elem[K, V]
}

type elem[K cmp.Ordered, V any] struct {
	key   K
	value V
}

func insert[K cmp.Ordered, V any](tree *Map[K, V], id int, elem elem[K, V], growth bool) (int, bool) {
	if id <= 0 {
		return addNode(tree, Node[K, V]{
			color: red,
			left:  -1,
			elem:  elem,
			right: -1,
		}, growth)
	}
	switch b := tree.nodes[id]; {
	case elem.key < b.elem.key:
		l, ok := insert(tree, b.left, elem, growth)
		if !ok {
			return 0, false
		}
		id, ok := addNode(tree, Node[K, V]{
			color: b.color,
			left:  l,
			elem:  b.elem,
			right: b.right,
		}, growth)
		if !ok {
			return 0, false
		}
		return balance(tree, id, growth)
	case elem.key > b.elem.key:
		r, ok := insert(tree, b.right, elem, growth)
		if !ok {
			return 0, false
		}
		id, ok := addNode(tree, Node[K, V]{
			color: b.color,
			left:  b.left,
			elem:  b.elem,
			right: r,
		}, growth)
		if !ok {
			return 0, false
		}
		return balance(tree, id, growth)
	default:
		return addNode(tree, Node[K, V]{
			color: b.color,
			left:  b.left,
			elem:  elem,
			right: b.right,
		}, growth)
	}
}

func balance[K cmp.Ordered, V any](tree *Map[K, V], id int, growth bool) (int, bool) {
	var (
		a, b, c, d int
		x, y, z    elem[K, V]
	)
	switch node := tree.nodes[id]; {
	case node.left >= 0 && tree.nodes[node.left].color == red:
		switch l := tree.nodes[node.left]; {
		case l.left >= 0 && tree.nodes[l.left].color == red:
			ll := tree.nodes[l.left]
			a = ll.left
			b = ll.right
			c = l.right
			d = node.right
			x = ll.elem
			y = l.elem
			z = node.elem
		case l.right >= 0 && tree.nodes[l.right].color == red:
			lr := tree.nodes[l.right]
			a = l.left
			b = lr.left
			c = lr.right
			d = node.right
			x = l.elem
			y = lr.elem
			z = node.elem
		default:
			return id, true
		}
	case node.right >= 0 && tree.nodes[node.right].color == red:
		switch r := tree.nodes[node.right]; {
		case r.left >= 0 && tree.nodes[r.left].color == red:
			rl := tree.nodes[r.left]
			a = node.left
			b = rl.left
			c = rl.right
			d = r.right
			x = node.elem
			y = rl.elem
			z = r.elem
		case r.right >= 0 && tree.nodes[r.right].color == red:
			rr := tree.nodes[r.right]
			a = node.left
			b = r.left
			c = rr.left
			d = rr.right
			x = node.elem
			y = r.elem
			z = rr.elem
		default:
			return id, true
		}
	default:
		return id, true
	}
	l, ok := addNode(tree, Node[K, V]{
		color: black,
		left:  a,
		elem:  x,
		right: b,
	}, growth)
	if !ok {
		return 0, false
	}
	r, ok := addNode(tree, Node[K, V]{
		color: black,
		left:  c,
		elem:  z,
		right: d,
	}, growth)
	if !ok {
		return 0, false
	}
	return addNode(tree, Node[K, V]{
		color: red,
		left:  l,
		elem:  y,
		right: r,
	}, growth)
}

func addNode[K cmp.Ordered, V any](tree *Map[K, V], node Node[K, V], growth bool) (int, bool) {
	if !growth && len(tree.nodes)+1 >= cap(tree.nodes) {
		return 0, false
	}
	tree.nodes = append(tree.nodes, node)
	return len(tree.nodes) - 1, true
}
