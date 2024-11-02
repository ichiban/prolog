package rbtree

import (
	"cmp"
	"errors"
	"iter"
)

var ErrTooManyNodes = errors.New("too many nodes")

type color int8

const (
	red color = iota
	black
)

// Map is a map backed by a binary search tree.
// Each tree node is stored in an internal slice.
// This implementation is based on red-black tree from Purely Functional Data Structures by Okasaki.
type Map[K cmp.Ordered, V any] struct {
	Nodes []Node[K, V]
	root  int
}

// Set stores a pair of key and value.
func (t *Map[K, V]) Set(key K, value V) (err error) {
	snapshot := *t
	defer func() {
		if err != nil {
			*t = snapshot
		}
	}()

	elem := elem[K, V]{key: key, value: value}
	id, err := insert(t, t.root, elem)
	if err != nil {
		return err
	}

	if n := t.Nodes[id]; n.color == red {
		id, err = addNode(t, Node[K, V]{
			color: black,
			left:  n.left,
			elem:  n.elem,
			right: n.right,
		})
		if err != nil {
			return err
		}
	}

	t.root = id
	return nil
}

// Get returns the associated value for a key.
func (t *Map[K, V]) Get(key K) (V, bool) {
	var zero V

	if t == nil || len(t.Nodes) == 0 {
		return zero, false
	}

	id := t.root
	for {
		if id < 0 {
			return zero, false
		}

		var (
			n = t.Nodes[id]
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

func (t *Map[K, V]) All() iter.Seq2[K, V] {
	return func(yield func(K, V) bool) {
		if t == nil || len(t.Nodes) == 0 {
			return
		}

		var (
			stack = []int{t.root}
			id    int
		)
		for len(stack) > 0 {
			stack, id = stack[:len(stack)-1], stack[len(stack)-1]

			var (
				n = t.Nodes[id]
				e = n.elem
			)
			if n.right >= 0 {
				stack = append(stack, n.right)
			}
			if n.left >= 0 {
				stack = append(stack, n.left)
			}
			if !yield(e.key, e.value) {
				return
			}
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

func insert[K cmp.Ordered, V any](tree *Map[K, V], id int, elem elem[K, V]) (int, error) {
	if id <= 0 {
		return addNode(tree, Node[K, V]{
			color: red,
			left:  -1,
			elem:  elem,
			right: -1,
		})
	}
	switch b := tree.Nodes[id]; {
	case elem.key < b.elem.key:
		l, err := insert(tree, b.left, elem)
		if err != nil {
			return 0, err
		}
		id, err := addNode(tree, Node[K, V]{
			color: b.color,
			left:  l,
			elem:  b.elem,
			right: b.right,
		})
		if err != nil {
			return 0, err
		}
		return balance(tree, id)
	case elem.key > b.elem.key:
		r, err := insert(tree, b.right, elem)
		if err != nil {
			return 0, err
		}
		id, err := addNode(tree, Node[K, V]{
			color: b.color,
			left:  b.left,
			elem:  b.elem,
			right: r,
		})
		if err != nil {
			return 0, err
		}
		return balance(tree, id)
	default:
		return addNode(tree, Node[K, V]{
			color: b.color,
			left:  b.left,
			elem:  elem,
			right: b.right,
		})
	}
}

func balance[K cmp.Ordered, V any](tree *Map[K, V], id int) (int, error) {
	var (
		a, b, c, d int
		x, y, z    elem[K, V]
	)
	switch node := tree.Nodes[id]; {
	case node.left >= 0 && tree.Nodes[node.left].color == red:
		switch l := tree.Nodes[node.left]; {
		case l.left >= 0 && tree.Nodes[l.left].color == red:
			ll := tree.Nodes[l.left]
			a = ll.left
			b = ll.right
			c = l.right
			d = node.right
			x = ll.elem
			y = l.elem
			z = node.elem
		case l.right >= 0 && tree.Nodes[l.right].color == red:
			lr := tree.Nodes[l.right]
			a = l.left
			b = lr.left
			c = lr.right
			d = node.right
			x = l.elem
			y = lr.elem
			z = node.elem
		default:
			return id, nil
		}
	case node.right >= 0 && tree.Nodes[node.right].color == red:
		switch r := tree.Nodes[node.right]; {
		case r.left >= 0 && tree.Nodes[r.left].color == red:
			rl := tree.Nodes[r.left]
			a = node.left
			b = rl.left
			c = rl.right
			d = r.right
			x = node.elem
			y = rl.elem
			z = r.elem
		case r.right >= 0 && tree.Nodes[r.right].color == red:
			rr := tree.Nodes[r.right]
			a = node.left
			b = r.left
			c = rr.left
			d = rr.right
			x = node.elem
			y = r.elem
			z = rr.elem
		default:
			return id, nil
		}
	default:
		return id, nil
	}
	l, err := addNode(tree, Node[K, V]{
		color: black,
		left:  a,
		elem:  x,
		right: b,
	})
	if err != nil {
		return 0, err
	}
	r, err := addNode(tree, Node[K, V]{
		color: black,
		left:  c,
		elem:  z,
		right: d,
	})
	if err != nil {
		return 0, err
	}
	return addNode(tree, Node[K, V]{
		color: red,
		left:  l,
		elem:  y,
		right: r,
	})
}

func addNode[K cmp.Ordered, V any](tree *Map[K, V], node Node[K, V]) (int, error) {
	if len(tree.Nodes)+1 > cap(tree.Nodes) {
		return 0, ErrTooManyNodes
	}
	tree.Nodes = append(tree.Nodes, node)
	return len(tree.Nodes) - 1, nil
}
