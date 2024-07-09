package internal

import (
	"errors"
	"fmt"
)

var (
	ErrTooManyBindings = errors.New("too many bindings")
)

type Variable int32

func NewVariable(env *Env) Variable {
	env.lastVariable++
	return env.lastVariable
}

func (v Variable) String() string {
	return fmt.Sprintf("_%d", v)
}

type color int8

const (
	red color = iota
	black
)

type Env struct {
	bindings     []binding
	root         bindingID
	lastVariable Variable
}

type bindingID int

// binding is a mapping from variables to terms.
type binding struct {
	// basically, this is Red-Black tree from Purely Functional Data Structures by Okazaki.
	color       color
	left, right bindingID
	keyValue
}

type keyValue struct {
	key   Variable
	value TermID
	// attributes?
}

// Lookup returns a term that the given Variable is bound to.
func (e *Env) Lookup(v Variable) (TermID, bool) {
	if len(e.bindings) == 0 {
		return 0, false
	}

	k := v
	if k%2 == 0 {
		k = -1 * k
	}

	node := e.bindings[e.root]
	for {
		switch {
		case k < node.key:
			id := node.left
			if id == 0 {
				return 0, false
			}
			node = e.bindings[id]
		case k > node.key:
			id := node.right
			if id == 0 {
				return 0, false
			}
			node = e.bindings[id]
		default:
			return node.value, true
		}
	}
}

// Bind adds a new entry to the environment.
func (e *Env) Bind(v Variable, t TermID) error {
	if len(e.bindings) == 0 {
		e.bindings, _ = cappend(e.bindings, binding{}) // Dummy record so that bindingID(0) can have a special meaning.
	}

	k := v
	if k%2 == 0 {
		k = -1 * k
	}

	id, err := e.insert(e.root, k, t)
	if err != nil {
		return err
	}

	if b := e.bindings[id]; b.color == red {
		b.color = black
		id, err = e.putBinding(b)
		if err != nil {
			return err
		}
	}

	e.root = id

	return nil
}

func (e *Env) insert(id bindingID, k Variable, v TermID) (bindingID, error) {
	if id == 0 {
		return e.putBinding(binding{color: red, keyValue: keyValue{key: k, value: v}})
	}
	switch b := e.bindings[id]; {
	case k < b.key:
		id, err := e.putBinding(b)
		if err != nil {
			return 0, err
		}
		e.bindings[id].left, err = e.insert(b.left, k, v)
		if err != nil {
			return 0, err
		}
		return e.balance(id)
	case k > b.key:
		id, err := e.putBinding(b)
		if err != nil {
			return 0, err
		}
		e.bindings[id].right, err = e.insert(b.right, k, v)
		if err != nil {
			return 0, err
		}
		return e.balance(id)
	default:
		id, err := e.putBinding(b)
		if err != nil {
			return 0, err
		}
		e.bindings[id].value = v
		return id, nil
	}
}

func (e *Env) balance(id bindingID) (bindingID, error) {
	var (
		a, b, c, d bindingID
		x, y, z    keyValue
	)
	switch node := e.bindings[id]; {
	case node.left != 0 && e.bindings[node.left].color == red:
		switch l := e.bindings[node.left]; {
		case l.left != 0 && e.bindings[l.left].color == red:
			ll := e.bindings[l.left]
			a = ll.left
			b = ll.right
			c = l.right
			d = node.right
			x = ll.keyValue
			y = l.keyValue
			z = node.keyValue
		case l.right != 0 && e.bindings[l.right].color == red:
			lr := e.bindings[l.right]
			a = l.left
			b = lr.left
			c = lr.right
			d = node.right
			x = l.keyValue
			y = lr.keyValue
			z = node.keyValue
		default:
			return id, nil
		}
	case node.right != 0 && e.bindings[node.right].color == red:
		switch r := e.bindings[node.right]; {
		case r.left != 0 && e.bindings[r.left].color == red:
			rl := e.bindings[r.left]
			a = node.left
			b = rl.left
			c = rl.right
			d = r.right
			x = node.keyValue
			y = rl.keyValue
			z = r.keyValue
		case r.right != 0 && e.bindings[r.right].color == red:
			rr := e.bindings[r.right]
			a = node.left
			b = r.left
			c = rr.left
			d = rr.right
			x = node.keyValue
			y = r.keyValue
			z = rr.keyValue
		default:
			return id, nil
		}
	default:
		return id, nil
	}
	l, err := e.putBinding(binding{color: black, left: a, right: b, keyValue: x})
	if err != nil {
		return 0, err
	}
	r, err := e.putBinding(binding{color: black, left: c, right: d, keyValue: z})
	if err != nil {
		return 0, err
	}
	return e.putBinding(binding{color: red, left: l, right: r, keyValue: y})
}

func (e *Env) putBinding(b binding) (bindingID, error) {
	var ok bool
	e.bindings, ok = cappend(e.bindings, b)
	if !ok {
		return 0, ErrTooManyBindings
	}
	return bindingID(len(e.bindings) - 1), nil
}
