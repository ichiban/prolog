package internal

import "errors"

var (
	errInvalidList = errors.New("invalid list")
)

// ListIterator is an iterator for a list.
type ListIterator struct {
	TermPool     *TermPool
	List         Term
	AllowPartial bool
	AllowCycle   bool

	current Term
	err     error

	// Variables for Brent's cycle detection algorithm
	tortoise, hare Term
	power, lam     int
}

// Next proceeds to the next element of the list and returns true if there's such an element.
func (i *ListIterator) Next() bool {
	if i.hare == 0 {
		i.hare = i.TermPool.Resolve(i.List)
	}
	if i.power == 0 {
		i.power = 1
	}
	if i.lam == 0 {
		i.lam = 1
	}

	if i.tortoise == i.hare && !i.AllowCycle { // Detected a cycle.
		i.err = errInvalidList
		return false
	}

	if i.power == i.lam {
		i.tortoise = i.hare
		i.power *= 2
		i.lam = 0
	}

	if _, ok := i.TermPool.Variable(i.hare); ok {
		if !i.AllowPartial {
			i.err = ErrInstantiation
		}
		return false
	}

	if a, ok := i.TermPool.Atom(i.hare); ok {
		if a != NewAtom("[]") {
			i.err = errInvalidList
		}
		return false
	}

	f, arg, ok := i.TermPool.Compound(i.hare)
	if !ok || f != (Functor{Name: Atom('.'), Arity: 2}) {
		i.err = errInvalidList
		return false
	}

	var err error
	i.current, err = arg(0)
	if err != nil {
		i.err = err
		return false
	}

	i.hare, err = arg(1)
	if err != nil {
		i.err = err
		return false
	}
	i.hare = i.TermPool.Resolve(i.hare)

	i.lam++
	return true
}

// Current returns the current element.
func (i *ListIterator) Current() Term {
	return i.current
}

// Err returns an error.
func (i *ListIterator) Err() error {
	return i.err
}

// Suffix returns the rest of the list.
func (i *ListIterator) Suffix() Term {
	if i.hare == 0 {
		return i.List
	}
	return i.hare
}

// seqIterator is an iterator for a sequence.
type seqIterator struct {
	TermPool *TermPool
	Seq      Term

	current Term
}

// Next proceeds to the next element of the sequence and returns true if there's such an element.
func (i *seqIterator) Next() bool {
	id := i.TermPool.Resolve(i.Seq)

	if _, ok := i.TermPool.Variable(id); ok {
		return false
	}

	if f, arg, ok := i.TermPool.Compound(id); ok && f == (Functor{Name: Atom(','), Arity: 2}) {
		i.current, _ = arg(0)
		i.Seq, _ = arg(1)
		return true
	}

	i.current, i.Seq = id, 0
	return true
}

// Current returns the current element.
func (i *seqIterator) Current() Term {
	return i.current
}

// altIterator is an iterator for alternatives.
type altIterator struct {
	TermPool *TermPool
	Alt      Term

	current Term
}

// Next proceeds to the next element of the alternatives and returns true if there's such an element.
func (i *altIterator) Next() bool {
	id := i.TermPool.Resolve(i.Alt)

	if _, ok := i.TermPool.Variable(id); ok {
		return false
	}

	if f, arg, ok := i.TermPool.Compound(id); ok && f == (Functor{Name: Atom(';'), Arity: 2}) {
		l, _ := arg(0)
		r, _ := arg(1)

		// if-then-else construct
		l = i.TermPool.Resolve(l)
		if f, _, ok := i.TermPool.Compound(l); ok && f == (Functor{Name: NewAtom("->"), Arity: 2}) {
			i.current, i.Alt = id, 0
			return true
		}

		i.current, i.Alt = l, r
		return true
	}

	i.current, i.Alt = id, 0
	return true
}

// Current returns the current element.
func (i *altIterator) Current() Term {
	return i.current
}

// anyIterator is an iterator for a list or a sequence.
type anyIterator struct {
	TermPool *TermPool
	Any      Term

	backend interface {
		Next() bool
		Current() Term
	}
}

// Next proceeds to the next element and returns true if there's such an element.
func (i *anyIterator) Next() bool {
	if i.backend == nil {
		id := i.TermPool.Resolve(i.Any)
		if f, _, ok := i.TermPool.Compound(id); ok && f == (Functor{Name: Atom('.'), Arity: 2}) {
			i.backend = &ListIterator{TermPool: i.TermPool, List: i.Any}
		} else {
			i.backend = &seqIterator{TermPool: i.TermPool, Seq: i.Any}
		}
	}

	return i.backend.Next()
}

// Current returns the current element.
func (i *anyIterator) Current() Term {
	return i.backend.Current()
}

// Err returns an error.
func (i *anyIterator) Err() error {
	b, ok := i.backend.(interface{ Err() error })
	if !ok {
		return nil
	}
	return b.Err()
}
