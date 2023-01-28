package engine

import "context"

// ListIterator is an iterator for a list.
type ListIterator struct {
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
func (i *ListIterator) Next(ctx context.Context) bool {
	if i.hare == nil {
		i.hare = Resolve(ctx, i.List)
	}
	if i.power == 0 {
		i.power = 1
	}
	if i.lam == 0 {
		i.lam = 1
	}

	if id(i.tortoise) == id(i.hare) && !i.AllowCycle { // Detected a cycle.
		i.err = typeError(ctx, validTypeList, i.List)
		return false
	}

	if i.power == i.lam {
		i.tortoise = i.hare
		i.power *= 2
		i.lam = 0
	}

	switch l := i.hare.(type) {
	case Variable:
		if !i.AllowPartial {
			i.err = InstantiationError(ctx)
		}
		return false
	case Atom:
		if l != atomEmptyList {
			i.err = typeError(ctx, validTypeList, i.List)
		}
		return false
	case Compound:
		if l.Functor() != atomDot || l.Arity() != 2 {
			i.err = typeError(ctx, validTypeList, i.List)
			return false
		}

		i.current, i.hare = l.Arg(0), Resolve(ctx, l.Arg(1))
		i.lam++
		return true
	default:
		i.err = typeError(ctx, validTypeList, i.List)
		return false
	}
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
	if i.hare == nil {
		return i.List
	}
	return i.hare
}

// seqIterator is an iterator for a sequence.
type seqIterator struct {
	Seq Term

	current Term
}

// Next proceeds to the next element of the sequence and returns true if there's such an element.
func (i *seqIterator) Next(ctx context.Context) bool {
	switch s := Resolve(ctx, i.Seq).(type) {
	case nil:
		return false
	case Compound:
		if s.Functor() != atomComma || s.Arity() != 2 {
			i.current = s
			i.Seq = nil
			return true
		}
		i.Seq = s.Arg(1)
		i.current = s.Arg(0)
		return true
	default:
		i.current = s
		i.Seq = nil
		return true
	}
}

// Current returns the current element.
func (i *seqIterator) Current() Term {
	return i.current
}

// altIterator is an iterator for alternatives.
type altIterator struct {
	Alt Term

	current Term
}

// Next proceeds to the next element of the alternatives and returns true if there's such an element.
func (i *altIterator) Next(ctx context.Context) bool {
	switch a := Resolve(ctx, i.Alt).(type) {
	case nil:
		return false
	case Compound:
		if a.Functor() != atomSemiColon || a.Arity() != 2 {
			i.current = a
			i.Alt = nil
			return true
		}

		// if-then-else construct
		if c, ok := Resolve(ctx, a.Arg(0)).(Compound); ok && c.Functor() == atomThen && c.Arity() == 2 {
			i.current = a
			i.Alt = nil
			return true
		}

		i.Alt = a.Arg(1)
		i.current = a.Arg(0)
		return true
	default:
		i.current = a
		i.Alt = nil
		return true
	}
}

// Current returns the current element.
func (i *altIterator) Current() Term {
	return i.current
}

// anyIterator is an iterator for a list or a sequence.
type anyIterator struct {
	Any Term

	backend interface {
		Next(context.Context) bool
		Current() Term
	}
}

// Next proceeds to the next element and returns true if there's such an element.
func (i *anyIterator) Next(ctx context.Context) bool {
	if i.backend == nil {
		if a, ok := Resolve(ctx, i.Any).(Compound); ok && a.Functor() == atomDot && a.Arity() == 2 {
			i.backend = &ListIterator{List: i.Any}
		} else {
			i.backend = &seqIterator{Seq: i.Any}
		}
	}

	return i.backend.Next(ctx)
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
