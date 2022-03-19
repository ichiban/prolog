package engine

// ListIterator is an iterator for a proper list.
type ListIterator struct {
	List Term
	Env  *Env

	current Term
	err     error
}

// Next proceeds to the next element of the list and returns true if there's such an element.
func (i *ListIterator) Next() bool {
	switch l := i.Env.Resolve(i.List).(type) {
	case Variable:
		i.err = ErrInstantiation
		return false
	case Atom:
		if l != "[]" {
			i.err = typeErrorList(l)
		}
		return false
	case *Compound:
		if l.Functor != "." || len(l.Args) != 2 {
			i.err = typeErrorList(l)
			return false
		}
		i.List = l.Args[1]
		i.current = l.Args[0]
		return true
	default:
		i.err = typeErrorList(l)
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
