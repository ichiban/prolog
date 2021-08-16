package engine

// Promise is a delayed execution that results in (bool, error). The zero value for Promise is equivalent to Bool(false).
type Promise struct {
	delayed []func() *Promise

	cutParent *Promise

	ok  bool
	err error
}

// Delay delays an execution of k.
func Delay(k ...func() *Promise) *Promise {
	return &Promise{delayed: k}
}

// Bool returns a promise that simply returns t, nil.
func Bool(ok bool) *Promise {
	return &Promise{ok: ok}
}

// Error returns a promise that simply returns false, err.
func Error(err error) *Promise {
	return &Promise{err: err}
}

func Cut(p, parent *Promise) *Promise {
	return &Promise{
		delayed: []func() *Promise{
			func() *Promise {
				return p
			},
		},
		cutParent: parent,
	}
}

// Force enforces the delayed execution and returns the result. (i.e. trampoline)
func (p *Promise) Force() (bool, error) {
	stack := promiseStack{p}
	for len(stack) > 0 {
		p := stack.pop()

		if len(p.delayed) == 0 {
			switch {
			case p.err != nil:
				return false, p.err
			case p.ok:
				return true, nil
			default:
				continue
			}
		}

		// If cut, we eliminate other possibilities.
		if p.cutParent != nil {
			for len(stack) > 0 {
				if pop := stack.pop(); pop == p.cutParent {
					break
				}
			}
			p.cutParent = nil // we don't have to do this again when we revisit.
		}

		// Try the alternatives from left to right.
		var q *Promise
		q, p.delayed, p.delayed[0] = p.delayed[0](), p.delayed[1:], nil
		stack = append(stack, p, q)
	}
	return false, nil
}

type promiseStack []*Promise

func (s *promiseStack) pop() *Promise {
	var p *Promise
	p, *s, (*s)[len(*s)-1] = (*s)[len(*s)-1], (*s)[:len(*s)-1], &Promise{}
	return p
}
