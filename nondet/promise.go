package nondet

// Promise is a delayed execution that results in (bool, error). The zero value for Promise is equivalent to Bool(false).
type Promise struct {
	delayed []func() Promise
	ok, cut bool
	err     error
}

// Delay delays an execution of k.
func Delay(k ...func() Promise) Promise {
	return Promise{delayed: k}
}

// Bool returns a promise that simply returns t, nil.
func Bool(t bool) Promise {
	return Promise{ok: t}
}

// Error returns a promise that simply returns false, err.
func Error(err error) Promise {
	return Promise{err: err}
}

func Cut(k Promise) Promise {
	l := k
	l.cut = true
	return l
}

// Force enforces the delayed execution and returns the result. (i.e. trampoline)
func (p Promise) Force() (bool, error) {
	stack := []Promise{p}
	for len(stack) > 0 {
		// pop
		var p Promise
		p, stack, stack[len(stack)-1] = stack[len(stack)-1], stack[:len(stack)-1], Promise{}

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

		// Try the alternatives from left to right.
		var q Promise
		q, p.delayed = p.delayed[0](), p.delayed[1:]
		if err := q.err; err != nil {
			return false, err
		}

		// If cut, we ignore the rest of p.
		if !q.cut {
			stack = append(stack, p)
		}

		stack = append(stack, q)
	}
	return false, nil
}
