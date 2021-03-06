package prolog

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

// Cut delays an execution of k while preventing other alternatives.
func Cut(k func() Promise) Promise {
	return Promise{delayed: []func() Promise{k}, cut: true}
}

// Bool returns a promise that simply returns t, nil.
func Bool(t bool) Promise {
	return Promise{ok: t}
}

// Error returns a promise that simply returns false, err.
func Error(err error) Promise {
	return Promise{err: err}
}

// Force enforces the delayed execution and returns the result. (i.e. trampoline)
func (p Promise) Force() (bool, error) {
	ks := p.delayed
	for len(ks) > 0 {
		// Try the leftmost alternative first.
		p := ks[0]()
		if err := p.err; err != nil {
			return false, err
		}
		if p.ok {
			return true, nil
		}

		// If cut, other alternatives are ignored.
		if p.cut {
			ks = p.delayed
			continue
		}

		ks = append(p.delayed, ks[1:]...)
	}
	return p.ok, p.err
}
