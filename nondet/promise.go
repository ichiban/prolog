package nondet

import (
	"fmt"
)

// Promise is a delayed execution that results in (bool, error). The zero value for Promise is equivalent to Bool(false).
type Promise struct {
	delayed []func() Promise
	ok      bool
	err     error
}

// Delay delays an execution of k.
func Delay(k ...func() Promise) Promise {
	return Promise{delayed: k}
}

// Cut delays an execution of k while preventing other alternatives.
func Cut(k Promise) Promise {
	return Delay(func() Promise {
		ok, err := k.Force()
		if err != nil {
			return Error(err)
		}
		return Error(cutError{ok: ok})
	})
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

		ks = append(p.delayed, ks[1:]...)
	}
	return p.ok, p.err
}

// Opaque
func Opaque(ok bool, err error) (bool, error) {
	switch err := err.(type) {
	case nil:
		return ok, nil
	case cutError:
		return err.ok, nil
	default:
		return false, err
	}
}

type cutError struct {
	ok bool
}

func (c cutError) Error() string {
	return fmt.Sprintf("cut: %t", c.ok)
}
