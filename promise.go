package prolog

import (
	"fmt"
	"iter"
)

// Promise is a computation that results in (bool, error). The zero value for Promise is equivalent to Eager(false, nil).
type Promise struct { // TODO: Do we really need Promise? Can func() error replace its continuation use cases?
	ok  bool // TODO: Do we really need this?
	err error

	next func() (Promise, bool)
	stop func()
}

// Eager returns a promise that simply returns (ok, err).
func Eager(ok bool, err error) Promise {
	return Promise{ok: ok, err: err}
}

// Lazy returns a promise that lazily returns multiple promises.
func Lazy(seq iter.Seq[Promise]) Promise {
	next, stop := iter.Pull(seq) // TODO: If we don't Force() this promise, the goroutine leaks?
	return Promise{next: next, stop: stop}
}

// Force enforces the delayed execution and returns the result.
func (p *Promise) Force() (ok bool, err error) {
	// Memoization
	defer func() {
		*p = Promise{ok: ok, err: err}
	}()

	// A stack for nested loops. Don't forget to stop outer loops when finished.
	stack := []Promise{*p}
	defer func() {
		for i := len(stack) - 1; i >= 0; i-- {
			stack[i].stop()
		}
	}()

	for len(stack) > 0 { // Trampoline
		var p Promise
		p, stack = stack[len(stack)-1], stack[:len(stack)-1]

		if p.next != nil {
			if n, ok := p.next(); ok {
				stack = append(stack, p, n)
			}
			continue
		}

		if err := p.err; err != nil {
			return false, err
		}

		if p.ok {
			return true, nil
		}
	}

	return false, nil
}

func ensurePromise(p *Promise) {
	if r := recover(); r != nil {
		*p = Eager(false, panicError(r))
	}
}

func panicError(r interface{}) error {
	return fmt.Errorf("panic: %v", r)
}
