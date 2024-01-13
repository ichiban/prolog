package engine

import (
	"context"
	"fmt"
)

var (
	truePromise  = &Promise{ok: true}
	falsePromise = &Promise{ok: false}
)

// Promise is a delayed execution that results in (bool, error). The zero value for Promise is equivalent to Bool(false).
type Promise struct {
	// delayed execution with multiple choices
	delayed []func(context.Context) *Promise

	// final result
	ok  bool
	err error

	// execution control
	cutParent *Promise
	repeat    bool
	recover   func(error) *Promise
}

// Delay delays an execution of k.
func Delay(k ...func(context.Context) *Promise) *Promise {
	return &Promise{delayed: k}
}

// Bool returns a promise that simply returns (ok, nil).
func Bool(ok bool) *Promise {
	if ok {
		return truePromise
	}
	return falsePromise
}

// Error returns a promise that simply returns (false, err).
func Error(err error) *Promise {
	return &Promise{err: err}
}

var dummyCutParent Promise

// cut returns a promise that once the execution reaches it, it eliminates other possible choices.
func cut(parent *Promise, k func(context.Context) *Promise) *Promise {
	if parent == nil {
		parent = &dummyCutParent
	}
	return &Promise{
		delayed:   []func(context.Context) *Promise{k},
		cutParent: parent,
	}
}

// repeat returns a promise that repeats k.
func repeat(k func(context.Context) *Promise) *Promise {
	return &Promise{
		delayed: []func(context.Context) *Promise{k},
		repeat:  true,
	}
}

// catch returns a promise with a recovering function.
// Once a promise results in error, the error goes through ancestor promises looking for a recovering function that
// returns a non-nil promise to continue on.
func catch(recover func(error) *Promise, k func(context.Context) *Promise) *Promise {
	return &Promise{
		delayed: []func(context.Context) *Promise{k},
		recover: recover,
	}
}

// Force enforces the delayed execution and returns the result. (i.e. trampoline)
func (p *Promise) Force(ctx context.Context) (ok bool, err error) {
	stack := promiseStack{p}
	for len(stack) > 0 {
		select {
		case <-ctx.Done():
			return false, ctx.Err()
		default:
			p := stack.pop()

			if len(p.delayed) == 0 {
				switch {
				case p.err != nil:
					if err := stack.recover(p.err); err != nil {
						return false, err
					}
					continue
				case p.ok:
					return true, nil
				default:
					continue
				}
			}

			// If cut, we eliminate other possibilities.
			if p.cutParent != nil {
				stack.popUntil(p.cutParent)
				p.cutParent = nil // we don't have to do this again when we revisit.
			}

			// Try the child promises from left to right.
			q := p.child(ctx)
			stack = append(stack, p, q)
		}
	}
	return false, nil
}

func (p *Promise) child(ctx context.Context) (promise *Promise) {
	defer ensurePromise(&promise)
	defer func() {
		if !p.repeat {
			p.delayed, p.delayed[0] = p.delayed[1:], nil
		}
	}()
	return p.delayed[0](ctx)
}

func ensurePromise(p **Promise) {
	if r := recover(); r != nil {
		*p = Error(panicError(r))
	}
}

func panicError(r interface{}) error {
	return fmt.Errorf("panic: %v", r)
}

type errorWithCaller struct {
	file string
	line int
	err  error
}

func (e errorWithCaller) Error() string {
	return fmt.Sprintf("%s (%s:%d)", e.err.Error(), e.file, e.line)
}

func (e errorWithCaller) Unwrap() error {
	return e.err
}

type promiseStack []*Promise

func (s *promiseStack) pop() *Promise {
	var p *Promise
	p, *s, (*s)[len(*s)-1] = (*s)[len(*s)-1], (*s)[:len(*s)-1], nil
	return p
}

func (s *promiseStack) popUntil(p *Promise) {
	for len(*s) > 0 {
		if pop := s.pop(); pop == p {
			break
		}
	}
}

func (s *promiseStack) recover(err error) error {
	// look for an ancestor promise with a recovering function that is applicable to the error.
	for len(*s) > 0 {
		pop := s.pop()
		if pop.recover == nil {
			continue
		}
		if q := pop.recover(err); q != nil {
			*s = append(*s, q)
			return nil
		}
	}

	// went through all the ancestor promises and still got the unhandled error.
	return err
}
