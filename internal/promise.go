package internal

import (
	"errors"
	"slices"
)

var errStackOverflow = errors.New("stack overflow")

// Promise is a delayed execution that results in (bool, error). The zero value for Promise is equivalent to Bool(false).
type Promise struct {
	ok  bool
	err error

	delayed   func(func(thunk func() Promise))
	recover   func(error) (Promise, bool)
	cut       bool
	cutParent bool
}

// Bool returns a promise that simply returns (ok, nil).
func Bool(ok bool) Promise {
	return Promise{ok: ok}
}

// Error returns a promise that simply returns (false, err).
func Error(err error) Promise {
	return Promise{err: err}
}

// Delay delays an execution of thunk.
func Delay(delayed func(yield func(thunk func() Promise))) Promise {
	return Promise{delayed: delayed}
}

func delayWithRecovery(delayed func(yield func(thunk func() Promise)), recover func(error) (Promise, bool)) Promise {
	return Promise{delayed: delayed, recover: recover}
}

func delayAsCutParent(delayed func(yield func(thunk func() Promise))) Promise {
	return Promise{delayed: delayed, cutParent: true}
}

func delayWithCut(delayed func(yield func(thunk func() Promise))) Promise {
	return Promise{delayed: delayed, cut: true}
}

// Force enforces the delayed execution and returns the result. (i.e. trampoline)
func (p Promise) Force(stack Stack) (bool, error) {
	if p.delayed == nil {
		return p.ok, p.err
	}

	if err := stack.push(state{thunk: func() Promise { return p }}); err != nil {
		return false, err
	}

	var (
		cutParent int
	)
	for len(stack.states) > 0 {
		s, _ := stack.pop()

		if s.thunk == nil { // Skips a recovery state.
			continue
		}

		p = s.thunk()
		if p.delayed == nil {
			if err := p.err; err != nil {
				if stack.recover(err) {
					continue
				}
				return false, err
			}

			if p.ok {
				return true, nil
			}

			continue
		}

		if p.cut {
			stack.cut(cutParent)
		}

		_ = stack.push(state{recover: p.recover})

		l := stack.len()

		if p.cutParent {
			cutParent = l - 1
		}

		var err error
		p.delayed(func(thunk func() Promise) {
			if err != nil {
				return
			}
			err = stack.push(state{thunk: thunk, cutParent: cutParent})
		})
		if err != nil {
			return false, err
		}
		slices.Reverse(stack.states[l:])
	}
	return false, nil
}

type state = struct {
	thunk     func() Promise
	recover   func(error) (Promise, bool)
	cutParent int
}

// Stack is an execution stack.
type Stack struct {
	states []state
}

// NewStack creates a stack of the given size.
func NewStack(size int) Stack {
	return Stack{states: make([]state, 0, size)}
}

func (s *Stack) len() int {
	return len(s.states)
}

func (s *Stack) push(state state) error {
	var ok bool
	s.states, ok = cappend(s.states[:], state)
	if !ok {
		return errStackOverflow
	}
	return nil
}

func (s *Stack) pop() (state, bool) {
	if len(s.states) == 0 {
		return state{}, false
	}
	var ret state
	s.states, ret = s.states[:len(s.states)-1], s.states[len(s.states)-1]
	return ret, true
}

func (s *Stack) cut(cutParent int) {
	s.states = s.states[:cutParent]
}

func (s *Stack) recover(err error) bool {
	var e state
	for len(s.states) > 0 {
		s.states, e = (s.states)[:len(s.states)-1], (s.states)[len(s.states)-1]
		r := e.recover
		if r == nil {
			continue
		}
		p, ok := r(err)
		if !ok {
			continue
		}
		s.states = append(s.states, state{thunk: func() Promise { return p }})
		return true
	}
	return false
}
