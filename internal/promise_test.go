package internal

import (
	"errors"
	"testing"
)

func TestPromise_Force(t *testing.T) {
	err := errors.New("some error")

	tests := []struct {
		title string
		p     Promise
		ok    bool
		err   error
	}{
		{title: "true", p: Bool(true), ok: true},
		{title: "false", p: Bool(false), ok: false},
		{title: "error", p: Error(err), err: err},
		{title: "delay", p: Delay(func(yield func(thunk func() Promise)) {
			yield(func() Promise {
				return Bool(false)
			})
			yield(func() Promise {
				return Bool(true)
			})
		}), ok: true},
		{title: "delay with recovery", p: delayWithRecovery(func(yield func(thunk func() Promise)) {
			yield(func() Promise {
				return Error(err)
			})
		}, func(err error) (Promise, bool) {
			return Bool(true), true
		}), ok: true},
		{title: "delay with recovery failure", p: delayWithRecovery(func(yield func(thunk func() Promise)) {
			yield(func() Promise {
				return Error(err)
			})
		}, func(err error) (Promise, bool) {
			return Bool(false), false
		}), err: err},
		{title: "cut", p: delayAsCutParent(func(yield func(thunk func() Promise)) {
			yield(func() Promise {
				return delayWithCut(func(yield func(thunk func() Promise)) {
					yield(func() Promise {
						return Bool(false)
					})
				})
			})
			yield(func() Promise {
				return Bool(true)
			})
		}), ok: false},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			stack := NewStack(10)
			ok, err := tt.p.Force(stack)
			if !errors.Is(err, tt.err) {
				t.Fatalf("want %v; got %v", tt.err, err)
			}
			if ok != tt.ok {
				t.Errorf("want %v; got %v", tt.ok, ok)
			}
		})
	}
}
