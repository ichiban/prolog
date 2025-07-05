package prolog

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
		{title: "true", p: Eager(true, nil), ok: true},
		{title: "false", p: Eager(false, nil), ok: false},
		{title: "error", p: Eager(false, err), err: err},
		{title: "lazy", p: Lazy(func(yield func(Promise) bool) {
			if !yield(Eager(false, nil)) {
				return
			}
			if !yield(Lazy(func(yield func(Promise) bool) {
				if !yield(Eager(false, nil)) {
					return
				}
				if !yield(Eager(true, nil)) {
					return
				}
			})) {
				return
			}
		}), ok: true},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			ok, err := tt.p.Force()
			if !errors.Is(err, tt.err) {
				t.Fatalf("want %v; got %v", tt.err, err)
			}
			if ok != tt.ok {
				t.Errorf("want %v; got %v", tt.ok, ok)
			}
		})
	}
}
