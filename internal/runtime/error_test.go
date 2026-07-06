package runtime

import (
	"testing"

	"github.com/ichiban/prolog/v2/internal/term"
)

func TestResourceError_Error(t *testing.T) {
	err := &ResourceError{Resource: "test"}
	if got, want := err.Error(), "insufficient resource: test"; got != want {
		t.Errorf("got: %v, want: %v", got, want)
	}
}

func TestTypeError_Error(t *testing.T) {
	arena := term.Arena{
		Heap: make(term.Heap, 0),
	}
	must := func(h term.Handle, err error) term.Handle {
		if err != nil {
			t.Fatal(err)
		}
		return h
	}
	err := &TypeError{
		ValidType: "integer",
		Culprit:   must(arena.PutAtom(term.NewAtomRune('a'))),
	}
	if got, want := err.Error(), "invalid type: expected integer"; got != want {
		t.Errorf("got: %v, want: %v", got, want)
	}
}

func TestUninstantiationError_Error(t *testing.T) {
	arena := term.Arena{
		Heap: make(term.Heap, 0),
	}
	must := func(h term.Handle, err error) term.Handle {
		if err != nil {
			t.Fatal(err)
		}
		return h
	}
	err := &UninstantiationError{Culprit: must(arena.PutAtom(term.NewAtomRune('a')))}
	if got, want := err.Error(), "uninstantiation error"; got != want {
		t.Errorf("got: %v, want: %v", got, want)
	}
}
