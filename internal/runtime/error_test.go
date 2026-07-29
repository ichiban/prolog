package runtime

import (
	"testing"

	"github.com/ichiban/prolog/v2/internal/term"
)

func TestResourceError_Error(t *testing.T) {
	err := &ResourceError{Resource: term.NewAtom("test")}
	if got, want := err.Error(), "insufficient resource: test"; got != want {
		t.Errorf("got: %v, want: %v", got, want)
	}
}

func TestTypeError_Error(t *testing.T) {
	err := &TypeError{
		ValidType: term.NewAtom("integer"),
		Culprit:   `a.`,
	}
	if got, want := err.Error(), "invalid type: valid type = integer, culprit = a., location = ./2"; got != want {
		t.Errorf("got: %v, want: %v", got, want)
	}
}

func TestUninstantiationError_Error(t *testing.T) {
	err := &UninstantiationError{Culprit: `a.`}
	if got, want := err.Error(), "uninstantiation error"; got != want {
		t.Errorf("got: %v, want: %v", got, want)
	}
}
