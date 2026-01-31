package runtime

import (
	"testing"

	"github.com/ichiban/prolog/v2"
)

func TestResourceError_Error(t *testing.T) {
	err := &ResourceError{Resource: "test"}
	if got, want := err.Error(), "insufficient resource: test"; got != want {
		t.Errorf("got: %v, want: %v", got, want)
	}
}

func TestTypeError_Error(t *testing.T) {
	err := &TypeError{ValidType: "integer", Culprit: prolog.Term{tag: prolog.termTagCharacter, value: 'a'}}
	if got, want := err.Error(), "invalid type: expected integer, got <character 97>"; got != want {
		t.Errorf("got: %v, want: %v", got, want)
	}
}

func TestUninstantiationError_Error(t *testing.T) {
	err := &UninstantiationError{Culprit: prolog.Term{tag: prolog.termTagCharacter, value: 'a'}}
	if got, want := err.Error(), "uninstantiation error: <character 97>"; got != want {
		t.Errorf("got: %v, want: %v", got, want)
	}
}
