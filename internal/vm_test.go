package internal

import (
	"context"
	"errors"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestVM_Arrive(t *testing.T) {
	t.Run("success", func(t *testing.T) {
		vm := VM{
			typeIn: NewAtom("user"),
			modules: map[Atom]*Module{
				NewAtom("user"): {
					procedures: map[Functor]procedureEntry{
						{Name: NewAtom("foo"), Arity: 1}: {procedure: Predicate1(func(ctx context.Context, t Term) Promise {
							return Continue(ctx)
						})},
					},
				},
			},
		}
		ok, err := vm.Arrive(context.Background(), NewAtom("user"), NewAtom("foo"), []Term{1}, Success).Force(Stack{})
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("unknown procedure", func(t *testing.T) {
		t.Run("error", func(t *testing.T) {
			vm := VM{
				typeIn: NewAtom("user"),
				modules: map[Atom]*Module{
					NewAtom("user"): {
						unknown: unknownError,
					},
				},
				Terms: NewHeap(1024),
			}
			ok, err := vm.Arrive(context.Background(), NewAtom("user"), NewAtom("foo"), []Term{1}, Success).Force(Stack{})
			var existenceError *ExistenceError
			if !errors.As(err, &existenceError) {
				t.Errorf("expected existenceError, got=%v", err)
			}
			if ok {
				t.Errorf("expected false")
			}
		})

		t.Run("warning", func(t *testing.T) {
			var warned bool
			vm := VM{
				typeIn: NewAtom("user"),
				modules: map[Atom]*Module{
					NewAtom("user"): {
						unknown: unknownWarning,
					},
				},
				Unknown: func(name Atom, args []Term) {
					warned = true
				},
			}
			ok, err := vm.Arrive(context.Background(), NewAtom("user"), NewAtom("foo"), []Term{1}, Success).Force(Stack{})
			if err != nil {
				t.Errorf("expected no error, got: %v", err)
			}
			if ok {
				t.Errorf("expected false")
			}

			if !warned {
				t.Errorf("expected true")
			}
		})

		t.Run("fail", func(t *testing.T) {
			vm := VM{
				typeIn: NewAtom("user"),
				modules: map[Atom]*Module{
					NewAtom("user"): {
						unknown: unknownFail,
					},
				},
			}
			ok, err := vm.Arrive(context.Background(), NewAtom("user"), NewAtom("foo"), []Term{1}, Success).Force(Stack{})
			if err != nil {
				t.Errorf("expected no error, got: %v", err)
			}
			if ok {
				t.Errorf("expected false")
			}
		})
	})
}

func TestVM_SetUserInput(t *testing.T) {
	t.Run("file", func(t *testing.T) {
		var vm VM
		if err := vm.SetUserInput(os.Stdin); err != nil {
			t.Errorf("expected no error, got: %v", err)
		}

		s, ok := vm.streams.GetByAlias(NewAtom("user_input"))
		if !ok {
			t.Errorf("expected to find user input")
		}
		if s.source != os.Stdin {
			t.Errorf("expected source to be os.Stdin")
		}
	})
}

func TestVM_SetUserOutput(t *testing.T) {
	t.Run("file", func(t *testing.T) {
		var vm VM
		if err := vm.SetUserOutput(os.Stdout); err != nil {
			t.Errorf("expected no error, got: %v", err)
		}

		s, ok := vm.streams.GetByAlias(NewAtom("user_output"))
		if !ok {
			t.Errorf("expected to find user output")
		}
		if s.sink != os.Stdout {
			t.Errorf("expected sink to be os.Stdout")
		}
	})
}
