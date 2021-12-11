package engine

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestVM_Arrive(t *testing.T) {
	t.Run("success", func(t *testing.T) {
		vm := VM{
			procedures: map[ProcedureIndicator]procedure{
				{Name: "foo", Arity: 1}: predicate1(func(t Term, k func(*Env) *Promise, env *Env) *Promise {
					return k(env)
				}),
			},
		}
		ok, err := vm.Arrive(ProcedureIndicator{Name: "foo", Arity: 1}, []Term{Atom("a")}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("unknown procedure", func(t *testing.T) {
		t.Run("error", func(t *testing.T) {
			vm := VM{
				unknown: unknownError,
			}
			ok, err := vm.Arrive(ProcedureIndicator{Name: "foo", Arity: 1}, []Term{Atom("a")}, Success, nil).Force(context.Background())
			assert.Equal(t, existenceErrorProcedure(&Compound{
				Functor: "/",
				Args:    []Term{Atom("foo"), Integer(1)},
			}), err)
			assert.False(t, ok)
		})

		t.Run("warning", func(t *testing.T) {
			var warned bool
			vm := VM{
				unknown: unknownWarning,
				OnUnknown: func(pi ProcedureIndicator, args []Term, env *Env) {
					assert.Equal(t, ProcedureIndicator{Name: "foo", Arity: 1}, pi)
					assert.Equal(t, []Term{Atom("a")}, args)
					assert.Nil(t, env)
					warned = true
				},
			}
			ok, err := vm.Arrive(ProcedureIndicator{Name: "foo", Arity: 1}, []Term{Atom("a")}, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
			assert.True(t, warned)
		})

		t.Run("fail", func(t *testing.T) {
			vm := VM{
				unknown: unknownFail,
			}
			ok, err := vm.Arrive(ProcedureIndicator{Name: "foo", Arity: 1}, []Term{Atom("a")}, Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.False(t, ok)
		})
	})
}

func TestNewProcedureIndicator(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		pi, err := NewProcedureIndicator(&Compound{
			Functor: "/",
			Args:    []Term{Atom("foo"), Integer(2)},
		}, nil)
		assert.NoError(t, err)
		assert.Equal(t, ProcedureIndicator{Name: "foo", Arity: 2}, pi)
	})

	t.Run("variable", func(t *testing.T) {
		pi, err := NewProcedureIndicator(Variable("PI"), nil)
		assert.Equal(t, InstantiationError(Variable("PI")), err)
		assert.Zero(t, pi)
	})

	t.Run("atomic", func(t *testing.T) {
		pi, err := NewProcedureIndicator(Atom("foo"), nil)
		assert.Equal(t, typeErrorPredicateIndicator(Atom("foo")), err)
		assert.Zero(t, pi)
	})

	t.Run("non-PI compound", func(t *testing.T) {
		pi, err := NewProcedureIndicator(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("a"), Atom("b")},
		}, nil)
		assert.Equal(t, typeErrorPredicateIndicator(&Compound{
			Functor: "foo",
			Args:    []Term{Atom("a"), Atom("b")},
		}), err)
		assert.Zero(t, pi)
	})

	t.Run("variable functor", func(t *testing.T) {
		pi, err := NewProcedureIndicator(&Compound{
			Functor: "/",
			Args:    []Term{Variable("Functor"), Integer(2)},
		}, nil)
		assert.Equal(t, InstantiationError(&Compound{
			Functor: "/",
			Args:    []Term{Variable("Functor"), Integer(2)},
		}), err)
		assert.Zero(t, pi)
	})

	t.Run("non-atom functor", func(t *testing.T) {
		pi, err := NewProcedureIndicator(&Compound{
			Functor: "/",
			Args:    []Term{Integer(0), Integer(2)},
		}, nil)
		assert.Equal(t, typeErrorPredicateIndicator(&Compound{
			Functor: "/",
			Args:    []Term{Integer(0), Integer(2)},
		}), err)
		assert.Zero(t, pi)
	})

	t.Run("variable arity", func(t *testing.T) {
		pi, err := NewProcedureIndicator(&Compound{
			Functor: "/",
			Args:    []Term{Atom("foo"), Variable("Arity")},
		}, nil)
		assert.Equal(t, InstantiationError(&Compound{
			Functor: "/",
			Args:    []Term{Atom("foo"), Variable("Arity")},
		}), err)
		assert.Zero(t, pi)
	})

	t.Run("non-integer arity", func(t *testing.T) {
		pi, err := NewProcedureIndicator(&Compound{
			Functor: "/",
			Args:    []Term{Atom("foo"), Atom("arity")},
		}, nil)
		assert.Equal(t, typeErrorPredicateIndicator(&Compound{
			Functor: "/",
			Args:    []Term{Atom("foo"), Atom("arity")},
		}), err)
		assert.Zero(t, pi)
	})
}

func TestProcedureIndicator_String(t *testing.T) {
	assert.Equal(t, `''/0`, ProcedureIndicator{}.String())
	assert.Equal(t, `foo/2`, ProcedureIndicator{Name: "foo", Arity: 2}.String())
}

func TestProcedureIndicator_Term(t *testing.T) {
	assert.Equal(t, &Compound{
		Functor: "/",
		Args:    []Term{Atom("foo"), Integer(2)},
	}, ProcedureIndicator{Name: "foo", Arity: 2}.Term())
}

func TestProcedureIndicator_Apply(t *testing.T) {
	t.Run("ok", func(t *testing.T) {
		c, err := ProcedureIndicator{Name: "foo", Arity: 2}.Apply(Atom("a"), Atom("b"))
		assert.NoError(t, err)
		assert.Equal(t, &Compound{
			Functor: "foo",
			Args:    []Term{Atom("a"), Atom("b")},
		}, c)
	})

	t.Run("wrong number of arguments", func(t *testing.T) {
		c, err := ProcedureIndicator{Name: "foo", Arity: 2}.Apply(Atom("a"), Atom("b"), Atom("c"))
		assert.Error(t, err)
		assert.Nil(t, c)
	})
}
