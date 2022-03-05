package engine

import (
	"context"
	"math"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestEvaluableFunctors_Is(t *testing.T) {
	t.Run("addition", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Integer(3), &Compound{Functor: "+", Args: []Term{Integer(1), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(3), &Compound{Functor: "+", Args: []Term{Integer(1), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(3), &Compound{Functor: "+", Args: []Term{Float(1), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(3), &Compound{Functor: "+", Args: []Term{Float(1), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("subtraction", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Integer(1), &Compound{Functor: "-", Args: []Term{Integer(3), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(1), &Compound{Functor: "-", Args: []Term{Integer(3), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(1), &Compound{Functor: "-", Args: []Term{Float(3), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(1), &Compound{Functor: "-", Args: []Term{Float(3), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("multiplication", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Integer(6), &Compound{Functor: "*", Args: []Term{Integer(3), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(6), &Compound{Functor: "*", Args: []Term{Integer(3), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(6), &Compound{Functor: "*", Args: []Term{Float(3), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(6), &Compound{Functor: "*", Args: []Term{Float(3), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("floating-point division", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Float(2), &Compound{Functor: "/", Args: []Term{Integer(4), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(2), &Compound{Functor: "/", Args: []Term{Integer(4), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(2), &Compound{Functor: "/", Args: []Term{Float(4), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(2), &Compound{Functor: "/", Args: []Term{Float(4), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("integer division", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Integer(2), &Compound{Functor: "//", Args: []Term{Integer(4), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(NewVariable(), &Compound{Functor: "//", Args: []Term{Integer(4), Float(2)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(2)), err)
		assert.False(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(NewVariable(), &Compound{Functor: "//", Args: []Term{Float(4), Integer(2)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(NewVariable(), &Compound{Functor: "//", Args: []Term{Integer(4), Integer(0)}}, Success, nil).Force(context.Background())
		assert.Equal(t, evaluationErrorZeroDivisor(), err)
		assert.False(t, ok)
	})

	t.Run("remainder", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Integer(-1), &Compound{Functor: "rem", Args: []Term{Integer(-21), Integer(4)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(NewVariable(), &Compound{Functor: "rem", Args: []Term{Integer(-21), Float(4)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(NewVariable(), &Compound{Functor: "rem", Args: []Term{Float(-21), Integer(4)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(-21)), err)
		assert.False(t, ok)
	})

	t.Run("mod", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Integer(3), &Compound{Functor: "mod", Args: []Term{Integer(-21), Integer(4)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(NewVariable(), &Compound{Functor: "mod", Args: []Term{Integer(-21), Float(4)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(NewVariable(), &Compound{Functor: "mod", Args: []Term{Float(-21), Integer(4)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(-21)), err)
		assert.False(t, ok)
	})

	t.Run("exponential", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Float(16), &Compound{Functor: "**", Args: []Term{Integer(4), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(16), &Compound{Functor: "**", Args: []Term{Integer(4), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(16), &Compound{Functor: "**", Args: []Term{Float(4), Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(16), &Compound{Functor: "**", Args: []Term{Float(4), Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("unary plus", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Integer(2), &Compound{Functor: "+", Args: []Term{Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(2), &Compound{Functor: "+", Args: []Term{Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("unary minus", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Integer(-2), &Compound{Functor: "-", Args: []Term{Integer(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(-2), &Compound{Functor: "-", Args: []Term{Float(2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("absolute value", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Float(2), &Compound{Functor: "abs", Args: []Term{Integer(-2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(2), &Compound{Functor: "abs", Args: []Term{Float(-2)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("arctangent", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Float(0), &Compound{Functor: "atan", Args: []Term{Integer(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(0), &Compound{Functor: "atan", Args: []Term{Float(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("ceiling", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Float(1), &Compound{Functor: "ceiling", Args: []Term{Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(1), &Compound{Functor: "ceiling", Args: []Term{Float(0.9)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("cosine", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Float(1.0), &Compound{Functor: "cos", Args: []Term{Integer(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(1.0), &Compound{Functor: "cos", Args: []Term{Float(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("natural antilogarithm", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Float(1.0), &Compound{Functor: "exp", Args: []Term{Integer(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(1.0), &Compound{Functor: "exp", Args: []Term{Float(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("square root", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Float(1.0), &Compound{Functor: "sqrt", Args: []Term{Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(1.0), &Compound{Functor: "sqrt", Args: []Term{Float(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("sign", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Integer(1), &Compound{Functor: "sign", Args: []Term{Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Integer(1), &Compound{Functor: "sign", Args: []Term{Integer(math.MaxInt64)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Integer(0), &Compound{Functor: "sign", Args: []Term{Integer(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Integer(-1), &Compound{Functor: "sign", Args: []Term{Integer(-1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Integer(-1), &Compound{Functor: "sign", Args: []Term{Integer(math.MinInt64)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(1), &Compound{Functor: "sign", Args: []Term{Float(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(1), &Compound{Functor: "sign", Args: []Term{Float(math.MaxFloat64)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(0), &Compound{Functor: "sign", Args: []Term{Float(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(-1), &Compound{Functor: "sign", Args: []Term{Float(-1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(-1), &Compound{Functor: "sign", Args: []Term{Float(-math.MaxFloat64)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		v := Variable("N")
		ok, err = DefaultEvaluableFunctors.Is(v, &Compound{Functor: "sign", Args: []Term{Float(math.NaN())}}, func(env *Env) *Promise {
			assert.True(t, math.IsNaN(float64(env.Resolve(v).(Float))))
			return Bool(true)
		}, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("float", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Float(1.0), &Compound{Functor: "float", Args: []Term{Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(1.0), &Compound{Functor: "float", Args: []Term{Float(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("floor", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Float(1), &Compound{Functor: "floor", Args: []Term{Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(1), &Compound{Functor: "floor", Args: []Term{Float(1.1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("natural logarithm", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Float(0), &Compound{Functor: "log", Args: []Term{Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(0), &Compound{Functor: "log", Args: []Term{Float(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("sine", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Float(0), &Compound{Functor: "sin", Args: []Term{Integer(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(0), &Compound{Functor: "sin", Args: []Term{Float(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("truncate", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Float(1), &Compound{Functor: "truncate", Args: []Term{Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(1), &Compound{Functor: "truncate", Args: []Term{Float(1.1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("round", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Float(1), &Compound{Functor: "round", Args: []Term{Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(Float(1), &Compound{Functor: "round", Args: []Term{Float(1.1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("bit-shift right", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Integer(2), &Compound{Functor: ">>", Args: []Term{Integer(4), Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(NewVariable(), &Compound{Functor: ">>", Args: []Term{Float(4), Integer(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(NewVariable(), &Compound{Functor: ">>", Args: []Term{Integer(4), Float(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(1)), err)
		assert.False(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(NewVariable(), &Compound{Functor: ">>", Args: []Term{Float(4), Float(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)
	})

	t.Run("bit-shift left", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Integer(8), &Compound{Functor: "<<", Args: []Term{Integer(4), Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(NewVariable(), &Compound{Functor: "<<", Args: []Term{Float(4), Integer(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(NewVariable(), &Compound{Functor: "<<", Args: []Term{Integer(4), Float(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(1)), err)
		assert.False(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(NewVariable(), &Compound{Functor: "<<", Args: []Term{Float(4), Float(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)
	})

	t.Run("bitwise and", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Integer(1), &Compound{Functor: "/\\", Args: []Term{Integer(5), Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(NewVariable(), &Compound{Functor: "/\\", Args: []Term{Float(5), Integer(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(5)), err)
		assert.False(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(NewVariable(), &Compound{Functor: "/\\", Args: []Term{Integer(5), Float(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(1)), err)
		assert.False(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(NewVariable(), &Compound{Functor: "/\\", Args: []Term{Float(5), Float(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(5)), err)
		assert.False(t, ok)
	})

	t.Run("bitwise or", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Integer(5), &Compound{Functor: "\\/", Args: []Term{Integer(4), Integer(1)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(NewVariable(), &Compound{Functor: "\\/", Args: []Term{Float(4), Integer(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(NewVariable(), &Compound{Functor: "\\/", Args: []Term{Integer(4), Float(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(1)), err)
		assert.False(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(NewVariable(), &Compound{Functor: "\\/", Args: []Term{Float(4), Float(1)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(4)), err)
		assert.False(t, ok)
	})

	t.Run("bitwise complement", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Integer(-1), &Compound{Functor: "\\", Args: []Term{Integer(0)}}, Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Is(NewVariable(), &Compound{Functor: "\\", Args: []Term{Float(0)}}, Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorInteger(Float(0)), err)
		assert.False(t, ok)
	})

	t.Run("expression is a variable", func(t *testing.T) {
		expression := Variable("Exp")

		ok, err := DefaultEvaluableFunctors.Is(Integer(0), expression, Success, nil).Force(context.Background())
		assert.Equal(t, ErrInstantiation, err)
		assert.False(t, ok)
	})

	t.Run("unknown constant", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Is(Integer(0), Atom("foo"), Success, nil).Force(context.Background())
		assert.Equal(t, typeErrorEvaluable(&Compound{
			Functor: "/",
			Args:    []Term{Atom("foo"), Integer(0)},
		}), err)
		assert.False(t, ok)
	})
}

func TestEvaluableFunctors_Equal(t *testing.T) {
	t.Run("same", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Equal(Integer(1), Integer(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Equal(Float(1), Integer(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Equal(Integer(1), Float(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.Equal(Float(1), Float(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("different", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.Equal(Integer(1), Integer(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)

		ok, err = DefaultEvaluableFunctors.Equal(Float(1), Integer(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)

		ok, err = DefaultEvaluableFunctors.Equal(Integer(1), Float(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)

		ok, err = DefaultEvaluableFunctors.Equal(Float(1), Float(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("lhs is a variable", func(t *testing.T) {
		lhs := Variable("LHS")

		ok, err := DefaultEvaluableFunctors.Equal(lhs, Integer(1), Success, nil).Force(context.Background())
		assert.Equal(t, ErrInstantiation, err)
		assert.False(t, ok)
	})

	t.Run("rhs is a variable", func(t *testing.T) {
		rhs := Variable("RHS")

		ok, err := DefaultEvaluableFunctors.Equal(Integer(1), rhs, Success, nil).Force(context.Background())
		assert.Equal(t, ErrInstantiation, err)
		assert.False(t, ok)
	})
}

func TestEvaluableFunctors_NotEqual(t *testing.T) {
	t.Run("same", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.NotEqual(Integer(1), Integer(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)

		ok, err = DefaultEvaluableFunctors.NotEqual(Float(1), Integer(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)

		ok, err = DefaultEvaluableFunctors.NotEqual(Integer(1), Float(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)

		ok, err = DefaultEvaluableFunctors.NotEqual(Float(1), Float(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})

	t.Run("different", func(t *testing.T) {
		ok, err := DefaultEvaluableFunctors.NotEqual(Integer(1), Integer(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.NotEqual(Float(1), Integer(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.NotEqual(Integer(1), Float(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)

		ok, err = DefaultEvaluableFunctors.NotEqual(Float(1), Float(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.True(t, ok)
	})

	t.Run("lhs is a variable", func(t *testing.T) {
		lhs := Variable("LHS")

		ok, err := DefaultEvaluableFunctors.NotEqual(lhs, Integer(1), Success, nil).Force(context.Background())
		assert.Equal(t, ErrInstantiation, err)
		assert.False(t, ok)
	})

	t.Run("rhs is a variable", func(t *testing.T) {
		rhs := Variable("RHS")

		ok, err := DefaultEvaluableFunctors.NotEqual(Integer(1), rhs, Success, nil).Force(context.Background())
		assert.Equal(t, ErrInstantiation, err)
		assert.False(t, ok)
	})
}

func TestEvaluableFunctors_LessThan(t *testing.T) {
	ok, err := DefaultEvaluableFunctors.LessThan(Integer(1), Integer(2), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultEvaluableFunctors.LessThan(Float(1), Integer(2), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultEvaluableFunctors.LessThan(Integer(1), Float(2), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultEvaluableFunctors.LessThan(Float(1), Float(2), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)
}

func TestEvaluableFunctors_GreaterThan(t *testing.T) {
	ok, err := DefaultEvaluableFunctors.GreaterThan(Integer(2), Integer(1), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultEvaluableFunctors.GreaterThan(Float(2), Integer(1), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultEvaluableFunctors.GreaterThan(Integer(2), Float(1), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultEvaluableFunctors.GreaterThan(Float(2), Float(1), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)
}

func TestEvaluableFunctors_LessThanOrEqual(t *testing.T) {
	ok, err := DefaultEvaluableFunctors.LessThanOrEqual(Integer(1), Integer(2), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultEvaluableFunctors.LessThanOrEqual(Float(1), Integer(2), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultEvaluableFunctors.LessThanOrEqual(Integer(1), Float(2), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultEvaluableFunctors.LessThanOrEqual(Float(1), Float(2), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)
}

func TestEvaluableFunctors_GreaterThanOrEqual(t *testing.T) {
	ok, err := DefaultEvaluableFunctors.GreaterThanOrEqual(Integer(2), Integer(1), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultEvaluableFunctors.GreaterThanOrEqual(Float(2), Integer(1), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultEvaluableFunctors.GreaterThanOrEqual(Integer(2), Float(1), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)

	ok, err = DefaultEvaluableFunctors.GreaterThanOrEqual(Float(2), Float(1), Success, nil).Force(context.Background())
	assert.NoError(t, err)
	assert.True(t, ok)
}

func TestPos(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		r, err := Pos(Integer(1), nil)
		assert.NoError(t, err)
		assert.Equal(t, Integer(1), r)
	})

	t.Run("float", func(t *testing.T) {
		r, err := Pos(Float(1), nil)
		assert.NoError(t, err)
		assert.Equal(t, Float(1), r)
	})

	t.Run("variable", func(t *testing.T) {
		_, err := Pos(Variable("X"), nil)
		assert.Equal(t, ErrInstantiation, err)
	})

	t.Run("non-evaluable", func(t *testing.T) {
		_, err := Pos(Atom("foo"), nil)
		assert.Equal(t, typeErrorEvaluable(Atom("foo")), err)
	})
}

func TestIntFloorDiv(t *testing.T) {
	t.Run("both integer", func(t *testing.T) {
		r, err := IntFloorDiv(Integer(1), Integer(1), nil)
		assert.NoError(t, err)
		assert.Equal(t, Integer(1), r)
	})

	t.Run("variable", func(t *testing.T) {
		t.Run("dividend", func(t *testing.T) {
			_, err := IntFloorDiv(Variable("X"), Integer(1), nil)
			assert.Equal(t, ErrInstantiation, err)
		})

		t.Run("divisor", func(t *testing.T) {
			_, err := IntFloorDiv(Integer(1), Variable("X"), nil)
			assert.Equal(t, ErrInstantiation, err)
		})
	})

	t.Run("non-integer", func(t *testing.T) {
		t.Run("dividend", func(t *testing.T) {
			_, err := IntFloorDiv(Float(1), Integer(1), nil)
			assert.Equal(t, typeErrorInteger(Float(1)), err)
		})

		t.Run("divisor", func(t *testing.T) {
			_, err := IntFloorDiv(Integer(1), Float(1), nil)
			assert.Equal(t, typeErrorInteger(Float(1)), err)
		})
	})

	t.Run("zero division", func(t *testing.T) {
		_, err := IntFloorDiv(Integer(1), Integer(0), nil)
		assert.Equal(t, evaluationErrorZeroDivisor(), err)
	})
}
