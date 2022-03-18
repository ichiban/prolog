package engine

import (
	"context"
	"math"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
)

func TestEvaluableFunctors_Is(t *testing.T) {
	efs := EvaluableFunctors{
		Unary: map[Atom]func(x Number) (Number, error){
			"foo": func(_ Number) (Number, error) {
				return Integer(1), nil
			},
		},
		Binary: map[Atom]func(x Number, y Number) (Number, error){
			"foo": func(_, _ Number) (Number, error) {
				return Integer(1), nil
			},
		},
	}

	t.Run("constant", func(t *testing.T) {
		_, err := efs.Is(Integer(1), Atom("foo"), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("unary", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			ok, err := efs.Is(Integer(1), Atom("foo").Apply(Integer(0)), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("undefined", func(t *testing.T) {
			_, err := efs.Is(Integer(1), Atom("bar").Apply(Integer(0)), Success, nil).Force(context.Background())
			assert.Error(t, err)
		})

		t.Run("invalid argument", func(t *testing.T) {
			_, err := efs.Is(Integer(1), Atom("foo").Apply(Variable("X")), Success, nil).Force(context.Background())
			assert.Error(t, err)
		})
	})

	t.Run("binary", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			ok, err := efs.Is(Integer(1), Atom("foo").Apply(Integer(0), Integer(0)), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("undefined", func(t *testing.T) {
			_, err := efs.Is(Integer(1), Atom("bar").Apply(Integer(0), Integer(0)), Success, nil).Force(context.Background())
			assert.Error(t, err)
		})

		t.Run("invalid argument", func(t *testing.T) {
			_, err := efs.Is(Integer(1), Atom("foo").Apply(Variable("X"), Integer(0)), Success, nil).Force(context.Background())
			assert.Error(t, err)
			_, err = efs.Is(Integer(1), Atom("foo").Apply(Integer(0), Variable("X")), Success, nil).Force(context.Background())
			assert.Error(t, err)
		})
	})

	t.Run("tertiary", func(t *testing.T) {
		_, err := efs.Is(Integer(1), Atom("foo").Apply(Integer(0), Integer(0), Integer(0)), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("other", func(t *testing.T) {
		_, err := efs.Is(Integer(1), &Stream{}, Success, nil).Force(context.Background())
		assert.Error(t, err)
	})
}

func TestEvaluableFunctors_Equal(t *testing.T) {
	var efs EvaluableFunctors

	t.Run("integer", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			ok, err := efs.Equal(Integer(1), Integer(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("float", func(t *testing.T) {
			ok, err := efs.Equal(Integer(1), Float(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("float", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			ok, err := efs.Equal(Float(1), Integer(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("float", func(t *testing.T) {
			ok, err := efs.Equal(Float(1), Float(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("e1 is a variable", func(t *testing.T) {
		_, err := efs.Equal(Integer(1), Variable("X"), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("e2 is a variable", func(t *testing.T) {
		_, err := efs.Equal(Variable("X"), Integer(1), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("ng", func(t *testing.T) {
		ok, err := efs.Equal(Integer(1), Integer(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestEvaluableFunctors_NotEqual(t *testing.T) {
	var efs EvaluableFunctors

	t.Run("integer", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			ok, err := efs.NotEqual(Integer(1), Integer(2), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("float", func(t *testing.T) {
			ok, err := efs.NotEqual(Integer(1), Float(2), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("float", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			ok, err := efs.NotEqual(Float(1), Integer(2), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("float", func(t *testing.T) {
			ok, err := efs.NotEqual(Float(1), Float(2), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("e1 is a variable", func(t *testing.T) {
		_, err := efs.NotEqual(Integer(1), Variable("X"), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("e2 is a variable", func(t *testing.T) {
		_, err := efs.NotEqual(Variable("X"), Integer(1), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("ng", func(t *testing.T) {
		ok, err := efs.NotEqual(Integer(1), Integer(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestEvaluableFunctors_LessThan(t *testing.T) {
	var efs EvaluableFunctors

	t.Run("integer", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			ok, err := efs.LessThan(Integer(1), Integer(2), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("float", func(t *testing.T) {
			ok, err := efs.LessThan(Integer(1), Float(2), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("float", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			ok, err := efs.LessThan(Float(1), Integer(2), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("float", func(t *testing.T) {
			ok, err := efs.LessThan(Float(1), Float(2), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("e1 is a variable", func(t *testing.T) {
		_, err := efs.LessThan(Integer(1), Variable("X"), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("e2 is a variable", func(t *testing.T) {
		_, err := efs.LessThan(Variable("X"), Integer(1), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("ng", func(t *testing.T) {
		ok, err := efs.LessThan(Integer(1), Integer(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestEvaluableFunctors_GreaterThan(t *testing.T) {
	var efs EvaluableFunctors

	t.Run("integer", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			ok, err := efs.GreaterThan(Integer(2), Integer(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("float", func(t *testing.T) {
			ok, err := efs.GreaterThan(Integer(2), Float(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("float", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			ok, err := efs.GreaterThan(Float(2), Integer(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("float", func(t *testing.T) {
			ok, err := efs.GreaterThan(Float(2), Float(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("e1 is a variable", func(t *testing.T) {
		_, err := efs.GreaterThan(Integer(1), Variable("X"), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("e2 is a variable", func(t *testing.T) {
		_, err := efs.GreaterThan(Variable("X"), Integer(1), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("ng", func(t *testing.T) {
		ok, err := efs.GreaterThan(Integer(1), Integer(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestEvaluableFunctors_LessThanOrEqual(t *testing.T) {
	var efs EvaluableFunctors

	t.Run("integer", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			ok, err := efs.LessThanOrEqual(Integer(1), Integer(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("float", func(t *testing.T) {
			ok, err := efs.LessThanOrEqual(Integer(1), Float(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("float", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			ok, err := efs.LessThanOrEqual(Float(1), Integer(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("float", func(t *testing.T) {
			ok, err := efs.LessThanOrEqual(Float(1), Float(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("e1 is a variable", func(t *testing.T) {
		_, err := efs.LessThanOrEqual(Integer(1), Variable("X"), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("e2 is a variable", func(t *testing.T) {
		_, err := efs.LessThanOrEqual(Variable("X"), Integer(1), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("ng", func(t *testing.T) {
		ok, err := efs.LessThanOrEqual(Integer(2), Integer(1), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestEvaluableFunctors_GreaterThanOrEqual(t *testing.T) {
	var efs EvaluableFunctors

	t.Run("integer", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			ok, err := efs.GreaterThanOrEqual(Integer(1), Integer(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("float", func(t *testing.T) {
			ok, err := efs.GreaterThanOrEqual(Integer(1), Float(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("float", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			ok, err := efs.GreaterThanOrEqual(Float(1), Integer(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})

		t.Run("float", func(t *testing.T) {
			ok, err := efs.GreaterThanOrEqual(Float(1), Float(1), Success, nil).Force(context.Background())
			assert.NoError(t, err)
			assert.True(t, ok)
		})
	})

	t.Run("e1 is a variable", func(t *testing.T) {
		_, err := efs.GreaterThanOrEqual(Integer(1), Variable("X"), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("e2 is a variable", func(t *testing.T) {
		_, err := efs.GreaterThanOrEqual(Variable("X"), Integer(1), Success, nil).Force(context.Background())
		assert.Error(t, err)
	})

	t.Run("ng", func(t *testing.T) {
		ok, err := efs.GreaterThanOrEqual(Integer(1), Integer(2), Success, nil).Force(context.Background())
		assert.NoError(t, err)
		assert.False(t, ok)
	})
}

func TestAdd(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				r, err := Add(Integer(1), Integer(1))
				assert.NoError(t, err)
				assert.Equal(t, Integer(2), r)
			})

			t.Run("overflow", func(t *testing.T) {
				t.Run("positive", func(t *testing.T) {
					_, err := Add(Integer(math.MaxInt64), Integer(1))
					assert.Equal(t, ErrIntOverflow, err)
				})

				t.Run("negative", func(t *testing.T) {
					_, err := Add(Integer(math.MinInt64), Integer(-1))
					assert.Equal(t, ErrIntOverflow, err)
				})
			})
		})

		t.Run("float", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				r, err := Add(Integer(1), Float(1))
				assert.NoError(t, err)
				assert.Equal(t, Float(2), r)
			})

			t.Run("overflow", func(t *testing.T) {
				t.Run("positive", func(t *testing.T) {
					_, err := Add(Integer(1), Float(math.MaxFloat64))
					assert.Equal(t, ErrFloatOverflow, err)
				})

				t.Run("negative", func(t *testing.T) {
					_, err := Add(Integer(-1), Float(-math.MaxFloat64))
					assert.Equal(t, ErrFloatOverflow, err)
				})
			})
		})
	})

	t.Run("float", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				r, err := Add(Float(1), Integer(1))
				assert.NoError(t, err)
				assert.Equal(t, Float(2), r)
			})
		})

		t.Run("float", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				r, err := Add(Float(1), Float(1))
				assert.NoError(t, err)
				assert.Equal(t, Float(2), r)
			})

			t.Run("overflow", func(t *testing.T) {
				t.Run("positive", func(t *testing.T) {
					_, err := Add(Float(math.MaxFloat64), Float(math.MaxFloat64))
					assert.Equal(t, ErrFloatOverflow, err)
				})

				t.Run("negative", func(t *testing.T) {
					_, err := Add(Float(-math.MaxFloat64), Float(-math.MaxFloat64))
					assert.Equal(t, ErrFloatOverflow, err)
				})
			})
		})
	})

	t.Run("not a number", func(t *testing.T) {
		_, err := Add(mockNumber{}, Integer(0))
		assert.Equal(t, ErrUndefined, err)
	})
}

func TestSub(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				r, err := Sub(Integer(1), Integer(1))
				assert.NoError(t, err)
				assert.Equal(t, Integer(0), r)
			})

			t.Run("overflow", func(t *testing.T) {
				t.Run("positive", func(t *testing.T) {
					_, err := Sub(Integer(math.MaxInt64), Integer(-1))
					assert.Equal(t, ErrIntOverflow, err)
				})

				t.Run("negative", func(t *testing.T) {
					_, err := Sub(Integer(math.MinInt64), Integer(1))
					assert.Equal(t, ErrIntOverflow, err)
				})
			})
		})

		t.Run("float", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				r, err := Sub(Integer(1), Float(1))
				assert.NoError(t, err)
				assert.Equal(t, Float(0), r)
			})

			t.Run("overflow", func(t *testing.T) {
				t.Run("positive", func(t *testing.T) {
					_, err := Sub(Integer(1), Float(-math.MaxFloat64))
					assert.Equal(t, ErrFloatOverflow, err)
				})

				t.Run("negative", func(t *testing.T) {
					_, err := Sub(Integer(-1), Float(math.MaxFloat64))
					assert.Equal(t, ErrFloatOverflow, err)
				})
			})
		})
	})

	t.Run("float", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				r, err := Sub(Float(1), Integer(1))
				assert.NoError(t, err)
				assert.Equal(t, Float(0), r)
			})
		})

		t.Run("float", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				r, err := Sub(Float(1), Float(1))
				assert.NoError(t, err)
				assert.Equal(t, Float(0), r)
			})

			t.Run("overflow", func(t *testing.T) {
				t.Run("positive", func(t *testing.T) {
					_, err := Sub(Float(math.MaxFloat64), Float(-math.MaxFloat64))
					assert.Equal(t, ErrFloatOverflow, err)
				})

				t.Run("negative", func(t *testing.T) {
					_, err := Sub(Float(-math.MaxFloat64), Float(math.MaxFloat64))
					assert.Equal(t, ErrFloatOverflow, err)
				})
			})
		})
	})

	t.Run("not a number", func(t *testing.T) {
		_, err := Sub(mockNumber{}, Integer(0))
		assert.Equal(t, ErrUndefined, err)
	})
}

func TestMul(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				r, err := Mul(Integer(1), Integer(1))
				assert.NoError(t, err)
				assert.Equal(t, Integer(1), r)

				t.Run("zero", func(t *testing.T) {
					r, err := Mul(Integer(1), Integer(0))
					assert.NoError(t, err)
					assert.Equal(t, Integer(0), r)
				})
			})

			t.Run("overflow", func(t *testing.T) {
				t.Run("positive", func(t *testing.T) {
					_, err := Mul(Integer(math.MaxInt64), Integer(2))
					assert.Equal(t, ErrIntOverflow, err)
				})

				t.Run("negative", func(t *testing.T) {
					_, err := Mul(Integer(math.MinInt64), Integer(2))
					assert.Equal(t, ErrIntOverflow, err)
				})

				t.Run("two's complement special case", func(t *testing.T) {
					_, err := Mul(Integer(-1), Integer(math.MinInt64))
					assert.Equal(t, ErrIntOverflow, err)
					_, err = Mul(Integer(math.MinInt64), Integer(-1))
					assert.Equal(t, ErrIntOverflow, err)
				})
			})
		})

		t.Run("float", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				r, err := Mul(Integer(1), Float(1))
				assert.NoError(t, err)
				assert.Equal(t, Float(1), r)
			})

			t.Run("overflow", func(t *testing.T) {
				t.Run("positive", func(t *testing.T) {
					_, err := Mul(Integer(2), Float(math.MaxFloat64))
					assert.Equal(t, ErrFloatOverflow, err)
				})

				t.Run("negative", func(t *testing.T) {
					_, err := Mul(Integer(2), Float(-math.MaxFloat64))
					assert.Equal(t, ErrFloatOverflow, err)
				})
			})
		})
	})

	t.Run("float", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				r, err := Mul(Float(1), Integer(1))
				assert.NoError(t, err)
				assert.Equal(t, Float(1), r)
			})

			t.Run("overflow", func(t *testing.T) {
				t.Run("positive", func(t *testing.T) {
					_, err := Mul(Float(math.MaxFloat64), Integer(2))
					assert.Equal(t, ErrFloatOverflow, err)
				})

				t.Run("negative", func(t *testing.T) {
					_, err := Mul(Float(-math.MaxFloat64), Integer(2))
					assert.Equal(t, ErrFloatOverflow, err)
				})
			})
		})

		t.Run("float", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				r, err := Mul(Float(1), Float(1))
				assert.NoError(t, err)
				assert.Equal(t, Float(1), r)
			})

			t.Run("overflow", func(t *testing.T) {
				t.Run("positive", func(t *testing.T) {
					_, err := Mul(Float(2), Float(math.MaxFloat64))
					assert.Equal(t, ErrFloatOverflow, err)
				})

				t.Run("negative", func(t *testing.T) {
					_, err := Mul(Float(-2), Float(math.MaxFloat64))
					assert.Equal(t, ErrFloatOverflow, err)
				})
			})

			t.Run("underflow", func(t *testing.T) {
				_, err := Mul(Float(0.5), Float(math.SmallestNonzeroFloat64))
				assert.Equal(t, ErrUnderflow, err)
			})
		})
	})

	t.Run("not a number", func(t *testing.T) {
		_, err := Mul(mockNumber{}, Integer(0))
		assert.Equal(t, ErrUndefined, err)
	})
}

func TestIntDiv(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				r, err := IntDiv(Integer(1), Integer(1))
				assert.NoError(t, err)
				assert.Equal(t, Integer(1), r)
			})

			t.Run("overflow", func(t *testing.T) {
				_, err := IntDiv(Integer(math.MinInt64), Integer(-1))
				assert.Equal(t, ErrIntOverflow, err)
			})

			t.Run("divided by zero", func(t *testing.T) {
				_, err := IntDiv(Integer(1), Integer(0))
				assert.Equal(t, ErrZeroDivisor, err)
			})
		})

		t.Run("not an integer", func(t *testing.T) {
			_, err := IntDiv(Integer(1), Float(1))
			assert.Equal(t, TypeErrorInteger(Float(1)), err)
		})
	})

	t.Run("not an integer", func(t *testing.T) {
		_, err := IntDiv(Float(1), Integer(1))
		assert.Equal(t, TypeErrorInteger(Float(1)), err)
	})
}

func TestDiv(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				r, err := Div(Integer(1), Integer(1))
				assert.NoError(t, err)
				assert.Equal(t, Float(1), r)
			})

			t.Run("divide by zero", func(t *testing.T) {
				_, err := Div(Integer(1), Integer(0))
				assert.Equal(t, ErrZeroDivisor, err)
			})
		})

		t.Run("float", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				r, err := Div(Integer(1), Float(1))
				assert.NoError(t, err)
				assert.Equal(t, Float(1), r)
			})

			t.Run("divide by zero", func(t *testing.T) {
				_, err := Div(Integer(1), Float(0))
				assert.Equal(t, ErrZeroDivisor, err)
			})
		})
	})

	t.Run("float", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				r, err := Div(Float(1), Integer(1))
				assert.NoError(t, err)
				assert.Equal(t, Float(1), r)
			})

			t.Run("divide by zero", func(t *testing.T) {
				_, err := Div(Float(1), Integer(0))
				assert.Equal(t, ErrZeroDivisor, err)
			})

			t.Run("underflow", func(t *testing.T) {
				_, err := Div(Float(math.SmallestNonzeroFloat64), Integer(2))
				assert.Equal(t, ErrUnderflow, err)
			})
		})

		t.Run("float", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				r, err := Div(Float(1), Float(1))
				assert.NoError(t, err)
				assert.Equal(t, Float(1), r)
			})

			t.Run("divide by zero", func(t *testing.T) {
				_, err := Div(Float(1), Float(0))
				assert.Equal(t, ErrZeroDivisor, err)
			})

			t.Run("overflow", func(t *testing.T) {
				t.Run("positive", func(t *testing.T) {
					_, err := Div(Float(math.MaxFloat64), Float(0.5))
					assert.Equal(t, ErrFloatOverflow, err)
				})

				t.Run("negative", func(t *testing.T) {
					_, err := Div(Float(-math.MaxFloat64), Float(0.5))
					assert.Equal(t, ErrFloatOverflow, err)
				})
			})

			t.Run("underflow", func(t *testing.T) {
				_, err := Div(Float(math.SmallestNonzeroFloat64), Float(2))
				assert.Equal(t, ErrUnderflow, err)
			})
		})
	})

	t.Run("not a number", func(t *testing.T) {
		_, err := Div(mockNumber{}, Integer(1))
		assert.Equal(t, ErrUndefined, err)
	})
}

func TestRem(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				r, err := Rem(Integer(1), Integer(1))
				assert.NoError(t, err)
				assert.Equal(t, Integer(0), r)
			})

			t.Run("divided by zero", func(t *testing.T) {
				_, err := Rem(Integer(1), Integer(0))
				assert.Equal(t, ErrZeroDivisor, err)
			})
		})

		t.Run("not an integer", func(t *testing.T) {
			_, err := Rem(Integer(1), mockNumber{})
			assert.Equal(t, TypeErrorInteger(mockNumber{}), err)
		})
	})

	t.Run("not an integer", func(t *testing.T) {
		_, err := Rem(mockNumber{}, Integer(1))
		assert.Equal(t, TypeErrorInteger(mockNumber{}), err)
	})
}

func TestMod(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				r, err := Mod(Integer(1), Integer(1))
				assert.NoError(t, err)
				assert.Equal(t, Integer(0), r)
			})

			t.Run("divided by zero", func(t *testing.T) {
				_, err := Mod(Integer(1), Integer(0))
				assert.Equal(t, ErrZeroDivisor, err)
			})
		})

		t.Run("not an integer", func(t *testing.T) {
			_, err := Mod(Integer(1), mockNumber{})
			assert.Equal(t, TypeErrorInteger(mockNumber{}), err)
		})
	})

	t.Run("not an integer", func(t *testing.T) {
		_, err := Mod(mockNumber{}, Integer(1))
		assert.Equal(t, TypeErrorInteger(mockNumber{}), err)
	})
}

func TestPos(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		r, err := Pos(Integer(1))
		assert.NoError(t, err)
		assert.Equal(t, Integer(1), r)
	})

	t.Run("float", func(t *testing.T) {
		r, err := Pos(Float(1))
		assert.NoError(t, err)
		assert.Equal(t, Float(1), r)
	})

	t.Run("not a number", func(t *testing.T) {
		_, err := Pos(mockNumber{})
		assert.Equal(t, ErrUndefined, err)
	})
}

func TestNeg(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			r, err := Neg(Integer(1))
			assert.NoError(t, err)
			assert.Equal(t, Integer(-1), r)
		})

		t.Run("overflow", func(t *testing.T) {
			_, err := Neg(Integer(math.MinInt64))
			assert.Equal(t, ErrIntOverflow, err)
		})
	})

	t.Run("float", func(t *testing.T) {
		r, err := Neg(Float(1))
		assert.NoError(t, err)
		assert.Equal(t, Float(-1), r)
	})

	t.Run("not a number", func(t *testing.T) {
		_, err := Neg(mockNumber{})
		assert.Equal(t, ErrUndefined, err)
	})
}

func TestAbs(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			t.Run("positive", func(t *testing.T) {
				r, err := Abs(Integer(1))
				assert.NoError(t, err)
				assert.Equal(t, Integer(1), r)
			})

			t.Run("negative", func(t *testing.T) {
				r, err := Abs(Integer(-1))
				assert.NoError(t, err)
				assert.Equal(t, Integer(1), r)
			})
		})

		t.Run("overflow", func(t *testing.T) {
			_, err := Abs(Integer(math.MinInt64))
			assert.Equal(t, ErrIntOverflow, err)
		})
	})

	t.Run("float", func(t *testing.T) {
		r, err := Abs(Float(-1))
		assert.NoError(t, err)
		assert.Equal(t, Float(1), r)
	})

	t.Run("not a number", func(t *testing.T) {
		_, err := Abs(mockNumber{})
		assert.Equal(t, ErrUndefined, err)
	})
}

func TestSign(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		t.Run("positive", func(t *testing.T) {
			r, err := Sign(Integer(1))
			assert.NoError(t, err)
			assert.Equal(t, Integer(1), r)
		})

		t.Run("zero", func(t *testing.T) {
			r, err := Sign(Integer(0))
			assert.NoError(t, err)
			assert.Equal(t, Integer(0), r)
		})

		t.Run("negative", func(t *testing.T) {
			r, err := Sign(Integer(-1))
			assert.NoError(t, err)
			assert.Equal(t, Integer(-1), r)
		})
	})

	t.Run("float", func(t *testing.T) {
		t.Run("positive", func(t *testing.T) {
			r, err := Sign(Float(1))
			assert.NoError(t, err)
			assert.Equal(t, Float(1), r)
		})

		t.Run("zero", func(t *testing.T) {
			r, err := Sign(Float(0))
			assert.NoError(t, err)
			assert.Equal(t, Float(0), r)
		})

		t.Run("negative", func(t *testing.T) {
			r, err := Sign(Float(-1))
			assert.NoError(t, err)
			assert.Equal(t, Float(-1), r)
		})
	})

	t.Run("not a number", func(t *testing.T) {
		_, err := Sign(mockNumber{})
		assert.Equal(t, ErrUndefined, err)
	})
}

func TestFloatIntegerPart(t *testing.T) {
	t.Run("float", func(t *testing.T) {
		r, err := FloatIntegerPart(Float(1.23))
		assert.NoError(t, err)
		assert.Equal(t, Float(1), r)
	})

	t.Run("not a float", func(t *testing.T) {
		_, err := FloatIntegerPart(Integer(1))
		assert.Equal(t, TypeErrorFloat(Integer(1)), err)
	})
}

func TestFloatFractionalPart(t *testing.T) {
	t.Run("float", func(t *testing.T) {
		r, err := FloatFractionalPart(Float(1.23))
		assert.NoError(t, err)
		assert.Equal(t, Float(0.22999999999999998), r)
	})

	t.Run("not a float", func(t *testing.T) {
		_, err := FloatFractionalPart(Integer(1))
		assert.Equal(t, TypeErrorFloat(Integer(1)), err)
	})
}

func TestAsFloat(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		r, err := AsFloat(Integer(1))
		assert.NoError(t, err)
		assert.Equal(t, Float(1), r)
	})

	t.Run("float", func(t *testing.T) {
		r, err := AsFloat(Float(1))
		assert.NoError(t, err)
		assert.Equal(t, Float(1), r)
	})

	t.Run("not a number", func(t *testing.T) {
		_, err := AsFloat(mockNumber{})
		assert.Equal(t, ErrUndefined, err)
	})
}

func TestFloor(t *testing.T) {
	t.Run("float", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			r, err := Floor(Float(1.9))
			assert.NoError(t, err)
			assert.Equal(t, Integer(1), r)
		})

		t.Run("overflow", func(t *testing.T) {
			t.Run("positive", func(t *testing.T) {
				_, err := Floor(2 * Float(math.MaxInt64))
				assert.Equal(t, ErrIntOverflow, err)
			})

			t.Run("negative", func(t *testing.T) {
				_, err := Floor(2 * Float(math.MinInt64))
				assert.Equal(t, ErrIntOverflow, err)
			})
		})
	})

	t.Run("not a float", func(t *testing.T) {
		_, err := Floor(Integer(1))
		assert.Equal(t, TypeErrorFloat(Integer(1)), err)
	})
}

func TestTruncate(t *testing.T) {
	t.Run("float", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			r, err := Truncate(Float(1.9))
			assert.NoError(t, err)
			assert.Equal(t, Integer(1), r)
		})

		t.Run("overflow", func(t *testing.T) {
			t.Run("positive", func(t *testing.T) {
				_, err := Truncate(2 * Float(math.MaxInt64))
				assert.Equal(t, ErrIntOverflow, err)
			})

			t.Run("negative", func(t *testing.T) {
				_, err := Truncate(2 * Float(math.MinInt64))
				assert.Equal(t, ErrIntOverflow, err)
			})
		})
	})

	t.Run("not a float", func(t *testing.T) {
		_, err := Truncate(Integer(1))
		assert.Equal(t, TypeErrorFloat(Integer(1)), err)
	})
}

func TestRound(t *testing.T) {
	t.Run("float", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			r, err := Round(Float(1.9))
			assert.NoError(t, err)
			assert.Equal(t, Integer(2), r)
		})

		t.Run("overflow", func(t *testing.T) {
			t.Run("positive", func(t *testing.T) {
				_, err := Round(2 * Float(math.MaxInt64))
				assert.Equal(t, ErrIntOverflow, err)
			})

			t.Run("negative", func(t *testing.T) {
				_, err := Round(2 * Float(math.MinInt64))
				assert.Equal(t, ErrIntOverflow, err)
			})
		})
	})

	t.Run("not a float", func(t *testing.T) {
		_, err := Round(Integer(1))
		assert.Equal(t, TypeErrorFloat(Integer(1)), err)
	})
}

func TestCeiling(t *testing.T) {
	t.Run("float", func(t *testing.T) {
		t.Run("ok", func(t *testing.T) {
			r, err := Ceiling(Float(1.9))
			assert.NoError(t, err)
			assert.Equal(t, Integer(2), r)
		})

		t.Run("overflow", func(t *testing.T) {
			t.Run("positive", func(t *testing.T) {
				_, err := Ceiling(2 * Float(math.MaxInt64))
				assert.Equal(t, ErrIntOverflow, err)
			})

			t.Run("negative", func(t *testing.T) {
				_, err := Ceiling(2 * Float(math.MinInt64))
				assert.Equal(t, ErrIntOverflow, err)
			})
		})
	})

	t.Run("not a float", func(t *testing.T) {
		_, err := Ceiling(Integer(1))
		assert.Equal(t, TypeErrorFloat(Integer(1)), err)
	})
}

func TestPower(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			r, err := Power(Integer(1), Integer(1))
			assert.NoError(t, err)
			assert.Equal(t, Float(1), r)
		})

		t.Run("float", func(t *testing.T) {
			r, err := Power(Integer(1), Float(1))
			assert.NoError(t, err)
			assert.Equal(t, Float(1), r)
		})

		t.Run("not a number", func(t *testing.T) {
			_, err := Power(Integer(1), mockNumber{})
			assert.Equal(t, ErrUndefined, err)
		})
	})

	t.Run("float", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			r, err := Power(Float(1), Integer(1))
			assert.NoError(t, err)
			assert.Equal(t, Float(1), r)
		})

		t.Run("float", func(t *testing.T) {
			r, err := Power(Float(1), Float(1))
			assert.NoError(t, err)
			assert.Equal(t, Float(1), r)
		})

		t.Run("not a number", func(t *testing.T) {
			_, err := Power(Float(1), mockNumber{})
			assert.Equal(t, ErrUndefined, err)
		})
	})

	t.Run("not a number", func(t *testing.T) {
		_, err := Power(mockNumber{}, Float(1))
		assert.Equal(t, ErrUndefined, err)
	})

	t.Run("overflow", func(t *testing.T) {
		_, err := Power(Float(math.MaxFloat64), Float(2))
		assert.Equal(t, ErrFloatOverflow, err)
	})

	t.Run("underflow", func(t *testing.T) {
		_, err := Power(Float(math.SmallestNonzeroFloat64), Float(2))
		assert.Equal(t, ErrUnderflow, err)
	})

	t.Run("undefined", func(t *testing.T) {
		t.Run("vx is negative and vy is not an integer", func(t *testing.T) {
			_, err := Power(Integer(-1), Float(1.1))
			assert.Equal(t, ErrUndefined, err)
		})

		t.Run("vx is zero and vy is negative", func(t *testing.T) {
			_, err := Power(Integer(0), Float(-1))
			assert.Equal(t, ErrUndefined, err)
		})
	})
}

func TestSin(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		r, err := Sin(Integer(0))
		assert.NoError(t, err)
		assert.Equal(t, Float(0), r)
	})

	t.Run("float", func(t *testing.T) {
		r, err := Sin(Float(0))
		assert.NoError(t, err)
		assert.Equal(t, Float(0), r)
	})

	t.Run("not a number", func(t *testing.T) {
		_, err := Sin(mockNumber{})
		assert.Equal(t, ErrUndefined, err)
	})
}

func TestCos(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		r, err := Cos(Integer(0))
		assert.NoError(t, err)
		assert.Equal(t, Float(1), r)
	})

	t.Run("float", func(t *testing.T) {
		r, err := Cos(Float(0))
		assert.NoError(t, err)
		assert.Equal(t, Float(1), r)
	})

	t.Run("not a number", func(t *testing.T) {
		_, err := Cos(mockNumber{})
		assert.Equal(t, ErrUndefined, err)
	})
}

func TestAtan(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		r, err := Atan(Integer(0))
		assert.NoError(t, err)
		assert.Equal(t, Float(0), r)
	})

	t.Run("float", func(t *testing.T) {
		r, err := Atan(Float(0))
		assert.NoError(t, err)
		assert.Equal(t, Float(0), r)
	})

	t.Run("not a number", func(t *testing.T) {
		_, err := Atan(mockNumber{})
		assert.Equal(t, ErrUndefined, err)
	})
}

func TestExp(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		r, err := Exp(Integer(0))
		assert.NoError(t, err)
		assert.Equal(t, Float(1), r)
	})

	t.Run("float", func(t *testing.T) {
		r, err := Exp(Float(0))
		assert.NoError(t, err)
		assert.Equal(t, Float(1), r)
	})

	t.Run("not a number", func(t *testing.T) {
		_, err := Exp(mockNumber{})
		assert.Equal(t, ErrUndefined, err)
	})

	t.Run("overflow", func(t *testing.T) {
		_, err := Exp(Float(math.MaxFloat64))
		assert.Equal(t, ErrFloatOverflow, err)
	})

	t.Run("underflow", func(t *testing.T) {
		_, err := Exp(Float(-math.MaxFloat64))
		assert.Equal(t, ErrUnderflow, err)
	})
}

func TestLog(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		r, err := Log(Integer(1))
		assert.NoError(t, err)
		assert.Equal(t, Float(0), r)
	})

	t.Run("float", func(t *testing.T) {
		r, err := Log(Float(1))
		assert.NoError(t, err)
		assert.Equal(t, Float(0), r)
	})

	t.Run("not a number", func(t *testing.T) {
		_, err := Log(mockNumber{})
		assert.Equal(t, ErrUndefined, err)
	})

	t.Run("undefined", func(t *testing.T) {
		_, err := Log(Float(0))
		assert.Equal(t, ErrUndefined, err)
	})
}

func TestSqrt(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		r, err := Sqrt(Integer(0))
		assert.NoError(t, err)
		assert.Equal(t, Float(0), r)
	})

	t.Run("float", func(t *testing.T) {
		r, err := Sqrt(Float(0))
		assert.NoError(t, err)
		assert.Equal(t, Float(0), r)
	})

	t.Run("not a number", func(t *testing.T) {
		_, err := Sqrt(mockNumber{})
		assert.Equal(t, ErrUndefined, err)
	})

	t.Run("undefined", func(t *testing.T) {
		_, err := Sqrt(Float(-1))
		assert.Equal(t, ErrUndefined, err)
	})
}

func TestBitwiseRightShift(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			r, err := BitwiseRightShift(Integer(16), Integer(2))
			assert.NoError(t, err)
			assert.Equal(t, Integer(4), r)
		})

		t.Run("not an integer", func(t *testing.T) {
			_, err := BitwiseRightShift(Integer(16), Float(2))
			assert.Equal(t, TypeErrorInteger(Float(2)), err)
		})
	})

	t.Run("not an integer", func(t *testing.T) {
		_, err := BitwiseRightShift(Float(16), Integer(2))
		assert.Equal(t, TypeErrorInteger(Float(16)), err)
	})
}

func TestBitwiseLeftShift(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			r, err := BitwiseLeftShift(Integer(16), Integer(2))
			assert.NoError(t, err)
			assert.Equal(t, Integer(64), r)
		})

		t.Run("not an integer", func(t *testing.T) {
			_, err := BitwiseLeftShift(Integer(16), Float(2))
			assert.Equal(t, TypeErrorInteger(Float(2)), err)
		})
	})

	t.Run("not an integer", func(t *testing.T) {
		_, err := BitwiseLeftShift(Float(16), Integer(2))
		assert.Equal(t, TypeErrorInteger(Float(16)), err)
	})
}

func TestBitwiseAnd(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			r, err := BitwiseAnd(Integer(10), Integer(12))
			assert.NoError(t, err)
			assert.Equal(t, Integer(8), r)
		})

		t.Run("not an integer", func(t *testing.T) {
			_, err := BitwiseAnd(Integer(10), Float(12))
			assert.Equal(t, TypeErrorInteger(Float(12)), err)
		})
	})

	t.Run("not an integer", func(t *testing.T) {
		_, err := BitwiseAnd(Float(10), Integer(12))
		assert.Equal(t, TypeErrorInteger(Float(10)), err)
	})
}

func TestBitwiseOr(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			r, err := BitwiseOr(Integer(10), Integer(12))
			assert.NoError(t, err)
			assert.Equal(t, Integer(14), r)
		})

		t.Run("not an integer", func(t *testing.T) {
			_, err := BitwiseOr(Integer(10), Float(12))
			assert.Equal(t, TypeErrorInteger(Float(12)), err)
		})
	})

	t.Run("not an integer", func(t *testing.T) {
		_, err := BitwiseOr(Float(10), Integer(12))
		assert.Equal(t, TypeErrorInteger(Float(10)), err)
	})
}

func TestBitwiseComplement(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		r, err := BitwiseComplement(Integer(10))
		assert.NoError(t, err)
		r, err = BitwiseComplement(r)
		assert.NoError(t, err)
		assert.Equal(t, Integer(10), r)
	})

	t.Run("not an integer", func(t *testing.T) {
		_, err := BitwiseComplement(Float(10))
		assert.Equal(t, TypeErrorInteger(Float(10)), err)
	})
}

func TestIntFloorDiv(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			t.Run("ok", func(t *testing.T) {
				r, err := IntFloorDiv(Integer(1), Integer(1))
				assert.NoError(t, err)
				assert.Equal(t, Integer(1), r)
			})

			t.Run("overflow", func(t *testing.T) {
				_, err := IntFloorDiv(Integer(math.MinInt64), Integer(-1))
				assert.Equal(t, ErrIntOverflow, err)
			})

			t.Run("divided by zero", func(t *testing.T) {
				_, err := IntFloorDiv(Integer(1), Integer(0))
				assert.Equal(t, ErrZeroDivisor, err)
			})
		})

		t.Run("not an integer", func(t *testing.T) {
			_, err := IntFloorDiv(Integer(1), Float(1))
			assert.Equal(t, TypeErrorInteger(Float(1)), err)
		})
	})

	t.Run("not an integer", func(t *testing.T) {
		_, err := IntFloorDiv(Float(1), Integer(1))
		assert.Equal(t, TypeErrorInteger(Float(1)), err)
	})
}

func TestMax(t *testing.T) {
	t.Run("integer", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			r, err := Max(Integer(1), Integer(2))
			assert.NoError(t, err)
			assert.Equal(t, Integer(2), r)

			r, err = Max(Integer(1), Integer(1))
			assert.NoError(t, err)
			assert.Equal(t, Integer(1), r)
		})

		t.Run("float", func(t *testing.T) {
			r, err := Max(Integer(1), Float(2))
			assert.NoError(t, err)
			assert.Equal(t, Float(2), r)

			r, err = Max(Integer(1), Float(1))
			assert.NoError(t, err)
			assert.Equal(t, Integer(1), r)
		})

		t.Run("not a number", func(t *testing.T) {
			_, err := Max(Integer(1), mockNumber{})
			assert.Equal(t, ErrUndefined, err)
		})
	})

	t.Run("float", func(t *testing.T) {
		t.Run("integer", func(t *testing.T) {
			r, err := Max(Float(1), Integer(2))
			assert.NoError(t, err)
			assert.Equal(t, Integer(2), r)

			r, err = Max(Float(1), Integer(1))
			assert.NoError(t, err)
			assert.Equal(t, Float(1), r)
		})

		t.Run("float", func(t *testing.T) {
			r, err := Max(Float(1), Float(2))
			assert.NoError(t, err)
			assert.Equal(t, Float(2), r)

			r, err = Max(Float(1), Float(1))
			assert.NoError(t, err)
			assert.Equal(t, Float(1), r)
		})

		t.Run("not a number", func(t *testing.T) {
			_, err := Max(Float(1), mockNumber{})
			assert.Equal(t, ErrUndefined, err)
		})
	})

	t.Run("not a number", func(t *testing.T) {
		_, err := Max(mockNumber{}, Integer(1))
		assert.Equal(t, ErrUndefined, err)
	})
}

type mockNumber struct {
	mock.Mock
}

func (m mockNumber) Unify(t Term, occursCheck bool, env *Env) (*Env, bool) {
	args := m.Called(t, occursCheck, env)
	return args.Get(0).(*Env), args.Bool(1)
}

func (m mockNumber) Unparse(emit func(Token), env *Env, opts ...WriteOption) {
	_ = m.Called(emit, env, opts)
}

func (m mockNumber) Compare(t Term, env *Env) int64 {
	args := m.Called(t, env)
	return int64(args.Int(0))
}

func (m mockNumber) number() {
	_ = m.Called()
}
