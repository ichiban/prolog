package engine

import (
	"context"
	"math"
)

// EvaluableFunctors is a set of unary/binary functions.
type EvaluableFunctors struct {
	Unary  map[Atom]func(x Term, env *Env) (Term, error)
	Binary map[Atom]func(x, y Term, env *Env) (Term, error)
}

// Is evaluates expression and unifies the result with result.
func (fs EvaluableFunctors) Is(result, expression Term, k func(*Env) *Promise, env *Env) *Promise {
	v, err := fs.eval(expression, env)
	if err != nil {
		return Error(err)
	}
	return Delay(func(context.Context) *Promise {
		return Unify(result, v, k, env)
	})
}

// Equal succeeds iff lhs equals to rhs.
func (fs EvaluableFunctors) Equal(lhs, rhs Term, k func(*Env) *Promise, env *Env) *Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i == j
	}, func(f Float, g Float) bool {
		return f == g
	}, env)
}

// NotEqual succeeds iff lhs doesn't equal to rhs.
func (fs EvaluableFunctors) NotEqual(lhs, rhs Term, k func(*Env) *Promise, env *Env) *Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i != j
	}, func(f Float, g Float) bool {
		return f != g
	}, env)
}

// LessThan succeeds iff lhs is less than rhs.
func (fs EvaluableFunctors) LessThan(lhs, rhs Term, k func(*Env) *Promise, env *Env) *Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i < j
	}, func(f Float, g Float) bool {
		return f < g
	}, env)
}

// GreaterThan succeeds iff lhs is greater than rhs.
func (fs EvaluableFunctors) GreaterThan(lhs, rhs Term, k func(*Env) *Promise, env *Env) *Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i > j
	}, func(f Float, g Float) bool {
		return f > g
	}, env)
}

// LessThanOrEqual succeeds iff lhs is less than or equal to rhs.
func (fs EvaluableFunctors) LessThanOrEqual(lhs, rhs Term, k func(*Env) *Promise, env *Env) *Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i <= j
	}, func(f Float, g Float) bool {
		return f <= g
	}, env)
}

// GreaterThanOrEqual succeeds iff lhs is greater than or equal to rhs.
func (fs EvaluableFunctors) GreaterThanOrEqual(lhs, rhs Term, k func(*Env) *Promise, env *Env) *Promise {
	return fs.compare(lhs, rhs, k, func(i Integer, j Integer) bool {
		return i >= j
	}, func(f Float, g Float) bool {
		return f >= g
	}, env)
}

func (fs EvaluableFunctors) compare(lhs, rhs Term, k func(*Env) *Promise, pi func(Integer, Integer) bool, pf func(Float, Float) bool, env *Env) *Promise {
	l, err := fs.eval(lhs, env)
	if err != nil {
		return Error(err)
	}

	r, err := fs.eval(rhs, env)
	if err != nil {
		return Error(err)
	}

	switch l := l.(type) {
	case Integer:
		switch r := r.(type) {
		case Integer:
			if !pi(l, r) {
				return Bool(false)
			}
			return k(env)
		case Float:
			if !pf(Float(l), r) {
				return Bool(false)
			}
			return k(env)
		default:
			return Error(typeErrorEvaluable(r))
		}
	case Float:
		switch r := r.(type) {
		case Integer:
			if !pf(l, Float(r)) {
				return Bool(false)
			}
			return k(env)
		case Float:
			if !pf(l, r) {
				return Bool(false)
			}
			return k(env)
		default:
			return Error(typeErrorEvaluable(r))
		}
	default:
		return Error(typeErrorEvaluable(l))
	}
}

func (fs EvaluableFunctors) eval(expression Term, env *Env) (_ Term, err error) {
	defer func() {
		if r := recover(); r != nil {
			if e, _ := r.(error); e.Error() == "runtime error: integer divide by zero" {
				err = evaluationErrorZeroDivisor()
				return
			}
			panic(r)
		}
	}()

	switch t := env.Resolve(expression).(type) {
	case Variable:
		return nil, ErrInstantiation
	case Atom: // TODO: constants?
		return nil, typeErrorEvaluable(&Compound{
			Functor: "/",
			Args:    []Term{t, Integer(0)},
		})
	case Integer, Float:
		return t, nil
	case *Compound:
		switch len(t.Args) {
		case 1:
			f, ok := fs.Unary[t.Functor]
			if !ok {
				return nil, typeErrorEvaluable(&Compound{
					Functor: "/",
					Args: []Term{
						t.Functor,
						Integer(1),
					},
				})
			}
			x, err := fs.eval(t.Args[0], env)
			if err != nil {
				return nil, err
			}
			return f(x, env)
		case 2:
			f, ok := fs.Binary[t.Functor]
			if !ok {
				return nil, typeErrorEvaluable(&Compound{
					Functor: "/",
					Args: []Term{
						t.Functor,
						Integer(2),
					},
				})
			}
			x, err := fs.eval(t.Args[0], env)
			if err != nil {
				return nil, err
			}
			y, err := fs.eval(t.Args[1], env)
			if err != nil {
				return nil, err
			}
			return f(x, y, env)
		}
	}
	return nil, typeErrorEvaluable(expression)
}

// DefaultEvaluableFunctors is a EvaluableFunctors with builtin functions.
var DefaultEvaluableFunctors = EvaluableFunctors{
	Unary: map[Atom]func(Term, *Env) (Term, error){
		`+`:        Pos,
		`-`:        unaryNumber(func(i int64) int64 { return -1 * i }, func(n float64) float64 { return -1 * n }),
		`abs`:      unaryFloat(math.Abs),
		`atan`:     unaryFloat(math.Atan),
		`ceiling`:  unaryFloat(math.Ceil),
		`cos`:      unaryFloat(math.Cos),
		`exp`:      unaryFloat(math.Exp),
		`sqrt`:     unaryFloat(math.Sqrt),
		`sign`:     unaryNumber(sgn, sgnf),
		`float`:    unaryFloat(func(n float64) float64 { return n }),
		`floor`:    unaryFloat(math.Floor),
		`log`:      unaryFloat(math.Log),
		`sin`:      unaryFloat(math.Sin),
		`truncate`: unaryFloat(math.Trunc),
		`round`:    unaryFloat(math.Round),
		`\`:        unaryInteger(func(i int64) int64 { return ^i }),
	},
	Binary: map[Atom]func(Term, Term, *Env) (Term, error){
		`+`:   binaryNumber(func(i, j int64) int64 { return i + j }, func(n, m float64) float64 { return n + m }),
		`-`:   binaryNumber(func(i, j int64) int64 { return i - j }, func(n, m float64) float64 { return n - m }),
		`*`:   binaryNumber(func(i, j int64) int64 { return i * j }, func(n, m float64) float64 { return n * m }),
		`/`:   binaryFloat(func(n float64, m float64) float64 { return n / m }),
		`//`:  binaryInteger(func(i, j int64) int64 { return i / j }),
		`div`: IntFloorDiv,
		`rem`: binaryInteger(func(i, j int64) int64 { return i % j }),
		`mod`: binaryInteger(func(i, j int64) int64 { return (i%j + j) % j }),
		`**`:  binaryFloat(math.Pow),
		`>>`:  binaryInteger(func(i, j int64) int64 { return i >> j }),
		`<<`:  binaryInteger(func(i, j int64) int64 { return i << j }),
		`/\`:  binaryInteger(func(i, j int64) int64 { return i & j }),
		`\/`:  binaryInteger(func(i, j int64) int64 { return i | j }),
	},
}

func sgn(i int64) int64 {
	return i>>63 | int64(uint64(-i)>>63)
}

func sgnf(f float64) float64 {
	switch {
	case f < 0:
		return -1
	case f == 0:
		return 0
	case f > 0:
		return 1
	default: // NaN
		return f
	}
}

func unaryInteger(f func(i int64) int64) func(Term, *Env) (Term, error) {
	return func(x Term, env *Env) (Term, error) {
		i, ok := env.Resolve(x).(Integer)
		if !ok {
			return nil, typeErrorInteger(x)
		}

		return Integer(f(int64(i))), nil
	}
}

func binaryInteger(f func(i, j int64) int64) func(Term, Term, *Env) (Term, error) {
	return func(x, y Term, env *Env) (Term, error) {
		i, ok := env.Resolve(x).(Integer)
		if !ok {
			return nil, typeErrorInteger(x)
		}

		j, ok := env.Resolve(y).(Integer)
		if !ok {
			return nil, typeErrorInteger(y)
		}

		return Integer(f(int64(i), int64(j))), nil
	}
}

func unaryFloat(f func(n float64) float64) func(Term, *Env) (Term, error) {
	return func(x Term, env *Env) (Term, error) {
		switch x := env.Resolve(x).(type) {
		case Integer:
			return Float(f(float64(x))), nil
		case Float:
			return Float(f(float64(x))), nil
		default:
			return nil, typeErrorEvaluable(x)
		}
	}
}

func binaryFloat(f func(n float64, m float64) float64) func(Term, Term, *Env) (Term, error) {
	return func(x, y Term, env *Env) (Term, error) {
		switch x := env.Resolve(x).(type) {
		case Integer:
			switch y := env.Resolve(y).(type) {
			case Integer:
				return Float(f(float64(x), float64(y))), nil
			case Float:
				return Float(f(float64(x), float64(y))), nil
			default:
				return nil, typeErrorEvaluable(y)
			}
		case Float:
			switch y := env.Resolve(y).(type) {
			case Integer:
				return Float(f(float64(x), float64(y))), nil
			case Float:
				return Float(f(float64(x), float64(y))), nil
			default:
				return nil, typeErrorEvaluable(y)
			}
		default:
			return nil, typeErrorEvaluable(x)
		}
	}
}

func unaryNumber(fi func(i int64) int64, ff func(n float64) float64) func(Term, *Env) (Term, error) {
	return func(x Term, env *Env) (Term, error) {
		switch x := env.Resolve(x).(type) {
		case Integer:
			return Integer(fi(int64(x))), nil
		case Float:
			return Float(ff(float64(x))), nil
		default:
			return nil, typeErrorEvaluable(x)
		}
	}
}

func binaryNumber(fi func(i, j int64) int64, ff func(n, m float64) float64) func(Term, Term, *Env) (Term, error) {
	return func(x, y Term, env *Env) (Term, error) {
		switch x := env.Resolve(x).(type) {
		case Integer:
			switch y := env.Resolve(y).(type) {
			case Integer:
				return Integer(fi(int64(x), int64(y))), nil
			case Float:
				return Float(ff(float64(x), float64(y))), nil
			default:
				return nil, typeErrorEvaluable(y)
			}
		case Float:
			switch y := env.Resolve(y).(type) {
			case Integer:
				return Float(ff(float64(x), float64(y))), nil
			case Float:
				return Float(ff(float64(x), float64(y))), nil
			default:
				return nil, typeErrorEvaluable(y)
			}
		default:
			return nil, typeErrorEvaluable(x)
		}
	}
}

// Pos returns a number x.
func Pos(x Term, env *Env) (Term, error) {
	switch x := env.Resolve(x).(type) {
	case Variable:
		return nil, ErrInstantiation
	case Integer, Float:
		return x, nil
	default:
		return nil, typeErrorEvaluable(x)
	}
}

// IntFloorDiv returns flooring integer division of x by y.
func IntFloorDiv(x, y Term, env *Env) (Term, error) {
	switch x := env.Resolve(x).(type) {
	case Variable:
		return nil, ErrInstantiation
	case Integer:
		switch y := env.Resolve(y).(type) {
		case Variable:
			return nil, ErrInstantiation
		case Integer:
			if y == 0 {
				return nil, evaluationErrorZeroDivisor()
			}
			return Integer(math.Floor(float64(x) / float64(y))), nil
		default:
			return nil, typeErrorInteger(y)
		}
	default:
		return nil, typeErrorInteger(x)
	}
}
