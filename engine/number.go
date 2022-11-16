package engine

import (
	"errors"
	"math"
)

var (
	maxInt = Integer(math.MaxInt64)
	minInt = Integer(math.MinInt64)
)

// DefaultEvaluableFunctors is a EvaluableFunctors with builtin functions.
var DefaultEvaluableFunctors = EvaluableFunctors{
	Constant: map[Atom]Number{
		atomPi: Float(math.Pi),
	},
	Unary: map[Atom]func(Number) (Number, error){
		atomMinus:               Neg,
		atomAbs:                 Abs,
		atomSign:                Sign,
		atomFloatIntegerPart:    FloatIntegerPart,
		atomFloatFractionalPart: FloatFractionalPart,
		atomFloat:               AsFloat,
		atomFloor:               Floor,
		atomTruncate:            Truncate,
		atomRound:               Round,
		atomCeiling:             Ceiling,

		atomSin:  Sin,
		atomCos:  Cos,
		atomAtan: Atan,
		atomExp:  Exp,
		atomLog:  Log,
		atomSqrt: Sqrt,

		atomBackSlash: BitwiseComplement,

		atomPlus: Pos,
		atomAsin: Asin,
		atomAcos: Acos,
		atomTan:  Tan,
	},
	Binary: map[Atom]func(Number, Number) (Number, error){
		atomPlus:       Add,
		atomMinus:      Sub,
		atomAsterisk:   Mul,
		atomSlashSlash: IntDiv,
		atomSlash:      Div,
		atomRem:        Rem,
		atomMod:        Mod,

		atomAsteriskAsterisk: Power,

		atomBitwiseRightShift: BitwiseRightShift,
		atomBitwiseLeftShift:  BitwiseLeftShift,
		atomBitwiseAnd:        BitwiseAnd,
		atomBitwiseOr:         BitwiseOr,

		atomDiv:   IntFloorDiv,
		atomMax:   Max,
		atomMin:   Min,
		atomCaret: IntegerPower,
		atomAtan2: Atan2,
		atomXor:   Xor,
	},
}

// Number is a prolog number, either Integer or Float.
type Number interface {
	Term
	number()
}

// EvaluableFunctors is a set of unary/binary functions.
type EvaluableFunctors struct {
	// Constant is a set of constants.
	Constant map[Atom]Number

	// Unary is a set of functions of arity 1.
	Unary map[Atom]func(x Number) (Number, error)

	// Binary is a set of functions of arity 2.
	Binary map[Atom]func(x, y Number) (Number, error)
}

// Is evaluates expression and unifies the result with result.
func (e EvaluableFunctors) Is(result, expression Term, k func(*Env) *Promise, env *Env) *Promise {
	v, err := e.eval(expression, env)
	if err != nil {
		return Error(err)
	}
	return Unify(result, v, k, env)
}

// Equal succeeds iff e1 equals to e2.
func (e EvaluableFunctors) Equal(e1, e2 Term, k func(*Env) *Promise, env *Env) *Promise {
	ev1, err := e.eval(e1, env)
	if err != nil {
		return Error(err)
	}

	ev2, err := e.eval(e2, env)
	if err != nil {
		return Error(err)
	}

	var ok bool
	switch ev1 := ev1.(type) {
	case Integer:
		switch ev2 := ev2.(type) {
		case Integer:
			ok = eqI(ev1, ev2)
		case Float:
			ok = eqIF(ev1, ev2)
		}
	case Float:
		switch ev2 := ev2.(type) {
		case Integer:
			ok = eqFI(ev1, ev2)
		case Float:
			ok = eqF(ev1, ev2)
		}
	}
	if !ok {
		return Bool(false)
	}
	return k(env)
}

// NotEqual succeeds iff e1 doesn't equal to e2.
func (e EvaluableFunctors) NotEqual(e1, e2 Term, k func(*Env) *Promise, env *Env) *Promise {
	ev1, err := e.eval(e1, env)
	if err != nil {
		return Error(err)
	}

	ev2, err := e.eval(e2, env)
	if err != nil {
		return Error(err)
	}

	var ok bool
	switch ev1 := ev1.(type) {
	case Integer:
		switch ev2 := ev2.(type) {
		case Integer:
			ok = neqI(ev1, ev2)
		case Float:
			ok = neqIF(ev1, ev2)
		}
	case Float:
		switch ev2 := ev2.(type) {
		case Integer:
			ok = neqFI(ev1, ev2)
		case Float:
			ok = neqF(ev1, ev2)
		}
	}
	if !ok {
		return Bool(false)
	}
	return k(env)
}

// LessThan succeeds iff e1 is less than e2.
func (e EvaluableFunctors) LessThan(e1, e2 Term, k func(*Env) *Promise, env *Env) *Promise {
	ev1, err := e.eval(e1, env)
	if err != nil {
		return Error(err)
	}

	ev2, err := e.eval(e2, env)
	if err != nil {
		return Error(err)
	}

	var ok bool
	switch ev1 := ev1.(type) {
	case Integer:
		switch ev2 := ev2.(type) {
		case Integer:
			ok = lssI(ev1, ev2)
		case Float:
			ok = lssIF(ev1, ev2)
		}
	case Float:
		switch ev2 := ev2.(type) {
		case Integer:
			ok = lssFI(ev1, ev2)
		case Float:
			ok = lssF(ev1, ev2)
		}
	}
	if !ok {
		return Bool(false)
	}
	return k(env)
}

// GreaterThan succeeds iff e1 is greater than e2.
func (e EvaluableFunctors) GreaterThan(e1, e2 Term, k func(*Env) *Promise, env *Env) *Promise {
	ev1, err := e.eval(e1, env)
	if err != nil {
		return Error(err)
	}

	ev2, err := e.eval(e2, env)
	if err != nil {
		return Error(err)
	}

	var ok bool
	switch ev1 := ev1.(type) {
	case Integer:
		switch ev2 := ev2.(type) {
		case Integer:
			ok = gtrI(ev1, ev2)
		case Float:
			ok = gtrIF(ev1, ev2)
		}
	case Float:
		switch ev2 := ev2.(type) {
		case Integer:
			ok = gtrFI(ev1, ev2)
		case Float:
			ok = gtrF(ev1, ev2)
		}
	}
	if !ok {
		return Bool(false)
	}
	return k(env)
}

// LessThanOrEqual succeeds iff e1 is less than or equal to e2.
func (e EvaluableFunctors) LessThanOrEqual(e1, e2 Term, k func(*Env) *Promise, env *Env) *Promise {
	ev1, err := e.eval(e1, env)
	if err != nil {
		return Error(err)
	}

	ev2, err := e.eval(e2, env)
	if err != nil {
		return Error(err)
	}

	var ok bool
	switch ev1 := ev1.(type) {
	case Integer:
		switch ev2 := ev2.(type) {
		case Integer:
			ok = leqI(ev1, ev2)
		case Float:
			ok = leqIF(ev1, ev2)
		}
	case Float:
		switch ev2 := ev2.(type) {
		case Integer:
			ok = leqFI(ev1, ev2)
		case Float:
			ok = leqF(ev1, ev2)
		}
	}
	if !ok {
		return Bool(false)
	}
	return k(env)
}

// GreaterThanOrEqual succeeds iff e1 is greater than or equal to e2.
func (e EvaluableFunctors) GreaterThanOrEqual(e1, e2 Term, k func(*Env) *Promise, env *Env) *Promise {
	ev1, err := e.eval(e1, env)
	if err != nil {
		return Error(err)
	}

	ev2, err := e.eval(e2, env)
	if err != nil {
		return Error(err)
	}

	var ok bool
	switch ev1 := ev1.(type) {
	case Integer:
		switch ev2 := ev2.(type) {
		case Integer:
			ok = geqI(ev1, ev2)
		case Float:
			ok = geqIF(ev1, ev2)
		}
	case Float:
		switch ev2 := ev2.(type) {
		case Integer:
			ok = geqFI(ev1, ev2)
		case Float:
			ok = geqF(ev1, ev2)
		}
	}
	if !ok {
		return Bool(false)
	}
	return k(env)
}

func (e EvaluableFunctors) eval(expression Term, env *Env) (_ Number, err error) {
	defer func() {
		var ev ExceptionalValue
		if errors.As(err, &ev) {
			err = EvaluationError(ev, env)
		}
	}()

	switch t := env.Resolve(expression).(type) {
	case Variable:
		return nil, InstantiationError(env)
	case Atom:
		c, ok := e.Constant[t]
		if !ok {
			return nil, TypeError(ValidTypeEvaluable, atomSlash.Apply(t, Integer(0)), env)
		}
		return c, nil
	case Number:
		return t, nil
	case Compound:
		switch arity := t.Arity(); arity {
		case 1:
			f, ok := e.Unary[t.Functor()]
			if !ok {
				return nil, TypeError(ValidTypeEvaluable, atomSlash.Apply(t.Functor(), Integer(1)), env)
			}
			x, err := e.eval(t.Arg(0), env)
			if err != nil {
				return nil, err
			}
			return f(x)
		case 2:
			f, ok := e.Binary[t.Functor()]
			if !ok {
				return nil, TypeError(ValidTypeEvaluable, atomSlash.Apply(t.Functor(), Integer(2)), env)
			}
			x, err := e.eval(t.Arg(0), env)
			if err != nil {
				return nil, err
			}
			y, err := e.eval(t.Arg(1), env)
			if err != nil {
				return nil, err
			}
			return f(x, y)
		default:
			return nil, TypeError(ValidTypeEvaluable, atomSlash.Apply(t.Functor(), Integer(arity)), env)
		}
	default:
		return nil, TypeError(ValidTypeEvaluable, atomSlash.Apply(t, Integer(0)), env)
	}
}

// Add returns sum of 2 numbers.
func Add(x, y Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		switch y := y.(type) {
		case Integer:
			return addI(x, y)
		case Float:
			return addIF(x, y)
		}
	case Float:
		switch y := y.(type) {
		case Integer:
			return addFI(x, y)
		case Float:
			return addF(x, y)
		}
	}
	return nil, ExceptionalValueUndefined
}

// Sub returns subtraction of 2 numbers.
func Sub(x, y Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		switch y := y.(type) {
		case Integer:
			return subI(x, y)
		case Float:
			return subIF(x, y)
		}
	case Float:
		switch y := y.(type) {
		case Integer:
			return subFI(x, y)
		case Float:
			return subF(x, y)
		}
	}
	return nil, ExceptionalValueUndefined
}

// Mul returns multiplication of 2 numbers.
func Mul(x, y Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		switch y := y.(type) {
		case Integer:
			return mulI(x, y)
		case Float:
			return mulIF(x, y)
		}
	case Float:
		switch y := y.(type) {
		case Integer:
			return mulFI(x, y)
		case Float:
			return mulF(x, y)
		}
	}
	return nil, ExceptionalValueUndefined
}

// IntDiv returns integer division of 2 numbers.
func IntDiv(x, y Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		switch y := y.(type) {
		case Integer:
			return intDivI(x, y)
		default:
			return nil, TypeError(ValidTypeInteger, y, nil)
		}
	default:
		return nil, TypeError(ValidTypeInteger, x, nil)
	}
}

// Div returns division of 2 numbers
func Div(x, y Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		switch y := y.(type) {
		case Integer:
			return divII(x, y)
		case Float:
			return divIF(x, y)
		}
	case Float:
		switch y := y.(type) {
		case Integer:
			return divFI(x, y)
		case Float:
			return divF(x, y)
		}
	}
	return nil, ExceptionalValueUndefined
}

// Rem returns remainder of 2 numbers.
func Rem(x, y Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		switch y := y.(type) {
		case Integer:
			return remI(x, y)
		default:
			return nil, TypeError(ValidTypeInteger, y, nil)
		}
	default:
		return nil, TypeError(ValidTypeInteger, x, nil)
	}
}

// Mod returns modulo of 2 numbers.
func Mod(x, y Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		switch y := y.(type) {
		case Integer:
			return modI(x, y)
		default:
			return nil, TypeError(ValidTypeInteger, y, nil)
		}
	default:
		return nil, TypeError(ValidTypeInteger, x, nil)
	}
}

// Neg returns the negation of a number.
func Neg(x Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		return negI(x)
	case Float:
		return negF(x), nil
	default:
		return nil, ExceptionalValueUndefined
	}
}

// Abs returns the absolute value of x.
func Abs(x Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		return absI(x)
	case Float:
		return absF(x), nil
	default:
		return nil, ExceptionalValueUndefined
	}
}

// Sign returns +1, 0, or -1 depending on the sign of x.
func Sign(x Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		return signI(x), nil
	case Float:
		return signF(x), nil
	default:
		return nil, ExceptionalValueUndefined
	}
}

// FloatIntegerPart returns the integer part of x.
func FloatIntegerPart(x Number) (Number, error) {
	switch x := x.(type) {
	case Float:
		return intPartF(x), nil
	default:
		return nil, TypeError(ValidTypeFloat, x, nil)
	}
}

// FloatFractionalPart returns the fractional part of x.
func FloatFractionalPart(x Number) (Number, error) {
	switch x := x.(type) {
	case Float:
		return fractPartF(x), nil
	default:
		return nil, TypeError(ValidTypeFloat, x, nil)
	}
}

// AsFloat returns x as engine.Float.
func AsFloat(x Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		return floatItoF(x), nil
	case Float:
		return floatFtoF(x), nil
	default:
		return nil, ExceptionalValueUndefined
	}
}

// Floor returns the greatest integer value less than or equal to x.
func Floor(x Number) (Number, error) {
	switch x := x.(type) {
	case Float:
		return floorFtoI(x)
	default:
		return nil, TypeError(ValidTypeFloat, x, nil)
	}
}

// Truncate returns the integer value of x.
func Truncate(x Number) (Number, error) {
	switch x := x.(type) {
	case Float:
		return truncateFtoI(x)
	default:
		return nil, TypeError(ValidTypeFloat, x, nil)
	}
}

// Round returns the nearest integer of x.
func Round(x Number) (Number, error) {
	switch x := x.(type) {
	case Float:
		return roundFtoI(x)
	default:
		return nil, TypeError(ValidTypeFloat, x, nil)
	}
}

// Ceiling returns the least integer value greater than or equal to x.
func Ceiling(x Number) (Number, error) {
	switch x := x.(type) {
	case Float:
		return ceilingFtoI(x)
	default:
		return nil, TypeError(ValidTypeFloat, x, nil)
	}
}

// Power returns the base-x exponential of y.
func Power(x, y Number) (Number, error) {
	var vx float64
	switch x := x.(type) {
	case Integer:
		vx = float64(x)
	case Float:
		vx = float64(x)
	default:
		return nil, ExceptionalValueUndefined
	}

	var vy float64
	switch y := y.(type) {
	case Integer:
		vy = float64(y)
	case Float:
		vy = float64(y)
	default:
		return nil, ExceptionalValueUndefined
	}

	// 9.3.1.3 d) special case
	if vx == 0 && vy < 0 {
		return nil, ExceptionalValueUndefined
	}

	r := Float(math.Pow(vx, vy))

	switch {
	case math.IsInf(float64(r), 0):
		return nil, ExceptionalValueFloatOverflow
	case r == 0 && vx != 0: // Underflow: r can be 0 iff x = 0.
		return nil, ExceptionalValueUnderflow
	case math.IsNaN(float64(r)):
		return nil, ExceptionalValueUndefined
	default:
		return r, nil
	}
}

// Sin returns the sine of x.
func Sin(x Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		return Float(math.Sin(float64(x))), nil
	case Float:
		return Float(math.Sin(float64(x))), nil
	default:
		return nil, ExceptionalValueUndefined
	}
}

// Cos returns the cosine of x.
func Cos(x Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		return Float(math.Cos(float64(x))), nil
	case Float:
		return Float(math.Cos(float64(x))), nil
	default:
		return nil, ExceptionalValueUndefined
	}
}

// Atan returns the arctangent of x.
func Atan(x Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		return Float(math.Atan(float64(x))), nil
	case Float:
		return Float(math.Atan(float64(x))), nil
	default:
		return nil, ExceptionalValueUndefined
	}
}

// Exp returns the base-e exponential of x.
func Exp(x Number) (Number, error) {
	var vx float64
	switch x := x.(type) {
	case Integer:
		vx = float64(x)
	case Float:
		vx = float64(x)
	default:
		return nil, ExceptionalValueUndefined
	}

	// Positive overflow:
	//        e^x > max
	//   log(e^x) > log(max)
	// x * log(e) > log(max)
	//          x > log(max)
	if vx > math.Log(math.MaxFloat64) {
		return nil, ExceptionalValueFloatOverflow
	}

	r := Float(math.Exp(vx))

	if r == 0 { // e^x != 0.
		return nil, ExceptionalValueUnderflow
	}

	return r, nil
}

// Log returns the natural logarithm of x.
func Log(x Number) (Number, error) {
	var vx float64
	switch x := x.(type) {
	case Integer:
		vx = float64(x)
	case Float:
		vx = float64(x)
	default:
		return nil, ExceptionalValueUndefined
	}

	if vx <= 0 {
		return nil, ExceptionalValueUndefined
	}

	return Float(math.Log(vx)), nil
}

// Sqrt returns the square root of x.
func Sqrt(x Number) (Number, error) {
	var vx float64
	switch x := x.(type) {
	case Integer:
		vx = float64(x)
	case Float:
		vx = float64(x)
	default:
		return nil, ExceptionalValueUndefined
	}

	if vx < 0 {
		return nil, ExceptionalValueUndefined
	}

	return Float(math.Sqrt(vx)), nil
}

// BitwiseRightShift returns n bit-shifted by s to the right.
func BitwiseRightShift(n, s Number) (Number, error) {
	switch n := n.(type) {
	case Integer:
		switch s := s.(type) {
		case Integer:
			return Integer(n >> s), nil
		default:
			return nil, TypeError(ValidTypeInteger, s, nil)
		}
	default:
		return nil, TypeError(ValidTypeInteger, n, nil)
	}
}

// BitwiseLeftShift returns n bit-shifted by s to the left.
func BitwiseLeftShift(n, s Number) (Number, error) {
	switch n := n.(type) {
	case Integer:
		switch s := s.(type) {
		case Integer:
			return Integer(n << s), nil
		default:
			return nil, TypeError(ValidTypeInteger, s, nil)
		}
	default:
		return nil, TypeError(ValidTypeInteger, n, nil)
	}
}

// BitwiseAnd returns the logical AND on each bit.
func BitwiseAnd(b1, b2 Number) (Number, error) {
	switch b1 := b1.(type) {
	case Integer:
		switch b2 := b2.(type) {
		case Integer:
			return b1 & b2, nil
		default:
			return nil, TypeError(ValidTypeInteger, b2, nil)
		}
	default:
		return nil, TypeError(ValidTypeInteger, b1, nil)
	}
}

// BitwiseOr returns the logical OR on each bit.
func BitwiseOr(b1, b2 Number) (Number, error) {
	switch b1 := b1.(type) {
	case Integer:
		switch b2 := b2.(type) {
		case Integer:
			return b1 | b2, nil
		default:
			return nil, TypeError(ValidTypeInteger, b2, nil)
		}
	default:
		return nil, TypeError(ValidTypeInteger, b1, nil)
	}
}

// BitwiseComplement returns the logical negation on each bit.
func BitwiseComplement(b1 Number) (Number, error) {
	switch b1 := b1.(type) {
	case Integer:
		return ^b1, nil
	default:
		return nil, TypeError(ValidTypeInteger, b1, nil)
	}
}

// Pos returns x as is.
func Pos(x Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		return posI(x)
	case Float:
		return posF(x)
	default:
		return nil, ExceptionalValueUndefined
	}
}

// IntFloorDiv returns the integer floor division.
func IntFloorDiv(x, y Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		switch y := y.(type) {
		case Integer:
			return intFloorDivI(x, y)
		default:
			return nil, TypeError(ValidTypeInteger, y, nil)
		}
	default:
		return nil, TypeError(ValidTypeInteger, x, nil)
	}
}

// Max returns the maximum of x or y.
func Max(x, y Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		switch y := y.(type) {
		case Integer:
			if x < y {
				return y, nil
			}
			return x, nil
		case Float:
			if floatItoF(x) < y {
				return y, nil
			}
			return x, nil
		default:
			return nil, ExceptionalValueUndefined
		}
	case Float:
		switch y := y.(type) {
		case Integer:
			if x < floatItoF(y) {
				return y, nil
			}
			return x, nil
		case Float:
			if x < y {
				return y, nil
			}
			return x, nil
		default:
			return nil, ExceptionalValueUndefined
		}
	default:
		return nil, ExceptionalValueUndefined
	}
}

// Min returns the minimum of x or y.
func Min(x, y Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		switch y := y.(type) {
		case Integer:
			if x > y {
				return y, nil
			}
			return x, nil
		case Float:
			if floatItoF(x) > y {
				return y, nil
			}
			return x, nil
		default:
			return nil, ExceptionalValueUndefined
		}
	case Float:
		switch y := y.(type) {
		case Integer:
			if x > floatItoF(y) {
				return y, nil
			}
			return x, nil
		case Float:
			if x > y {
				return y, nil
			}
			return x, nil
		default:
			return nil, ExceptionalValueUndefined
		}
	default:
		return nil, ExceptionalValueUndefined
	}
}

// IntegerPower returns x raised to the power of y.
func IntegerPower(x, y Number) (Number, error) {
	vx, ok := x.(Integer)
	if !ok {
		return Power(x, y)
	}

	vy, ok := y.(Integer)
	if !ok {
		return Power(x, y)
	}

	if vy < 0 {
		switch vx {
		case 0:
			return nil, ExceptionalValueUndefined
		case 1, -1:
			vy, err := negI(vy) // y can be minInt
			if err != nil {
				return nil, err
			}
			r, _ := intPow(vx, vy) // Since x is either 1 or -1, no errors occur.
			return intDivI(1, r)
		default:
			return nil, TypeError(ValidTypeFloat, vx, nil)
		}
	}

	return intPow(vx, vy)
}

// Loosely based on https://www.programminglogic.com/fast-exponentiation-algorithms/
func intPow(a, b Integer) (Integer, error) {
	var (
		r   = Integer(1)
		err error
	)
	for {
		if b&1 != 0 {
			r, err = mulI(r, a)
			if err != nil {
				return 0, err
			}
		}

		b >>= 1
		if b == 0 {
			break
		}

		a, err = mulI(a, a)
		if err != nil {
			return 0, err
		}
	}
	return r, nil
}

// Asin returns the arc sine of x.
func Asin(x Number) (Number, error) {
	var vx float64
	switch x := x.(type) {
	case Integer:
		vx = float64(x)
	case Float:
		vx = float64(x)
	default:
		return nil, ExceptionalValueUndefined
	}

	if vx > 1 || vx < -1 {
		return nil, ExceptionalValueUndefined
	}

	return Float(math.Asin(vx)), nil
}

// Acos returns the arc cosine of x.
func Acos(x Number) (Number, error) {
	var vx float64
	switch x := x.(type) {
	case Integer:
		vx = float64(x)
	case Float:
		vx = float64(x)
	default:
		return nil, ExceptionalValueUndefined
	}

	if vx > 1 || vx < -1 {
		return nil, ExceptionalValueUndefined
	}

	return Float(math.Acos(vx)), nil
}

// Atan2 returns the arc tangent of y/x.
func Atan2(y, x Number) (Number, error) {
	var vy float64
	switch y := y.(type) {
	case Integer:
		vy = float64(y)
	case Float:
		vy = float64(y)
	default:
		return nil, ExceptionalValueUndefined
	}

	var vx float64
	switch x := x.(type) {
	case Integer:
		vx = float64(x)
	case Float:
		vx = float64(x)
	default:
		return nil, ExceptionalValueUndefined
	}

	if vx == 0 && vy == 0 {
		return nil, ExceptionalValueUndefined
	}

	return Float(math.Atan2(vy, vx)), nil
}

// Tan returns the tangent of x.
func Tan(x Number) (Number, error) {
	var vx float64
	switch x := x.(type) {
	case Integer:
		vx = float64(x)
	case Float:
		vx = float64(x)
	default:
		return nil, ExceptionalValueUndefined
	}

	return Float(math.Tan(vx)), nil
}

// Xor returns the bitwise exclusive or of x and y.
func Xor(x, y Number) (Number, error) {
	vx, ok := x.(Integer)
	if !ok {
		return nil, TypeError(ValidTypeInteger, x, nil)
	}

	vy, ok := y.(Integer)
	if !ok {
		return nil, TypeError(ValidTypeInteger, y, nil)
	}

	return vx ^ vy, nil
}

// Comparison

func eqF(x, y Float) bool {
	return x == y
}

func eqI(m, n Integer) bool {
	return m == n
}

func eqFI(x Float, n Integer) bool {
	y := floatItoF(n)
	return eqF(x, y)
}

func eqIF(n Integer, y Float) bool {
	return eqFI(y, n)
}

func neqF(x, y Float) bool {
	return x != y
}

func neqI(m, n Integer) bool {
	return m != n
}

func neqFI(x Float, n Integer) bool {
	y := floatItoF(n)
	return neqF(x, y)
}

func neqIF(n Integer, y Float) bool {
	return neqFI(y, n)
}

func lssF(x, y Float) bool {
	return x < y
}

func lssI(m, n Integer) bool {
	return m < n
}

func lssFI(x Float, n Integer) bool {
	y := floatItoF(n)
	return lssF(x, y)
}

func lssIF(n Integer, y Float) bool {
	return gtrFI(y, n)
}

func leqF(x, y Float) bool {
	return x <= y
}

func leqI(m, n Integer) bool {
	return m <= n
}

func leqFI(x Float, n Integer) bool {
	y := floatItoF(n)
	return leqF(x, y)
}

func leqIF(n Integer, y Float) bool {
	return geqFI(y, n)
}

func gtrF(x, y Float) bool {
	return x > y
}

func gtrI(m, n Integer) bool {
	return m > n
}

func gtrFI(x Float, n Integer) bool {
	y := floatItoF(n)
	return gtrF(x, y)
}

func gtrIF(n Integer, y Float) bool {
	return lssFI(y, n)
}

func geqF(x, y Float) bool {
	return x >= y
}

func geqI(m, n Integer) bool {
	return m >= n
}

func geqFI(x Float, n Integer) bool {
	y := floatItoF(n)
	return geqF(x, y)
}

func geqIF(n Integer, y Float) bool {
	return leqFI(y, n)
}

// Type conversion operations

func floatItoF(n Integer) Float {
	return Float(n)
}

func floatFtoF(x Float) Float {
	return x
}

func floorFtoI(x Float) (Integer, error) {
	f := math.Floor(float64(x))
	if f > float64(maxInt) || f < float64(minInt) {
		return 0, ExceptionalValueIntOverflow
	}
	return Integer(f), nil
}

func truncateFtoI(x Float) (Integer, error) {
	t := math.Trunc(float64(x))
	if t > float64(maxInt) || t < float64(minInt) {
		return 0, ExceptionalValueIntOverflow
	}
	return Integer(t), nil
}

func roundFtoI(x Float) (Integer, error) {
	r := math.Round(float64(x))
	if r > float64(maxInt) || r < float64(minInt) {
		return 0, ExceptionalValueIntOverflow
	}
	return Integer(r), nil
}

func ceilingFtoI(x Float) (Integer, error) {
	c := math.Ceil(float64(x))
	if c > float64(maxInt) || c < float64(minInt) {
		return 0, ExceptionalValueIntOverflow
	}
	return Integer(c), nil
}

// Integer operations

func addI(x, y Integer) (Integer, error) {
	switch {
	case y > 0 && x > maxInt-y:
		return 0, ExceptionalValueIntOverflow
	case y < 0 && x < minInt-y:
		return 0, ExceptionalValueIntOverflow
	default:
		return x + y, nil
	}
}

func subI(x, y Integer) (Integer, error) {
	switch {
	case y < 0 && x > maxInt+y:
		return 0, ExceptionalValueIntOverflow
	case y > 0 && x < minInt+y:
		return 0, ExceptionalValueIntOverflow
	default:
		return x - y, nil
	}
}

func mulI(x, y Integer) (Integer, error) {
	switch {
	case x == -1 && y == minInt:
		return 0, ExceptionalValueIntOverflow
	case x == minInt && y == -1:
		return 0, ExceptionalValueIntOverflow
	case y == 0:
		return 0, nil
	default:
		r := x * y
		if r/y != x {
			return 0, ExceptionalValueIntOverflow
		}
		return r, nil
	}
}

func intDivI(x, y Integer) (Integer, error) {
	switch {
	case y == 0:
		return 0, ExceptionalValueZeroDivisor
	case x == minInt && y == -1:
		// Two's complement special case
		return 0, ExceptionalValueIntOverflow
	default:
		return x / y, nil
	}
}

func remI(x, y Integer) (Integer, error) {
	if y == 0 {
		return 0, ExceptionalValueZeroDivisor
	}
	return x - ((x / y) * y), nil
}

func modI(x, y Integer) (Integer, error) {
	if y == 0 {
		return 0, ExceptionalValueZeroDivisor
	}
	return x - (Integer(math.Floor(float64(x)/float64(y))) * y), nil
}

func negI(x Integer) (Integer, error) {
	// Two's complement special case
	if x == minInt {
		return 0, ExceptionalValueIntOverflow
	}
	return -x, nil
}

func absI(x Integer) (Integer, error) {
	switch {
	case x == minInt:
		return 0, ExceptionalValueIntOverflow
	case x < 0:
		return -x, nil
	default:
		return x, nil
	}
}

func signI(x Integer) Integer {
	switch {
	case x > 0:
		return 1
	case x < 0:
		return -1
	default:
		return 0
	}
}

func posI(x Integer) (Integer, error) {
	return x, nil
}

func intFloorDivI(x, y Integer) (Integer, error) {
	switch {
	case x == minInt && y == -1:
		return 0, ExceptionalValueIntOverflow
	case y == 0:
		return 0, ExceptionalValueZeroDivisor
	default:
		return Integer(math.Floor(float64(x) / float64(y))), nil
	}
}

// Float operations

func addF(x, y Float) (Float, error) {
	switch {
	case y > 0 && x > math.MaxFloat64-y:
		return 0, ExceptionalValueFloatOverflow
	case y < 0 && x < -math.MaxFloat64-y:
		return 0, ExceptionalValueFloatOverflow
	}

	return x + y, nil
}

func subF(x, y Float) (Float, error) {
	return addF(x, -y)
}

func mulF(x, y Float) (Float, error) {
	switch {
	case y != 0 && x > math.MaxFloat64/y:
		return 0, ExceptionalValueFloatOverflow
	case y != 0 && x < -math.MaxFloat64/y:
		return 0, ExceptionalValueFloatOverflow
	}

	r := x * y

	// Underflow: x*y = 0 iff x = 0 or y = 0.
	if r == 0 && x != 0 && y != 0 {
		return 0, ExceptionalValueUnderflow
	}

	return r, nil
}

func divF(x, y Float) (Float, error) {
	switch {
	case y == 0:
		return 0, ExceptionalValueZeroDivisor
	case x > math.MaxFloat64*y:
		return 0, ExceptionalValueFloatOverflow
	case x < -math.MaxFloat64*y:
		return 0, ExceptionalValueFloatOverflow
	}

	r := x / y

	// Underflow: x/y = 0 iff x = 0 and y != 0.
	if r == 0 && x != 0 {
		return 0, ExceptionalValueUnderflow
	}

	return r, nil
}

func negF(x Float) Float {
	return -x
}

func absF(x Float) Float {
	return Float(math.Abs(float64(x)))
}

func signF(x Float) Float {
	switch {
	case x > 0:
		return 1
	case x < 0:
		return -1
	default:
		return 0
	}
}

func intPartF(x Float) Float {
	s := signF(x)
	return s * Float(math.Floor(math.Abs(float64(x))))
}

func fractPartF(x Float) Float {
	i := intPartF(x)
	return x - i
}

func posF(x Float) (Float, error) {
	return x, nil
}

// Mixed mode operations

func addFI(x Float, n Integer) (Float, error) {
	return addF(x, Float(n))
}

func addIF(n Integer, x Float) (Float, error) {
	return addF(Float(n), x)
}

func subFI(x Float, n Integer) (Float, error) {
	return subF(x, Float(n))
}

func subIF(n Integer, x Float) (Float, error) {
	return subF(Float(n), x)
}

func mulFI(x Float, n Integer) (Float, error) {
	return mulF(x, Float(n))
}

func mulIF(n Integer, x Float) (Float, error) {
	return mulF(Float(n), x)
}

func divFI(x Float, n Integer) (Float, error) {
	return divF(x, Float(n))
}

func divIF(n Integer, x Float) (Float, error) {
	return divF(Float(n), x)
}

func divII(n, m Integer) (Float, error) {
	return divF(Float(n), Float(m))
}
