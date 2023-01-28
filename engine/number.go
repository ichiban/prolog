package engine

import (
	"context"
	"errors"
	"math"
)

var (
	maxInt = Integer(math.MaxInt64)
	minInt = Integer(math.MinInt64)
)

var constants = map[Atom]Number{
	atomPi: Float(math.Pi),
}

var unaryFunctors = map[Atom]func(Number) (Number, error){
	atomMinus:               neg,
	atomAbs:                 abs,
	atomSign:                sign,
	atomFloatIntegerPart:    floatIntegerPart,
	atomFloatFractionalPart: floatFractionalPart,
	atomFloat:               asFloat,
	atomFloor:               floor,
	atomTruncate:            truncate,
	atomRound:               round,
	atomCeiling:             ceiling,
	atomSin:                 sin,
	atomCos:                 cos,
	atomAtan:                atan,
	atomExp:                 exp,
	atomLog:                 log,
	atomSqrt:                sqrt,
	atomBackSlash:           bitwiseComplement,
	atomPlus:                pos,
	atomAsin:                asin,
	atomAcos:                acos,
	atomTan:                 tan,
}

var binaryFunctors = map[Atom]func(Number, Number) (Number, error){
	atomPlus:              add,
	atomMinus:             sub,
	atomAsterisk:          mul,
	atomSlashSlash:        intDiv,
	atomSlash:             div,
	atomRem:               rem,
	atomMod:               mod,
	atomAsteriskAsterisk:  power,
	atomBitwiseRightShift: bitwiseRightShift,
	atomBitwiseLeftShift:  bitwiseLeftShift,
	atomBitwiseAnd:        bitwiseAnd,
	atomBitwiseOr:         bitwiseOr,
	atomDiv:               intFloorDiv,
	atomMax:               max,
	atomMin:               min,
	atomCaret:             integerPower,
	atomAtan2:             atan2,
	atomXor:               xor,
}

// Number is a prolog number, either Integer or Float.
type Number interface {
	Term
	number()
}

func eval(ctx context.Context, expression Term) (_ Number, err error) {
	defer func() {
		var ev exceptionalValue
		if errors.As(err, &ev) {
			err = evaluationError(ctx, ev)
		}
	}()

	switch t := Resolve(ctx, expression).(type) {
	case Variable:
		return nil, InstantiationError(ctx)
	case Atom:
		c, ok := constants[t]
		if !ok {
			return nil, typeError(ctx, validTypeEvaluable, atomSlash.Apply(t, Integer(0)))
		}
		return c, nil
	case Number:
		return t, nil
	case Compound:
		switch arity := t.Arity(); arity {
		case 1:
			f, ok := unaryFunctors[t.Functor()]
			if !ok {
				return nil, typeError(ctx, validTypeEvaluable, atomSlash.Apply(t.Functor(), Integer(1)))
			}
			x, err := eval(ctx, t.Arg(0))
			if err != nil {
				return nil, err
			}
			return f(x)
		case 2:
			f, ok := binaryFunctors[t.Functor()]
			if !ok {
				return nil, typeError(ctx, validTypeEvaluable, atomSlash.Apply(t.Functor(), Integer(2)))
			}
			x, err := eval(ctx, t.Arg(0))
			if err != nil {
				return nil, err
			}
			y, err := eval(ctx, t.Arg(1))
			if err != nil {
				return nil, err
			}
			return f(x, y)
		default:
			return nil, typeError(ctx, validTypeEvaluable, atomSlash.Apply(t.Functor(), Integer(arity)))
		}
	default:
		return nil, typeError(ctx, validTypeEvaluable, atomSlash.Apply(t, Integer(0)))
	}
}

// Is evaluates expression and unifies the result with result.
func Is(ctx context.Context, result, expression Term) *Promise {
	v, err := eval(ctx, expression)
	if err != nil {
		return Error(err)
	}
	return Unify(ctx, result, v)
}

// Equal succeeds iff e1 equals to e2.
func Equal(ctx context.Context, e1, e2 Term) *Promise {
	ev1, err := eval(ctx, e1)
	if err != nil {
		return Error(err)
	}

	ev2, err := eval(ctx, e2)
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
	return Continue(ctx)
}

// NotEqual succeeds iff e1 doesn't equal to e2.
func NotEqual(ctx context.Context, e1, e2 Term) *Promise {
	ev1, err := eval(ctx, e1)
	if err != nil {
		return Error(err)
	}

	ev2, err := eval(ctx, e2)
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
	return Continue(ctx)
}

// LessThan succeeds iff e1 is less than e2.
func LessThan(ctx context.Context, e1, e2 Term) *Promise {
	ev1, err := eval(ctx, e1)
	if err != nil {
		return Error(err)
	}

	ev2, err := eval(ctx, e2)
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
	return Continue(ctx)
}

// GreaterThan succeeds iff e1 is greater than e2.
func GreaterThan(ctx context.Context, e1, e2 Term) *Promise {
	ev1, err := eval(ctx, e1)
	if err != nil {
		return Error(err)
	}

	ev2, err := eval(ctx, e2)
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
	return Continue(ctx)
}

// LessThanOrEqual succeeds iff e1 is less than or equal to e2.
func LessThanOrEqual(ctx context.Context, e1, e2 Term) *Promise {
	ev1, err := eval(ctx, e1)
	if err != nil {
		return Error(err)
	}

	ev2, err := eval(ctx, e2)
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
	return Continue(ctx)
}

// GreaterThanOrEqual succeeds iff e1 is greater than or equal to e2.
func GreaterThanOrEqual(ctx context.Context, e1, e2 Term) *Promise {
	ev1, err := eval(ctx, e1)
	if err != nil {
		return Error(err)
	}

	ev2, err := eval(ctx, e2)
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
	return Continue(ctx)
}

// add returns sum of 2 numbers.
func add(x, y Number) (Number, error) {
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
	return nil, exceptionalValueUndefined
}

// sub returns subtraction of 2 numbers.
func sub(x, y Number) (Number, error) {
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
	return nil, exceptionalValueUndefined
}

// mul returns multiplication of 2 numbers.
func mul(x, y Number) (Number, error) {
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
	return nil, exceptionalValueUndefined
}

// intDiv returns integer division of 2 numbers.
func intDiv(x, y Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		switch y := y.(type) {
		case Integer:
			return intDivI(x, y)
		default:
			return nil, typeError(context.Background(), validTypeInteger, y)
		}
	default:
		return nil, typeError(context.Background(), validTypeInteger, x)
	}
}

// div returns division of 2 numbers
func div(x, y Number) (Number, error) {
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
	return nil, exceptionalValueUndefined
}

// rem returns remainder of 2 numbers.
func rem(x, y Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		switch y := y.(type) {
		case Integer:
			return remI(x, y)
		default:
			return nil, typeError(context.Background(), validTypeInteger, y)
		}
	default:
		return nil, typeError(context.Background(), validTypeInteger, x)
	}
}

// mod returns modulo of 2 numbers.
func mod(x, y Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		switch y := y.(type) {
		case Integer:
			return modI(x, y)
		default:
			return nil, typeError(context.Background(), validTypeInteger, y)
		}
	default:
		return nil, typeError(context.Background(), validTypeInteger, x)
	}
}

// neg returns the negation of a number.
func neg(x Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		return negI(x)
	case Float:
		return negF(x), nil
	default:
		return nil, exceptionalValueUndefined
	}
}

// abs returns the absolute value of x.
func abs(x Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		return absI(x)
	case Float:
		return absF(x), nil
	default:
		return nil, exceptionalValueUndefined
	}
}

// sign returns +1, 0, or -1 depending on the sign of x.
func sign(x Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		return signI(x), nil
	case Float:
		return signF(x), nil
	default:
		return nil, exceptionalValueUndefined
	}
}

// floatIntegerPart returns the integer part of x.
func floatIntegerPart(x Number) (Number, error) {
	switch x := x.(type) {
	case Float:
		return intPartF(x), nil
	default:
		return nil, typeError(context.Background(), validTypeFloat, x)
	}
}

// floatFractionalPart returns the fractional part of x.
func floatFractionalPart(x Number) (Number, error) {
	switch x := x.(type) {
	case Float:
		return fractPartF(x), nil
	default:
		return nil, typeError(context.Background(), validTypeFloat, x)
	}
}

// asFloat returns x as engine.Float.
func asFloat(x Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		return floatItoF(x), nil
	case Float:
		return floatFtoF(x), nil
	default:
		return nil, exceptionalValueUndefined
	}
}

// floor returns the greatest integer value less than or equal to x.
func floor(x Number) (Number, error) {
	switch x := x.(type) {
	case Float:
		return floorFtoI(x)
	default:
		return nil, typeError(context.Background(), validTypeFloat, x)
	}
}

// truncate returns the integer value of x.
func truncate(x Number) (Number, error) {
	switch x := x.(type) {
	case Float:
		return truncateFtoI(x)
	default:
		return nil, typeError(context.Background(), validTypeFloat, x)
	}
}

// round returns the nearest integer of x.
func round(x Number) (Number, error) {
	switch x := x.(type) {
	case Float:
		return roundFtoI(x)
	default:
		return nil, typeError(context.Background(), validTypeFloat, x)
	}
}

// ceiling returns the least integer value greater than or equal to x.
func ceiling(x Number) (Number, error) {
	switch x := x.(type) {
	case Float:
		return ceilingFtoI(x)
	default:
		return nil, typeError(context.Background(), validTypeFloat, x)
	}
}

// power returns the base-x exponential of y.
func power(x, y Number) (Number, error) {
	var vx float64
	switch x := x.(type) {
	case Integer:
		vx = float64(x)
	case Float:
		vx = float64(x)
	default:
		return nil, exceptionalValueUndefined
	}

	var vy float64
	switch y := y.(type) {
	case Integer:
		vy = float64(y)
	case Float:
		vy = float64(y)
	default:
		return nil, exceptionalValueUndefined
	}

	// 9.3.1.3 d) special case
	if vx == 0 && vy < 0 {
		return nil, exceptionalValueUndefined
	}

	r := Float(math.Pow(vx, vy))

	switch {
	case math.IsInf(float64(r), 0):
		return nil, exceptionalValueFloatOverflow
	case r == 0 && vx != 0: // Underflow: r can be 0 iff x = 0.
		return nil, exceptionalValueUnderflow
	case math.IsNaN(float64(r)):
		return nil, exceptionalValueUndefined
	default:
		return r, nil
	}
}

// sin returns the sine of x.
func sin(x Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		return Float(math.Sin(float64(x))), nil
	case Float:
		return Float(math.Sin(float64(x))), nil
	default:
		return nil, exceptionalValueUndefined
	}
}

// cos returns the cosine of x.
func cos(x Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		return Float(math.Cos(float64(x))), nil
	case Float:
		return Float(math.Cos(float64(x))), nil
	default:
		return nil, exceptionalValueUndefined
	}
}

// atan returns the arctangent of x.
func atan(x Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		return Float(math.Atan(float64(x))), nil
	case Float:
		return Float(math.Atan(float64(x))), nil
	default:
		return nil, exceptionalValueUndefined
	}
}

// exp returns the base-e exponential of x.
func exp(x Number) (Number, error) {
	var vx float64
	switch x := x.(type) {
	case Integer:
		vx = float64(x)
	case Float:
		vx = float64(x)
	default:
		return nil, exceptionalValueUndefined
	}

	// Positive overflow:
	//        e^x > max
	//   log(e^x) > log(max)
	// x * log(e) > log(max)
	//          x > log(max)
	if vx > math.Log(math.MaxFloat64) {
		return nil, exceptionalValueFloatOverflow
	}

	r := Float(math.Exp(vx))

	if r == 0 { // e^x != 0.
		return nil, exceptionalValueUnderflow
	}

	return r, nil
}

// log returns the natural logarithm of x.
func log(x Number) (Number, error) {
	var vx float64
	switch x := x.(type) {
	case Integer:
		vx = float64(x)
	case Float:
		vx = float64(x)
	default:
		return nil, exceptionalValueUndefined
	}

	if vx <= 0 {
		return nil, exceptionalValueUndefined
	}

	return Float(math.Log(vx)), nil
}

// sqrt returns the square root of x.
func sqrt(x Number) (Number, error) {
	var vx float64
	switch x := x.(type) {
	case Integer:
		vx = float64(x)
	case Float:
		vx = float64(x)
	default:
		return nil, exceptionalValueUndefined
	}

	if vx < 0 {
		return nil, exceptionalValueUndefined
	}

	return Float(math.Sqrt(vx)), nil
}

// bitwiseRightShift returns n bit-shifted by s to the right.
func bitwiseRightShift(n, s Number) (Number, error) {
	switch n := n.(type) {
	case Integer:
		switch s := s.(type) {
		case Integer:
			return Integer(n >> s), nil
		default:
			return nil, typeError(context.Background(), validTypeInteger, s)
		}
	default:
		return nil, typeError(context.Background(), validTypeInteger, n)
	}
}

// bitwiseLeftShift returns n bit-shifted by s to the left.
func bitwiseLeftShift(n, s Number) (Number, error) {
	switch n := n.(type) {
	case Integer:
		switch s := s.(type) {
		case Integer:
			return Integer(n << s), nil
		default:
			return nil, typeError(context.Background(), validTypeInteger, s)
		}
	default:
		return nil, typeError(context.Background(), validTypeInteger, n)
	}
}

// bitwiseAnd returns the logical AND on each bit.
func bitwiseAnd(b1, b2 Number) (Number, error) {
	switch b1 := b1.(type) {
	case Integer:
		switch b2 := b2.(type) {
		case Integer:
			return b1 & b2, nil
		default:
			return nil, typeError(context.Background(), validTypeInteger, b2)
		}
	default:
		return nil, typeError(context.Background(), validTypeInteger, b1)
	}
}

// bitwiseOr returns the logical OR on each bit.
func bitwiseOr(b1, b2 Number) (Number, error) {
	switch b1 := b1.(type) {
	case Integer:
		switch b2 := b2.(type) {
		case Integer:
			return b1 | b2, nil
		default:
			return nil, typeError(context.Background(), validTypeInteger, b2)
		}
	default:
		return nil, typeError(context.Background(), validTypeInteger, b1)
	}
}

// bitwiseComplement returns the logical negation on each bit.
func bitwiseComplement(b1 Number) (Number, error) {
	switch b1 := b1.(type) {
	case Integer:
		return ^b1, nil
	default:
		return nil, typeError(context.Background(), validTypeInteger, b1)
	}
}

// pos returns x as is.
func pos(x Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		return posI(x)
	case Float:
		return posF(x)
	default:
		return nil, exceptionalValueUndefined
	}
}

// intFloorDiv returns the integer floor division.
func intFloorDiv(x, y Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		switch y := y.(type) {
		case Integer:
			return intFloorDivI(x, y)
		default:
			return nil, typeError(context.Background(), validTypeInteger, y)
		}
	default:
		return nil, typeError(context.Background(), validTypeInteger, x)
	}
}

// max returns the maximum of x or y.
func max(x, y Number) (Number, error) {
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
			return nil, exceptionalValueUndefined
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
			return nil, exceptionalValueUndefined
		}
	default:
		return nil, exceptionalValueUndefined
	}
}

// min returns the minimum of x or y.
func min(x, y Number) (Number, error) {
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
			return nil, exceptionalValueUndefined
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
			return nil, exceptionalValueUndefined
		}
	default:
		return nil, exceptionalValueUndefined
	}
}

// integerPower returns x raised to the power of y.
func integerPower(x, y Number) (Number, error) {
	vx, ok := x.(Integer)
	if !ok {
		return power(x, y)
	}

	vy, ok := y.(Integer)
	if !ok {
		return power(x, y)
	}

	if vy < 0 {
		switch vx {
		case 0:
			return nil, exceptionalValueUndefined
		case 1, -1:
			vy, err := negI(vy) // y can be minInt
			if err != nil {
				return nil, err
			}
			r, _ := intPow(vx, vy) // Since x is either 1 or -1, no errors occur.
			return intDivI(1, r)
		default:
			return nil, typeError(context.Background(), validTypeFloat, vx)
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

// asin returns the arc sine of x.
func asin(x Number) (Number, error) {
	var vx float64
	switch x := x.(type) {
	case Integer:
		vx = float64(x)
	case Float:
		vx = float64(x)
	default:
		return nil, exceptionalValueUndefined
	}

	if vx > 1 || vx < -1 {
		return nil, exceptionalValueUndefined
	}

	return Float(math.Asin(vx)), nil
}

// acos returns the arc cosine of x.
func acos(x Number) (Number, error) {
	var vx float64
	switch x := x.(type) {
	case Integer:
		vx = float64(x)
	case Float:
		vx = float64(x)
	default:
		return nil, exceptionalValueUndefined
	}

	if vx > 1 || vx < -1 {
		return nil, exceptionalValueUndefined
	}

	return Float(math.Acos(vx)), nil
}

// atan2 returns the arc tangent of y/x.
func atan2(y, x Number) (Number, error) {
	var vy float64
	switch y := y.(type) {
	case Integer:
		vy = float64(y)
	case Float:
		vy = float64(y)
	default:
		return nil, exceptionalValueUndefined
	}

	var vx float64
	switch x := x.(type) {
	case Integer:
		vx = float64(x)
	case Float:
		vx = float64(x)
	default:
		return nil, exceptionalValueUndefined
	}

	if vx == 0 && vy == 0 {
		return nil, exceptionalValueUndefined
	}

	return Float(math.Atan2(vy, vx)), nil
}

// tan returns the tangent of x.
func tan(x Number) (Number, error) {
	var vx float64
	switch x := x.(type) {
	case Integer:
		vx = float64(x)
	case Float:
		vx = float64(x)
	default:
		return nil, exceptionalValueUndefined
	}

	return Float(math.Tan(vx)), nil
}

// xor returns the bitwise exclusive or of x and y.
func xor(x, y Number) (Number, error) {
	vx, ok := x.(Integer)
	if !ok {
		return nil, typeError(context.Background(), validTypeInteger, x)
	}

	vy, ok := y.(Integer)
	if !ok {
		return nil, typeError(context.Background(), validTypeInteger, y)
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
		return 0, exceptionalValueIntOverflow
	}
	return Integer(f), nil
}

func truncateFtoI(x Float) (Integer, error) {
	t := math.Trunc(float64(x))
	if t > float64(maxInt) || t < float64(minInt) {
		return 0, exceptionalValueIntOverflow
	}
	return Integer(t), nil
}

func roundFtoI(x Float) (Integer, error) {
	r := math.Round(float64(x))
	if r > float64(maxInt) || r < float64(minInt) {
		return 0, exceptionalValueIntOverflow
	}
	return Integer(r), nil
}

func ceilingFtoI(x Float) (Integer, error) {
	c := math.Ceil(float64(x))
	if c > float64(maxInt) || c < float64(minInt) {
		return 0, exceptionalValueIntOverflow
	}
	return Integer(c), nil
}

// Integer operations

func addI(x, y Integer) (Integer, error) {
	switch {
	case y > 0 && x > maxInt-y:
		return 0, exceptionalValueIntOverflow
	case y < 0 && x < minInt-y:
		return 0, exceptionalValueIntOverflow
	default:
		return x + y, nil
	}
}

func subI(x, y Integer) (Integer, error) {
	switch {
	case y < 0 && x > maxInt+y:
		return 0, exceptionalValueIntOverflow
	case y > 0 && x < minInt+y:
		return 0, exceptionalValueIntOverflow
	default:
		return x - y, nil
	}
}

func mulI(x, y Integer) (Integer, error) {
	switch {
	case x == -1 && y == minInt:
		return 0, exceptionalValueIntOverflow
	case x == minInt && y == -1:
		return 0, exceptionalValueIntOverflow
	case y == 0:
		return 0, nil
	default:
		r := x * y
		if r/y != x {
			return 0, exceptionalValueIntOverflow
		}
		return r, nil
	}
}

func intDivI(x, y Integer) (Integer, error) {
	switch {
	case y == 0:
		return 0, exceptionalValueZeroDivisor
	case x == minInt && y == -1:
		// Two's complement special case
		return 0, exceptionalValueIntOverflow
	default:
		return x / y, nil
	}
}

func remI(x, y Integer) (Integer, error) {
	if y == 0 {
		return 0, exceptionalValueZeroDivisor
	}
	return x - ((x / y) * y), nil
}

func modI(x, y Integer) (Integer, error) {
	if y == 0 {
		return 0, exceptionalValueZeroDivisor
	}
	return x - (Integer(math.Floor(float64(x)/float64(y))) * y), nil
}

func negI(x Integer) (Integer, error) {
	// Two's complement special case
	if x == minInt {
		return 0, exceptionalValueIntOverflow
	}
	return -x, nil
}

func absI(x Integer) (Integer, error) {
	switch {
	case x == minInt:
		return 0, exceptionalValueIntOverflow
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
		return 0, exceptionalValueIntOverflow
	case y == 0:
		return 0, exceptionalValueZeroDivisor
	default:
		return Integer(math.Floor(float64(x) / float64(y))), nil
	}
}

// Float operations

func addF(x, y Float) (Float, error) {
	switch {
	case y > 0 && x > math.MaxFloat64-y:
		return 0, exceptionalValueFloatOverflow
	case y < 0 && x < -math.MaxFloat64-y:
		return 0, exceptionalValueFloatOverflow
	}

	return x + y, nil
}

func subF(x, y Float) (Float, error) {
	return addF(x, -y)
}

func mulF(x, y Float) (Float, error) {
	switch {
	case y != 0 && x > math.MaxFloat64/y:
		return 0, exceptionalValueFloatOverflow
	case y != 0 && x < -math.MaxFloat64/y:
		return 0, exceptionalValueFloatOverflow
	}

	r := x * y

	// Underflow: x*y = 0 iff x = 0 or y = 0.
	if r == 0 && x != 0 && y != 0 {
		return 0, exceptionalValueUnderflow
	}

	return r, nil
}

func divF(x, y Float) (Float, error) {
	switch {
	case y == 0:
		return 0, exceptionalValueZeroDivisor
	case x > math.MaxFloat64*y:
		return 0, exceptionalValueFloatOverflow
	case x < -math.MaxFloat64*y:
		return 0, exceptionalValueFloatOverflow
	}

	r := x / y

	// Underflow: x/y = 0 iff x = 0 and y != 0.
	if r == 0 && x != 0 {
		return 0, exceptionalValueUnderflow
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
