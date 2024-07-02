package engine

import (
	"errors"
	"github.com/cockroachdb/apd"
	"math"
)

var (
	maxInt        = Integer(math.MaxInt64)
	minInt        = Integer(math.MinInt64)
	oneFloat      = NewFloatFromInt64(1)
	minusOneFloat = NewFloatFromInt64(-1)
)

var constants = map[Atom]Number{
	atomPi: func() Number {
		f, err := NewFloatFromString("3.14159265358979323846264338327950288419716939937510582097494459")
		// should not occur
		if err != nil {
			panic(err)
		}
		return f
	}(),
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
	atomExp:                 exp,
	atomLog:                 log,
	atomSqrt:                sqrt,
	atomBackSlash:           bitwiseComplement,
	atomPlus:                pos,
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
	atomXor:               xor,
}

// Number is a prolog number, either Integer or Float.
type Number interface {
	Term
	number()
}

func eval(expression Term, env *Env) (_ Number, err error) {
	defer func() {
		var ev exceptionalValue
		if errors.As(err, &ev) {
			err = evaluationError(ev, env)
		}
	}()

	switch t := env.Resolve(expression).(type) {
	case Variable:
		return nil, InstantiationError(env)
	case Atom:
		c, ok := constants[t]
		if !ok {
			return nil, typeError(validTypeEvaluable, atomSlash.Apply(t, Integer(0)), env)
		}
		return c, nil
	case Number:
		return t, nil
	case Compound:
		switch arity := t.Arity(); arity {
		case 1:
			f, ok := unaryFunctors[t.Functor()]
			if !ok {
				return nil, typeError(validTypeEvaluable, atomSlash.Apply(t.Functor(), Integer(1)), env)
			}
			x, err := eval(t.Arg(0), env)
			if err != nil {
				return nil, err
			}
			return f(x)
		case 2:
			f, ok := binaryFunctors[t.Functor()]
			if !ok {
				return nil, typeError(validTypeEvaluable, atomSlash.Apply(t.Functor(), Integer(2)), env)
			}
			x, err := eval(t.Arg(0), env)
			if err != nil {
				return nil, err
			}
			y, err := eval(t.Arg(1), env)
			if err != nil {
				return nil, err
			}
			return f(x, y)
		default:
			return nil, typeError(validTypeEvaluable, atomSlash.Apply(t.Functor(), Integer(arity)), env)
		}
	default:
		return nil, typeError(validTypeEvaluable, atomSlash.Apply(t, Integer(0)), env)
	}
}

// Is evaluates expression and unifies the result with result.
func Is(vm *VM, result, expression Term, k Cont, env *Env) *Promise {
	v, err := eval(expression, env)
	if err != nil {
		return Error(err)
	}
	return Unify(vm, result, v, k, env)
}

// Equal succeeds iff e1 equals to e2.
func Equal(_ *VM, e1, e2 Term, k Cont, env *Env) *Promise {
	ev1, err := eval(e1, env)
	if err != nil {
		return Error(err)
	}

	ev2, err := eval(e2, env)
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
func NotEqual(_ *VM, e1, e2 Term, k Cont, env *Env) *Promise {
	ev1, err := eval(e1, env)
	if err != nil {
		return Error(err)
	}

	ev2, err := eval(e2, env)
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
func LessThan(_ *VM, e1, e2 Term, k Cont, env *Env) *Promise {
	ev1, err := eval(e1, env)
	if err != nil {
		return Error(err)
	}

	ev2, err := eval(e2, env)
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
func GreaterThan(_ *VM, e1, e2 Term, k Cont, env *Env) *Promise {
	ev1, err := eval(e1, env)
	if err != nil {
		return Error(err)
	}

	ev2, err := eval(e2, env)
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
func LessThanOrEqual(_ *VM, e1, e2 Term, k Cont, env *Env) *Promise {
	ev1, err := eval(e1, env)
	if err != nil {
		return Error(err)
	}

	ev2, err := eval(e2, env)
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
func GreaterThanOrEqual(_ *VM, e1, e2 Term, k Cont, env *Env) *Promise {
	ev1, err := eval(e1, env)
	if err != nil {
		return Error(err)
	}

	ev2, err := eval(e2, env)
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
			return nil, typeError(validTypeInteger, y, nil)
		}
	default:
		return nil, typeError(validTypeInteger, x, nil)
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
			return nil, typeError(validTypeInteger, y, nil)
		}
	default:
		return nil, typeError(validTypeInteger, x, nil)
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
			return nil, typeError(validTypeInteger, y, nil)
		}
	default:
		return nil, typeError(validTypeInteger, x, nil)
	}
}

// neg returns the negation of a number.
func neg(x Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		return negI(x)
	case Float:
		return negF(x)
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
		return intPartF(x)
	default:
		return nil, typeError(validTypeFloat, x, nil)
	}
}

// floatFractionalPart returns the fractional part of x.
func floatFractionalPart(x Number) (Number, error) {
	switch x := x.(type) {
	case Float:
		return fractPartF(x)
	default:
		return nil, typeError(validTypeFloat, x, nil)
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
		return nil, typeError(validTypeFloat, x, nil)
	}
}

// truncate returns the integer value of x.
func truncate(x Number) (Number, error) {
	switch x := x.(type) {
	case Float:
		return truncateFtoI(x)
	default:
		return nil, typeError(validTypeFloat, x, nil)
	}
}

// round returns the nearest integer of x.
func round(x Number) (Number, error) {
	switch x := x.(type) {
	case Float:
		return roundFtoI(x)
	default:
		return nil, typeError(validTypeFloat, x, nil)
	}
}

// ceiling returns the least integer value greater than or equal to x.
func ceiling(x Number) (Number, error) {
	switch x := x.(type) {
	case Float:
		return ceilingFtoI(x)
	default:
		return nil, typeError(validTypeFloat, x, nil)
	}
}

// power returns the base-x exponential of y.
func power(x, y Number) (Number, error) {
	vx, err := numberToFloat(x)
	if err != nil {
		return nil, err
	}

	vy, err := numberToFloat(y)
	if err != nil {
		return nil, err
	}

	// 9.3.1.3 d) special case
	if vx.Zero() && vy.Negative() {
		return nil, exceptionalValueUndefined
	}

	var dec apd.Decimal
	c, err := decimal128Ctx.Pow(&dec, vx.dec, vy.dec)
	if err != nil {
		return nil, mapDecimalConditionErr(c)
	}

	return Float{dec: &dec}, nil
}

// exp returns the base-e exponential of x.
func exp(x Number) (Number, error) {
	f, err := numberToFloat(x)
	if err != nil {
		return nil, err
	}

	var dec apd.Decimal
	c, err := decimal128Ctx.Exp(&dec, f.dec)
	if err != nil {
		return nil, mapDecimalConditionErr(c)
	}

	return Float{dec: &dec}, nil

	//dec := decimal.WithContext(decimal.Context128)
	//r := Float{
	//	dec: decimal.Context128.Exp(dec, f.dec),
	//}
	//if !dec.IsFinite() {
	//	return Float{}, exceptionalValueUnderflow
	//}
	//
	//return r, r.Err()
}

// log returns the natural logarithm of x.
func log(x Number) (Number, error) {
	f, err := numberToFloat(x)
	if err != nil {
		return nil, err
	}

	if f.Negative() || f.Zero() {
		return nil, exceptionalValueUndefined
	}

	var dec apd.Decimal
	c, err := decimal128Ctx.Ln(&dec, f.dec)
	if err != nil {
		return nil, mapDecimalConditionErr(c)
	}

	return Float{dec: &dec}, nil
}

// sqrt returns the square root of x.
func sqrt(x Number) (Number, error) {
	f, err := numberToFloat(x)
	if err != nil {
		return nil, err
	}

	if f.Negative() {
		return nil, exceptionalValueUndefined
	}

	var dec apd.Decimal
	c, err := decimal128Ctx.Sqrt(&dec, f.dec)
	if err != nil {
		return nil, mapDecimalConditionErr(c)
	}

	return Float{dec: &dec}, nil
}

// bitwiseRightShift returns n bit-shifted by s to the right.
func bitwiseRightShift(n, s Number) (Number, error) {
	switch n := n.(type) {
	case Integer:
		switch s := s.(type) {
		case Integer:
			return Integer(n >> s), nil
		default:
			return nil, typeError(validTypeInteger, s, nil)
		}
	default:
		return nil, typeError(validTypeInteger, n, nil)
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
			return nil, typeError(validTypeInteger, s, nil)
		}
	default:
		return nil, typeError(validTypeInteger, n, nil)
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
			return nil, typeError(validTypeInteger, b2, nil)
		}
	default:
		return nil, typeError(validTypeInteger, b1, nil)
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
			return nil, typeError(validTypeInteger, b2, nil)
		}
	default:
		return nil, typeError(validTypeInteger, b1, nil)
	}
}

// bitwiseComplement returns the logical negation on each bit.
func bitwiseComplement(b1 Number) (Number, error) {
	switch b1 := b1.(type) {
	case Integer:
		return ^b1, nil
	default:
		return nil, typeError(validTypeInteger, b1, nil)
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
			return nil, typeError(validTypeInteger, y, nil)
		}
	default:
		return nil, typeError(validTypeInteger, x, nil)
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
			if NewFloatFromInt64(int64(x)).Lt(y) {
				return y, nil
			}
			return x, nil
		default:
			return nil, exceptionalValueUndefined
		}
	case Float:
		switch y := y.(type) {
		case Integer:
			if x.Lt(NewFloatFromInt64(int64(y))) {
				return y, nil
			}
			return x, nil
		case Float:
			if x.Lt(y) {
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
			if NewFloatFromInt64(int64(x)).Gt(y) {
				return y, nil
			}
			return x, nil
		default:
			return nil, exceptionalValueUndefined
		}
	case Float:
		switch y := y.(type) {
		case Integer:
			if x.Gt(NewFloatFromInt64(int64(y))) {
				return y, nil
			}
			return x, nil
		case Float:
			if x.Gt(y) {
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
			return nil, typeError(validTypeFloat, vx, nil)
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

// xor returns the bitwise exclusive or of x and y.
func xor(x, y Number) (Number, error) {
	vx, ok := x.(Integer)
	if !ok {
		return nil, typeError(validTypeInteger, x, nil)
	}

	vy, ok := y.(Integer)
	if !ok {
		return nil, typeError(validTypeInteger, y, nil)
	}

	return vx ^ vy, nil
}

// Comparison

func eqF(x, y Float) bool {
	return x.Eq(y)
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
	return !x.Eq(y)
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
	return x.Lt(y)
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
	return x.Lte(y)
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
	return x.Gt(y)
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
	return x.Gte(y)
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
	return NewFloatFromInt64(int64(n))
}

func floatFtoF(x Float) Float {
	return x
}

func floorFtoI(x Float) (Integer, error) {
	var dec apd.Decimal
	c, err := decimal128Ctx.Floor(&dec, x.dec)
	if err != nil {
		return 0, mapDecimalConditionErr(c)
	}

	i64, err := dec.Int64()
	if err != nil {
		return 0, exceptionalValueIntOverflow
	}

	return Integer(i64), nil
}

func truncateFtoI(x Float) (Integer, error) {
	if x.Negative() {
		return ceilingFtoI(x)
	}
	return floorFtoI(x)
}

func roundFtoI(x Float) (Integer, error) {
	var dec apd.Decimal
	c, err := decimal128Ctx.RoundToIntegralExact(&dec, x.dec)
	if err != nil {
		return 0, mapDecimalConditionErr(c)
	}

	i64, err := dec.Int64()
	if err != nil {
		return 0, exceptionalValueIntOverflow
	}

	return Integer(i64), nil
}

func ceilingFtoI(x Float) (Integer, error) {
	var dec apd.Decimal
	c, err := decimal128Ctx.Ceil(&dec, x.dec)
	if err != nil {
		return 0, mapDecimalConditionErr(c)
	}

	i64, err := dec.Int64()
	if err != nil {
		return 0, exceptionalValueIntOverflow
	}

	return Integer(i64), nil
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
	var dec apd.Decimal
	c, err := decimal128Ctx.Add(&dec, x.dec, y.dec)
	if err != nil {
		return Float{}, mapDecimalConditionErr(c)
	}

	return Float{dec: &dec}, nil
}

func subF(x, y Float) (Float, error) {
	var dec apd.Decimal
	c, err := decimal128Ctx.Sub(&dec, x.dec, y.dec)
	if err != nil {
		return Float{}, mapDecimalConditionErr(c)
	}

	return Float{dec: &dec}, nil
}

func mulF(x, y Float) (Float, error) {
	var dec apd.Decimal
	c, err := decimal128Ctx.Mul(&dec, x.dec, y.dec)
	if err != nil {
		return Float{}, mapDecimalConditionErr(c)
	}

	return Float{dec: &dec}, nil
}

func divF(x, y Float) (Float, error) {
	var dec apd.Decimal
	c, err := decimal128Ctx.Quo(&dec, x.dec, y.dec)
	if err != nil {
		return Float{}, mapDecimalConditionErr(c)
	}

	return Float{dec: &dec}, nil
}

func negF(x Float) (Float, error) {
	return mulF(x, minusOneFloat)
}

func absF(x Float) Float {
	var dec apd.Decimal
	return Float{dec: dec.Abs(x.dec)}
}

func signF(x Float) Float {
	return NewFloatFromInt64(int64(x.dec.Sign()))
}

func intPartF(x Float) (Float, error) {
	var dec apd.Decimal
	c, err := decimal128Ctx.Floor(&dec, x.dec)
	if err != nil {
		return Float{}, mapDecimalConditionErr(c)
	}

	return Float{dec: &dec}, nil
}

func fractPartF(x Float) (Float, error) {
	i, err := intPartF(x)
	if err != nil {
		return Float{}, err
	}

	var dec apd.Decimal
	c, err := decimal128Ctx.Sub(&dec, x.dec, i.dec)
	if err != nil {
		return Float{}, mapDecimalConditionErr(c)
	}

	return Float{dec: &dec}, nil
}

func posF(x Float) (Float, error) {
	return x, nil
}

// Mixed mode operations

func addFI(x Float, n Integer) (Float, error) {
	return addF(x, NewFloatFromInt64(int64(n)))
}

func addIF(n Integer, x Float) (Float, error) {
	return addF(NewFloatFromInt64(int64(n)), x)
}

func subFI(x Float, n Integer) (Float, error) {
	return subF(x, NewFloatFromInt64(int64(n)))
}

func subIF(n Integer, x Float) (Float, error) {
	return subF(NewFloatFromInt64(int64(n)), x)
}

func mulFI(x Float, n Integer) (Float, error) {
	return mulF(x, NewFloatFromInt64(int64(n)))
}

func mulIF(n Integer, x Float) (Float, error) {
	return mulF(NewFloatFromInt64(int64(n)), x)
}

func divFI(x Float, n Integer) (Float, error) {
	return divF(x, NewFloatFromInt64(int64(n)))
}

func divIF(n Integer, x Float) (Float, error) {
	return divF(NewFloatFromInt64(int64(n)), x)
}

func divII(n, m Integer) (Float, error) {
	return divF(NewFloatFromInt64(int64(n)), NewFloatFromInt64(int64(m)))
}

// Utility function to cast a Number to a Float
func numberToFloat(x Number) (Float, error) {
	switch x := x.(type) {
	case Integer:
		return NewFloatFromInt64(int64(x)), nil
	case Float:
		return x, nil
	default:
		return Float{}, exceptionalValueUndefined
	}
}
