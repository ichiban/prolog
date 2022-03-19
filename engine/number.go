package engine

import "math"

// DefaultEvaluableFunctors is a EvaluableFunctors with builtin functions.
var DefaultEvaluableFunctors = EvaluableFunctors{
	Unary: map[Atom]func(Number) (Number, error){
		`-`:                     Neg,
		`abs`:                   Abs,
		`sign`:                  Sign,
		`float_integer_part`:    FloatIntegerPart,
		`float_fractional_part`: FloatFractionalPart,
		`float`:                 AsFloat,
		`floor`:                 Floor,
		`truncate`:              Truncate,
		`round`:                 Round,
		`ceiling`:               Ceiling,

		`sin`:  Sin,
		`cos`:  Cos,
		`atan`: Atan,
		`exp`:  Exp,
		`log`:  Log,
		`sqrt`: Sqrt,

		`\`: BitwiseComplement,

		`+`: Pos,
	},
	Binary: map[Atom]func(Number, Number) (Number, error){
		`+`:   Add,
		`-`:   Sub,
		`*`:   Mul,
		`//`:  IntDiv,
		`/`:   Div,
		`rem`: Rem,
		`mod`: Mod,

		`**`: Power,

		`>>`: BitwiseRightShift,
		`<<`: BitwiseLeftShift,
		`/\`: BitwiseAnd,
		`\/`: BitwiseOr,

		`div`: IntFloorDiv,
		`max`: Max,
		`min`: Min,
		`^`:   IntegerPower,
	},
}

// Number is a prolog number, either Integer or Float.
type Number interface {
	Term
	number()
}

// EvaluableFunctors is a set of unary/binary functions.
type EvaluableFunctors struct {
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

func (e EvaluableFunctors) eval(expression Term, env *Env) (Number, error) {
	switch t := env.Resolve(expression).(type) {
	case Variable:
		return nil, ErrInstantiation
	case Atom: // TODO: constants?
		return nil, typeErrorEvaluable(&Compound{
			Functor: "/",
			Args:    []Term{t, Integer(0)},
		})
	case Number:
		return t, nil
	case *Compound:
		switch arity := len(t.Args); arity {
		case 1:
			f, ok := e.Unary[t.Functor]
			if !ok {
				return nil, typeErrorEvaluable(&Compound{
					Functor: "/",
					Args: []Term{
						t.Functor,
						Integer(1),
					},
				})
			}
			x, err := e.eval(t.Args[0], env)
			if err != nil {
				return nil, err
			}
			return f(x)
		case 2:
			f, ok := e.Binary[t.Functor]
			if !ok {
				return nil, typeErrorEvaluable(&Compound{
					Functor: "/",
					Args: []Term{
						t.Functor,
						Integer(2),
					},
				})
			}
			x, err := e.eval(t.Args[0], env)
			if err != nil {
				return nil, err
			}
			y, err := e.eval(t.Args[1], env)
			if err != nil {
				return nil, err
			}
			return f(x, y)
		default:
			return nil, typeErrorEvaluable(&Compound{
				Functor: "/",
				Args:    []Term{t.Functor, Integer(arity)},
			})
		}
	default:
		return nil, typeErrorEvaluable(&Compound{
			Functor: "/",
			Args:    []Term{t, Integer(0)},
		})
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
	return nil, ErrUndefined
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
	return nil, ErrUndefined
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
	return nil, ErrUndefined
}

// IntDiv returns integer division of 2 numbers.
func IntDiv(x, y Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		switch y := y.(type) {
		case Integer:
			return intDivI(x, y)
		default:
			return nil, TypeErrorInteger(y)
		}
	default:
		return nil, TypeErrorInteger(x)
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
	return nil, ErrUndefined
}

// Rem returns remainder of 2 numbers.
func Rem(x, y Number) (Number, error) {
	switch x := x.(type) {
	case Integer:
		switch y := y.(type) {
		case Integer:
			return remI(x, y)
		default:
			return nil, TypeErrorInteger(y)
		}
	default:
		return nil, TypeErrorInteger(x)
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
			return nil, TypeErrorInteger(y)
		}
	default:
		return nil, TypeErrorInteger(x)
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
		return nil, ErrUndefined
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
		return nil, ErrUndefined
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
		return nil, ErrUndefined
	}
}

// FloatIntegerPart returns the integer part of x.
func FloatIntegerPart(x Number) (Number, error) {
	switch x := x.(type) {
	case Float:
		return intPartF(x), nil
	default:
		return nil, TypeErrorFloat(x)
	}
}

// FloatFractionalPart returns the fractional part of x.
func FloatFractionalPart(x Number) (Number, error) {
	switch x := x.(type) {
	case Float:
		return fractPartF(x), nil
	default:
		return nil, TypeErrorFloat(x)
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
		return nil, ErrUndefined
	}
}

// Floor returns the greatest integer value less than or equal to x.
func Floor(x Number) (Number, error) {
	switch x := x.(type) {
	case Float:
		return floorFtoI(x)
	default:
		return nil, TypeErrorFloat(x)
	}
}

// Truncate returns the integer value of x.
func Truncate(x Number) (Number, error) {
	switch x := x.(type) {
	case Float:
		return truncateFtoI(x)
	default:
		return nil, TypeErrorFloat(x)
	}
}

// Round returns the nearest integer of x.
func Round(x Number) (Number, error) {
	switch x := x.(type) {
	case Float:
		return roundFtoI(x)
	default:
		return nil, TypeErrorFloat(x)
	}
}

// Ceiling returns the least integer value greater than or equal to x.
func Ceiling(x Number) (Number, error) {
	switch x := x.(type) {
	case Float:
		return ceilingFtoI(x)
	default:
		return nil, TypeErrorFloat(x)
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
		return nil, ErrUndefined
	}

	var vy float64
	switch y := y.(type) {
	case Integer:
		vy = float64(y)
	case Float:
		vy = float64(y)
	default:
		return nil, ErrUndefined
	}

	// 9.3.1.3 d) special case
	if vx == 0 && vy < 0 {
		return nil, ErrUndefined
	}

	r := Float(math.Pow(vx, vy))

	switch {
	case math.IsInf(float64(r), 0):
		return nil, ErrFloatOverflow
	case r == 0 && vx != 0: // Underflow: r can be 0 iff x = 0.
		return nil, ErrUnderflow
	case math.IsNaN(float64(r)):
		return nil, ErrUndefined
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
		return nil, ErrUndefined
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
		return nil, ErrUndefined
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
		return nil, ErrUndefined
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
		return nil, ErrUndefined
	}

	// Positive overflow:
	//        e^x > max
	//   log(e^x) > log(max)
	// x * log(e) > log(max)
	//          x > log(max)
	if vx > math.Log(math.MaxFloat64) {
		return nil, ErrFloatOverflow
	}

	r := Float(math.Exp(vx))

	if r == 0 { // e^x != 0.
		return nil, ErrUnderflow
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
		return nil, ErrUndefined
	}

	if vx <= 0 {
		return nil, ErrUndefined
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
		return nil, ErrUndefined
	}

	if vx < 0 {
		return nil, ErrUndefined
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
			return nil, TypeErrorInteger(s)
		}
	default:
		return nil, TypeErrorInteger(n)
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
			return nil, TypeErrorInteger(s)
		}
	default:
		return nil, TypeErrorInteger(n)
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
			return nil, TypeErrorInteger(b2)
		}
	default:
		return nil, TypeErrorInteger(b1)
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
			return nil, TypeErrorInteger(b2)
		}
	default:
		return nil, TypeErrorInteger(b1)
	}
}

// BitwiseComplement returns the logical negation on each bit.
func BitwiseComplement(b1 Number) (Number, error) {
	switch b1 := b1.(type) {
	case Integer:
		return ^b1, nil
	default:
		return nil, TypeErrorInteger(b1)
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
		return nil, ErrUndefined
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
			return nil, TypeErrorInteger(y)
		}
	default:
		return nil, TypeErrorInteger(x)
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
			return nil, ErrUndefined
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
			return nil, ErrUndefined
		}
	default:
		return nil, ErrUndefined
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
			return nil, ErrUndefined
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
			return nil, ErrUndefined
		}
	default:
		return nil, ErrUndefined
	}
}

// IntegerPower returns x raised to the power of y.
func IntegerPower(x, y Number) (Number, error) {
	if x, ok := x.(Integer); ok {
		if y, ok := y.(Integer); ok {
			if x != 1 && y < -1 {
				return nil, TypeErrorFloat(x)
			}

			r, err := Power(x, y)
			if err != nil {
				return nil, err
			}
			return truncateFtoI(r.(Float))
		}
	}
	return Power(x, y)
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
	if f > math.MaxInt64 || f < math.MinInt64 {
		return 0, ErrIntOverflow
	}
	return Integer(f), nil
}

func truncateFtoI(x Float) (Integer, error) {
	t := math.Trunc(float64(x))
	if t > math.MaxInt64 || t < math.MinInt64 {
		return 0, ErrIntOverflow
	}
	return Integer(t), nil
}

func roundFtoI(x Float) (Integer, error) {
	r := math.Round(float64(x))
	if r > math.MaxInt64 || r < math.MinInt64 {
		return 0, ErrIntOverflow
	}
	return Integer(r), nil
}

func ceilingFtoI(x Float) (Integer, error) {
	c := math.Ceil(float64(x))
	if c > math.MaxInt64 || c < math.MinInt64 {
		return 0, ErrIntOverflow
	}
	return Integer(c), nil
}

// Integer operations

func addI(x, y Integer) (Integer, error) {
	switch {
	case y > 0 && x > math.MaxInt64-y:
		return 0, ErrIntOverflow
	case y < 0 && x < math.MinInt64-y:
		return 0, ErrIntOverflow
	default:
		return x + y, nil
	}
}

func subI(x, y Integer) (Integer, error) {
	switch {
	case y < 0 && x > math.MaxInt64+y:
		return 0, ErrIntOverflow
	case y > 0 && x < math.MinInt64+y:
		return 0, ErrIntOverflow
	default:
		return x - y, nil
	}
}

func mulI(x, y Integer) (Integer, error) {
	switch {
	case x == -1 && y == math.MinInt64:
		return 0, ErrIntOverflow
	case x == math.MinInt64 && y == -1:
		return 0, ErrIntOverflow
	case y == 0:
		return 0, nil
	case x > math.MaxInt64/y:
		return 0, ErrIntOverflow
	case x < math.MinInt64/y:
		return 0, ErrIntOverflow
	default:
		return x * y, nil
	}
}

func intDivI(x, y Integer) (Integer, error) {
	switch {
	case y == 0:
		return 0, ErrZeroDivisor
	case x == math.MinInt64 && y == -1:
		// Two's complement special case
		return 0, ErrIntOverflow
	default:
		return x / y, nil
	}
}

func remI(x, y Integer) (Integer, error) {
	if y == 0 {
		return 0, ErrZeroDivisor
	}
	return x - ((x / y) * y), nil
}

func modI(x, y Integer) (Integer, error) {
	if y == 0 {
		return 0, ErrZeroDivisor
	}
	return x - (Integer(math.Floor(float64(x)/float64(y))) * y), nil
}

func negI(x Integer) (Integer, error) {
	// Two's complement special case
	if x == math.MinInt64 {
		return 0, ErrIntOverflow
	}
	return -x, nil
}

func absI(x Integer) (Integer, error) {
	switch {
	case x == math.MinInt64:
		return 0, ErrIntOverflow
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
	case x == math.MinInt64 && y == -1:
		return 0, ErrIntOverflow
	case y == 0:
		return 0, ErrZeroDivisor
	default:
		return Integer(math.Floor(float64(x) / float64(y))), nil
	}
}

// Float operations

func addF(x, y Float) (Float, error) {
	switch {
	case y > 0 && x > math.MaxFloat64-y:
		return 0, ErrFloatOverflow
	case y < 0 && x < -math.MaxFloat64-y:
		return 0, ErrFloatOverflow
	}

	return x + y, nil
}

func subF(x, y Float) (Float, error) {
	return addF(x, -y)
}

func mulF(x, y Float) (Float, error) {
	switch {
	case y != 0 && x > math.MaxFloat64/y:
		return 0, ErrFloatOverflow
	case y != 0 && x < -math.MaxFloat64/y:
		return 0, ErrFloatOverflow
	}

	r := x * y

	// Underflow: x*y = 0 iff x = 0 or y = 0.
	if r == 0 && x != 0 && y != 0 {
		return 0, ErrUnderflow
	}

	return r, nil
}

func divF(x, y Float) (Float, error) {
	switch {
	case y == 0:
		return 0, ErrZeroDivisor
	case x > math.MaxFloat64*y:
		return 0, ErrFloatOverflow
	case x < -math.MaxFloat64*y:
		return 0, ErrFloatOverflow
	}

	r := x / y

	// Underflow: x/y = 0 iff x = 0 and y != 0.
	if r == 0 && x != 0 {
		return 0, ErrUnderflow
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
