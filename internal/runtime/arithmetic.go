package runtime

import "math"

type ExceptionalValue int8

const (
	FloatOverflow ExceptionalValue = iota
	IntOverflow
	Underflow
	ZeroDivisor
	Undefined
)

func (e ExceptionalValue) Error() string {
	return exceptionalValueNames[e]
}

var exceptionalValueNames = [...]string{
	FloatOverflow: "float_overflow",
	IntOverflow:   "int_overflow",
	Underflow:     "underflow",
	ZeroDivisor:   "zero_divisor",
	Undefined:     "undefined",
}

func addI(x, y int64) (int64, error) {
	switch {
	case y > 0 && x > math.MaxInt64-y:
		return 0, IntOverflow
	case y < 0 && x < math.MinInt64-y:
		return 0, IntOverflow
	default:
		return x + y, nil
	}
}

func addF(x, y float64) (float64, error) {
	switch {
	case y > 0 && x > math.MaxFloat64-y:
		return 0, FloatOverflow
	case y < 0 && x < -math.MaxFloat64-y:
		return 0, FloatOverflow
	default:
		return x + y, nil
	}
}

func addIF(x int64, y float64) (float64, error) {
	return addF(float64(x), y)
}

func addFI(x float64, y int64) (float64, error) {
	return addF(x, float64(y))
}

func subI(x, y int64) (int64, error) {
	switch {
	case y < 0 && x > math.MaxInt64+y:
		return 0, IntOverflow
	case y > 0 && x < math.MinInt64+y:
		return 0, IntOverflow
	default:
		return x - y, nil
	}
}

func subF(x, y float64) (float64, error) {
	return addF(x, -y)
}

func subFI(x float64, n int64) (float64, error) {
	return subF(x, float64(n))
}

func subIF(n int64, x float64) (float64, error) {
	return subF(float64(n), x)
}

func mulI(x, y int64) (int64, error) {
	switch {
	case x == -1 && y == math.MinInt64:
		return 0, IntOverflow
	case x == math.MinInt64 && y == -1:
		return 0, IntOverflow
	case y == 0:
		return 0, nil
	default:
		r := x * y
		if r/y != x {
			return 0, IntOverflow
		}
		return r, nil
	}
}

func mulF(x, y float64) (float64, error) {
	switch {
	case y != 0 && math.Abs(x) > math.MaxFloat64/math.Abs(y):
		return 0, FloatOverflow
	}

	r := x * y

	// Underflow: x*y = 0 iff x = 0 or y = 0.
	if r == 0 && x != 0 && y != 0 {
		return 0, Underflow
	}

	return r, nil
}

func mulIF(n int64, x float64) (float64, error) {
	return mulF(float64(n), x)
}

func mulFI(x float64, n int64) (float64, error) {
	return mulF(x, float64(n))
}

func intDivI(x, y int64) (int64, error) {
	switch {
	case y == 0:
		return 0, ZeroDivisor
	case x == math.MinInt64 && y == -1:
		// Two's complement special case
		return 0, IntOverflow
	default:
		return x / y, nil
	}
}

func divI(n, m int64) (float64, error) {
	return divF(float64(n), float64(m))
}

func divF(x, y float64) (float64, error) {
	switch {
	case y == 0:
		return 0, ZeroDivisor
	case math.Abs(x) > math.MaxFloat64*math.Abs(y):
		return 0, FloatOverflow
	}

	r := x / y

	// Underflow: x/y = 0 iff x = 0 and y != 0.
	if r == 0 && x != 0 {
		return 0, Underflow
	}

	return r, nil
}

func divIF(n int64, x float64) (float64, error) {
	return divF(float64(n), x)
}

func divFI(x float64, n int64) (float64, error) {
	return divF(x, float64(n))
}

func remI(x, y int64) (int64, error) {
	if y == 0 {
		return 0, ZeroDivisor
	}
	return x - ((x / y) * y), nil
}

func modI(x, y int64) (int64, error) {
	if y == 0 {
		return 0, ZeroDivisor
	}
	return x - (int64(math.Floor(float64(x)/float64(y))) * y), nil
}

func negI(x int64) (int64, error) {
	// Two's complement special case
	if x == math.MinInt64 {
		return 0, IntOverflow
	}
	return -x, nil
}

func negF(x float64) float64 {
	return -x
}

func absI(x int64) (int64, error) {
	switch {
	case x == math.MinInt64:
		return 0, IntOverflow
	case x < 0:
		return -x, nil
	default:
		return x, nil
	}
}

func absF(x float64) float64 {
	return math.Abs(float64(x))
}

func signI(x int64) int64 {
	switch {
	case x > 0:
		return 1
	case x < 0:
		return -1
	default:
		return 0
	}
}

func signF(x float64) float64 {
	switch {
	case x > 0:
		return 1
	case x < 0:
		return -1
	default:
		return 0
	}
}

func posI(x int64) (int64, error) {
	return x, nil
}

func posF(x float64) (float64, error) {
	return x, nil
}

func intFloorDivI(x, y int64) (int64, error) {
	switch {
	case x == math.MinInt64 && y == -1:
		return 0, IntOverflow
	case y == 0:
		return 0, ZeroDivisor
	default:
		return int64(math.Floor(float64(x) / float64(y))), nil
	}
}

func intPartF(x float64) float64 {
	s := signF(x)
	return s * math.Floor(math.Abs(x))
}

func fractPartF(x float64) float64 {
	i := intPartF(x)
	return x - i
}

func eqI(m, n int64) bool {
	return m == n
}

func eqF(x, y float64) bool {
	return x == y
}

func eqFI(x float64, n int64) bool {
	y := floatItoF(n)
	return eqF(x, y)
}

func eqIF(n int64, y float64) bool {
	return eqFI(y, n)
}

func neqF(x, y float64) bool {
	return x != y
}

func neqI(m, n int64) bool {
	return m != n
}

func neqFI(x float64, n int64) bool {
	y := floatItoF(n)
	return neqF(x, y)
}

func neqIF(n int64, y float64) bool {
	return neqFI(y, n)
}

func lssF(x, y float64) bool {
	return x < y
}

func lssI(m, n int64) bool {
	return m < n
}

func lssFI(x float64, n int64) bool {
	y := floatItoF(n)
	return lssF(x, y)
}

func lssIF(n int64, y float64) bool {
	return gtrFI(y, n)
}

func leqF(x, y float64) bool {
	return x <= y
}

func leqI(m, n int64) bool {
	return m <= n
}

func leqFI(x float64, n int64) bool {
	y := floatItoF(n)
	return leqF(x, y)
}

func leqIF(n int64, y float64) bool {
	return geqFI(y, n)
}

func gtrF(x, y float64) bool {
	return x > y
}

func gtrI(m, n int64) bool {
	return m > n
}

func gtrFI(x float64, n int64) bool {
	y := floatItoF(n)
	return gtrF(x, y)
}

func gtrIF(n int64, y float64) bool {
	return lssFI(y, n)
}

func geqF(x, y float64) bool {
	return x >= y
}

func geqI(m, n int64) bool {
	return m >= n
}

func geqFI(x float64, n int64) bool {
	y := floatItoF(n)
	return geqF(x, y)
}

func geqIF(n int64, y float64) bool {
	return leqFI(y, n)
}

// Type conversion operations

func floatItoF(n int64) float64 {
	return float64(n)
}

func floatFtoF(x float64) float64 {
	return x
}

func floorFtoI(x float64) (int64, error) {
	f := math.Floor(x)
	if f >= float64(math.MaxInt64) || f < float64(math.MinInt64) {
		return 0, IntOverflow
	}
	return int64(f), nil
}

func truncateFtoI(x float64) (int64, error) {
	t := math.Trunc(x)
	if t >= float64(math.MaxInt64) || t < float64(math.MinInt64) {
		return 0, IntOverflow
	}
	return int64(t), nil
}

func roundFtoI(x float64) (int64, error) {
	r := math.Round(x)
	if r >= float64(math.MaxInt64) || r < float64(math.MinInt64) {
		return 0, IntOverflow
	}
	return int64(r), nil
}

func ceilingFtoI(x float64) (int64, error) {
	c := math.Ceil(x)
	if c >= float64(math.MaxInt64) || c < float64(math.MinInt64) {
		return 0, IntOverflow
	}
	return int64(c), nil
}
