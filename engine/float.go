package engine

// Float is a prolog floating-point number.
type Float float64

func (f Float) number() {}

// Compare compares the float to another term.
func (f Float) Compare(t Term, env *Env) int64 {
	switch t := env.Resolve(t).(type) {
	case Variable:
		return 1
	case Float:
		switch d := f - t; {
		case d < 0:
			return -1
		case d > 0:
			return 1
		default:
			return 0
		}
	default:
		return -1
	}
}
