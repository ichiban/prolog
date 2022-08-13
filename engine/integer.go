package engine

// Integer is a prolog integer.
type Integer int64

func (i Integer) number() {}

// Compare compares the integer to another term.
func (i Integer) Compare(t Term, env *Env) int64 {
	switch t := env.Resolve(t).(type) {
	case Variable, Float:
		return 1
	case Integer:
		return int64(i - t)
	default:
		return -1
	}
}
