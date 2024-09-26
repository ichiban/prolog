package internal

import "slices"

// cappend stands for capped append.
func cappend[S ~[]E, E any](slice S, elems ...E) (S, bool) {
	if len(slice)+len(elems) > cap(slice) {
		return slice, false
	}
	return append(slice, elems...), true
}

func AppendUnique[S ~[]E, E comparable](slice S, elems ...E) S {
	elems = slices.DeleteFunc(elems, func(e E) bool {
		return slices.Contains(slice, e)
	})
	return append(slice, elems...)
}
