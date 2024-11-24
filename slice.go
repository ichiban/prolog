package prolog

// cappend stands for capped append.
func cappend[S ~[]E, E any](slice S, elems ...E) (S, bool) {
	if len(slice)+len(elems) > cap(slice) {
		return slice, false
	}
	return append(slice, elems...), true
}
