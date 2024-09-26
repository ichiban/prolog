package internal

type StringPool struct {
	entries []stringEntry
}

type stringEntry struct {
	offset int32 // So that we can do binary search for the entry.
	value  string
	rest   Term
}

func (r *StringPool) Put(str string, rest Term) (Term, error) {
	var ok bool
	r.entries, ok = cappend(r.entries, stringEntry{
		offset: 0, // TODO: last offset + length of last string
		value:  str,
		rest:   rest,
	})
	if !ok {
		return Term{}, errTooManyStrings
	}

	return Term{
		tag:     termTagString,
		payload: int32(len(r.entries) - 1),
	}, nil
}
