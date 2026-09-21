package syntax

import "slices"

type CharConversion struct {
	Disabled bool
	Entries  []CharConversionEntry
}

func (c *CharConversion) Map(r rune) rune {
	if c == nil || c.Disabled {
		return r
	}
	i := slices.IndexFunc(c.Entries, func(entry CharConversionEntry) bool {
		return entry.In == r
	})
	if i < 0 {
		return r
	}
	return c.Entries[i].Out
}

type CharConversionEntry struct {
	In, Out rune
}
