package ring

import "io"

type runeWithSize struct {
	rune rune
	size int
}

type RuneReader struct {
	base io.RuneReader
	buf  *Buffer[runeWithSize]
}

func NewRuneReader(r io.RuneReader, size int) *RuneReader {
	return &RuneReader{
		base: r,
		buf:  NewBuffer[runeWithSize](size),
	}
}

func (r *RuneReader) ReadRune() (rune, int, error) {
	if r.buf.Empty() {
		c, n, err := r.base.ReadRune()
		if err != nil {
			return c, n, err
		}
		r.buf.Put(runeWithSize{rune: c, size: n})
	}
	rs := r.buf.Get()
	return rs.rune, rs.size, nil
}

func (r *RuneReader) UnreadRune() error {
	r.buf.Backup()
	return nil
}
