package ring

import (
	"io"
	"reflect"
	"strings"
	"testing"
)

func TestRuneReader_ReadRune(t *testing.T) {
	tests := []struct {
		title string
		str   string
		size  int
		r     rune
		n     int
		err   error
	}{
		{title: "EOF", str: "", size: 0, r: 0, n: 0, err: io.EOF},
		{title: "ok", str: "foo", size: 2, r: 'f', n: 1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			rr := NewRuneReader(strings.NewReader(tt.str), tt.size)
			r, n, err := rr.ReadRune()
			if !reflect.DeepEqual(err, tt.err) {
				t.Errorf("ReadRune() error = %v, wantErr %v", err, tt.err)
			}
			if r != tt.r {
				t.Errorf("ReadRune() r = %v, want %v", r, tt.r)
			}
			if n != tt.n {
				t.Errorf("ReadRune() n = %v, want %v", n, tt.n)
			}
		})
	}
}
