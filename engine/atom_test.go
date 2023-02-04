package engine

import (
	"bytes"
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestAtom_WriteTerm(t *testing.T) {
	tests := []struct {
		name   string
		opts   WriteOptions
		output string
	}{
		{name: `a`, opts: WriteOptions{quoted: false}, output: `a`},
		{name: `a`, opts: WriteOptions{quoted: true}, output: `a`},
		{name: "\a\b\f\n\r\t\v\x00\\'\"`", opts: WriteOptions{quoted: false}, output: "\a\b\f\n\r\t\v\x00\\'\"`"},
		{name: "\a\b\f\n\r\t\v\x00\\'\"`", opts: WriteOptions{quoted: true}, output: "'\\a\\b\\f\\n\\r\\t\\v\\x0\\\\\\\\'\"`'"},
		{name: `,`, opts: WriteOptions{quoted: false}, output: `,`},
		{name: `,`, opts: WriteOptions{quoted: true}, output: `','`},
		{name: `[]`, opts: WriteOptions{quoted: false}, output: `[]`},
		{name: `[]`, opts: WriteOptions{quoted: true}, output: `[]`},
		{name: `{}`, opts: WriteOptions{quoted: false}, output: `{}`},
		{name: `{}`, opts: WriteOptions{quoted: true}, output: `{}`},
		{name: `-`, output: `-`},
		{name: `-`, opts: WriteOptions{ops: operators{atomPlus: {}, atomMinus: {}}, left: operator{specifier: operatorSpecifierFY, name: atomPlus}}, output: ` (-)`},
		{name: `-`, opts: WriteOptions{ops: operators{atomPlus: {}, atomMinus: {}}, right: operator{name: atomPlus}}, output: `(-)`},
		{name: `X`, opts: WriteOptions{quoted: true, left: operator{name: NewAtom(`F`)}}, output: ` 'X'`},  // So that it won't be 'F''X'.
		{name: `X`, opts: WriteOptions{quoted: true, right: operator{name: NewAtom(`F`)}}, output: `'X' `}, // So that it won't be 'X''F'.
		{name: `foo`, opts: WriteOptions{left: operator{name: NewAtom(`bar`)}}, output: ` foo`},            // So that it won't be barfoo.
		{name: `foo`, opts: WriteOptions{right: operator{name: NewAtom(`bar`)}}, output: `foo `},           // So that it won't be foobar.},
	}

	var buf bytes.Buffer
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			buf.Reset()
			assert.NoError(t, NewAtom(tt.name).WriteTerm(&buf, &tt.opts, nil))
			assert.Equal(t, tt.output, buf.String())
		})
	}
}

func TestAtom_Compare(t *testing.T) {
	x := NewVariable()

	tests := []struct {
		title string
		a     Atom
		t     Term
		o     int
	}{
		{title: `a > X`, a: NewAtom("a"), t: x, o: 1},
		{title: `a > 1.0`, a: NewAtom("a"), t: Float(1), o: 1},
		{title: `a > 1`, a: NewAtom("a"), t: Integer(1), o: 1},
		{title: `a > 'Z'`, a: NewAtom("a"), t: NewAtom("Z"), o: 1},
		{title: `a = a`, a: NewAtom("a"), t: NewAtom("a"), o: 0},
		{title: `a < b`, a: NewAtom("a"), t: NewAtom("b"), o: -1},
		{title: `a < f(a)`, a: NewAtom("a"), t: NewAtom("f").Apply(NewAtom("a")), o: -1},
	}

	for _, tt := range tests {
		t.Run(tt.title, func(t *testing.T) {
			assert.Equal(t, tt.o, tt.a.Compare(tt.t, nil))
		})
	}
}
