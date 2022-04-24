package main

import (
	"io"

	"github.com/ichiban/prolog"
)

func New(r io.Reader, w io.Writer) *prolog.Interpreter {
	i := prolog.New(r, w)
	i.Register2("call_nth", i.CallNth)
	return i
}
