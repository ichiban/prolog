package main

import (
	"io"

	"github.com/ichiban/prolog"
	"github.com/ichiban/prolog/engine"
)

func New(r io.Reader, w io.Writer) *prolog.Interpreter {
	i := prolog.New(r, w)
	i.Register2("call_nth", i.CallNth)
	i.Register4("skip_max_list", engine.SkipMaxList)
	return i
}
