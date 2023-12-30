package main

import (
	"context"
	"embed"
	_ "embed"
	"os"

	"github.com/ichiban/prolog"
)

//go:embed prolog
var prologTexts embed.FS

func main() {
	p := prolog.New(nil, os.Stdout)
	p.FS = prologTexts
	if err := p.Load(context.Background(), "prolog/hello.pl"); err != nil {
		panic(err)
	}
}
