package main

import (
	"context"
	"embed"
	"flag"
	"fmt"

	"github.com/ichiban/prolog"
	"github.com/ichiban/prolog/internal"
)

//go:embed prolog
var prologTexts embed.FS

func main() {
	var n int
	flag.IntVar(&n, "n", 3, "the number of disks")
	flag.Parse()

	i := prolog.New(nil, nil)
	i.FS = prologTexts
	if err := i.LoadFile(context.Background(), "prolog/hanoi.pl"); err != nil {
		panic(err)
	}

	m := i.TypeInModule()
	m.SetPredicate2("actuate", func(_ *internal.VM, x internal.Term, y internal.Term, k internal.Cont, env *internal.Env) *internal.Promise {
		fmt.Printf("move a disk from %s to %s.\n", env.Resolve(x), env.Resolve(y))
		return k(env)
	})

	sols, err := i.Query(`hanoi(?).`, n)
	if err != nil {
		panic(err)
	}
	defer func() {
		if err := sols.Close(); err != nil {
			panic(err)
		}
	}()

	if !sols.Next() {
		panic("failed")
	}
}
