package main

import (
	"context"
	"embed"
	"fmt"
	"github.com/ichiban/prolog/engine"

	"github.com/ichiban/prolog"
)

//go:embed prolog
var prologTexts embed.FS

func main() {
	// Instantiates a new Prolog interpreter without any builtin predicates nor operators.
	p := new(prolog.Interpreter)
	p.FS = prologTexts

	// In this vanilla interpreter, even the infix operator `:-` is not defined.
	// Instead of writing `:-(mortal(X), human(X)).`, you may want to define the infix operator first.

	// To define operators, register op/3.
	m := p.Module()
	m.Register3("op", engine.Op)

	// Then, define the infix operator with priority 1200 and specifier XFX.
	if err := p.QuerySolution(`op(1200, xfx, :-).`).Err(); err != nil {
		panic(err)
	}

	// You may also want to register other predicates or define other operators to match your use case.
	// You can use p.Register0~5 to register any builtin/custom predicates of respective arity.

	// Now you can load a Prolog program with infix `:-`.
	if err := p.Load(context.Background(), "prolog/main.pl"); err != nil {
		panic(err)
	}

	// Run the Prolog program.
	sols, err := p.Query(`mortal(Who).`)
	if err != nil {
		panic(err)
	}
	defer sols.Close()

	for sols.Next() {
		var s struct {
			Who string
		}
		if err := sols.Scan(&s); err != nil {
			panic(err)
		}
		fmt.Printf("Who = %s\n", s.Who)
		// ==> Who = socrates
	}
}
