package main

import (
	"context"
	"embed"
	"fmt"

	"github.com/ichiban/prolog"
)

//go:embed prolog
var prologTexts embed.FS

// http://www.cse.unsw.edu.au/~billw/dictionaries/prolog/cut.html
func main() {
	p := prolog.New(nil, nil)
	p.FS = prologTexts
	if err := p.LoadFile(context.Background(), "prolog/main.pl"); err != nil {
		panic(err)
	}

	for _, q := range []string{
		`teaches(dr_fred, Course), studies(Student, Course).`,
		`teaches(dr_fred, Course), !, studies(Student, Course).`,
		`teaches(dr_fred, Course), studies(Student, Course), !.`,
		`!, teaches(dr_fred, Course), studies(Student, Course).`,
	} {
		fmt.Printf("%s\n", q)

		sols, err := p.Query(q)
		if err != nil {
			panic(err)
		}

		for sols.Next() {
			var s struct {
				Course  string
				Student string
			}
			if err := sols.Scan(&s); err != nil {
				panic(err)
			}
			fmt.Printf("\t%+v\n", s)
		}

		fmt.Printf("\n")
		if err := sols.Close(); err != nil {
			panic(err)
		}
	}
}
