package main

import (
	"fmt"

	"github.com/ichiban/prolog"
)

// http://www.cse.unsw.edu.au/~billw/dictionaries/prolog/cut.html
func main() {
	p := prolog.New(nil, nil)
	if err := p.Exec(`
teaches(dr_fred, history).
teaches(dr_fred, english).
teaches(dr_fred, drama).
teaches(dr_fiona, physics).

studies(alice, english).
studies(angus, english).
studies(amelia, drama).
studies(alex, physics).
`); err != nil {
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
