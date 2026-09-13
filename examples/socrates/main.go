package main

import (
	"context"
	"embed"
	"fmt"

	"github.com/ichiban/prolog/v2"
)

//go:embed src
var src embed.FS

func main() {
	// Construct a Prolog interpreter.
	p := prolog.New()

	// Mount a fs.FS that contains a Prolog file.
	if err := p.MountFS("", src); err != nil {
		panic(err)
	}

	ctx := context.Background()

	// Load the file.
	if err := p.Load(ctx, "", "src/human.pl"); err != nil {
		panic(err)
	}

	// Define a struct type with fields which name corresponds with a variable in the query.
	type result struct {
		Who prolog.Atom
	}

	// Iterates over solutions.
	for r, err := range p.Query[result](ctx, `mortal(Who).`) {
		// Check if an error occurred while querying.
		if err != nil {
			panic(err)
		}

		fmt.Printf("Who = %s\n", r.Who) // ==> Who = socrates
	}
}
