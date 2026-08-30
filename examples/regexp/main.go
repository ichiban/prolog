package main

import (
	"context"
	"fmt"
	"regexp"

	"github.com/ichiban/prolog/v2"
)

func main() {
	// Construct an interpreter.
	p := prolog.New()

	// Register a Go function as a custom predicate.
	if err := p.Register3("regexp", func(ctx context.Context, e prolog.Execution, pattern, text, match prolog.Term) prolog.Outcome {
		// First, check the arguments.
		p, err := e.String(pattern)
		if err != nil {
			return e.Error(err)
		}
		t, err := e.String(text)
		if err != nil {
			return e.Error(err)
		}
		if !e.Variable(match) { // An output argument may or may not be a variable.
			if _, err := e.String(match); err != nil {
				return e.Error(err)
			}
		}

		// Then, execute the core logic of the predicate.
		r, err := regexp.Compile(p)
		if err != nil {
			return e.Error(err)
		}

		// Lastly, return an outcome: Success, Failure, etc.
		// Here it's Nondet, because there can be multiple solutions.
		return e.Nondet(func(yield func(prolog.Outcome) bool) {
			// Inside Nondet, yield as many outcomes as you want while executing your logic.
			for _, m := range r.FindAllString(t, -1) {
				// If you run into an error, yield it as an error outcome.
				// After that, return from the iterator.
				s, err := e.NewString(m)
				if err != nil {
					_ = yield(e.Error(err))
					return
				}

				// A custom predicate with an output argument typically ends with a Unification outcome.
				// It's Success if the two terms unify and Failure if they don't.
				if !yield(e.Unification(match, s)) {
					return
				}
			}
		})
	}); err != nil {
		panic(err)
	}

	ctx := context.Background()

	// Now your custom predicate acts as a builtin predicate.
	for r, err := range p.Query[struct {
		Match string
	}](ctx, `regexp("[0-9]+", "a1b22c333", Match).`) {
		if err != nil {
			panic(err)
		}

		fmt.Printf("Match = %q\n", r.Match)
		// ==> Match = "1"
		// ==> Match = "22"
		// ==> Match = "333"
	}
}
