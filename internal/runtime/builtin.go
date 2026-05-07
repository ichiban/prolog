package runtime

import "github.com/ichiban/prolog/v2/internal/term"

type BuiltIn struct {
	PI     term.Functor
	Inline bool
	// TODO: Implement the rest.
}
