package syntax

import (
	"fmt"

	"github.com/ichiban/prolog/v2/internal/term"
)

type Serialized string

func Serialize(arena *term.Arena, t term.Handle) Serialized {
	return Serialized(fmt.Sprintf("%s .", &Formatter{
		Arena:  arena,
		Term:   t,
		Quoted: true,
	}))
}

func Deserialize(arena *term.Arena, s Serialized) (term.Handle, error) {
	return ParseTerm(string(s), Arena(arena))
}
