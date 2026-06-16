package runtime

import (
	"fmt"
	"testing"

	"github.com/ichiban/prolog/v2/internal/ir"
	"github.com/ichiban/prolog/v2/internal/term"
)

func TestEngine_Call(t *testing.T) {
	e := Engine{
		Arena: &term.Arena{
			Heap: make(term.Heap, 0, 1024),
		},
	}
	if err := e.BuiltinSet.Set(term.NewFunctor(term.NewAtom("true"), 1), True0); err != nil {
		t.Fatal(err)
	}
	if err := e.BuiltinSet.Set(term.NewFunctor(term.NewAtom("call"), 2), Call1); err != nil {
		t.Fatal(err)
	}

	c := Compiler{
		Engine: &e,
	}

	var m ir.Module
	if err := c.CompileModule(t.Context(), &m, `
p.
p.
`); err != nil {
		t.Fatal(err)
	}

	if err := e.LoadModule(&m); err != nil {
		t.Fatal(err)
	}

	fmt.Printf("image: \n%v\n", &e.Image)

	g, err := e.PutAtom(term.NewAtomRune('p'))
	if err != nil {
		t.Fatal(err)
	}

	count := 0
	for err := range e.Call(t.Context(), g) {
		if err != nil {
			t.Fatal(err)
		}
		count++
	}

	if count != 2 {
		t.Errorf("got %d, want 2", count)
	}
}
