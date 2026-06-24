package runtime

import (
	"fmt"
	"strings"
	"testing"

	"github.com/ichiban/prolog/v2/internal/ir"
	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
)

func TestEngine_LoadModule(t *testing.T) {
	tests := []struct {
		title string
		text  string
		image string
	}{
		{
			title: "simple facts",
			text: `
p.
p.
p.
`,
			image: `
   0          true/0: proceed
   1          call/2: builtin 1
   2                  execute true/1
   3          true/1: builtin 0
   4                  execute true/1
   5             p/1: nondet
   6                  try_me_else 8
   7                  execute true/1
   8                  retry_me_else 10
   9                  execute true/1
  10                  trust_me
  11                  execute true/1
`,
		},
		{
			title: "facts with arguments",
			text: `
p(a).
p(b).
p(c).
`,
			image: `
   0          true/0: proceed
   1          call/2: builtin 1
   2                  execute true/1
   3          true/1: builtin 0
   4                  execute true/1
   5             p/2: switch p/2
   6                  try_me_else 10
   7             (a): get_constant a A0
   8                  move X0 A1
   9                  execute true/1
  10                  retry_me_else 14
  11             (b): get_constant b A0
  12                  move X0 A1
  13                  execute true/1
  14                  trust_me
  15             (c): get_constant c A0
  16                  move X0 A1
  17                  execute true/1
`,
		},
		{
			title: "facts with duplicate arguments",
			text: `
p(a).
p(b).
p(a).
`,
			image: `
   0          true/0: proceed
   1          call/2: builtin 1
   2                  execute true/1
   3          true/1: builtin 0
   4                  execute true/1
   5             p/2: nondet
   6                  try_me_else 10
   7             (a): get_constant a A0
   8                  move X0 A1
   9                  execute true/1
  10                  retry_me_else 14
  11             (b): get_constant b A0
  12                  move X0 A1
  13                  execute true/1
  14                  trust_me
  15                  get_constant a A0
  16                  move X0 A1
  17                  execute true/1
`,
		},
		{
			title: "rules",
			text:  `p(a). p(b). p(c). q(1). q(2). q(3). r(X, Y) :- p(X), q(Y).`,
			image: `
   0          true/0: proceed
   1          call/2: builtin 1
   2                  execute true/1
   3          true/1: builtin 0
   4                  execute true/1
   5             p/2: switch p/2
   6                  try_me_else 10
   7             (a): get_constant a A0
   8                  move X0 A1
   9                  execute true/1
  10                  retry_me_else 14
  11             (b): get_constant b A0
  12                  move X0 A1
  13                  execute true/1
  14                  trust_me
  15             (c): get_constant c A0
  16                  move X0 A1
  17                  execute true/1
  18             q/2: switch q/2
  19                  try_me_else 23
  20             (1): get_constant 1 A0
  21                  move X0 A1
  22                  execute true/1
  23                  retry_me_else 27
  24             (2): get_constant 2 A0
  25                  move X0 A1
  26                  execute true/1
  27                  trust_me
  28             (3): get_constant 3 A0
  29                  move X0 A1
  30                  execute true/1
  31             r/3: put_structure q/2 A3
  32                  write_value X1
  33                  write_value X2
  34                  move X1 A3
  35                  execute p/2
`,
		},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			e := Engine{
				Arena: &term.Arena{
					Heap: make(term.Heap, 0, 1024),
				},
				BuiltinSet: NewBuiltinSet(),
			}

			c := Compiler{
				Engine: &e,
			}

			var m ir.Module
			if err := c.CompileModule(t.Context(), &m, test.text); err != nil {
				t.Fatal(err)
			}

			if err := e.LoadModule(&m); err != nil {
				t.Fatal(err)
			}

			var (
				got  = strings.Split(e.Image.String(), "\n")
				want = strings.Split(test.image, "\n")[1:]
			)
			for i := range max(len(got), len(want)) {
				var g, w string
				if i < len(got) {
					g = got[i]
				}
				if i < len(want) {
					w = want[i]
				}
				if g != w {
					t.Errorf("got %q, want %q", g, w)
				}
			}
		})
	}
}

func TestEngine_Call(t *testing.T) {
	tests := []struct {
		title   string
		text    string
		goal    string
		results []string
	}{
		{
			title:   "simple facts",
			text:    `p. p. p. p.`,
			goal:    `p.`,
			results: []string{``, ``, ``, ``},
		},
		{
			title: "facts with arguments (nondeterministic)",
			text:  `p(a). p(b). p(c).`,
			goal:  `p(X).`,
			results: []string{
				`X = a`,
				`X = b`,
				`X = c`,
			},
		},
		{
			title:   "facts with arguments (deterministic)",
			text:    `p(a). p(b). p(c).`,
			goal:    `p(b).`,
			results: []string{``},
		},
		{
			title:   "facts with arguments (failure)",
			text:    `p(a). p(b). p(c).`,
			goal:    `p(d).`,
			results: []string{},
		},
		{
			title: "rule and facts",
			text:  `p(a). p(b). p(c). q(1). q(2). q(3). r(X, Y) :- p(X), q(Y).`,
			goal:  `r(X, Y).`,
			results: []string{
				`X = a, Y = 1`,
				`X = a, Y = 2`,
				`X = a, Y = 3`,
				`X = b, Y = 1`,
				`X = b, Y = 2`,
				`X = b, Y = 3`,
				`X = c, Y = 1`,
				`X = c, Y = 2`,
				`X = c, Y = 3`,
			},
		},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			e := Engine{
				Arena: &term.Arena{
					Heap: make(term.Heap, 0, 1024),
				},
			}

			c := Compiler{
				Engine: &e,
			}

			var m ir.Module
			if err := c.CompileModule(t.Context(), &m, test.text); err != nil {
				t.Fatal(err)
			}

			if err := e.LoadModule(&m); err != nil {
				t.Fatal(err)
			}

			var vars []syntax.ParsedVariable
			g, err := syntax.ParseTerm(test.goal,
				syntax.Arena(e.Arena),
				syntax.Variables(&vars),
			)
			if err != nil {
				t.Fatal(err)
			}

			var results []string
			for err := range e.Call(t.Context(), g) {
				if err != nil {
					t.Fatal(err)
				}
				var result []string
				for _, v := range vars {
					result = append(result, fmt.Sprintf("%s = %s", v.Name, &syntax.Formatter{Arena: e.Arena, Term: v.Variable}))
				}
				results = append(results, strings.Join(result, ", "))
			}

			if len(results) != len(test.results) {
				t.Errorf("got %d, want %d", len(results), len(test.results))
			}
			for i := range results {
				if got := results[i]; got != test.results[i] {
					t.Errorf("got %s, want %s", got, test.results[i])
				}
			}
		})
	}
}
