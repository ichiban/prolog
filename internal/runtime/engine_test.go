package runtime

import (
	"fmt"
	"strings"
	"testing"

	"github.com/ichiban/prolog/v2/internal/ir"
	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
	"github.com/ichiban/prolog/v2/internal/wam"
)

func TestEngine_LoadModule(t *testing.T) {
	tests := []struct {
		title    string
		text     string
		builtins *BuiltinSet
		image    string
	}{
		{
			title: "simple facts",
			text: `
p.
p.
p.
`,
			image: `
   0             p/1: nondet
   1                  try_me_else 3
   2                  execute true/1
   3                  retry_me_else 5
   4                  execute true/1
   5                  trust_me
   6                  execute true/1
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
   0             p/2: switch p/2
   1                  try_me_else 5
   2             (a): get_constant a, A1
   3                  move X1, X2
   4                  execute true/1
   5                  retry_me_else 9
   6             (b): get_constant b, A1
   7                  move X1, X2
   8                  execute true/1
   9                  trust_me
  10             (c): get_constant c, A1
  11                  move X1, X2
  12                  execute true/1
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
   0             p/2: nondet
   1                  try_me_else 5
   2             (a): get_constant a, A1
   3                  move X1, X2
   4                  execute true/1
   5                  retry_me_else 9
   6             (b): get_constant b, A1
   7                  move X1, X2
   8                  execute true/1
   9                  trust_me
  10                  get_constant a, A1
  11                  move X1, X2
  12                  execute true/1
`,
		},
		{
			title: "repeated argument variables",
			text:  `p(X, X).`,
			image: `
   0             p/3: get_value X1, A2
   1                  move X1, X3
   2                  execute true/1
`,
		},
		{
			title: "structure in head",
			text:  `p(f(X, X, a, _)).`,
			image: `
   0             p/2: get_structure f/4, A1
   1                  unify_variable X3
   2           (f/4): unify_value X3
   3                  unify_constant a
   4                  unify_void
   5                  move X1, X2
   6                  execute true/1
`,
		},
		{
			title: "body",
			text:  `p(X) :- q(X, Y, Y, a, _).`,
			image: `
   0             p/2: move X6, X2
   1                  put_variable X3, A2
   2                  put_constant a, A4
   3                  put_variable X5, A5
   4                  execute q/6
`,
		},
		{
			title: "structure in body",
			text:  `p(X) :- q(f(X, Y, Y, a, _)).`,
			image: `
   0             p/2: put_structure f/5, A3
   1                  write_value X1
   2                  write_variable X4
   3                  write_value X4
   4                  write_constant a
   5                  write_void
   6                  move X1, X3
   7                  execute q/2
`,
		},
		{
			title: "simple conjunction",
			text:  `p(X) :- q(X), r(X), s(X).`,
			image: `
   0             p/2: put_structure r/2, A3
   1                  write_value X1
   2                  write_variable X4
   3                  push_structure s/2, A4
   4                  write_value X1
   5                  write_value X2
   6                  move X2, X3
   7                  execute q/2
`,
		},
		{
			title: "simple disjunction",
			text:  `p(X) :- q(X); r(X); s(X).`,
			image: `
   0             p/2: execute $aux1/2
   1         $aux1/2: nondet
   2                  try_me_else 4
   3                  execute q/2
   4                  retry_me_else 6
   5                  execute r/2
   6                  trust_me
   7                  execute s/2
`,
		},
		{
			title: "neck cut",
			text:  `p :- !, q.`,
			image: `
   0             p/1: put_cut
   1                  execute q/1
`,
		},
		{
			title: "deep cut",
			text:  `p :- q, !, r.`,
			image: `
   0             p/1: put_structure $cut_to/2, A2
   1                  push_cut
   2                  write_variable X3
   3                  push_structure r/1, A3
   4                  write_value X1
   5                  move X1, X2
   6                  execute q/1
`,
		},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			e := Engine{
				Arena: &term.Arena{
					Heap: make(term.Heap, 0, 1024),
				},
				Image: wam.Image{
					Code: []wam.Instruction{},
				},
				BuiltinSet: &BuiltinSet{},
			}

			c := Compiler{
				Engine: &e,
			}

			var m ir.Module
			if err := c.CompileText(t.Context(), &m, test.text); err != nil {
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
					Heap: make(term.Heap, 0, 3*1024),
				},
			}

			if err := e.LoadSystem(t.Context()); err != nil {
				t.Fatal(err)
			}

			c := Compiler{
				Engine: &e,
			}

			var m ir.Module
			if err := c.CompileText(t.Context(), &m, test.text); err != nil {
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
