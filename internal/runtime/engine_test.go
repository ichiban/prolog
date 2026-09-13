package runtime

import (
	"fmt"
	"slices"
	"strings"
	"testing"
	"weak"

	"github.com/ichiban/prolog/v2/internal/wam"

	"github.com/ichiban/prolog/v2/internal/ir"
	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
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
   0          true/0: proceed
   1             p/1: nondet
   2                  try_me_else 4
   3                  execute true/1
   4                  retry_me_else 6
   5                  execute true/1
   6                  trust_me
   7                  execute true/1
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
   1             p/2: switch p/2
   2                  try_me_else 6
   3             (a): get_constant a, A1
   4                  move X1, X2
   5                  execute true/1
   6                  retry_me_else 10
   7             (b): get_constant b, A1
   8                  move X1, X2
   9                  execute true/1
  10                  trust_me
  11             (c): get_constant c, A1
  12                  move X1, X2
  13                  execute true/1
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
   1             p/2: nondet
   2                  try_me_else 6
   3             (a): get_constant a, A1
   4                  move X1, X2
   5                  execute true/1
   6                  retry_me_else 10
   7             (b): get_constant b, A1
   8                  move X1, X2
   9                  execute true/1
  10                  trust_me
  11                  get_constant a, A1
  12                  move X1, X2
  13                  execute true/1
`,
		},
		{
			title: "repeated argument variables",
			text:  `p(X, X).`,
			image: `
   0          true/0: proceed
   1             p/3: nondet
   2                  nop
   3                  get_value X1, A2
   4                  move X1, X3
   5                  execute true/1
`,
		},
		{
			title: "structure in head",
			text:  `p(f(X, X, a, _)).`,
			image: `
   0          true/0: proceed
   1             p/2: nondet
   2                  nop
   3           (f/4): get_structure f/4, A1
   4                  unify_variable X3
   5                  unify_value X3
   6                  unify_constant a
   7                  unify_void
   8                  move X1, X2
   9                  execute true/1
`,
		},
		{
			title: "body",
			text:  `p(X) :- q(X, Y, Y, a, _).`,
			image: `
   0          true/0: proceed
   1             p/2: nondet
   2                  nop
   3                  move X6, X2
   4                  put_variable X3, A2
   5                  put_constant a, A4
   6                  put_variable X5, A5
   7                  execute q/6
`,
		},
		{
			title: "structure in body",
			text:  `p(X) :- q(f(X, Y, Y, a, _)).`,
			image: `
   0          true/0: proceed
   1             p/2: nondet
   2                  nop
   3                  put_structure f/5, A3
   4                  write_value X1
   5                  write_variable X4
   6                  write_value X4
   7                  write_constant a
   8                  write_void
   9                  move X1, X3
  10                  execute q/2
`,
		},
		{
			title: "simple conjunction",
			text:  `p(X) :- q(X), r(X), s(X).`,
			image: `
   0          true/0: proceed
   1             p/2: nondet
   2                  nop
   3                  put_structure r/2, A3
   4                  write_value X1
   5                  write_variable X4
   6                  push_structure s/2, A4
   7                  write_value X1
   8                  write_value X2
   9                  move X2, X3
  10                  execute q/2
`,
		},
		{
			title: "simple disjunction",
			text:  `p(X) :- q(X); r(X); s(X).`,
			image: `
   0          true/0: proceed
   1             p/2: nondet
   2                  nop
   3                  execute $aux1/2
   4         $aux1/2: nondet
   5                  try_me_else 7
   6                  execute q/2
   7                  retry_me_else 9
   8                  execute r/2
   9                  trust_me
  10                  execute s/2
`,
		},
		{
			title: "neck cut",
			text:  `p :- !, q.`,
			image: `
   0          true/0: proceed
   1             p/1: nondet
   2                  nop
   3                  put_cut
   4                  execute q/1
`,
		},
		{
			title: "deep cut",
			text:  `p :- q, !, r.`,
			image: `
   0          true/0: proceed
   1             p/1: nondet
   2                  nop
   3                  put_structure $cut_to/2, A2
   4                  push_cut
   5                  write_variable X3
   6                  push_structure r/1, A3
   7                  write_value X1
   8                  move X1, X2
   9                  execute q/1
`,
		},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			e := Engine{
				Arena:      term.NewArena(1024),
				BuiltinSet: &BuiltinSet{},
				Ops:        *syntax.NewOperatorSet(),
			}

			c := Compiler{
				Engine: &e,
			}

			var m ir.Module
			if err := c.CompileText(t.Context(), &m, test.text); err != nil {
				t.Fatal(err)
			}

			if err := e.LoadModule(t.Context(), &m); err != nil {
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

// A multifile predicate extended by a later text ends up with the same layout
// as if its clauses were contiguous, first-argument dispatch included.
func TestEngine_LoadModule_extension(t *testing.T) {
	e := Engine{
		Arena:      term.NewArena(1024),
		BuiltinSet: &BuiltinSet{},
		Ops:        *syntax.NewOperatorSet(),
	}
	c := Compiler{
		Engine: &e,
	}

	var m1 ir.Module
	if err := c.CompileText(t.Context(), &m1, `p(a).`); err != nil {
		t.Fatal(err)
	}
	if err := e.LoadModule(t.Context(), &m1); err != nil {
		t.Fatal(err)
	}

	bpi := term.NewFunctor(term.NewAtom("p"), 2)
	p := e.Predicates[bpi]
	p.Multifile = true
	e.Predicates[bpi] = p

	var m2 ir.Module
	if err := c.CompileText(t.Context(), &m2, `p(b).`); err != nil {
		t.Fatal(err)
	}
	if err := e.LoadModule(t.Context(), &m2); err != nil {
		t.Fatal(err)
	}

	want := `
   0          true/0: proceed
   1             p/2: switch p/2
   2                  try_me_else 6
   3             (a): get_constant a, A1
   4                  move X1, X2
   5                  execute true/1
   6                  trust_me
   7             (b): get_constant b, A1
   8                  move X1, X2
   9                  execute true/1
`
	var (
		got = strings.Split(e.Image.String(), "\n")
		w   = strings.Split(want, "\n")[1:]
	)
	for i := range max(len(got), len(w)) {
		var g, x string
		if i < len(got) {
			g = got[i]
		}
		if i < len(w) {
			x = w[i]
		}
		if g != x {
			t.Errorf("got %q, want %q", g, x)
		}
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

	// Collecting before every execute turns a root that is merely missing into a
	// root that is missing right now, so the same table doubles as the GC test.
	// Without it a bad root set only shows up when a collection happens to land
	// between the write and the read.
	for _, gcEvery := range []bool{false, true} {
		t.Run(fmt.Sprintf("gcEveryExecute=%v", gcEvery), func(t *testing.T) {
			gcEveryExecute = gcEvery
			defer func() { gcEveryExecute = false }()
			testEngineCall(t, tests)
		})
	}
}

func testEngineCall(t *testing.T, tests []struct {
	title   string
	text    string
	goal    string
	results []string
}) {
	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			e := Engine{
				Arena: term.NewArena(6 * 1024),
				Ops:   *syntax.NewOperatorSet(),
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

			if err := e.LoadModule(t.Context(), &m); err != nil {
				t.Fatal(err)
			}

			var vns []term.VariableName
			g, err := syntax.ParseTerm(strings.NewReader(test.goal),
				syntax.Arena(e.Arena),
				syntax.VariableNames(&vns),
			)
			if err != nil {
				t.Fatal(err)
			}

			// The bindings are read after every solution, and solving collects.
			defer e.AddRoots(func(yield func(*term.Cell) bool) {
				for i := range vns {
					if !yield(&vns[i].Variable) {
						return
					}
				}
			})()

			var results []string
			for err := range e.Call(t.Context(), g) {
				if err != nil {
					t.Fatal(err)
				}
				var result []string
				for _, v := range vns {
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

func TestEngine_AddRoots(t *testing.T) {
	e := Engine{Arena: term.NewArena(64)}
	a, b := must(e.PutVariable()), must(e.PutVariable())

	removeA := e.AddRoots(slices.Values([]*term.Cell{&a}))
	removeB := e.AddRoots(slices.Values([]*term.Cell{&b}))

	roots := slices.Collect(e.roots())
	if !slices.Contains(roots, &a) || !slices.Contains(roots, &b) {
		t.Errorf("expected both sources to be rooted, got: %v", roots)
	}

	// Removing one source leaves the other alone.
	removeA()
	roots = slices.Collect(e.roots())
	if slices.Contains(roots, &a) {
		t.Errorf("expected %v not to be rooted, got: %v", &a, roots)
	}
	if !slices.Contains(roots, &b) {
		t.Errorf("expected %v to be rooted, got: %v", &b, roots)
	}

	removeB()
	if roots := slices.Collect(e.roots()); slices.Contains(roots, &b) {
		t.Errorf("expected %v not to be rooted, got: %v", &b, roots)
	}
}

func TestEngine_roots(t *testing.T) {
	e := Engine{Arena: term.NewArena(64)}

	e.Input = must(e.PutStream(term.Stream{Alias: term.NewAtom("user_input")}))
	e.Output = must(e.PutStream(term.Stream{Alias: term.NewAtom("user_output")}))

	// The image outlives every collection, and a constant that isn't immediate
	// holds a heap address.
	e.Constants = []term.Cell{must(e.PutFloat(3.5))}
	pi := term.NewFunctor(term.NewAtom("p"), 2)
	e.Predicates = map[term.Functor]wam.Predicate{
		pi: {FirstArgIndex: []wam.FirstArg{{FirstArgKey: wam.FirstArgKey{Term: must(e.PutFloat(2.5))}}}},
	}

	exec := &Execution{Engine: &e, liveRegs: 1}
	exec.tempVars[1] = must(e.PutVariable())
	e.executions = []*Execution{exec}

	external := must(e.PutVariable())
	defer e.AddRoots(slices.Values([]*term.Cell{&external}))()

	roots := slices.Collect(e.roots())
	for _, want := range []*term.Cell{
		&e.Input,
		&e.Output,
		&e.Constants[0],
		&e.Predicates[pi].FirstArgIndex[0].Term,
		&exec.tempVars[1],
		&external,
	} {
		if !slices.Contains(roots, want) {
			t.Errorf("expected %v to be rooted, got: %v", want, roots)
		}
	}
}

func TestEngine_roots_deduplicates(t *testing.T) {
	e := Engine{Arena: term.NewArena(64)}

	// Backtracking into a delayed built-in pushes a fresh choice point that
	// points at the same captured list, so one cell hangs off two frames. GC
	// relocates whatever it's handed, and relocating a cell twice moves it to an
	// address that was never its own.
	held := new(must(e.PutVariable()))
	activation := Activation{
		captured: []weak.Pointer[term.Cell]{weak.Make(held)},
	}
	exec := &Execution{Engine: &e}
	exec.stack = []stackFrame{
		{tempVars: must(e.PutAtom(term.NewAtom("$temp_vars"))), activation: &activation},
		{tempVars: must(e.PutAtom(term.NewAtom("$temp_vars"))), activation: &activation},
	}
	e.executions = []*Execution{exec}

	var n int
	for c := range e.roots() {
		if c == held {
			n++
		}
	}
	if n != 1 {
		t.Errorf("expected: %d, got: %d", 1, n)
	}
}

func TestEngine_GC(t *testing.T) {
	e := Engine{Arena: term.NewArena(64)}

	_ = must(e.PutVariable()) // Garbage underneath everything live, so the heap moves.

	e.Constants = []term.Cell{must(e.PutFloat(3.5))}

	v := must(e.PutVariable())
	tvs := must(e.PutCompound(term.NewAtom("$temp_vars"), v))
	heapTop := len(e.Heap)

	_ = must(e.PutVariable()) // Garbage the choice point would drop anyway.

	exec := &Execution{Engine: &e, liveRegs: 1}
	exec.tempVars[1] = v
	exec.stack = []stackFrame{{tempVars: tvs, heapTop: heapTop}}
	e.executions = []*Execution{exec}

	e.GC(e.roots(), e.heapTops())

	if got, ok := e.Float(e.Constants[0]); !ok || got != 3.5 {
		t.Errorf("expected: %v, got: %v", 3.5, got)
	}
	if _, ok := e.Variable(e.Deref(exec.tempVars[1])); !ok {
		t.Errorf("expected an unbound variable, got: %v", e.Deref(exec.tempVars[1]))
	}
	// The choice point still names the same variable as the register does.
	if got, want := e.Arg(exec.stack[0].tempVars, 0), exec.tempVars[1]; got != want {
		t.Errorf("expected: %v, got: %v", want, got)
	}
	// Everything still live was allocated below the choice point, so its saved
	// heap top comes down to the new end of the heap. Left alone it would name
	// the pre-collection boundary and backtracking would stretch the heap back
	// over the cells that were just reclaimed.
	if got, want := exec.stack[0].heapTop, len(e.Heap); got != want {
		t.Errorf("expected: %d, got: %d", want, got)
	}
	if got, want := len(e.Heap), 4; got != want { // The float's bits, v, and $temp_vars/1.
		t.Errorf("expected: %d, got: %d", want, got)
	}
}
