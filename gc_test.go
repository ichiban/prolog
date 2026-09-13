package prolog

import (
	"testing"
	"testing/fstest"
)

// gcText holds a recursion deep enough to fill a small heap several times over,
// plus the constants whose cells outlive a collection.
const gcText = `
count(0).
count(N) :- N > 0, M is N - 1, count(M).
gen(N, N).
gen(N, X) :- M is N + 1, gen(M, X).
mklist(0, []).
mklist(N, [N|T]) :- N > 0, M is N - 1, mklist(M, T).
pi(3.14159).
big(1234567890123).
str("hello").
val(1).
val(2).
`

// TestInterpreter_Query_gc runs each query on a heap small enough to collect
// several times, and again on one large enough never to collect. Anything the
// root set misses shows up as the two disagreeing.
func TestInterpreter_Query_gc(t *testing.T) {
	tests := []struct {
		title string
		query string
		want  string
	}{
		{"query variables survive", `count(400), X = hello.`, `hello`},
		{"a float constant in the code image survives", `count(400), pi(X).`, `3.14159`},
		{"an integer too wide for a cell survives", `count(400), big(X).`, `1234567890123`},
		{"a string constant survives", `count(400), str(X).`, `[h,e,l,l,o]`},
		{"a nested call doesn't collect its caller", `Z = marker, findall(_, count(400), _), X = Z.`, `marker`},
		{"backtracking across a collection finds the solution", `gen(0, N), N >= 400, X = N.`, `400`},
		{"a term built across collections stays whole", `mklist(300, L), L = [X|_].`, `300`},
		{"bagof re-enters the engine and keeps its groups", `bagof(Y, val(Y), L), count(400), L = [X|_].`, `1`},
	}

	for _, test := range tests {
		t.Run(test.title, func(t *testing.T) {
			const (
				collects      = 40 * 1024
				neverCollects = 1 << 22
			)
			want := gcSolution(t, neverCollects, test.query)
			if want != test.want {
				t.Fatalf("without collecting: got X = %s, want %s", want, test.want)
			}
			if got := gcSolution(t, collects, test.query); got != test.want {
				t.Errorf("collecting: got X = %s, want %s", got, test.want)
			}
		})
	}
}

// gcSolution returns the first solution's binding of X, on a heap of the given size.
func gcSolution(t *testing.T, heapSize int32, query string) string {
	t.Helper()

	i := New(HeapSize(heapSize))
	i.SetWarn(func(err error) { t.Logf("warn: %v", err) })
	if err := i.MountFS("", fstest.MapFS{"gc.pl": &fstest.MapFile{Data: []byte(gcText)}}); err != nil {
		t.Fatal(err)
	}
	if err := i.Load(t.Context(), "", "gc.pl"); err != nil {
		t.Fatal(err)
	}

	for r, err := range i.Query[map[string]Raw](t.Context(), query) {
		if err != nil {
			t.Fatalf("%s: %v", query, err)
		}
		return string(r["X"])
	}
	return "<no solution>"
}
