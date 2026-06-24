package prolog

import (
	"embed"
	"fmt"
	"sort"
	"strings"
	"testing"
)

//go:embed testdata
var testdata embed.FS

func TestInterpreter_Query(t *testing.T) {
	tests := []struct {
		title   string
		loaded  []string
		query   string
		results []string
	}{
		{
			title:  "simple deterministic",
			loaded: []string{"testdata/p.pl"},
			query:  "p(a).",
			results: []string{
				"",
			},
		},
		{
			title:  "simple nondeterministic",
			loaded: []string{"testdata/p.pl"},
			query:  "p(X).",
			results: []string{
				"X = a",
				"X = b",
				"X = c",
			},
		},
	}

	for _, test := range tests {
		t.Run(test.query, func(t *testing.T) {
			i := New(&Config{
				HeapSize: 1024,
				SourceFS: testdata,
			})

			for _, l := range test.loaded {
				if err := i.Load(t.Context(), l); err != nil {
					t.Fatal(err)
				}
			}

			var results []map[string]Raw
			for result, err := range Query[map[string]Raw](t.Context(), i, test.query) {
				if err != nil {
					t.Fatal(err)
				}
				results = append(results, result)
			}

			for i := range max(len(results), len(test.results)) {
				var got, want string
				if i < len(results) {
					got = formatResult(results[i])
				}
				if i < len(test.results) {
					want = test.results[i]
				}
				if got != want {
					t.Errorf("got %q, want %q", got, want)
				}
			}
		})
	}
}

func formatResult(result map[string]Raw) string {
	elems := make([]string, 0, len(result))
	for k, v := range result {
		elems = append(elems, fmt.Sprintf("%s = %s", k, v))
	}
	sort.Strings(elems)
	return strings.Join(elems, ", ")
}
