package prolog

import (
	"embed"
	"fmt"
	"sort"
	"strings"
	"testing"

	"github.com/ichiban/prolog/v2/internal/runtime"
	"github.com/ichiban/prolog/v2/internal/syntax"
	"github.com/ichiban/prolog/v2/internal/term"
)

//go:embed testdata
var testdata embed.FS

func TestInterpreter_Query(t *testing.T) {
	tests := []struct {
		loaded  []string
		query   string
		results []string
		errTerm string
	}{
		/*
			Examples in ISO/IEC 13211-1.
		*/
		// 7.8.1.4
		{query: `true.`, results: []string{""}},
		// 7.8.2.4
		{query: `fail.`, results: []string{}},
		// 7.8.3.4
		{query: `call(!).`, results: []string{""}},
		{query: `call(fail).`, results: []string{}},
		{query: `call((fail, X)).`, results: []string{}},
		{query: `call((fail, call(1))).`, results: []string{}},
		{loaded: []string{"testdata/7.8.3.4.pl"}, query: `b(X).`, errTerm: `instantiation_error`},
		{loaded: []string{"testdata/7.8.3.4.pl"}, query: `b(3).`, errTerm: `type_error(callable,3)`}, // type_error(callable,(write(3),3))
		{loaded: []string{"testdata/7.8.3.4.pl"}, query: `Z = !, call((Z = !, a(X), Z)).`, results: []string{"X = 1, Z = !"}},
		{loaded: []string{"testdata/7.8.3.4.pl"}, query: `call((Z=!, a(X), Z)).`, results: []string{"X = 1, Z = !"}},
		{loaded: []string{"testdata/7.8.3.4.pl"}, query: `call((write(3), X)).`, errTerm: `instantiation_error`},
		{query: `call(X).`, errTerm: `instantiation_error`},
		{query: `call(1).`, errTerm: `type_error(callable,1)`},
		{query: `call((fail, 1)).`, errTerm: `type_error(callable,(fail,1))`},
		{query: `call((write(3), 1)).`, errTerm: `type_error(callable,1)`}, // type_error(callable,(write(3),1))
		{query: `call((1;true)).`, errTerm: `type_error(callable,1)`},      // type_error(callable,(1;true))
		// 7.8.4.4
		{query: `!.`, results: []string{""}},
		{query: `(!, fail; true).`, results: []string{}},
		{query: `(call(!), fail; true).`, results: []string{""}},
		{loaded: []string{"testdata/7.8.4.4.pl"}, query: `twice(_), !, write('Forwards '), fail.`, results: []string{}},
		{query: `(!; write('No ')), write('Cut disjunction '), fail.`, results: []string{}},
		{loaded: []string{"testdata/7.8.4.4.pl"}, query: `twice(_), (write('No '); !), write('Cut '), fail.`, results: []string{}},
		{loaded: []string{"testdata/7.8.4.4.pl"}, query: `twice(_), (!, fail; write('No ')).`, results: []string{}},
		{loaded: []string{"testdata/7.8.4.4.pl"}, query: `twice(X), call(X), write('Forwards '), fail.`, results: []string{}},
		{loaded: []string{"testdata/7.8.4.4.pl"}, query: `goal(X), call(X), write('Forwards '), fail.`, results: []string{}},
		{loaded: []string{"testdata/7.8.4.4.pl"}, query: `twice(_), \+(\+(!)), write('Forwards '), fail.`, results: []string{}},
		{loaded: []string{"testdata/7.8.4.4.pl"}, query: `twice(_), once(!), write('Forwards '), fail.`, results: []string{}},
		{loaded: []string{"testdata/7.8.4.4.pl"}, query: `twice(_), call(!), write('Forwards '), fail.`, results: []string{}},
		// 7.8.5.4
		{query: `','(X=1, var(X)).`, results: []string{}},
		{query: `','(var(X), X=1).`, results: []string{"X = 1"}},
		{query: `','(X = true, call(X)).`, results: []string{"X = true"}},
		// 7.8.6.4
		{query: `';'(true, fail).`, results: []string{""}},
		{query: `';'((!, fail), true).`, results: []string{}},
		{query: `';'(!, call(3)).`, results: []string{""}},
		{query: `';'((X = 1, !), X = 2).`, results: []string{"X = 1"}},
		{query: `','(';'(X=1, X=2), ';'(true, !)).`, results: []string{"X = 1", "X = 1"}},
		// 7.8.7.4
		{query: `'->'(true, true).`, results: []string{""}},
		{query: `'->'(true, fail).`, results: []string{}},
		{query: `'->'(fail, true).`, results: []string{}},
		{query: `'->'(true, X=1).`, results: []string{"X = 1"}},
		{query: `'->'(';'(X=1, X=2), true).`, results: []string{"X = 1"}},
		{query: `'->'(true, ';'(X=1, X=2)).`, results: []string{"X = 1", "X = 2"}},
		// 7.8.8.4
		{query: `';'('->'(true, true), fail).`, results: []string{""}},
		{query: `';'('->'(fail, true), true).`, results: []string{""}},
		{query: `';'('->'(true, fail), fail).`, results: []string{}},
		{query: `';'('->'(true, X=1), X=2).`, results: []string{"X = 1"}},
		{query: `';'('->'(fail, X=1), X=2).`, results: []string{"X = 2"}},
		{query: `';'('->'(true, ';'(X=1, X=2)), true).`, results: []string{"X = 1", "X = 2"}},
		{query: `';'('->'(';'(X=1, X=2), true), true).`, results: []string{"X = 1"}},
		{query: `';'(('->'(!, fail), true), true).`, results: []string{""}},
		// 7.8.9.4
		{loaded: []string{"testdata/7.8.9.4.pl"}, query: `catch(foo(5), test(Y), true).`, results: []string{"Y = 10"}},
		{loaded: []string{"testdata/7.8.9.4.pl"}, query: `catch(bar(3), Z, true).`, results: []string{"Z = 3"}},
		{loaded: []string{"testdata/7.8.9.4.pl"}, query: `catch(true, _, 3).`, results: []string{""}},
		{loaded: []string{"testdata/7.8.9.4.pl"}, query: `catch(true, C, write(demoen)), throw(bla).`, errTerm: `system_error`},
		{loaded: []string{"testdata/7.8.9.4.pl"}, query: `catch(car(X), Y, true).`, results: []string{"Y = 1"}},
		{loaded: []string{"testdata/7.8.9.4.pl"}, query: `catch(number_chars(X, ['1', 'a', '0']), error(syntax_error(_), _), fail).`, results: []string{}},
		{loaded: []string{"testdata/7.8.9.4.pl"}, query: `catch(g, C, write(h1)).`, results: []string{"C = c"}},
		{loaded: []string{"testdata/7.8.9.4.pl"}, query: `catch(coo(X), Y, true).`, results: []string{"Y = error(instantiation_error,throw/1)"}},
		// TODO:
		/*
			Other test cases.
		*/
		{
			query:   `true, true.`,
			results: []string{""},
		},
		{
			query:   `true, fail.`,
			results: []string{},
		},
		{
			query:   `!.`,
			results: []string{""},
		},
		{
			loaded: []string{"testdata/p.pl"},
			query:  "p(a).",
			results: []string{
				"",
			},
		},
		{
			loaded: []string{"testdata/p.pl"},
			query:  "p(X).",
			results: []string{
				"X = a",
				"X = b",
				"X = c",
			},
		},
		{
			loaded: []string{"testdata/cut.pl"},
			query:  "only_cut(a).",
			results: []string{
				"",
			},
		},
		{
			loaded: []string{"testdata/cut.pl"},
			query:  "neck_cut(X).",
			results: []string{
				"X = a",
				"X = b",
			},
		},
		{
			loaded: []string{"testdata/cut.pl"},
			query:  "deep_cut(X).",
			results: []string{
				"X = a",
			},
		},
	}

	for _, test := range tests {
		t.Run(test.query, func(t *testing.T) {
			i := New(2 * 1024)
			i.SetSourceFS(testdata)

			if err := i.engine.LoadSystem(t.Context()); err != nil {
				t.Fatal(err)
			}

			for _, l := range test.loaded {
				if err := i.Load(t.Context(), l); err != nil {
					t.Fatal(err)
				}
			}

			var (
				results []Result
				pvs     ParsedVariables
			)
			for result, err := range Query[Result](t.Context(), i, test.query, Variables(&pvs)) {
				if err != nil {
					var (
						arena   = i.engine.Arena
						origErr = err
					)
					et, err := runtime.ErrorTerm(i.engine.Arena, err)
					if err != nil {
						fmt.Printf("image: \n%s\n", &i.engine.Image)
						t.Fatal(origErr)
					}
					if f, ok := arena.Functor(et); !ok || f != term.NewFunctor(term.NewAtom("error"), 2) {
						t.Fatal(origErr)
					}
					et = arena.Arg(et, 0)
					varNames := make(map[term.Handle]term.Atom, len(pvs))
					for _, pv := range pvs {
						varNames[pv.Variable] = term.NewAtom(pv.Name)
					}
					if s := fmt.Sprintf("%s", &syntax.Formatter{
						Arena:        arena,
						Term:         et,
						VariableName: varNames,
					}); s != test.errTerm {
						t.Errorf("expected %s, got %s", test.errTerm, s)
					}
				}
				results = append(results, result)
			}

			for j := range max(len(results), len(test.results)) {
				var got, want string
				if j < len(results) {
					got = formatResult(results[j])
				}
				if j < len(test.results) {
					want = test.results[j]
				}
				if got != want {
					t.Errorf("got %q, want %q", got, want)
					t.Errorf("image: \n%s\n", &i.engine.Image)
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

func must[T any](v T, err error) T {
	if err != nil {
		panic(err)
	}
	return v
}
