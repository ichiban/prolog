package prolog

import (
	"embed"
	"fmt"
	"sort"
	"strings"
	"testing"

	"github.com/ichiban/prolog/v2/internal/runtime"
	"github.com/ichiban/prolog/v2/internal/syntax"
)

//go:embed testdata
var testdata embed.FS

func TestInterpreter_Query(t *testing.T) {
	tests := []struct {
		loaded  []string
		query   string
		results []string
		err     string
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
		{loaded: []string{"testdata/7.8.3.4.pl"}, query: `b(X).`, err: `instantiation_error`},
		{loaded: []string{"testdata/7.8.3.4.pl"}, query: `b(3).`, err: `type_error(callable,3)`}, // type_error(callable,(write(3),3))
		{loaded: []string{"testdata/7.8.3.4.pl"}, query: `Z = !, call((Z = !, a(X), Z)).`, results: []string{"X = 1, Z = !"}},
		{loaded: []string{"testdata/7.8.3.4.pl"}, query: `call((Z=!, a(X), Z)).`, results: []string{"X = 1, Z = !"}},
		{loaded: []string{"testdata/7.8.3.4.pl"}, query: `call((write(3), X)).`, err: `instantiation_error`},
		{query: `call(X).`, err: `instantiation_error`},
		{query: `call(1).`, err: `type_error(callable,1)`},
		{query: `call((fail, 1)).`, err: `type_error(callable,(fail,1))`},
		{query: `call((write(3), 1)).`, err: `type_error(callable,1)`}, // type_error(callable,(write(3),1))
		{query: `call((1;true)).`, err: `type_error(callable,1)`},      // type_error(callable,(1;true))
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
		{loaded: []string{"testdata/7.8.9.4.pl"}, query: `catch(true, C, write(demoen)), throw(bla).`, err: `system_error`},
		{loaded: []string{"testdata/7.8.9.4.pl"}, query: `catch(car(X), Y, true).`, results: []string{"Y = 1"}},
		{loaded: []string{"testdata/7.8.9.4.pl"}, query: `catch(number_chars(X, ['1', 'a', '0']), error(syntax_error(_), _), fail).`, results: []string{}},
		{loaded: []string{"testdata/7.8.9.4.pl"}, query: `catch(g, C, write(h1)).`, results: []string{"C = c"}},
		{loaded: []string{"testdata/7.8.9.4.pl"}, query: `catch(coo(X), Y, true).`, results: []string{"Y = error(instantiation_error,throw/1)"}},
		// 8.2.1.4
		{query: `'='(1, 1).`, results: []string{""}},
		{query: `'='(X, 1).`, results: []string{"X = 1"}},
		{query: `'='(X, Y).`, results: []string{""}}, // TODO: Confirm actually X and Y are unified.
		{query: `'='(_, _).`, results: []string{""}},
		{query: `'='(X, Y), '='(X, abc).`, results: []string{"X = abc, Y = abc"}},
		{query: `'='(f(X, def), f(def, Y)).`, results: []string{"X = def, Y = def"}},
		{query: `'='(1, 2).`, results: []string{}},
		{query: `'='(1, 1.0).`, results: []string{}},
		{query: `'='(g(X), f(f(X))).`, results: []string{}},
		{query: `'='(f(X, 1), f(a(X))).`, results: []string{}},
		{query: `'='(f(X, Y, X), f(a(X), a(Y), Y, 2)).`, results: []string{}},
		{query: `'='(X, a(X)).`, results: []string{"X = a(...)"}},
		{query: `'='(f(X, 1), f(a(X), 2)).`, results: []string{}},
		{query: `'='(f(1, X, 1), f(2, a(X), 2)).`, results: []string{}},
		{query: `'='(f(1, X), f(2, a(X))).`, results: []string{}},
		{query: `'='(f(X, Y, X, 1), f(a(X), a(Y), Y, 2)).`, results: []string{}},
		// 8.2.2.4
		{query: `unify_with_occurs_check(1, 1).`, results: []string{""}},
		{query: `unify_with_occurs_check(X, 1).`, results: []string{"X = 1"}},
		{query: `unify_with_occurs_check(X, Y).`, results: []string{""}}, // TODO: Confirm actually X and Y are unified.
		{query: `unify_with_occurs_check(_, _).`, results: []string{""}},
		{query: `unify_with_occurs_check(X, Y), unify_with_occurs_check(X, abc).`, results: []string{"X = abc, Y = abc"}},
		{query: `unify_with_occurs_check(f(X, def), f(def, Y)).`, results: []string{"X = def, Y = def"}},
		{query: `unify_with_occurs_check(1, 2).`, results: []string{}},
		{query: `unify_with_occurs_check(1, 1.0).`, results: []string{}},
		{query: `unify_with_occurs_check(g(X), f(f(X))).`, results: []string{}},
		{query: `unify_with_occurs_check(f(X, 1), f(a(X))).`, results: []string{}},
		{query: `unify_with_occurs_check(f(X, Y, X), f(a(X), a(Y), Y, 2)).`, results: []string{}},
		{query: `unify_with_occurs_check(X, a(X)).`, results: []string{}},
		{query: `unify_with_occurs_check(f(X, 1), f(a(X), 2)).`, results: []string{}},
		{query: `unify_with_occurs_check(f(1, X, 1), f(2, a(X), 2)).`, results: []string{}},
		{query: `unify_with_occurs_check(f(1, X), f(2, a(X))).`, results: []string{}},
		{query: `unify_with_occurs_check(f(X, Y, X, 1), f(a(X), a(Y), Y, 2)).`, results: []string{}},
		// 8.2.3.4
		{query: `'\\='(1, 1).`, results: []string{}},
		{query: `\=(X, 1).`, results: []string{}},
		{query: `'\\='(X, Y).`, results: []string{}},
		{query: `\=(_, _).`, results: []string{}},
		{query: `\=(f(X, def), f(def, Y)).`, results: []string{}},
		{query: `'\\='(1, 2).`, results: []string{""}},
		{query: `\=(1, 1.0).`, results: []string{""}},
		{query: `'\\='(g(X), f(f(X))).`, results: []string{""}},
		{query: `\=(f(X, 1), f(a(X))).`, results: []string{""}},
		{query: `'\\='(f(X, Y, X), f(a(X), a(Y), Y, 2)).`, results: []string{""}},
		{query: `\=(X, a(X)).`, results: []string{}},
		{query: `'\\='(f(X, 1), f(a(X), 2)).`, results: []string{""}},
		{query: `'\\='(f(1, X, 1), f(2, a(X), 2)).`, results: []string{""}},
		{query: `\=(f(2, X), f(2, a(X))).`, results: []string{}},
		{query: `'\\='(f(X, Y, X, 1), f(a(X), a(Y), Y, 2)).`, results: []string{""}},
		// 8.2.4.4
		{query: `subsumes_term(a, a).`, results: []string{""}},
		{query: `subsumes_term(f(X, Y), f(Z, Z)).`, results: []string{""}},
		{query: `subsumes_term(f(Z, Z), f(X, Y)).`, results: []string{}},
		{query: `subsumes_term(g(X), g(f(X))).`, results: []string{}},
		{query: `subsumes_term(X, f(X)).`, results: []string{}},
		{query: `subsumes_term(X, Y), subsumes_term(Y, f(X)).`, results: []string{""}},
		// 8.3.1.4
		{query: `var(foo).`, results: []string{}},
		{query: `var(Foo).`, results: []string{""}},
		{query: `foo=Foo, var(Foo).`, results: []string{}},
		{query: `var(_).`, results: []string{""}},
		// 8.3.2.4
		{query: `atom(atom).`, results: []string{""}},
		{query: `atom('string').`, results: []string{""}}, // NOTE: This is technically not a string.
		{query: `atom(a(b)).`, results: []string{}},
		{query: `atom(Var).`, results: []string{}},
		{query: `atom([]).`, results: []string{""}},
		{query: `atom(6).`, results: []string{}},
		{query: `atom(3.3).`, results: []string{}},
		// 8.3.3.4
		{query: `integer(3).`, results: []string{""}},
		{query: `integer(-3).`, results: []string{""}},
		{query: `integer(3.3).`, results: []string{}},
		{query: `integer(X).`, results: []string{}},
		{query: `integer(atom).`, results: []string{}},
		// 8.3.4.4
		{query: `float(3.3).`, results: []string{""}},
		{query: `float(-3.3).`, results: []string{""}},
		{query: `float(3).`, results: []string{}},
		{query: `float(atom).`, results: []string{}},
		{query: `float(X).`, results: []string{}},
		// 8.3.5.4
		{query: `atomic(atom).`, results: []string{""}},
		{query: `atomic(a(b)).`, results: []string{}},
		{query: `atomic(Var).`, results: []string{}},
		{query: `atomic(6).`, results: []string{""}},
		{query: `atomic(3.3).`, results: []string{""}},
		// 8.3.6.4
		{query: `compound(33.3).`, results: []string{}},
		{query: `compound(-33.3).`, results: []string{}},
		{query: `compound(-a).`, results: []string{""}},
		{query: `compound(_).`, results: []string{}},
		{query: `compound(a).`, results: []string{}},
		{query: `compound(a(b)).`, results: []string{""}},
		{query: `compound([]).`, results: []string{}},
		{query: `compound([a]).`, results: []string{""}},
		// 8.3.7.4
		{query: `nonvar(33.3).`, results: []string{""}},
		{query: `nonvar(foo).`, results: []string{""}},
		{query: `nonvar(Foo).`, results: []string{}},
		{query: `foo = Foo, nonvar(Foo).`, results: []string{"Foo = foo"}},
		{query: `nonvar(_).`, results: []string{}},
		{query: `nonvar(a(b)).`, results: []string{""}},
		// 8.3.8.4
		{query: `number(3).`, results: []string{""}},
		{query: `number(3.3).`, results: []string{""}},
		{query: `number(-3).`, results: []string{""}},
		{query: `number(a).`, results: []string{}},
		{query: `number(X).`, results: []string{}},
		// 8.3.9.4
		{query: `callable(a).`, results: []string{""}},
		{query: `callable(3).`, results: []string{}},
		{query: `callable(X).`, results: []string{}},
		{query: `callable((1,2)).`, results: []string{""}},
		// 8.3.10.4
		{query: `ground(3).`, results: []string{""}},
		{query: `ground(a(1, _)).`, results: []string{}},
		// 8.3.11.4
		{query: `acyclic_term(a(1, _)).`, results: []string{""}},
		{query: `X = f(X), acyclic_term(X).`, results: []string{}},
		// 8.4.1.4
		{query: `'@=<'(1.0, 1).`, results: []string{""}},
		{query: `'@<'(1.0, 1).`, results: []string{""}},
		{query: `'\\=='(1, 1).`, results: []string{}},
		{query: `'@=<'(aardvark, zebra).`, results: []string{""}},
		{query: `'@=<'(short, short).`, results: []string{""}},
		{query: `'@=<'(short, shorter).`, results: []string{""}},
		{query: `'@<'(foo(a, b), north(a)).`, results: []string{}},
		{query: `'@>'(foo(b), foo(a)).`, results: []string{""}},
		{query: `'@<'(foo(a, X), foo(b, Y)).`, results: []string{""}},
		{query: `'@<'(foo(X, a), foo(Y, b)).`, results: []string{""}},
		{query: `'@=<'(X, X).`, results: []string{""}},
		{query: `'=='(X, X).`, results: []string{""}},
		{query: `'@=<'(X, Y).`, results: []string{""}},
		{query: `'=='(X, Y).`, results: []string{}},
		{query: `\==(_, _).`, results: []string{""}},
		{query: `'=='(_, _).`, results: []string{}},
		{query: `'@=<'(_, _).`, results: []string{""}},
		{query: `'@=<'(foo(X, a), foo(Y, b)).`, results: []string{""}},
		// 8.4.2.4
		{query: `compare(Order, 3, 5).`, results: []string{"Order = <"}},
		{query: `compare(Order, d, d).`, results: []string{"Order = ="}},
		{query: `compare(Order, Order, <).`, results: []string{"Order = <"}},
		{query: `compare(<, <, <).`, results: []string{}},
		{query: `compare(1+2, 3, 3.0).`, err: "type_error(atom,1+2)"},
		{query: `compare(>=, 3, 3.0).`, err: "domain_error(order,>=)"},
		// TODO: sort/2, keysort/2
		// 8.5.1.4
		{query: `functor(foo(a, b, c), foo, 3).`, results: []string{""}},
		{query: `functor(foo(a, b, c), X, Y).`, results: []string{"X = foo, Y = 3"}},
		// {query: `functor(X, foo, 3).`, results: []string{"X = foo(_, _, _)"}}, TODO: X = foo(_1571,_1572,_1573)
		{query: `functor(mats(A, B), A, B).`, results: []string{"A = mats, B = 2"}},
		{query: `functor(foo(a), foo, 2).`, results: []string{}},
		{query: `functor(foo(a), fo, 1).`, results: []string{}},
		{query: `functor(1, X, Y).`, results: []string{"X = 1, Y = 0"}},
		{query: `functor(X, 1.1, 0).`, results: []string{"X = 1.1"}},
		{query: `functor([_|_], '.', 2).`, results: []string{""}},
		{query: `functor([], [], 0).`, results: []string{""}},
		{query: `functor(X, Y, 3).`, err: "instantiation_error"},
		{query: `functor(X, foo, N).`, err: "instantiation_error"},
		{query: `functor(F, 1.5, 1).`, err: "type_error(atom,1.5)"},
		{query: `functor(F, foo(a), 1).`, err: "type_error(atomic,foo(a))"},
		// {query: `current_prolog_flag(max_arity, A), X is A + 1, functor(T, foo, X).`, err: "representation_error(max_arity)"}, TODO: What is our max_arity?
		{query: `Minus_1 is 0 - 1, functor(F, foo, Minus_1).`, err: "domain_error(not_less_than_zero,-1)"},
		// 8.5.2.4
		{query: `arg(1, foo(a, b), a).`, results: []string{""}},
		{query: `arg(1, foo(a, b), X).`, results: []string{"X = a"}},
		{query: `arg(1, foo(X, b), a).`, results: []string{"X = a"}},
		{query: `arg(1, foo(X, b), Y).`, results: []string{""}}, // TODO: Confirm actually X and Y are unified.
		{query: `arg(1, foo(a, b), b).`, results: []string{}},
		{query: `arg(0, foo(a, b), foo).`, results: []string{}},
		{query: `arg(3, foo(3, 4), N).`, results: []string{}},
		{query: `arg(X, foo(a, b), a).`, err: "instantiation_error"},
		{query: `arg(1, X, a).`, err: "instantiation_error"},
		{query: `arg(0, atom, A).`, err: "type_error(compound,atom)"},
		{query: `arg(0, 3, A).`, err: "type_error(compound,3)"},
		{query: `arg(1, foo(X), u(X)).`, results: []string{"X = u(...)"}},
		// 8.5.3.4
		{query: `'=..'(foo(a, b), [foo, a, b]).`, results: []string{""}},
		{query: `'=..'(X, [foo, a, b]).`, results: []string{"X = foo(a,b)"}},
		{query: `'=..'(foo(a, b), L).`, results: []string{"L = [foo,a,b]"}},
		{query: `'=..'(foo(X, b), [foo, a, Y]).`, results: []string{"X = a, Y = b"}},
		{query: `'=..'(1, [1]).`, results: []string{""}},
		{query: `'=..'(foo(a, b), [foo, b, a]).`, results: []string{}},
		{query: `'=..'(X, Y).`, err: "instantiation_error"},
		{query: `'=..'(X, [foo, a | Y]).`, err: "instantiation_error"},
		{query: `'=..'(X, [foo | bar]).`, err: "type_error(list,[foo|bar])"},
		{query: `'=..'(X, [Foo, bar]).`, err: "instantiation_error"},
		{query: `'=..'(X, [3, 1]).`, err: "type_error(atom,3)"},
		{query: `'=..'(X, [1.1, foo]).`, err: "type_error(atom,1.1)"},
		{query: `'=..'(X, [a(b), 1]).`, err: "type_error(atom,a(b))"},
		{query: `'=..'(X, 4).`, err: "type_error(list,4)"},
		{query: `'=..'(f(X), [f, u(X)]).`, results: []string{"X = u(...)"}},
		// 8.5.4.4
		{query: `copy_term(X, Y).`, results: []string{""}},
		{query: `copy_term(X, 3).`, results: []string{""}},
		{query: `copy_term(_, a).`, results: []string{""}},
		{query: `copy_term(a+X, X+b).`, results: []string{"X = a"}},
		{query: `copy_term(_, _).`, results: []string{""}},
		{query: `copy_term(X+X+Y, A+B+B).`, results: []string{""}}, // TODO: A = B
		{query: `copy_term(a, b).`, results: []string{}},
		{query: `copy_term(a+X, X+b), copy_term(a+X, X+b).`, results: []string{}},
		{query: `copy_term(demoen(X, X), demoen(Y, f(Y))).`, results: []string{"Y = f(...)"}},
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
			)
			for result, err := range Query[Result](t.Context(), i, test.query) {
				if err != nil {
					if test.err == "" {
						t.Fatal(err)
					}
					errTerm, err := runtime.ErrorTerm(i.engine.Arena, err)
					if err != nil {
						t.Fatal(err)
					}
					s := fmt.Sprintf("%s", &syntax.Formatter{Arena: i.engine.Arena, Term: errTerm})
					if !strings.Contains(s, test.err) {
						t.Errorf("Expected error %q, got %q", test.err, s)
					}
					continue
				}
				results = append(results, result)
			}

			if len(results) != len(test.results) {
				t.Fatalf("expected %d results, got %d", len(test.results), len(results))
			}

			for j := range len(results) {
				got := formatResult(results[j])
				want := test.results[j]
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
