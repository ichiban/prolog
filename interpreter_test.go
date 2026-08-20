package prolog

import (
	"bytes"
	"embed"
	"io"
	"io/fs"
	"os"
	"slices"
	"strings"
	"testing"
)

//go:embed testdata
var testdata embed.FS

func TestInterpreter_Query(t *testing.T) {
	tests := []struct {
		loaded       []string
		setup        []string
		teardown     []string
		input        string
		query        string
		expectations [][]string
		adInfinitum  bool
		output       string
		ignoreOutput bool
		err          string
	}{
		/*
			Examples in ISO/IEC 13211-1.
		*/
		// 7.8.1.4
		{query: `true.`, expectations: [][]string{{`true.`}}},
		// 7.8.2.4
		{query: `fail.`, expectations: [][]string{}},
		// 7.8.3.4
		{query: `call(!).`, expectations: [][]string{{`true.`}}},
		{query: `call(fail).`, expectations: [][]string{}},
		{query: `call((fail, X)).`, expectations: [][]string{}},
		{query: `call((fail, call(1))).`, expectations: [][]string{}},
		{loaded: []string{"testdata/7.8.3.4.pl"}, query: `b(X).`, err: `instantiation_error`, ignoreOutput: true},
		{loaded: []string{"testdata/7.8.3.4.pl"}, query: `b(3).`, output: `3`, err: `type_error(callable,3)`}, // type_error(callable,(write(3),3))
		{loaded: []string{"testdata/7.8.3.4.pl"}, query: `Z = !, call((Z = !, a(X), Z)).`, expectations: [][]string{
			{`X = 1.`, `Z = !.`},
		}},
		{loaded: []string{"testdata/7.8.3.4.pl"}, query: `call((Z=!, a(X), Z)).`, expectations: [][]string{
			{`X = 1.`, `Z = !.`},
		}},
		{loaded: []string{"testdata/7.8.3.4.pl"}, query: `call((write(3), X)).`, err: `instantiation_error`, ignoreOutput: true},
		{query: `call(X).`, err: `instantiation_error`},
		{query: `call(1).`, err: `type_error(callable,1)`},
		{query: `call((fail, 1)).`, err: `type_error(callable,(fail,1))`},
		{query: `call((write(3), 1)).`, err: `type_error(callable,1)`, ignoreOutput: true}, // type_error(callable,(write(3),1))
		{query: `call((1;true)).`, err: `type_error(callable,1)`},                          // type_error(callable,(1;true))
		// 7.8.4.4
		{query: `!.`, expectations: [][]string{
			{`true.`},
		}},
		{query: `(!, fail; true).`, expectations: [][]string{}},
		{query: `(call(!), fail; true).`, expectations: [][]string{
			{`true.`},
		}},
		{loaded: []string{"testdata/7.8.4.4.pl"}, query: `twice(_), !, write('Forwards '), fail.`, expectations: [][]string{}, output: `C Forwards `},
		{query: `(!; write('No ')), write('Cut disjunction '), fail.`, expectations: [][]string{}, output: `Cut disjunction `},
		{loaded: []string{"testdata/7.8.4.4.pl"}, query: `twice(_), (write('No '); !), write('Cut '), fail.`, expectations: [][]string{}, output: `C No Cut Cut `},
		{loaded: []string{"testdata/7.8.4.4.pl"}, query: `twice(_), (!, fail; write('No ')).`, expectations: [][]string{}, output: `C `},
		{loaded: []string{"testdata/7.8.4.4.pl"}, query: `twice(X), call(X), write('Forwards '), fail.`, expectations: [][]string{}, output: `C Forwards Moss Forwards `},
		{loaded: []string{"testdata/7.8.4.4.pl"}, query: `goal(X), call(X), write('Forwards '), fail.`, expectations: [][]string{}, output: `C Forwards Three Forwards `},
		{loaded: []string{"testdata/7.8.4.4.pl"}, query: `twice(_), \+(\+(!)), write('Forwards '), fail.`, expectations: [][]string{}, output: `C Forwards Moss Forwards `},
		{loaded: []string{"testdata/7.8.4.4.pl"}, query: `twice(_), once(!), write('Forwards '), fail.`, expectations: [][]string{}, output: `C Forwards Moss Forwards `},
		{loaded: []string{"testdata/7.8.4.4.pl"}, query: `twice(_), call(!), write('Forwards '), fail.`, expectations: [][]string{}, output: `C Forwards Moss Forwards `},
		// 7.8.5.4
		{query: `','(X=1, var(X)).`, expectations: [][]string{}},
		{query: `','(var(X), X=1).`, expectations: [][]string{
			{`X = 1.`},
		}},
		{query: `','(X = true, call(X)).`, expectations: [][]string{
			{`X = true.`},
		}},
		// 7.8.6.4
		{query: `';'(true, fail).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `';'((!, fail), true).`, expectations: [][]string{}},
		{query: `';'(!, call(3)).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `';'((X = 1, !), X = 2).`, expectations: [][]string{
			{`X = 1.`},
		}},
		{query: `','(';'(X=1, X=2), ';'(true, !)).`, expectations: [][]string{
			{`X = 1.`},
			{`X = 1.`},
		}},
		// 7.8.7.4
		{query: `'->'(true, true).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `'->'(true, fail).`, expectations: [][]string{}},
		{query: `'->'(fail, true).`, expectations: [][]string{}},
		{query: `'->'(true, X=1).`, expectations: [][]string{
			{`X = 1.`},
		}},
		{query: `'->'(';'(X=1, X=2), true).`, expectations: [][]string{
			{`X = 1.`},
		}},
		{query: `'->'(true, ';'(X=1, X=2)).`, expectations: [][]string{
			{`X = 1.`},
			{`X = 2.`},
		}},
		// 7.8.8.4
		{query: `';'('->'(true, true), fail).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `';'('->'(fail, true), true).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `';'('->'(true, fail), fail).`, expectations: [][]string{}},
		{query: `';'('->'(true, X=1), X=2).`, expectations: [][]string{
			{`X = 1.`},
		}},
		{query: `';'('->'(fail, X=1), X=2).`, expectations: [][]string{
			{`X = 2.`},
		}},
		{query: `';'('->'(true, ';'(X=1, X=2)), true).`, expectations: [][]string{
			{`X = 1.`},
			{`X = 2.`},
		}},
		{query: `';'('->'(';'(X=1, X=2), true), true).`, expectations: [][]string{
			{`X = 1.`},
		}},
		{query: `';'(('->'(!, fail), true), true).`, expectations: [][]string{
			{`true.`},
		}},
		// 7.8.9.4
		{loaded: []string{"testdata/7.8.9.4.pl"}, query: `catch(foo(5), test(Y), true).`, expectations: [][]string{
			{`Y = 10.`},
		}},
		{loaded: []string{"testdata/7.8.9.4.pl"}, query: `catch(bar(3), Z, true).`, expectations: [][]string{
			{`Z = 3.`},
		}},
		{loaded: []string{"testdata/7.8.9.4.pl"}, query: `catch(true, _, 3).`, expectations: [][]string{
			{`true.`},
		}},
		{loaded: []string{"testdata/7.8.9.4.pl"}, query: `catch(true, C, write(demoen)), throw(bla).`, err: `system_error`},
		{loaded: []string{"testdata/7.8.9.4.pl"}, query: `catch(car(X), Y, true).`, expectations: [][]string{
			{`Y = 1.`},
		}},
		{loaded: []string{"testdata/7.8.9.4.pl"}, query: `catch(number_chars(X, ['1', 'a', '0']), error(syntax_error(_), _), fail).`, expectations: [][]string{}},
		{loaded: []string{"testdata/7.8.9.4.pl"}, query: `catch(g, C, write(h1)).`, expectations: [][]string{
			{`C = c.`},
		}, output: `h1`},
		{loaded: []string{"testdata/7.8.9.4.pl"}, query: `catch(coo(X), Y, true).`, expectations: [][]string{
			{`Y = error(instantiation_error,throw/1).`},
		}},
		// 8.2.1.4
		{query: `'='(1, 1).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `'='(X, 1).`, expectations: [][]string{
			{`X = 1.`},
		}},
		{query: `'='(X, Y).`, expectations: [][]string{
			{`X = Y.`},
		}},
		{query: `'='(_, _).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `'='(X, Y), '='(X, abc).`, expectations: [][]string{
			{`X = abc.`, `Y = abc.`},
		}},
		{query: `'='(f(X, def), f(def, Y)).`, expectations: [][]string{
			{`X = def.`, `Y = def.`},
		}},
		{query: `'='(1, 2).`, expectations: [][]string{}},
		{query: `'='(1, 1.0).`, expectations: [][]string{}},
		{query: `'='(g(X), f(f(X))).`, expectations: [][]string{}},
		{query: `'='(f(X, 1), f(a(X))).`, expectations: [][]string{}},
		{query: `'='(f(X, Y, X), f(a(X), a(Y), Y, 2)).`, expectations: [][]string{}},
		{query: `'='(X, a(X)).`, expectations: [][]string{
			{`X = a(X).`},
		}},
		{query: `'='(f(X, 1), f(a(X), 2)).`, expectations: [][]string{}},
		{query: `'='(f(1, X, 1), f(2, a(X), 2)).`, expectations: [][]string{}},
		{query: `'='(f(1, X), f(2, a(X))).`, expectations: [][]string{}},
		{query: `'='(f(X, Y, X, 1), f(a(X), a(Y), Y, 2)).`, expectations: [][]string{}},
		// 8.2.2.4
		{query: `unify_with_occurs_check(1, 1).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `unify_with_occurs_check(X, 1).`, expectations: [][]string{
			{`X = 1.`},
		}},
		{query: `unify_with_occurs_check(X, Y).`, expectations: [][]string{
			{`Y == X.`},
		}},
		{query: `unify_with_occurs_check(_, _).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `unify_with_occurs_check(X, Y), unify_with_occurs_check(X, abc).`, expectations: [][]string{
			{`X = abc.`, `Y = abc.`},
		}},
		{query: `unify_with_occurs_check(f(X, def), f(def, Y)).`, expectations: [][]string{
			{`X = def.`, `Y = def.`},
		}},
		{query: `unify_with_occurs_check(1, 2).`, expectations: [][]string{}},
		{query: `unify_with_occurs_check(1, 1.0).`, expectations: [][]string{}},
		{query: `unify_with_occurs_check(g(X), f(f(X))).`, expectations: [][]string{}},
		{query: `unify_with_occurs_check(f(X, 1), f(a(X))).`, expectations: [][]string{}},
		{query: `unify_with_occurs_check(f(X, Y, X), f(a(X), a(Y), Y, 2)).`, expectations: [][]string{}},
		{query: `unify_with_occurs_check(X, a(X)).`, expectations: [][]string{}},
		{query: `unify_with_occurs_check(f(X, 1), f(a(X), 2)).`, expectations: [][]string{}},
		{query: `unify_with_occurs_check(f(1, X, 1), f(2, a(X), 2)).`, expectations: [][]string{}},
		{query: `unify_with_occurs_check(f(1, X), f(2, a(X))).`, expectations: [][]string{}},
		{query: `unify_with_occurs_check(f(X, Y, X, 1), f(a(X), a(Y), Y, 2)).`, expectations: [][]string{}},
		// 8.2.3.4
		{query: `'\\='(1, 1).`, expectations: [][]string{}},
		{query: `\=(X, 1).`, expectations: [][]string{}},
		{query: `'\\='(X, Y).`, expectations: [][]string{}},
		{query: `\=(_, _).`, expectations: [][]string{}},
		{query: `\=(f(X, def), f(def, Y)).`, expectations: [][]string{}},
		{query: `'\\='(1, 2).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `\=(1, 1.0).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `'\\='(g(X), f(f(X))).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `\=(f(X, 1), f(a(X))).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `'\\='(f(X, Y, X), f(a(X), a(Y), Y, 2)).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `\=(X, a(X)).`, expectations: [][]string{}},
		{query: `'\\='(f(X, 1), f(a(X), 2)).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `'\\='(f(1, X, 1), f(2, a(X), 2)).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `\=(f(2, X), f(2, a(X))).`, expectations: [][]string{}},
		{query: `'\\='(f(X, Y, X, 1), f(a(X), a(Y), Y, 2)).`, expectations: [][]string{
			{`true.`},
		}},
		// 8.2.4.4
		{query: `subsumes_term(a, a).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `subsumes_term(f(X, Y), f(Z, Z)).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `subsumes_term(f(Z, Z), f(X, Y)).`, expectations: [][]string{}},
		{query: `subsumes_term(g(X), g(f(X))).`, expectations: [][]string{}},
		{query: `subsumes_term(X, f(X)).`, expectations: [][]string{}},
		{query: `subsumes_term(X, Y), subsumes_term(Y, f(X)).`, expectations: [][]string{
			{`true.`},
		}},
		// 8.3.1.4
		{query: `var(foo).`, expectations: [][]string{}},
		{query: `var(Foo).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `foo=Foo, var(Foo).`, expectations: [][]string{}},
		{query: `var(_).`, expectations: [][]string{
			{`true.`},
		}},
		// 8.3.2.4
		{query: `atom(atom).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `atom('string').`, expectations: [][]string{
			{`true.`}, // NOTE: This is technically not a string.
		}},
		{query: `atom(a(b)).`, expectations: [][]string{}},
		{query: `atom(Var).`, expectations: [][]string{}},
		{query: `atom([]).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `atom(6).`, expectations: [][]string{}},
		{query: `atom(3.3).`, expectations: [][]string{}},
		// 8.3.3.4
		{query: `integer(3).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `integer(-3).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `integer(3.3).`, expectations: [][]string{}},
		{query: `integer(X).`, expectations: [][]string{}},
		{query: `integer(atom).`, expectations: [][]string{}},
		// 8.3.4.4
		{query: `float(3.3).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `float(-3.3).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `float(3).`, expectations: [][]string{}},
		{query: `float(atom).`, expectations: [][]string{}},
		{query: `float(X).`, expectations: [][]string{}},
		// 8.3.5.4
		{query: `atomic(atom).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `atomic(a(b)).`, expectations: [][]string{}},
		{query: `atomic(Var).`, expectations: [][]string{}},
		{query: `atomic(6).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `atomic(3.3).`, expectations: [][]string{
			{`true.`},
		}},
		// 8.3.6.4
		{query: `compound(33.3).`, expectations: [][]string{}},
		{query: `compound(-33.3).`, expectations: [][]string{}},
		{query: `compound(-a).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `compound(_).`, expectations: [][]string{}},
		{query: `compound(a).`, expectations: [][]string{}},
		{query: `compound(a(b)).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `compound([]).`, expectations: [][]string{}},
		{query: `compound([a]).`, expectations: [][]string{
			{`true.`},
		}},
		// 8.3.7.4
		{query: `nonvar(33.3).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `nonvar(foo).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `nonvar(Foo).`, expectations: [][]string{}},
		{query: `foo = Foo, nonvar(Foo).`, expectations: [][]string{
			{`Foo = foo.`},
		}},
		{query: `nonvar(_).`, expectations: [][]string{}},
		{query: `nonvar(a(b)).`, expectations: [][]string{
			{`true.`},
		}},
		// 8.3.8.4
		{query: `number(3).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `number(3.3).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `number(-3).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `number(a).`, expectations: [][]string{}},
		{query: `number(X).`, expectations: [][]string{}},
		// 8.3.9.4
		{query: `callable(a).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `callable(3).`, expectations: [][]string{}},
		{query: `callable(X).`, expectations: [][]string{}},
		{query: `callable((1,2)).`, expectations: [][]string{
			{`true.`},
		}},
		// 8.3.10.4
		{query: `ground(3).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `ground(a(1, _)).`, expectations: [][]string{}},
		// 8.3.11.4
		{query: `acyclic_term(a(1, _)).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `X = f(X), acyclic_term(X).`, expectations: [][]string{}},
		// 8.4.1.4
		{query: `'@=<'(1.0, 1).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `'@<'(1.0, 1).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `'\\=='(1, 1).`, expectations: [][]string{}},
		{query: `'@=<'(aardvark, zebra).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `'@=<'(short, short).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `'@=<'(short, shorter).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `'@<'(foo(a, b), north(a)).`, expectations: [][]string{}},
		{query: `'@>'(foo(b), foo(a)).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `'@<'(foo(a, X), foo(b, Y)).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `'@<'(foo(X, a), foo(Y, b)).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `'@=<'(X, X).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `'=='(X, X).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `'@=<'(X, Y).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `'=='(X, Y).`, expectations: [][]string{}},
		{query: `\==(_, _).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `'=='(_, _).`, expectations: [][]string{}},
		{query: `'@=<'(_, _).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `'@=<'(foo(X, a), foo(Y, b)).`, expectations: [][]string{
			{`true.`},
		}},
		// 8.4.2.4
		{query: `compare(Order, 3, 5).`, expectations: [][]string{
			{`Order = (<).`},
		}},
		{query: `compare(Order, d, d).`, expectations: [][]string{
			{`Order = (=).`},
		}},
		{query: `compare(Order, Order, <).`, expectations: [][]string{
			{`Order = (<).`},
		}},
		{query: `compare(<, <, <).`, expectations: [][]string{}},
		{query: `compare(1+2, 3, 3.0).`, err: "type_error(atom,1+2)"},
		{query: `compare(>=, 3, 3.0).`, err: "domain_error(order,>=)"},
		// 8.4.3.4
		{query: `sort([1, 1], Sorted).`, expectations: [][]string{
			{`Sorted = [1].`},
		}},
		{query: `sort([1+Y, z, a, V, 1, 2, V, 1, 7.0, 8.0, 1+Y, 1+2, 8.0, -a, -X, a], Sorted).`, expectations: [][]string{
			{`Sorted = [V, 7.0, 8.0, 1, 2, a, z, -X, -a, 1+Y, 1+2].`},
		}},
		{query: `sort([X, 1], [1, 1]).`, expectations: [][]string{
			{`X = 1.`},
		}},
		{query: `sort([1, 1], [1, 1]).`, expectations: [][]string{}},
		{query: `sort([V], V).`, expectations: [][]string{
			{`V = [V].`},
		}},
		{query: `sort([f(U),U,U,f(V),f(U),V], L).`, expectations: [][]string{
			{`L = [U,V,f(U),f(V)].`},
		}},
		// 8.4.4.4
		{query: `keysort([1-1, 1-1], Sorted).`, expectations: [][]string{
			{`Sorted = [1-1, 1-1].`},
		}},
		{query: `keysort([2-99, 1-a, 3-f(_), 1-z, 1-a, 2-44], Sorted).`, expectations: [][]string{
			{`Sorted = [1-a, 1-z, 1-a, 2-99, 2-44, 3-f(_)].`},
		}},
		{query: `keysort([X-1, 1-1], [2-1, 1-1]).`, expectations: [][]string{
			{`X = 2.`},
		}},
		// {query: `Pairs = [1-2|Pairs], keysort(Pairs, Sorted).`, err: "type_error(list,Pairs)"}, TODO: Match err with variables.
		{query: `keysort([V-V], V).`, expectations: [][]string{
			{`V = [V-V].`},
		}},
		// 8.5.1.4
		{query: `functor(foo(a, b, c), foo, 3).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `functor(foo(a, b, c), X, Y).`, expectations: [][]string{
			{`X = foo.`, `Y = 3.`},
		}},
		{query: `functor(X, foo, 3).`, expectations: [][]string{
			{`X = foo(_, _, _).`},
		}},
		{query: `functor(mats(A, B), A, B).`, expectations: [][]string{
			{`A = mats.`, `B = 2.`},
		}},
		{query: `functor(foo(a), foo, 2).`, expectations: [][]string{}},
		{query: `functor(foo(a), fo, 1).`, expectations: [][]string{}},
		{query: `functor(1, X, Y).`, expectations: [][]string{
			{`X = 1.`, `Y = 0.`},
		}},
		{query: `functor(X, 1.1, 0).`, expectations: [][]string{
			{`X = 1.1.`},
		}},
		{query: `functor([_|_], '.', 2).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `functor([], [], 0).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `functor(X, Y, 3).`, err: "instantiation_error"},
		{query: `functor(X, foo, N).`, err: "instantiation_error"},
		{query: `functor(F, 1.5, 1).`, err: "type_error(atom,1.5)"},
		{query: `functor(F, foo(a), 1).`, err: "type_error(atomic,foo(a))"},
		// {query: `current_prolog_flag(max_arity, A), X is A + 1, functor(T, foo, X).`, err: "representation_error(max_arity)"}, TODO: What is our max_arity?
		{query: `Minus_1 is 0 - 1, functor(F, foo, Minus_1).`, err: "domain_error(not_less_than_zero,-1)"},
		// 8.5.2.4
		{query: `arg(1, foo(a, b), a).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `arg(1, foo(a, b), X).`, expectations: [][]string{
			{`X = a.`},
		}},
		{query: `arg(1, foo(X, b), a).`, expectations: [][]string{
			{`X = a.`},
		}},
		{query: `arg(1, foo(X, b), Y).`, expectations: [][]string{
			{`Y = X.`},
		}},
		{query: `arg(1, foo(a, b), b).`, expectations: [][]string{}},
		{query: `arg(0, foo(a, b), foo).`, expectations: [][]string{}},
		{query: `arg(3, foo(3, 4), N).`, expectations: [][]string{}},
		{query: `arg(X, foo(a, b), a).`, err: "instantiation_error"},
		{query: `arg(1, X, a).`, err: "instantiation_error"},
		{query: `arg(0, atom, A).`, err: "type_error(compound,atom)"},
		{query: `arg(0, 3, A).`, err: "type_error(compound,3)"},
		{query: `arg(1, foo(X), u(X)).`, expectations: [][]string{
			{`X = u(X).`},
		}},
		// 8.5.3.4
		{query: `'=..'(foo(a, b), [foo, a, b]).`, expectations: [][]string{{`true.`}}},
		{query: `'=..'(X, [foo, a, b]).`, expectations: [][]string{
			{`X = foo(a,b).`},
		}},
		{query: `'=..'(foo(a, b), L).`, expectations: [][]string{
			{`L = [foo,a,b].`},
		}},
		{query: `'=..'(foo(X, b), [foo, a, Y]).`, expectations: [][]string{
			{`X = a.`, `Y = b.`},
		}},
		{query: `'=..'(1, [1]).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `'=..'(foo(a, b), [foo, b, a]).`, expectations: [][]string{}},
		{query: `'=..'(X, Y).`, err: "instantiation_error"},
		{query: `'=..'(X, [foo, a | Y]).`, err: "instantiation_error"},
		{query: `'=..'(X, [foo | bar]).`, err: "type_error(list,[foo|bar])"},
		{query: `'=..'(X, [Foo, bar]).`, err: "instantiation_error"},
		{query: `'=..'(X, [3, 1]).`, err: "type_error(atom,3)"},
		{query: `'=..'(X, [1.1, foo]).`, err: "type_error(atom,1.1)"},
		{query: `'=..'(X, [a(b), 1]).`, err: "type_error(atom,a(b))"},
		{query: `'=..'(X, 4).`, err: "type_error(list,4)"},
		{query: `'=..'(f(X), [f, u(X)]).`, expectations: [][]string{
			{`X = u(X).`},
		}},
		// 8.5.4.4
		{query: `copy_term(X, Y).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `copy_term(X, 3).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `copy_term(_, a).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `copy_term(a+X, X+b).`, expectations: [][]string{
			{`X = a.`},
		}},
		{query: `copy_term(_, _).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `copy_term(X+X+Y, A+B+B).`, expectations: [][]string{
			{`A = B.`},
		}},
		{query: `copy_term(a, b).`, expectations: [][]string{}},
		{query: `copy_term(a+X, X+b), copy_term(a+X, X+b).`, expectations: [][]string{}},
		{query: `copy_term(demoen(X, X), demoen(Y, f(Y))).`, expectations: [][]string{
			{`Y = f(Y).`},
		}},
		// 8.5.5.4
		{query: `term_variables(t, Vars).`, expectations: [][]string{
			{`Vars = [].`},
		}},
		{query: `term_variables(A+B*C/B-D, Vars).`, expectations: [][]string{
			{`Vars = [A,B,C,D].`},
		}},
		// {query: `term_variables(t, [_, _|a]).`, err: "type_error(list,[_,_|a])"}, TODO: Match err with variables.
		{query: `S=B+T, T=A*B, term_variables(S, Vars).`, expectations: [][]string{
			{`S = B+A*B.`, `T = A*B.`, `Vars = [B,A].`},
		}},
		{query: `T=A*B, S=B+T, term_variables(S, Vars).`, expectations: [][]string{
			{`S = B+A*B.`, `T = A*B.`, `Vars = [B,A].`},
		}},
		{query: `term_variables(A+B+B, [B|Vars]).`, expectations: [][]string{
			{`A = B.`, `Vars = [B].`},
		}},
		{query: `term_variables(X+Vars, Vars), Vars = [_, _].`, expectations: [][]string{
			{`true.`},
		}},
		// 8.6.1.4
		{query: `'is'(Result, 3+11.0).`, expectations: [][]string{
			{`Result = 14.0.`},
		}},
		{query: `X = 1+2, Y is X * 3.`, expectations: [][]string{
			{`X = 1+2.`, `Y = 9.`},
		}},
		{query: `'is'(3, 3).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `'is'(3, 3.0).`, expectations: [][]string{}},
		{query: `'is'(foo, 77).`, expectations: [][]string{}},
		{query: `'is'(77, N).`, err: "instantiation_error"},
		// 8.7.1.4
		{query: `'=:='(0, 1).`, expectations: [][]string{}},
		{query: `'=\\='(0, 1).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `'<'(0, 1).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `'>'(0, 1).`, expectations: [][]string{}},
		{query: `'>='(0, 1).`, expectations: [][]string{}},
		{query: `'=<'(0, 1).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `'=:='(1.0, 1).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `'=\\='(1.0, 1).`, expectations: [][]string{}},
		{query: `'<'(1.0, 1).`, expectations: [][]string{}},
		{query: `'>'(1.0, 1).`, expectations: [][]string{}},
		{query: `'>='(1.0, 1).`, expectations: [][]string{{`true.`}}},
		{query: `'=<'(1.0, 1).`, expectations: [][]string{{`true.`}}},
		{query: `'=:='(3*2, 7-1).`, expectations: [][]string{{`true.`}}},
		{query: `'=\\='(3*2, 7-1).`, expectations: [][]string{}},
		{query: `'<'(3*2, 7-1).`, expectations: [][]string{}},
		{query: `'>'(3*2, 7-1).`, expectations: [][]string{}},
		{query: `'>='(3*2, 7-1).`, expectations: [][]string{{`true.`}}},
		{query: `'=<'(3*2, 7-1).`, expectations: [][]string{{`true.`}}},
		{query: `'=:='(X, 5).`, err: "instantiation_error"},
		{query: `=\=(X, 5).`, err: "instantiation_error"},
		{query: `'<'(X, 5).`, err: "instantiation_error"},
		{query: `'>'(X, 5).`, err: "instantiation_error"},
		{query: `'>='(X, 5).`, err: "instantiation_error"},
		{query: `'=<'(X, 5).`, err: "instantiation_error"},
		// 8.8.1.4
		{loaded: []string{"testdata/8.8.pl"}, query: `clause(cat, true).`, expectations: [][]string{
			{`true.`},
		}},
		{loaded: []string{"testdata/8.8.pl"}, query: `clause(dog, true).`, expectations: [][]string{
			{`true.`},
		}},
		{loaded: []string{"testdata/8.8.pl"}, query: `clause(legs(I, 6), Body).`, expectations: [][]string{
			{`Body = insect(I).`},
		}},
		{loaded: []string{"testdata/8.8.pl"}, query: `clause(legs(C, 7), Body).`, expectations: [][]string{
			{`Body = (call(C), call(C)).`},
		}},
		{loaded: []string{"testdata/8.8.pl"}, query: `clause(insect(I), T).`, expectations: [][]string{
			{`I = ant.`, `T = true.`},
			{`I = bee.`, `T = true.`},
		}},
		{loaded: []string{"testdata/8.8.pl"}, query: `clause(x, Body).`, expectations: [][]string{}},
		{loaded: []string{"testdata/8.8.pl"}, query: `clause(_, B).`, err: "instantiation_error"},
		{loaded: []string{"testdata/8.8.pl"}, query: `clause(4, X).`, err: "type_error(callable,4)"},
		{loaded: []string{"testdata/8.8.pl"}, query: `clause(elk(N), Body).`, err: "permission_error(access,private_procedure,elk/1)"},
		{loaded: []string{"testdata/8.8.pl"}, query: `clause(atom(_), Body).`, err: "permission_error(access,private_procedure,atom/1)"},
		{loaded: []string{"testdata/8.8.pl"}, query: `clause(legs(A, 6), insect(f(A))).`, expectations: [][]string{
			{`true.`},
		}},
		// 8.8.2.4
		{loaded: []string{"testdata/8.8.pl"}, query: `current_predicate(dog/0).`, expectations: [][]string{
			{`true.`},
		}},
		{loaded: []string{"testdata/8.8.pl"}, query: `current_predicate(current_predicate/1).`, expectations: [][]string{}},
		{loaded: []string{"testdata/8.8.pl"}, query: `current_predicate(elk/Arity).`, expectations: [][]string{
			{`Arity = 1.`},
		}},
		{loaded: []string{"testdata/8.8.pl"}, query: `current_predicate(foo/A).`, expectations: [][]string{}},
		{loaded: []string{"testdata/8.8.pl"}, query: `current_predicate(Name/1).`, expectations: [][]string{
			{`Name = elk.`},
			{`Name = insect.`},
		}},
		{loaded: []string{"testdata/8.8.pl"}, query: `current_predicate(4).`, err: "type_error(predicate_indicator,4)"},
		// 8.9.1.4
		{loaded: []string{"testdata/8.9.1.4.pl"}, query: `asserta(legs(octopus, 8)).`, expectations: [][]string{
			{`true.`},
		}},
		{loaded: []string{"testdata/8.9.1.4.pl"}, query: `asserta((legs(A, 4) :- animal(A))).`, expectations: [][]string{
			{`true.`},
		}},
		{loaded: []string{"testdata/8.9.1.4.pl"}, query: `asserta((foo(X) :- X, call(X))).`, expectations: [][]string{
			{`true.`},
		}},
		{loaded: []string{"testdata/8.9.1.4.pl"}, query: `asserta(_).`, err: "instantiation_error"},
		{loaded: []string{"testdata/8.9.1.4.pl"}, query: `asserta(4).`, err: "type_error(callable,4)"},
		{loaded: []string{"testdata/8.9.1.4.pl"}, query: `asserta((foo :- 4)).`, err: "type_error(callable,4)"},
		{loaded: []string{"testdata/8.9.1.4.pl"}, query: `asserta((atom(_) :- true)).`, err: "permission_error(modify,static_procedure,atom/1)"},
		// 8.9.2.4
		{loaded: []string{"testdata/8.9.2.4.pl"}, query: `assertz(legs(spider, 8)).`, expectations: [][]string{
			{`true.`},
		}},
		{loaded: []string{"testdata/8.9.2.4.pl"}, query: `assertz((legs(B, 2) :- bird(B))).`, expectations: [][]string{
			{`true.`},
		}},
		{loaded: []string{"testdata/8.9.2.4.pl"}, query: `assertz((foo(X) :- X -> call(X))).`, expectations: [][]string{
			{`true.`},
		}},
		{loaded: []string{"testdata/8.9.2.4.pl"}, query: `assertz(_).`, err: "instantiation_error"},
		{loaded: []string{"testdata/8.9.2.4.pl"}, query: `assertz(4).`, err: "type_error(callable,4)"},
		{loaded: []string{"testdata/8.9.2.4.pl"}, query: `assertz((foo :- 4)).`, err: "type_error(callable,4)"},
		{loaded: []string{"testdata/8.9.2.4.pl"}, query: `assertz((atom(_) :- true)).`, err: "permission_error(modify,static_procedure,atom/1)"},
		// 8.9.3.4
		{loaded: []string{"testdata/8.9.3.4.pl"}, query: `retract(legs(octopus, 8)).`, expectations: [][]string{
			{`\+clause(legs(octopus, 8), true).`},
		}},
		{loaded: []string{"testdata/8.9.3.4.pl"}, query: `retract(legs(spider, 6)).`, expectations: [][]string{}},
		{loaded: []string{"testdata/8.9.3.4.pl"}, query: `retract((legs(X, 2) :- T)).`, expectations: [][]string{
			{`T = bird(X).`, `\+clause(legs(B, 2), bird(B)).`},
		}},
		{loaded: []string{"testdata/8.9.3.4.pl"}, setup: []string{
			`retract(legs(octopus, 8)).`,
			`retract((legs(X, 2) :- T)).`,
		}, query: `retract((legs(X, Y) :- Z)).`, expectations: [][]string{
			{`Y = 4.`, `Z = animal(X).`, `\+clause(legs(A, 4), animal(A)).`},
			{`Y = 6.`, `Z = insect(X).`, `\+clause(legs(A, 6), insect(A)).`},
			{`Y = 8.`, `X = spider.`, `Z = true.`, `\+clause(legs(spider, 8), true).`},
		}},
		{loaded: []string{"testdata/8.9.3.4.pl"}, setup: []string{
			`retract(legs(octopus, 8)).`,
			`retract((legs(X, 2) :- T)).`,
			`retract((legs(X, Y) :- Z)).`,
		}, query: `retract((legs(X, Y) :- Z)).`, expectations: [][]string{}},
		{loaded: []string{"testdata/8.9.3.4.pl"}, query: `retract(insect(I)), write(I), retract(insect(bee)), fail.`, expectations: [][]string{}, output: `antbee`},
		{loaded: []string{"testdata/8.9.3.4.pl"}, query: `retract((foo(A) :- A, call(A))).`, expectations: [][]string{
			{`true.`},
		}},
		{loaded: []string{"testdata/8.9.3.4.pl"}, query: `retract((foo(C) :- A -> B)).`, expectations: [][]string{
			{`A = call(C).`, `B = call(C).`, `\+clause(foo(X), (call(X) -> call(X))).`},
		}},
		{loaded: []string{"testdata/8.9.3.4.pl"}, query: `retract((X :- in_eec(Y))).`, err: "instantiation_error"},
		{loaded: []string{"testdata/8.9.3.4.pl"}, query: `retract((4 :- X)).`, err: "type_error(callable,4)"},
		{loaded: []string{"testdata/8.9.3.4.pl"}, query: `retract((atom(X) :- X == '[]')).`, err: "permission_error(modify,static_procedure,atom/1)"},
		// 8.9.4.4
		{setup: []string{`assertz(foo(a, b)).`}, query: `abolish(foo/2).`, expectations: [][]string{
			{`\+clause(foo(X, Y), true).`},
		}},
		{query: `abolish(foo/_).`, err: "instantiation_error"},
		{query: `abolish(foo).`, err: "type_error(predicate_indicator,foo)"},
		// {query: `abolish(foo(_)).`, err: "type_error(predicate_indicator,foo(_))"}, TODO: Match err with variables.
		{query: `abolish(abolish/1).`, err: "permission_error(modify,static_procedure,abolish/1)"},
		// 8.9.5.4
		{loaded: []string{"testdata/8.9.5.4.pl"}, query: `retractall(insect(bee)).`, expectations: [][]string{
			{`\+insect(bee).`},
		}},
		{loaded: []string{"testdata/8.9.5.4.pl"}, query: `retractall(insect(_)).`, expectations: [][]string{
			{`\+insect(_).`},
		}},
		{loaded: []string{"testdata/8.9.5.4.pl"}, query: `retractall(insect(spider)).`, expectations: [][]string{
			{`true.`},
		}},
		{loaded: []string{"testdata/8.9.5.4.pl"}, query: `retractall(mammal(_)).`, expectations: [][]string{
			{`true.`},
		}},
		{loaded: []string{"testdata/8.9.5.4.pl"}, query: `retractall(3).`, err: "type_error(callable,3)"},
		{loaded: []string{"testdata/8.9.5.4.pl"}, query: `retractall(retractall(_)).`, err: "permission_error(modify,static_procedure,retractall/1)"},
		// 8.10.1.4
		{query: `findall(X, (X=1; X=2), S).`, expectations: [][]string{
			{`S = [1, 2].`},
		}},
		{query: `findall(X+Y, (X=1), S).`, expectations: [][]string{
			{`S = [1+_].`},
		}},
		{query: `findall(X, fail, L).`, expectations: [][]string{
			{`L = [].`},
		}},
		{query: `findall(X, (X=1; X=1), S).`, expectations: [][]string{
			{`S = [1, 1].`},
		}},
		{query: `findall(X, (X=2; X=1), [1, 2]).`, expectations: [][]string{}},
		{query: `findall(X, (X=1; X=2), [X, Y]).`, expectations: [][]string{
			{`X = 1.`, `Y = 2.`},
		}},
		{query: `findall(X, Goal, S).`, err: "instantiation_error"},
		{query: `findall(X, 4, S).`, err: "type_error(callable,4)"},
		// 8.10.2.4
		{query: `bagof(X, (X=1; X=2), S).`, expectations: [][]string{
			{`S = [1, 2].`},
		}},
		{query: `bagof(X, (X=1; X=2), X).`, expectations: [][]string{
			{`X = [1, 2].`},
		}},
		{query: `bagof(X, (X=Y ; X=Z), S).`, expectations: [][]string{
			{`S = [Y, Z].`},
		}},
		{query: `bagof(X, fail, S).`, expectations: [][]string{}},
		{query: `bagof(1, (Y=1 ; Y=2), L).`, expectations: [][]string{
			{`L = [1].`, `Y = 1.`},
			{`L = [1].`, `Y = 2.`},
		}},
		{query: `bagof(f(X, Y), (X=a ; Y=b), L).`, expectations: [][]string{
			{`L = [f(a, _), f(_, b)].`},
		}},
		{query: `bagof(X, Y^((X=1, Y=1) ; (X=2, Y=2)), S).`, expectations: [][]string{
			{`S = [1, 2].`},
		}},
		{query: `bagof(X, Y^((X=1 ; Y=1) ; (X=2, Y=2)), S).`, expectations: [][]string{
			{`S = [1, _, 2].`},
		}},
		{setup: []string{`set_prolog_flag(unknown, warning).`}, query: `bagof(X, (Y^(X=1 ; Y=2) ; X=3), S).`, expectations: [][]string{
			{`S = [3].`, `Y = _.`}, // Also, warning on undefined procedure ^/2.
		}},
		{query: `bagof(X, (X=Y ; X=Z ; Y=1), S).`, expectations: [][]string{
			{`S = [Y, Z].`},
			{`S = [_].`, `Y = 1.`},
		}},
		{setup: []string{
			`assertz(a(1, f(_))).`,
			`assertz(a(2, f(_))).`,
		}, query: `bagof(X, a(X, Y), L).`, expectations: [][]string{
			{`L = [1, 2].`, `Y = f(_).`},
		}},
		{setup: []string{
			`assertz(b(1, 1)).`,
			`assertz(b(1, 1)).`,
			`assertz(b(1, 2)).`,
			`assertz(b(2, 1)).`,
			`assertz(b(2, 2)).`,
			`assertz(b(2, 2)).`,
		}, query: `bagof(X, b(X, Y), L).`, expectations: [][]string{
			{`L = [1, 1, 2].`, `Y = 1.`},
			{`L = [1, 2, 2].`, `Y = 2.`},
		}},
		{query: `bagof(X, Y^Z, L).`, err: "instantiation_error"},
		{query: `bagof(X, 1, L).`, err: "type_error(callable,1)"},
		// 8.10.3.4
		{query: `setof(X, (X=1; X=2), S).`, expectations: [][]string{
			{`S = [1, 2].`},
		}},
		{query: `bagof(X, (X=1; X=2), X).`, expectations: [][]string{
			{`X = [1, 2].`},
		}},
		{query: `setof(X, (X=2; X=1), S).`, expectations: [][]string{
			{`S = [1, 2].`},
		}},
		{query: `setof(X, (X=2; X=2), S).`, expectations: [][]string{
			{`S = [2].`},
		}},
		{query: `setof(X, (X=Y ; X=Z), S).`, expectations: [][]string{
			{`S = [Y, Z].`},
		}},
		{query: `setof(X, fail, S).`, expectations: [][]string{}},
		{query: `setof(1, (Y=2 ; Y=1), L).`, expectations: [][]string{
			{`L = [1].`, `Y = 2.`},
			{`L = [1].`, `Y = 1.`},
		}},
		{query: `setof(f(X, Y), (X=a ; Y=b), L).`, expectations: [][]string{
			{`L = [f(_, b), f(a, _)].`},
		}},
		{query: `setof(X, Y^((X=1, Y=1) ; (X=2, Y=2)), S).`, expectations: [][]string{
			{`S = [1, 2].`},
		}},
		{query: `setof(X, Y^((X=1 ; Y=1) ; (X=2, Y=2)), S).`, expectations: [][]string{
			{`S = [_, 1, 2].`},
		}},
		{setup: []string{`set_prolog_flag(unknown, warning).`}, query: `setof(X, (Y^(X=1 ; Y=2) ; X=3), S).`, expectations: [][]string{
			{`S = [3].`, `Y = _.`}, // Also, warning on undefined procedure ^/2.
		}},
		{query: `setof(X, (X=Y ; X=Z ; Y=1), S).`, expectations: [][]string{
			{`S = [Y, Z].`},
			{`S = [_].`, `Y = 1.`},
		}},
		{loaded: []string{"testdata/8.10.3.4.pl"}, query: `setof(X, a(X, Y), L).`, expectations: [][]string{
			{`L = [1, 2].`, `Y = f(_).`},
		}},
		{loaded: []string{"testdata/8.10.3.4.pl"}, query: `setof(X, member(X,[f(U,b),f(V,c)]), L).`, expectations: [][]string{
			{`L = [f(U,b), f(V,c)].`},
		}},
		{loaded: []string{"testdata/8.10.3.4.pl"}, query: `setof(X, member(X,[f(U,b),f(V,c)]), [f(a,c),f(a,b)]).`, expectations: [][]string{}},
		{loaded: []string{"testdata/8.10.3.4.pl"}, query: `setof(X, member(X,[f(b,U),f(c,V)]), [f(b,a),f(c,a)]).`, expectations: [][]string{
			{`U = a.`, `V = a.`},
		}},
		{loaded: []string{"testdata/8.10.3.4.pl"}, query: `setof(X, member(X,[V,U,f(U),f(V)]), L).`, expectations: [][]string{
			{`L = [U, V, f(U), f(V)].`},
		}},
		{loaded: []string{"testdata/8.10.3.4.pl"}, query: `setof(X, member(X,[V,U,f(U),f(V)]), [a,b,f(a),f(b)]).`, expectations: [][]string{
			{`U = b.`, `V = a.`},
		}},
		{loaded: []string{"testdata/8.10.3.4.pl"}, query: `setof(X, member(X,[V,U,f(U),f(V)]), [a,b,f(b),f(a)]).`, expectations: [][]string{}},
		{loaded: []string{"testdata/8.10.3.4.pl"}, query: `setof(X, (exists(U,V)^member(X,[V,U,f(U),f(V)])), [a,b,f(b),f(a)]).`, expectations: [][]string{
			{},
		}},
		{loaded: []string{"testdata/8.10.3.4.pl"}, query: `setof(X, b(X, Y), L).`, expectations: [][]string{
			{`L = [1, 2].`, `Y = 1.`},
			{`L = [1, 2].`, `Y = 2.`},
		}},
		{loaded: []string{"testdata/8.10.3.4.pl"}, query: `setof(X-Xs,Y^setof(Y,b(X,Y),Xs),L).`, expectations: [][]string{
			{`L = [1-[1,2], 2-[1,2]].`},
		}},
		{loaded: []string{"testdata/8.10.3.4.pl"}, query: `setof(X-Xs,setof(Y,b(X,Y),Xs),L).`, expectations: [][]string{
			{`L = [1-[1,2], 2-[1,2]].`, `Y = _.`},
		}},
		{loaded: []string{"testdata/8.10.3.4.pl"}, query: `setof(X-Xs,bagof(Y,d(X,Y),Xs),L).`, expectations: [][]string{
			{`L = [1-[1,2,1], 2-[2,1,2]].`, `Y = _.`},
		}},
		// 8.12.1.4
		{input: "qwerty ...", query: `get_char(Char).`, expectations: [][]string{
			{`Char = q.`, `get_char(w).`},
		}},
		{input: "qwerty ...", query: `get_code(Code).`, expectations: [][]string{
			{`Code = 0'q.`, `get_char(w).`},
		}},
		{input: "qwerty ...", query: `get_char(user_input, Char).`, expectations: [][]string{
			{`Char = q.`, `get_char(user_input, w).`},
		}},
		{input: "qwerty ...", query: `get_code(user_input, Code).`, expectations: [][]string{
			{`Code = 0'q.`, `get_char(user_input, w).`},
		}},
		{input: "'qwerty' ...", query: `get_char(user_input, Char).`, expectations: [][]string{
			{`Char = ''''.`, `get_char(user_input, q).`},
		}},
		{input: "'qwerty' ...", query: `get_code(user_input, Code).`, expectations: [][]string{
			{`Code = 0'''.`, `get_char(user_input, q).`},
		}},
		{input: "qwerty ...", query: `get_char(user_input, q).`, expectations: [][]string{
			{`true.`, `get_char(user_input, w).`},
		}},
		{input: "qwerty ...", query: `get_code(user_input, 0'q).`, expectations: [][]string{
			{`true.`, `get_char(user_input, w).`},
		}},
		{input: "", query: `get_char(user_input, Char).`, expectations: [][]string{
			{`Char = end_of_file.`, `current_input(S), stream_property(S, end_of_stream(past)).`},
		}},
		{input: "", query: `get_code(user_input, Code).`, expectations: [][]string{
			{`Code = -1.`, `current_input(S), stream_property(S, end_of_stream(past)).`},
		}},
		{query: `get_char(user_output, X).`, err: "permission_error(input,stream,user_output)"},
		{query: `get_code(user_output, X).`, err: "permission_error(input,stream,user_output)"},
		// 8.12.2.4
		{input: "qwerty ...", query: `peek_char(Char).`, expectations: [][]string{
			{`Char = q.`, `get_char(q).`},
		}},
		{input: "qwerty ...", query: `peek_code(Code).`, expectations: [][]string{
			{`Code = 0'q.`, `get_char(q).`},
		}},
		{input: "qwerty ...", query: `peek_char(user_input, Char).`, expectations: [][]string{
			{`Char = q.`, `get_char(user_input, q).`},
		}},
		{input: "qwerty ...", query: `peek_code(user_input, Code).`, expectations: [][]string{
			{`Code = 0'q.`, `get_char(user_input, q).`},
		}},
		{input: "'qwerty' ...", query: `peek_char(user_input, Char).`, expectations: [][]string{
			{`Char = ''''.`, `get_char(user_input, '''').`},
		}},
		{input: "'qwerty' ...", query: `peek_code(user_input, Code).`, expectations: [][]string{
			{`Code = 0'''.`, `get_char(user_input, '''').`},
		}},
		{input: "qwerty ...", query: `peek_char(user_input, p).`, expectations: [][]string{}},
		{input: "qwerty ...", query: `peek_code(user_input, 0'p).`, expectations: [][]string{}},
		{input: "", query: `peek_char(user_input, Char).`, expectations: [][]string{
			{`Char = end_of_file.`, `current_input(S), stream_property(S, end_of_stream(past)).`},
		}},
		{input: "", query: `peek_code(user_input, Code).`, expectations: [][]string{
			{`Code = -1.`, `current_input(S), stream_property(S, end_of_stream(past)).`},
		}},
		{input: "", setup: []string{
			`open('testdata/8.12.2.4.txt', read, _, [alias(s), eof_action(error)]).`,
			`get_char(s, end_of_file).`,
		}, teardown: []string{
			`close(s).`,
		}, query: `peek_char(s, Char).`, err: "permission_error(input,past_end_of_stream,s)"},
		{query: `peek_char(user_output, X).`, err: "permission_error(input,stream,user_output)"},
		{query: `peek_code(user_output, X).`, err: "permission_error(input,stream,user_output)"},
		// 8.12.3.4
		{query: `put_char(t).`, expectations: [][]string{
			{`true.`},
		}, output: "t"},
		{query: `put_char(user_output, 'A').`, expectations: [][]string{
			{`true.`},
		}, output: "A"},
		{query: `put_code(0't).`, expectations: [][]string{
			{`true.`},
		}, output: "t"},
		{query: `put_code(user_output, 0't).`, expectations: [][]string{
			{`true.`},
		}, output: "t"},
		{query: `nl, put_char(a).`, expectations: [][]string{
			{`true.`},
		}, output: `
a`},
		{query: `nl(user_output), put_char(user_output, a).`, expectations: [][]string{
			{`true.`},
		}, output: `
a`},
		{query: `put_char(user_output, C).`, err: "instantiation_error"},
		{query: `put_char(user_output, 'ty').`, err: "type_error(character,ty)"},
		{query: `nl(Str).`, err: "instantiation_error"},
		{query: `nl(user_input).`, err: "permission_error(output,stream,user_input)"},
		// 8.13.1.4
		{setup: []string{
			`open('testdata/8.13.1.4.bin', read, _, [alias(s), type(binary)]).`,
			`set_input(s).`,
		}, teardown: []string{
			`close(s).`,
		}, query: `get_byte(Byte).`, expectations: [][]string{
			{`Byte = 113.`, `get_byte(119).`},
		}},
		{setup: []string{
			`open('testdata/8.13.1.4.bin', read, _, [alias(s), type(binary)]).`,
		}, teardown: []string{
			`close(s).`,
		}, query: `get_byte(s, Byte).`, expectations: [][]string{
			{`Byte = 113.`, `get_byte(s, 119).`},
		}},
		{setup: []string{
			`open('testdata/8.13.1.4.bin', read, _, [alias(s), type(binary)]).`,
		}, teardown: []string{
			`close(s).`,
		}, query: `get_byte(s, 114).`, expectations: [][]string{}},
		{setup: []string{
			`open('testdata/empty.bin', read, _, [alias(s), type(binary)]).`,
		}, teardown: []string{
			`close(s).`,
		}, query: `get_byte(s, Byte).`, expectations: [][]string{
			{`Byte = -1.`, `stream_property(S, alias(s)), stream_property(S, end_of_stream(past)).`},
		}},
		{query: `get_byte(user_output, X).`, err: "permission_error(input,stream,user_output)"},
		// 8.13.2.4
		{setup: []string{
			`open('testdata/8.13.1.4.bin', read, _, [alias(s), type(binary)]).`,
			`set_input(s).`,
		}, teardown: []string{
			`close(s).`,
		}, query: `peek_byte(Byte).`, expectations: [][]string{
			{`Byte = 113.`, `get_byte(113).`},
		}},
		{setup: []string{
			`open('testdata/8.13.1.4.bin', read, _, [alias(s), type(binary)]).`,
		}, teardown: []string{
			`close(s).`,
		}, query: `peek_byte(s, Byte).`, expectations: [][]string{
			{`Byte = 113.`, `get_byte(s, 113).`},
		}},
		{setup: []string{
			`open('testdata/8.13.1.4.bin', read, _, [alias(s), type(binary)]).`,
		}, teardown: []string{
			`close(s).`,
		}, query: `peek_byte(s, 114).`, expectations: [][]string{}},
		{setup: []string{
			`open('testdata/empty.bin', read, _, [alias(s), type(binary)]).`,
		}, teardown: []string{
			`close(s).`,
		}, query: `peek_byte(s, Byte).`, expectations: [][]string{
			{`Byte = -1.`},
		}},
		{query: `peek_byte(user_output, X).`, err: "permission_error(input,stream,user_output)"},
		// 8.13.3.4
		{setup: []string{
			`open('test', write, _, [alias(w), type(binary)]).`,
			`set_output(w).`,
			`open('test', read, _, [alias(r), type(binary)]).`,
			`set_input(r).`,
		}, teardown: []string{
			`close(w).`,
			`close(r).`,
		}, query: `put_byte(84).`, expectations: [][]string{
			{`true.`, `peek_byte(84).`},
		}},
		{setup: []string{
			`open('test', write, _, [alias(w), type(binary)]).`,
			`set_output(w).`,
			`open('test', read, _, [alias(r), type(binary)]).`,
			`set_input(r).`,
		}, teardown: []string{
			`close(w).`,
			`close(r).`,
		}, query: `put_byte(w, 84).`, expectations: [][]string{
			{`true.`, `peek_byte(84).`},
		}},
		{query: `put_byte(user_output, C).`, err: "instantiation_error"},
		{query: `put_byte(user_output, 'ty').`, err: "type_error(byte,ty)"},
		// 8.14.1.4
		{input: `term1. term2. ...`, query: `read(T).`, expectations: [][]string{
			{`T = term1.`, `read(term2).`},
		}},
		{input: `term1. term2. ...`, query: `read(user_input, T).`, expectations: [][]string{
			{`T = term1.`, `read(user_input, term2).`},
		}},
		{input: `foo(A+Roger, A+_). term2. ...`, query: `read_term(user_input, T, [variables(VL), variable_names(VN), singletons(VS)]).`, expectations: [][]string{
			{`T = foo(X1+X2, X1+X3).`, `VL = [X1, X2, X3].`, `VN = ['A' = X1, 'Roger' = X2].`, `VS = ['Roger' = X2].`, `read(user_input, term2).`},
		}},
		{input: `3.1. term2. ...`, query: `read(4.1).`, expectations: [][]string{}},
		{input: `foo 123. term2. ...`, query: `read(T).`, err: "syntax_error('term(): next(): unexpected token \"integer(123)\"')"},
		{input: `3.1`, query: `read(T).`, err: "syntax_error('term(): next(): unexpected end of file')"},
		// 8.14.2.4
		{query: `S = user_output, write_term(S, [1,2,3], []).`, expectations: [][]string{
			{`true.`},
		}, output: `[1,2,3]`},
		{query: `write_canonical([1,2,3]).`, expectations: [][]string{
			{`true.`},
		}, output: `'.'(1,'.'(2,'.'(3,[])))`},
		{query: `S = user_output, write_term(S, '1<2', []).`, expectations: [][]string{
			{`true.`},
		}, output: `1<2`},
		{query: `S = user_output, writeq(S, '1<2').`, expectations: [][]string{
			{`true.`},
		}, output: `'1<2'`},
		{query: `writeq('$VAR'(0)).`, expectations: [][]string{
			{`true.`},
		}, output: `A`},
		{query: `S = user_output, write_term(S, '$VAR'(1), [numbervars(false)]).`, expectations: [][]string{
			{`true.`},
		}, output: `$VAR(1)`},
		{query: `S = user_output, write_term(S, '$VAR'(51), [numbervars(true)]).`, expectations: [][]string{
			{`true.`},
		}, output: `Z1`},
		// 8.14.3.4
		{query: `op(30, xfy, ++).`, expectations: [][]string{
			{`current_op(30, xfy, ++).`},
		}},
		{setup: []string{
			`op(30, xfy, ++).`,
		}, query: `op(0, yfx, ++).`, expectations: [][]string{
			{`\+current_op(30, xfy, ++).`},
		}},
		{query: `op(max, xfy, ++).`, err: "type_error(integer,max)"},
		{query: `op(-30, xfy, ++).`, err: "domain_error(operator_priority,-30)"},
		{query: `op(1201, xfy, ++).`, err: "domain_error(operator_priority,1201)"},
		{query: `op(30, XFY, ++).`, err: "instantiation_error"},
		{query: `op(30, yfy, ++).`, err: "domain_error(operator_specifier,yfy)"},
		{query: `op(30, xfy, 0).`, err: "type_error(list,0)"},
		{query: `op(30, xfy, ++), op(40, xfx, ++).`, expectations: [][]string{
			{`current_op(40, xfx, ++).`, `\+current_op(30, xfy, ++).`},
		}},
		{query: `op(30, xfy, ++), op(50, yf, ++).`, err: "permission_error(create,operator,++)"},
		// 8.14.4.4
		{query: `current_op(P, xfy, OP).`, expectations: [][]string{
			{`P = 1105.`, `OP = ('|').`},
			{`P = 1100.`, `OP = (';').`},
			{`P = 1050.`, `OP = ('->').`},
			{`P = 1000.`, `OP = (',').`},
			{`P = 600.`, `OP = (':').`},
			{`P = 200.`, `OP = ('^').`},
		}},
		// 8.14.5.4
		{query: `char_conversion('&', ',').`, input: `a&a.`, expectations: [][]string{
			{`read(X), X = (a,a).`},
		}},
		{query: `char_conversion('’', '\'').`, input: `’a&a’.`, expectations: [][]string{
			{`read(X), X = 'a&a'.`},
		}},
		{setup: []string{
			`op(1000, xfy, '&').`,
		}, query: `char_conversion('ａ', 'a').`, input: `ａ&ａ.`, expectations: [][]string{
			{`read(X), X = (a&a).`},
		}},
		{setup: []string{
			`op(1000, xfy, '&').`,
			`char_conversion('&', ',').`,
		}, query: `char_conversion('&', '&').`, input: `a&a.`, expectations: [][]string{
			{`read(X), X = (a&a).`},
		}},
		// 8.14.6.4
		{setup: []string{
			`char_conversion('ａ', a).`,
			`char_conversion('α', a).`,
		}, query: `current_char_conversion(C, a).`, expectations: [][]string{
			{`C = 'ａ'.`},
			{`C = 'α'.`},
		}},
		// 8.15.1.4
		{query: `'\\+'(true).`, expectations: [][]string{}},
		{query: `\+(!).`, expectations: [][]string{}},
		{query: `'\\+'((!, fail)).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `(X=1; X=2), \+((!, fail)).`, expectations: [][]string{
			{`X = 1.`},
			{`X = 2.`},
		}},
		{query: `'\\+'(4 = 5).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `\+(3).`, err: "type_error(callable,3)"},
		{query: `'\\+'(X).`, err: "instantiation_error"},
		{query: `\+(X = f(X)).`, expectations: [][]string{}},
		// 8.15.2.4
		{query: `once(!).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `once(!), (X=1; X=2).`, expectations: [][]string{
			{`X = 1.`},
			{`X = 2.`},
		}},
		{query: `once(repeat).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `once(fail).`, expectations: [][]string{}},
		{query: `once(X = f(X)).`, expectations: [][]string{
			{`X = f(X).`},
		}},
		// 8.15.3.4
		{query: `repeat, write('hello '), fail.`, err: "system_error", output: `hello hello hello hello hello `},
		{query: `repeat, !, fail.`, expectations: [][]string{}},
		// 8.15.4.4
		{query: `call(integer, 3).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `call(functor(F,c), 0).`, expectations: [][]string{
			{`F = c.`},
		}},
		// TODO: atom_concat/3
		// {query: `call(call(call(atom_concat, pro), log), Atom).`, expectations: [][]string{
		// {`Atom = prolog.`},
		// }},
		{query: `call(;, X = 1, X = 2).`, expectations: [][]string{
			{`X = 1.`},
			{`X = 2.`},
		}},
		{query: `call(;, (true->fail), X=1).`, expectations: [][]string{}},
		{loaded: []string{"testdata/8.15.4.4.pl"}, query: `maplist(>(3), [1, 2]).`, expectations: [][]string{
			{`true.`},
		}},
		{loaded: []string{"testdata/8.15.4.4.pl"}, query: `maplist(>(3), [1, 2, 3]).`, expectations: [][]string{}},
		{loaded: []string{"testdata/8.15.4.4.pl"}, query: `maplist(=(X), Xs).`, expectations: [][]string{
			{`Xs = [].`},
			{`Xs = [X].`},
			{`Xs = [X, X].`},
			{`Xs = [X, X, X].`},
		}, adInfinitum: true},
		// 8.15.5.4
		{query: `false.`, expectations: [][]string{}},
		// 8.16.1.4
		{query: `atom_length('enchanted evening', N).`, expectations: [][]string{
			{`N = 17.`},
		}},
		{query: `atom_length('enchanted\
 evening', N).`, expectations: [][]string{
			{`N = 17.`},
		}},
		{query: `atom_length('', N).`, expectations: [][]string{
			{`N = 0.`},
		}},
		{query: `atom_length('scarlet', 5).`, expectations: [][]string{}},
		{query: `atom_length(Atom, 4).`, err: "instantiation_error"},
		{query: `atom_length(1.23, 4).`, err: "type_error(atom,1.23)"},
		{query: `atom_length(atom, '4').`, err: "type_error(integer,'4')"},
		// 8.16.2.4
		{query: `atom_concat('hello', ' world', S3).`, expectations: [][]string{
			{`S3 = 'hello world'.`},
		}},
		{query: `atom_concat(T, ' world', 'small world').`, expectations: [][]string{
			{`T = 'small'.`},
		}},
		{query: `atom_concat('hello', ' world', 'small world').`, expectations: [][]string{}},
		{query: `atom_concat(T1, T2, 'hello').`, expectations: [][]string{
			{`T1 = ''.`, `T2 = 'hello'.`},
			{`T1 = 'h'.`, `T2 = 'ello'.`},
			{`T1 = 'he'.`, `T2 = 'llo'.`},
			{`T1 = 'hel'.`, `T2 = 'lo'.`},
			{`T1 = 'hell'.`, `T2 = 'o'.`},
			{`T1 = 'hello'.`, `T2 = ''.`},
		}},
		{query: `atom_concat(small, V2, V4).`, err: "instantiation_error"},
		// 8.16.3.4
		{query: `sub_atom(abracadabra, 0, 5, _, S2).`, expectations: [][]string{
			{`S2 = 'abrac'.`},
		}},
		{query: `sub_atom(abracadabra, _, 5, 0, S2).`, expectations: [][]string{
			{`S2 = 'dabra'.`},
		}},
		{query: `sub_atom(abracadabra, 3, L, 3, S2).`, expectations: [][]string{
			{`L = 5.`, `S2 = 'acada'.`},
		}},
		{query: `sub_atom(abracadabra, B, 2, A, ab).`, expectations: [][]string{
			{`B = 0.`, `A = 9.`},
			{`B = 7.`, `A = 2.`},
		}},
		{query: `sub_atom('Banana', 3, 2, _, S2).`, expectations: [][]string{
			{`S2 = 'an'.`},
		}},
		{query: `sub_atom('charity', _, 3, _, S2).`, expectations: [][]string{
			{`S2 = 'cha'.`},
			{`S2 = 'har'.`},
			{`S2 = 'ari'.`},
			{`S2 = 'rit'.`},
			{`S2 = 'ity'.`},
		}},
		{query: `sub_atom('ab', Start, Length, _, Sub_atom).`, expectations: [][]string{
			{`Start = 0.`, `Length = 0.`, `Sub_atom = ''.`},
			{`Start = 0.`, `Length = 1.`, `Sub_atom = 'a'.`},
			{`Start = 0.`, `Length = 2.`, `Sub_atom = 'ab'.`},
			{`Start = 1.`, `Length = 0.`, `Sub_atom = ''.`},
			{`Start = 1.`, `Length = 1.`, `Sub_atom = 'b'.`},
			{`Start = 2.`, `Length = 0.`, `Sub_atom = ''.`},
		}},
		// 8.16.4.4
		{query: `atom_chars('', L).`, expectations: [][]string{
			{`L = [].`},
		}},
		{query: `atom_chars([], L).`, expectations: [][]string{
			{`L = ['[', ']'].`},
		}},
		{query: `atom_chars('''', L).`, expectations: [][]string{
			{`L = [''''].`},
		}},
		{query: `atom_chars('ant', L).`, expectations: [][]string{
			{`L = ['a', 'n', 't'].`},
		}},
		{query: `atom_chars(Str, ['s', 'o', 'p']).`, expectations: [][]string{
			{`Str = 'sop'.`},
		}},
		{query: `atom_chars('North', ['N' | X]).`, expectations: [][]string{
			{`X = ['o', 'r', 't', 'h'].`},
		}},
		{query: `atom_chars('soap', ['s', 'o', 'p']).`, expectations: [][]string{}},
		{query: `atom_chars(X, Y).`, err: "instantiation_error"},
		// 8.16.5.4
		{query: `atom_codes('', L).`, expectations: [][]string{
			{`L = [].`},
		}},
		{query: `atom_codes([], L).`, expectations: [][]string{
			{`L = [0'[, 0']].`},
		}},
		{query: `atom_codes('''', L).`, expectations: [][]string{
			{`L = [0'''].`},
		}},
		{query: `atom_codes('ant', L).`, expectations: [][]string{
			{`L = [0'a, 0'n, 0't].`},
		}},
		{query: `atom_codes(Str, [0's, 0'o, 0'p]).`, expectations: [][]string{
			{`Str = 'sop'.`},
		}},
		{query: `atom_codes('North', [0'N | X]).`, expectations: [][]string{
			{`X = [0'o, 0'r, 0't, 0'h].`},
		}},
		{query: `atom_codes('soap', [0's, 0'o, 0'p]).`, expectations: [][]string{}},
		{query: `atom_codes(X, Y).`, err: "instantiation_error"},
		// 8.16.6.4
		{query: `char_code('a', Code).`, expectations: [][]string{
			{`Code = 0'a.`},
		}},
		{query: `char_code(Str, 99).`, expectations: [][]string{
			{`Str = c.`},
		}},
		{query: `char_code(Str, 0'c).`, expectations: [][]string{
			{`Str = c.`},
		}},
		{query: `char_code(Str, 163).`, expectations: [][]string{
			{`Str = '£'.`},
		}},
		{query: `char_code('b', 84).`, expectations: [][]string{}},
		{query: `char_code('ab', Int).`, err: "type_error(character,ab)"},
		{query: `char_code(C, I).`, err: "instantiation_error"},
		// 8.16.7.4
		{query: `number_chars(33, L).`, expectations: [][]string{
			{`L = ['3', '3'].`},
		}},
		{query: `number_chars(33, ['3', '3']).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `number_chars(33.0, L).`, expectations: [][]string{
			{`L = ['3', '3', '.', '0'].`},
		}},
		{query: `number_chars(X, ['3', '.', '3', 'E', '+', '0']).`, expectations: [][]string{
			{`X = 3.3.`},
		}},
		{query: `number_chars(3.3, ['3', '.', '3', 'E', '+', '0']).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `number_chars(A, [-, '2', '5']).`, expectations: [][]string{
			{`A = -25.`},
		}},
		{query: `number_chars(A, ['\n', ' ', '3']).`, expectations: [][]string{
			{`A = 3.`},
		}},
		{query: `number_chars(A, ['3', ' ']).`, err: "syntax_error"},
		{query: `number_chars(A, ['0', x, f]).`, expectations: [][]string{
			{`A = 15.`},
		}},
		{query: `number_chars(A, ['0', '''', a]).`, expectations: [][]string{
			{`A = 0'a.`},
		}},
		{query: `number_chars(A, ['4', '.', '2']).`, expectations: [][]string{
			{`A = 4.2.`},
		}},
		{query: `number_chars(A, ['4', '2', '.', '0', 'e', '-', '1']).`, expectations: [][]string{
			{`A = 4.2.`},
		}},
		// 8.16.8.4
		{query: `number_codes(33, L).`, expectations: [][]string{
			{`L = [0'3, 0'3].`},
		}},
		{query: `number_codes(33, [0'3, 0'3]).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `number_codes(33.0, L).`, expectations: [][]string{
			{`L = [0'3, 0'3, 0'., 0'0].`},
		}},
		{query: `number_codes(33.0, [0'3, 0'., 0'3, 0'E, 0'+, 0'0, 0'1]).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `number_codes(A, [0'-, 0'2, 0'5]).`, expectations: [][]string{
			{`A = -25.`},
		}},
		{query: `number_codes(A, [0' , 0'3]).`, expectations: [][]string{
			{`A = 3.`},
		}},
		{query: `number_codes(A, [0'0, 0'x, 0'f]).`, expectations: [][]string{
			{`A = 15.`},
		}},
		{query: `number_codes(A, [0'0, 0''', 0'a]).`, expectations: [][]string{
			{`A = 0'a.`},
		}},
		{query: `number_codes(A, [0'4, 0'., 0'2]).`, expectations: [][]string{
			{`A = 4.2.`},
		}},
		{query: `number_codes(A, [0'4, 0'2, 0'., 0'0, 0'e, 0'-, 0'1]).`, expectations: [][]string{
			{`A = 4.2.`},
		}},
		// 8.17.1.4
		{query: `set_prolog_flag(unknown, fail).`, expectations: [][]string{
			{`current_prolog_flag(unknown, fail).`},
		}},
		{query: `set_prolog_flag(X, off).`, err: "instantiation_error"},
		{query: `set_prolog_flag(5, decimals).`, err: "type_error(atom,5)"},
		{query: `set_prolog_flag(date, 'July 1988').`, err: "domain_error(flag,date)"},
		{query: `set_prolog_flag(debug, trace).`, err: "domain_error(flag_value,debug+trace)"},
		// 8.17.2.4
		{setup: []string{
			`set_prolog_flag(debug, off).`,
		}, query: `current_prolog_flag(debug, off).`, expectations: [][]string{
			{`true.`},
		}},
		{query: `current_prolog_flag(F, V).`, expectations: [][]string{
			{`F = bounded.`, `V = true.`},
			{`F = max_integer.`, `V = 9223372036854775807.`},
			{`F = min_integer.`, `V = -9223372036854775808.`},
			{`F = integer_rounding_function.`, `V = toward_zero.`},
			{`F = char_conversion.`, `V = true.`},
			{`F = debug.`, `V = off.`},
			{`F = max_arity.`, `V = 65535.`},
			{`F = unknown.`, `V = error.`},
			{`F = double_quotes.`, `V = chars.`},
		}},
		{query: `current_prolog_flag(5, _).`, err: "type_error(atom,5)"},
		// 8.17.3.4
		{query: `halt.`, err: "system_error"},
		// 8.17.4.4
		{query: `halt(1).`, err: "system_error"},
		{query: `halt(a).`, err: "type_error(integer,a)"},
		// 9.1.7
		{query: `X is '+'(7, 35).`, expectations: [][]string{
			{`X = 42.`},
		}},
		{query: `X is '+'(0, 3+11).`, expectations: [][]string{
			{`X = 14.`},
		}},
		{query: `X is '+'(0, 3.2+11).`, expectations: [][]string{
			{`X = 14.2.`},
		}},
		{query: `X is '+'(77, N).`, err: "instantiation_error"},
		{query: `X is '+'(foo, 77).`, err: "type_error(number,foo)"},
		{query: `X is '-'(7).`, expectations: [][]string{
			{`X = -7.`},
		}},
		{query: `X is '-'(3-11).`, expectations: [][]string{
			{`X = 8.`},
		}},
		{query: `X is '-'(3.2-11).`, expectations: [][]string{
			{`X = 7.8.`},
		}},
		{query: `X is '-'(N).`, err: "instantiation_error"},
		{query: `X is '-'(foo).`, err: "type_error(number,foo)"},
		{query: `X is '-'(7, 35).`, expectations: [][]string{
			{`X = -28.`},
		}},
		{query: `X is '-'(20, 3+11).`, expectations: [][]string{
			{`X = 6.`},
		}},
		{query: `X is '-'(0, 3.2+11).`, expectations: [][]string{
			{`X = -14.2.`},
		}},
		{query: `X is '-'(77, N).`, err: "instantiation_error"},
		{query: `X is '-'(foo, 77).`, err: "type_error(number,foo)"},
		{query: `X is '*'(7, 35).`, expectations: [][]string{
			{`X = 245.`},
		}},
		{query: `X is '*'(0, 3+11).`, expectations: [][]string{
			{`X = 0.`},
		}},
		{query: `X is '*'(1.5, 3.2+11).`, expectations: [][]string{
			{`X = 21.299999999999997.`},
		}},
		{query: `X is '*'(77, N).`, err: "instantiation_error"},
		{query: `X is '*'(foo, 77).`, err: "type_error(number,foo)"},
		{query: `X is '//'(7, 35).`, expectations: [][]string{
			{`X = 0.`},
		}},
		{query: `X is '/'(7.0, 35).`, expectations: [][]string{
			{`X = 0.2.`},
		}},
		{query: `X is '/'(7.0, 35).`, expectations: [][]string{
			{`X = 0.2.`},
		}},
		{query: `X is '//'(140, 3+11).`, expectations: [][]string{
			{`X = 10.`},
		}},
		{query: `X is '/'(20.164, 3.2+11).`, expectations: [][]string{
			{`X = 1.4200000000000002.`},
		}},
		{query: `X is '/'(7, -3).`, expectations: [][]string{
			{`X = -2.3333333333333335.`},
		}},
		{query: `X is '/'(-7, 3).`, expectations: [][]string{
			{`X = -2.3333333333333335.`},
		}},
		{query: `X is '/'(77, N).`, err: "instantiation_error"},
		{query: `X is '/'(foo, 77).`, err: "type_error(number,foo)"},
		{query: `X is '/'(3, 0).`, err: "evaluation_error(zero_divisor)"},
		{query: `X is mod(7, 3).`, expectations: [][]string{
			{`X = 1.`},
		}},
		{query: `X is mod(0, 3+11).`, expectations: [][]string{
			{`X = 0.`},
		}},
		{query: `X is mod(7, -2).`, expectations: [][]string{
			{`X = -1.`},
		}},
		{query: `X is mod(77, N).`, err: "instantiation_error"},
		{query: `X is mod(foo, 77).`, err: "type_error(number,foo)"},
		{query: `X is mod(7.5, 2).`, err: "type_error(integer,7.5)"},
		{query: `X is mod(7, 0).`, err: "evaluation_error(zero_divisor)"},
		{query: `X is floor(7.4).`, expectations: [][]string{
			{`X = 7.`},
		}},
		{query: `X is floor(-0.4).`, expectations: [][]string{
			{`X = -1.`},
		}},
		{query: `X is floor(7.4).`, expectations: [][]string{
			{`X = 7.`},
		}},
		{query: `X is round(7.5).`, expectations: [][]string{
			{`X = 8.`},
		}},
		{query: `X is round(7.6).`, expectations: [][]string{
			{`X = 8.`},
		}},
		{query: `X is round(-0.6).`, expectations: [][]string{
			{`X = -1.`},
		}},
		{query: `X is round(N).`, err: "instantiation_error"},
		{query: `X is ceiling(-0.5).`, expectations: [][]string{
			{`X = 0.`},
		}},
		{query: `X is truncate(-0.5).`, expectations: [][]string{
			{`X = 0.`},
		}},
		{query: `X is truncate(foo).`, err: "type_error(number,foo)"},
		{query: `X is float(7).`, expectations: [][]string{
			{`X = 7.0.`},
		}},
		{query: `X is float(7.3).`, expectations: [][]string{
			{`X = 7.3.`},
		}},
		{query: `X is float(5//3).`, expectations: [][]string{
			{`X = 1.0.`},
		}},
		{query: `X is float(N).`, err: "instantiation_error"},
		{query: `X is float(foo).`, err: "type_error(number,foo)"},
		{query: `X is abs(7).`, expectations: [][]string{
			{`X = 7.`},
		}},
		{query: `X is abs(3-11).`, expectations: [][]string{
			{`X = 8.`},
		}},
		{query: `X is abs(3.2-11.0).`, expectations: [][]string{
			{`X = 7.8.`},
		}},
		{query: `X is abs(N).`, err: "instantiation_error"},
		{query: `X is abs(foo).`, err: "type_error(number,foo)"},
		{query: `current_prolog_flag(max_integer, MI), X is '+'(MI, 1).`, err: "evaluation_error(int_overflow)"},
		{query: `current_prolog_flag(max_integer, MI), X is '-'('+'(MI, 1), 1).`, err: "evaluation_error(int_overflow)"},
		{query: `current_prolog_flag(max_integer, MI), X is '-'(-1, MI).`, expectations: [][]string{
			{`X = -9223372036854775808.`}, // ISO says it's evaluation_error(int_overflow).
		}},
		{query: `current_prolog_flag(max_integer, MI), X is '*'(MI, 2).`, err: "evaluation_error(int_overflow)"},
		{query: `current_prolog_flag(max_integer, MI), R is float(MI) * 2, X is floor(R).`, err: "evaluation_error(int_overflow)"},
		// 9.3.1.4
		{query: `X is '**'(5, 3).`, expectations: [][]string{
			{`X = 125.0.`},
		}},
		{query: `X is '**'(-5.0, 3).`, expectations: [][]string{
			{`X = -125.0.`},
		}},
		{query: `X is '**'(77, N).`, err: "instantiation_error"},
		{query: `X is '**'(foo, 2).`, err: "type_error(number,foo)"},
		{query: `X is '**'(5, 3.0).`, expectations: [][]string{
			{`X = 125.0.`},
		}},
		{query: `X is '**'(0.0, 0).`, expectations: [][]string{
			{`X = 1.0.`},
		}},
		// 9.3.2.4
		{query: `X is sin(0.0).`, expectations: [][]string{
			{`X = 0.0.`},
		}},
		{query: `X is sin(N).`, err: "instantiation_error"},
		{query: `X is sin(0).`, expectations: [][]string{
			{`X = 0.0.`},
		}},
		{query: `X is sin(foo).`, err: "type_error(number,foo)"},
		{query: `PI is atan(1.0) * 4, X is sin(PI / 2.0).`, expectations: [][]string{
			{`X = 1.0.`, `PI = 3.141592653589793.`},
		}},
		// 9.3.3.4
		{query: `X is cos(0.0).`, expectations: [][]string{
			{`X = 1.0.`},
		}},
		{query: `X is cos(N).`, err: "instantiation_error"},
		{query: `X is cos(0).`, expectations: [][]string{
			{`X = 1.0.`},
		}},
		{query: `X is cos(foo).`, err: "type_error(number,foo)"},
		// TODO: Don't know what ISO says here.
		// {query: `PI is atan(1.0) * 4, X is cos(PI / 2.0).`, expectations: [][]string{
		// 	{`X = 0.0.`, `PI = 3.141592653589793.`},
		// }},
		// 9.3.4.4
		{query: `X is atan(0.0).`, expectations: [][]string{
			{`X = 0.0.`},
		}},
		{query: `PI is atan(1.0) * 4.`, expectations: [][]string{
			{`X = 3.141592.`},
		}},
		{query: `X is atan(N).`, err: "instantiation_error"},
		{query: `X is atan(0).`, expectations: [][]string{
			{`X = 0.0.`},
		}},
		{query: `X is atan(foo).`, err: "type_error(number,foo)"},
		// 9.3.5.4
		{query: `X is exp(0.0).`, expectations: [][]string{
			{`X = 1.0.`},
		}},
		{query: `X is exp(1.0).`, expectations: [][]string{
			{`X = 2.718281828459045.`},
		}},
		{query: `X is exp(N).`, err: "instantiation_error"},
		{query: `X is exp(0).`, expectations: [][]string{
			{`X = 1.0.`},
		}},
		{query: `X is exp(foo).`, err: "type_error(number,foo)"},
		// 9.3.6.4
		{query: `X is log(1.0).`, expectations: [][]string{
			{`X = 0.0.`},
		}},
		{query: `X is log(2.7818).`, expectations: [][]string{
			{`X = 1.0230982001908928.`},
		}},
		{query: `X is log(N).`, err: "instantiation_error"},
		{query: `X is log(0).`, err: "evaluation_error(undefined)"},
		{query: `X is log(foo).`, err: "type_error(number,foo)"},
		{query: `X is log(0.0).`, err: "evaluation_error(undefined)"},
		// 9.3.7.4
		{query: `X is sqrt(0.0).`, expectations: [][]string{
			{`X = 0.0.`},
		}},
		{query: `X is sqrt(1).`, expectations: [][]string{
			{`X = 1.0.`},
		}},
		{query: `X is sqrt(1.21).`, expectations: [][]string{
			{`X = 1.1.`},
		}},
		{query: `X is sqrt(N).`, err: "instantiation_error"},
		{query: `X is sqrt(-1.0).`, err: "evaluation_error(undefined)"},
		{query: `X is sqrt(foo).`, err: "type_error(number,foo)"},
		// 9.3.8.4
		{query: `X is max(2, 3).`, expectations: [][]string{
			{`X = 3.`},
		}},
		{query: `X is max(2.0, 3).`, expectations: [][]string{
			{`X = 3.0.`},
		}},
		{query: `X is max(2, 3.0).`, expectations: [][]string{
			{`X = 3.0.`},
		}},
		{query: `X is max(0, 0.0).`, expectations: [][]string{
			{`X = 0.0.`},
		}},
		// 9.3.9.4
		{query: `X is min(2, 3).`, expectations: [][]string{
			{`X = 2.`},
		}},
		{query: `X is min(2.0, 3).`, expectations: [][]string{
			{`X = 2.0.`},
		}},
		{query: `X is min(2, 3.0).`, expectations: [][]string{
			{`X = 2.0.`},
		}},
		{query: `X is min(0, 0.0).`, expectations: [][]string{
			{`X = 0.0.`},
		}},
		// 9.3.10.4
		{query: `X is ^(0, 0).`, expectations: [][]string{
			{`X = 1.`},
		}},
		{query: `X is 3^1.0.`, expectations: [][]string{
			{`X = 3.0.`},
		}},
		{query: `X is 3^3.`, expectations: [][]string{
			{`X = 27.`},
		}},
		{query: `X is 3^27.`, expectations: [][]string{
			{`X = 7625597484987.`},
		}},
		{query: `X is 3^3^3.`, expectations: [][]string{
			{`X = 7625597484987.`},
		}},
		{query: `X is 2^(-1).`, err: "type_error(float,2)"},
		{query: `X is 2.0^(-1).`, expectations: [][]string{
			{`X = 0.5.`},
		}},
		{query: `X is 1^(-1).`, expectations: [][]string{
			{`X = 1.`},
		}},
		{query: `X is 0^0.`, expectations: [][]string{
			{`X = 1.`},
		}},
		{query: `X is 2^ -1.5.`, expectations: [][]string{
			{`X = 0.3535533905932738.`},
		}},
		// 9.3.11.4
		{query: `X is asin(0.5).`, expectations: [][]string{
			{`X = 0.5235987755982989.`},
		}},
		{query: `X is 2*asin(1).`, expectations: [][]string{
			{`X = 3.141592653589793.`},
		}},
		{query: `X is asin(2).`, err: "evaluation_error(undefined)"},
		// 9.3.12.4
		{query: `X is acos(-1).`, expectations: [][]string{
			{`X = 3.141592653589793.`},
		}},
		{query: `X is acos(0.5).`, expectations: [][]string{
			{`X = 1.0471975511965976.`},
		}},
		{query: `X is acos(1.5).`, err: "evaluation_error(undefined)"},
		// 9.3.13.4
		{query: `X is atan2(1,0).`, expectations: [][]string{
			{`X = 1.5707963267948966.`},
		}},
		{query: `X is atan2(0,-1).`, expectations: [][]string{
			{`X = 3.141592653589793.`},
		}},
		{query: `X is atan2(0,0).`, err: "evaluation_error(undefined)"},
		// 9.3.14.4
		{query: `X is tan(0.5).`, expectations: [][]string{
			{`X = 0.5463024898437905.`},
		}},
		// 0.3.15.4
		{query: `X is pi.`, expectations: [][]string{
			{`X = 3.141592653589793.`},
		}},
		// TODO:
		/*
			Other test cases.
		*/
		{
			query:        `true, true.`,
			expectations: [][]string{{`true.`}},
		},
		{
			query:        `true, fail.`,
			expectations: [][]string{},
		},
		{
			query:        `!.`,
			expectations: [][]string{{`true.`}},
		},
		{
			loaded: []string{"testdata/p.pl"},
			query:  "p(a).",
			expectations: [][]string{
				{`true.`},
			},
		},
		{
			loaded: []string{"testdata/p.pl"},
			query:  "p(X).",
			expectations: [][]string{
				{`X = a.`},
				{`X = b.`},
				{`X = c.`},
			},
		},
		{
			loaded: []string{"testdata/cut.pl"},
			query:  "only_cut(a).",
			expectations: [][]string{
				{`true.`},
			},
		},
		{
			loaded: []string{"testdata/cut.pl"},
			query:  "neck_cut(X).",
			expectations: [][]string{
				{`X = a.`},
				{`X = b.`},
			},
		},
		{
			loaded: []string{"testdata/cut.pl"},
			query:  "deep_cut(X).",
			expectations: [][]string{
				{`X = a.`},
			},
		},
	}

	for _, test := range tests {
		t.Run(test.query, func(t *testing.T) {
			dir, err := os.MkdirTemp("", "prolog-test-*")
			if err != nil {
				t.Fatal(err)
			}
			defer func(path string) {
				if err := os.RemoveAll(path); err != nil {
					t.Fatal(err)
				}
			}(dir)

			root, err := os.OpenRoot(dir)
			if err != nil {
				t.Fatal(err)
			}

			i := New(HeapSize(6*1024), Root(root))
			i.MountFS("testdata", must(fs.Sub(testdata, "testdata")))
			if err := i.SetUserInput(strings.NewReader(test.input)); err != nil {
				t.Fatal(err)
			}

			var buf bytes.Buffer
			if err := i.SetUserOutput(&cappedWriter{Writer: &buf, limit: 30}); err != nil {
				t.Fatal(err)
			}

			if err := i.engine.LoadSystem(t.Context()); err != nil {
				t.Fatal(err)
			}

			for _, l := range test.loaded {
				if err := i.Load(t.Context(), l); err != nil {
					t.Fatal(err)
				}
			}

			for _, p := range test.setup {
				for _, err := range Query[Result](t.Context(), i, p) {
					if err != nil {
						t.Fatal(err)
					}
				}
			}
			defer func() {
				for _, p := range test.teardown {
					for _, err := range Query[Result](t.Context(), i, p) {
						if err != nil {
							t.Fatal(err)
						}
					}
				}
			}()

			var (
				j   int
				vns []VariableName
			)
			for _, err := range Query[Result](t.Context(), i, test.query, VariableNames(&vns)) {
				if err != nil {
					if test.err == "" {
						t.Fatal(err)
					}
					if !strings.Contains(err.Error(), test.err) {
						t.Errorf("Expected error %q, got %q", test.err, err)
					}
					continue
				}
				if test.err != "" {
					t.Errorf("Expected error %q, got nothing", test.err)
				}
				if test.expectations != nil {
					if j >= len(test.expectations) {
						if test.adInfinitum {
							break
						}
						t.Errorf("Unexpected solution #%d, expected only %d", j+1, len(test.expectations))
						j++
						continue
					}
					for _, expectation := range test.expectations[j] {
						var (
							ok  bool
							vns = slices.Clone(vns)
						)
						for _, err := range Query[Result](t.Context(), i, expectation, VariableNames(&vns)) {
							if err != nil {
								t.Fatal(err)
							}
							ok = true
							break
						}
						if !ok {
							t.Errorf("expectation isn't met: %s", expectation)
						}
					}
				}
				j++
			}
			if test.expectations != nil && j != len(test.expectations) {
				t.Errorf("Expected %d solutions, got %d", len(test.expectations), j)
			}
			if !test.ignoreOutput {
				if output := buf.String(); output != test.output {
					t.Errorf("Expected output %q, got %q", test.output, output)
				}
			}
		})
	}
}

func must[T any](v T, err error) T {
	if err != nil {
		panic(err)
	}
	return v
}

type cappedWriter struct {
	io.Writer
	limit int
}

func (w *cappedWriter) Write(p []byte) (int, error) {
	var (
		i        = w.limit - len(p)
		limitErr error
	)
	if i > 0 {
		w.limit = i
	} else {
		p = p[:w.limit]
		w.limit = 0
		limitErr = io.ErrShortWrite
	}
	n, err := w.Writer.Write(p)
	if err != nil {
		return n, err
	}
	return n, limitErr
}
