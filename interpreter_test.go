package prolog

import (
	"embed"
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
		query        string
		expectations [][]string
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
		{loaded: []string{"testdata/7.8.3.4.pl"}, query: `b(X).`, err: `instantiation_error`},
		{loaded: []string{"testdata/7.8.3.4.pl"}, query: `b(3).`, err: `type_error(callable,3)`}, // type_error(callable,(write(3),3))
		{loaded: []string{"testdata/7.8.3.4.pl"}, query: `Z = !, call((Z = !, a(X), Z)).`, expectations: [][]string{
			{`X = 1.`, `Z = !.`},
		}},
		{loaded: []string{"testdata/7.8.3.4.pl"}, query: `call((Z=!, a(X), Z)).`, expectations: [][]string{
			{`X = 1.`, `Z = !.`},
		}},
		{loaded: []string{"testdata/7.8.3.4.pl"}, query: `call((write(3), X)).`, err: `instantiation_error`},
		{query: `call(X).`, err: `instantiation_error`},
		{query: `call(1).`, err: `type_error(callable,1)`},
		{query: `call((fail, 1)).`, err: `type_error(callable,(fail,1))`},
		{query: `call((write(3), 1)).`, err: `type_error(callable,1)`}, // type_error(callable,(write(3),1))
		{query: `call((1;true)).`, err: `type_error(callable,1)`},      // type_error(callable,(1;true))
		// 7.8.4.4
		{query: `!.`, expectations: [][]string{
			{`true.`},
		}},
		{query: `(!, fail; true).`, expectations: [][]string{}},
		{query: `(call(!), fail; true).`, expectations: [][]string{
			{`true.`},
		}},
		{loaded: []string{"testdata/7.8.4.4.pl"}, query: `twice(_), !, write('Forwards '), fail.`, expectations: [][]string{}},
		{query: `(!; write('No ')), write('Cut disjunction '), fail.`, expectations: [][]string{}},
		{loaded: []string{"testdata/7.8.4.4.pl"}, query: `twice(_), (write('No '); !), write('Cut '), fail.`, expectations: [][]string{}},
		{loaded: []string{"testdata/7.8.4.4.pl"}, query: `twice(_), (!, fail; write('No ')).`, expectations: [][]string{}},
		{loaded: []string{"testdata/7.8.4.4.pl"}, query: `twice(X), call(X), write('Forwards '), fail.`, expectations: [][]string{}},
		{loaded: []string{"testdata/7.8.4.4.pl"}, query: `goal(X), call(X), write('Forwards '), fail.`, expectations: [][]string{}},
		{loaded: []string{"testdata/7.8.4.4.pl"}, query: `twice(_), \+(\+(!)), write('Forwards '), fail.`, expectations: [][]string{}},
		{loaded: []string{"testdata/7.8.4.4.pl"}, query: `twice(_), once(!), write('Forwards '), fail.`, expectations: [][]string{}},
		{loaded: []string{"testdata/7.8.4.4.pl"}, query: `twice(_), call(!), write('Forwards '), fail.`, expectations: [][]string{}},
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
		}},
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
		// TODO: sort/2, keysort/2
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
		{loaded: []string{"testdata/8.9.3.4.pl"}, query: `retract(insect(I)), write(I), retract(insect(bee)), fail.`, expectations: [][]string{}},
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
			{`X = 1.`, `Y = 2`},
		}},
		{query: `findall(X, Goal, S).`, err: "instantiation_error"},
		{query: `findall(X, 4, S).`, err: "type_error(callable,4)"},
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
			i := New(4 * 1024)
			i.SetSourceFS(testdata)

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

			var (
				results []Result
				j       int
				vns     []VariableName
			)
			for result, err := range Query[Result](t.Context(), i, test.query, VariableNames(&vns)) {
				if err != nil {
					if test.err == "" {
						t.Fatal(err)
					}
					if !strings.Contains(err.Error(), test.err) {
						t.Errorf("Expected error %q, got %q", test.err, err)
					}
					continue
				}
				results = append(results, result)
				if test.expectations != nil {
					if j >= len(test.expectations) {
						t.Errorf("Unexpected solution #%d, expected only %d", j+1, len(test.expectations))
						j++
						continue
					}
					// Expectation queries allocate on the interpreter's arena.
					// Restore the heap and the variable names afterwards so
					// that the suspended query resumes on the exact state it
					// yielded with.
					heap := slices.Clone(i.engine.Heap)
					nvns := len(vns)
					for _, expectation := range test.expectations[j] {
						var ok bool
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
					i.engine.Heap = heap
					vns = vns[:nvns]
				}
				j++
			}

		})
	}
}
