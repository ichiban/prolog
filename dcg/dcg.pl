% https://www.complang.tuwien.ac.at/ulrich/iso-prolog/dcgs/dcgsdin140408.pdf

:- op(1105,xfy,'|').

phrase(GRBody, S0) :- phrase(GRBody, S0, []).
phrase(GRBody, S0, S) :-
    dcg_body(GRBody, S0, S, Goal),
    call(Goal).

% Expands a DCG rule into a Prolog rule, when no error condition applies.

dcg_rule(( NonTerminal, Terminals --> GRBody ), ( Head :- Body )) :-
    dcg_non_terminal(NonTerminal, S0, S, Head),
    dcg_body(GRBody, S0, S1, Goal1),
    dcg_terminals(Terminals, S, S1, Goal2),
    Body = ( Goal1, Goal2 ).
dcg_rule(( NonTerminal --> GRBody ), ( Head :- Body )) :-
    NonTerminal \= ( _, _ ),
    dcg_non_terminal(NonTerminal, S0, S, Head),
    dcg_body(GRBody, S0, S, Body).

dcg_non_terminal(NonTerminal, S0, S, Goal) :-
    NonTerminal =.. NonTerminalUniv,
    append(NonTerminalUniv, [S0, S], GoalUniv),
    Goal =.. GoalUniv.

dcg_terminals(Terminals, S0, S, S0 = List) :-
    append(Terminals, S, List).

dcg_body(Var, S0, S, Body) :-
    var(Var),
    Body = phrase(Var, S0, S).
dcg_body(GRBody, S0, S, Body) :-
    nonvar(GRBody),
    dcg_constr(GRBody),
    dcg_cbody(GRBody, S0, S, Body).
dcg_body(NonTerminal, S0, S, Goal) :-
    nonvar(NonTerminal),
    \+ dcg_constr(NonTerminal),
    NonTerminal \= ( _ -> _ ),
    dcg_non_terminal(NonTerminal, S0, S, Goal).

% The following constructs in a grammar rule body
% are defined in the corresponding subclauses.

dcg_constr([]).
dcg_constr([_|_]).
dcg_constr(( _, _ )).
dcg_constr(( _ ; _ )).
dcg_constr(( _ '|' _ )).
dcg_constr({_}).
dcg_constr(call(_)).
dcg_constr(phrase(_)).
dcg_constr(!).
dcg_constr(\+ _).

% The principal functor of the first argument indicates
% the construct to be expanded.

dcg_cbody([], S0, S, S0 = S ).
dcg_cbody([T|Ts], S0, S, Goal) :-
    dcg_terminals([T|Ts], S0, S, Goal).
dcg_cbody(( GRFirst, GRSecond ), S0, S, ( First, Second )) :-
    dcg_body(GRFirst, S0, S1, First),
    dcg_body(GRSecond, S1, S, Second).
dcg_cbody(( GREither ; GROr ), S0, S, ( Either ; Or )) :-
    \+ subsumes_term(( _ -> _ ),GREither),
    dcg_body(GREither, S0, S, Either),
    dcg_body(GROr, S0, S, Or).
dcg_cbody(( GREither ; GROr ), S0, S, ( Either ; Or )) :-
    subsumes_term(( _ -> _ ),GREither),
    dcg_cbody(GREither, S0, S, Either),
    dcg_body(GROr, S0, S, Or).
dcg_cbody(( GREither '|' GROr ), S0, S, ( Either ; Or )) :-
    dcg_body(GREither, S0, S, Either),
    dcg_body(GROr, S0, S, Or).
dcg_cbody({Goal}, S0, S, ( Goal, S0 = S )).
dcg_cbody(call(Cont), S0, S, call(Cont, S0, S)).
dcg_cbody(phrase(Body), S0, S, phrase(Body, S0, S)).
dcg_cbody(!, S0, S, ( !, S0 = S )).
dcg_cbody(\+ GRBody, S0, S, ( \+ Goal, S0 = S )) :-
    dcg_body(GRBody, S0, _, Goal).
dcg_cbody(( GRIf -> GRThen ), S0, S, ( If -> Then )) :-
    dcg_body(GRIf, S0, S1, If),
    dcg_body(GRThen, S1, S, Then).

% register dcg_rule/2 as term expansion.

term_expansion(X, Y) :- dcg_rule(X, Y).
