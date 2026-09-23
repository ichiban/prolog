:- module(module_lists, [app/3, all_secrets/1, guarded/1, stash/1]).

app([], L, L).
app([X|Xs], Ys, [X|Zs]) :- app(Xs, Ys, Zs).

secret(42).

all_secrets(L) :- findall(X, secret(X), L).

guarded(X) :- \+(secret(X)).

stash(X) :- assertz(stashed(X)).
