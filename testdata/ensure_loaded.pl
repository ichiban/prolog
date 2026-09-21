:- ensure_loaded('testdata/p.pl').
:- ensure_loaded('testdata/p.pl'). % No clauses will be added since the file is already loaded.

q(L) :- bagof(X, p(X), L).
