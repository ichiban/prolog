append(nil, L, L).
append(cons(X, L1), L2, cons(X, L3)) :- append(L1, L2, L3).
