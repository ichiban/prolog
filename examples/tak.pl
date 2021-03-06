tak(X, Y, Z, Z) :- X =< Y, !.
tak(X, Y, Z, A) :- X1 is X - 1, tak(X1, Y, Z, A1),
                   Y1 is Y - 1, tak(Y1, Z, X, A2),
                   Z1 is Z - 1, tak(Z1, X, Y, A3),
                   tak(A1, A2, A3, A).
