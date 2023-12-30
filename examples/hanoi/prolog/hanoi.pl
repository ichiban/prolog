hanoi(N) :- move(N, left, right, center).

move(0, _, _, _) :- !.
move(N, X, Y, Z) :-
  M is N - 1,
  move(M, X, Z, Y),
  actuate(X, Y),
  move(M, Z, Y, X).
