foo(X) :-
  Y is X * 2, throw(test(Y)).

bar(X) :-
  X = Y, throw(Y).

coo(X) :-
  throw(X).

car(X) :-
  X = 1, throw(X).

g :-
  catch(p, B, write(h2)),
  coo(c).

p.
p :-
  throw(b).
