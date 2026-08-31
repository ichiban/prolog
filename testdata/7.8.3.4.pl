b(X) :-
  Y = (write(X), X),
  call(Y).

a(1).
a(2).
