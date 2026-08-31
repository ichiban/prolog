maplist(_Cont, []).
maplist(Cont, [E|Es]) :-
  call(Cont, E),
  maplist(Cont, Es).
