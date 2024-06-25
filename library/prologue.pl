:- module(prologue, [
  member/2,
  append/3,
  length/2,
  select/3,
  succ/2,
  maplist/2,
  maplist/3,
  maplist/4,
  maplist/5,
  maplist/6,
  maplist/7,
  maplist/8,
  nth0/3,
  nth1/3,
  call_nth/2
]).

member(X, [X|_]).
member(X, [_|Xs]) :-
  member(X, Xs).

append([], Zs, Zs).
append([X|Xs], Ys, [X|Zs]) :-
  append(Xs, Ys, Zs).

length(List, Length) :-
  native:length(List, Length).

between(Lower, Upper, X) :-
  native:between(Lower, Upper, X).

select(E, [E|Xs], Xs).
select(E, [X|Xs], [X|Ys]) :-
  select(E, Xs, Ys).

succ(X, S) :-
  native:succ(X, S).

:- meta_predicate(maplist(1, ?)).
maplist(_Cont_1, []).
maplist(Cont_1, [E1|E1s]) :-
  call(Cont_1, E1),
  maplist(Cont_1, E1s).

:- meta_predicate(maplist(2, ?, ?)).
maplist(_Cont_2, [], []).
maplist(Cont_2, [E1|E1s], [E2|E2s]) :-
  call(Cont_2, E1, E2),
  maplist(Cont_2, E1s, E2s).

:- meta_predicate(maplist(3, ?, ?, ?)).
maplist(_Cont_3, [], [], []).
maplist(Cont_3, [E1|E1s], [E2|E2s], [E3|E3s]) :-
  call(Cont_3, E1, E2, E3),
  maplist(Cont_3, E1s, E2s, E3s).

:- meta_predicate(maplist(4, ?, ?, ?, ?)).
maplist(_Cont_4, [], [], [], []).
maplist(Cont_4, [E1|E1s], [E2|E2s], [E3|E3s], [E4|E4s]) :-
  call(Cont_4, E1, E2, E3, E4),
  maplist(Cont_4, E1s, E2s, E3s, E4s).

:- meta_predicate(maplist(5, ?, ?, ?, ?, ?)).
maplist(_Cont_5, [], [], [], [], []).
maplist(Cont_5, [E1|E1s], [E2|E2s], [E3|E3s], [E4|E4s], [E5|E5s]) :-
  call(Cont_5, E1, E2, E3, E4, E5),
  maplist(Cont_5, E1s, E2s, E3s, E4s, E5s).

:- meta_predicate(maplist(6, ?, ?, ?, ?, ?, ?)).
maplist(_Cont_6, [], [], [], [], [], []).
maplist(Cont_6, [E1|E1s], [E2|E2s], [E3|E3s], [E4|E4s], [E5|E5s], [E6|E6s]) :-
  call(Cont_6, E1, E2, E3, E4, E5, E6),
  maplist(Cont_6, E1s, E2s, E3s, E4s, E5s, E6s).

:- meta_predicate(maplist(7, ?, ?, ?, ?, ?, ?, ?)).
maplist(_Cont_7, [], [], [], [], [], [], []).
maplist(Cont_7, [E1|E1s], [E2|E2s], [E3|E3s], [E4|E4s], [E5|E5s], [E6|E6s], [E7|E7s]) :-
  call(Cont_7, E1, E2, E3, E4, E5, E6, E7),
  maplist(Cont_7, E1s, E2s, E3s, E4s, E5s, E6s, E7s).

nth0(N, List, Elem) :-
  native:nth0(N, List, Elem).

nth1(N, List, Elem) :-
  native:nth1(N, List, Elem).

call_nth(Goal, Nth) :-
  native:call_nth(Goal, Nth).
