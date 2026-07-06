
'$cut_to'('$cut').

! :- !.

A, B :- A, B.

A; B :- A; B.
'$or'(A, _) :- A.
'$or'(_, B) :- B.

If -> Then; _ :- If, !, Then.
If -> _; Else :- Else.
If -> Then :- If, !, Then.

X = Y :- X = Y.

% dummy
write(Term).


\+(G) :- call(G), !, fail.
\+(_).

once(G) :- call(G), !.
