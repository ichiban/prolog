:- dynamic(legs/2).
legs(A, 6) :- insect(A).

:- dynamic(insect/1).
insect(ant).
insect(bee).
