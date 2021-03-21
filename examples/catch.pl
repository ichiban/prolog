% https://www.deransart.fr/prolog/bips.html#catch

p :- true.
p :- throw(b).

q :- catch(p, B, write('hellop')), r(c).

r(X) :- throw(X).