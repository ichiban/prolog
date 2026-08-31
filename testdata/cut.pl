only_cut(X) :- !.
neck_cut(X) :- !, p(X).
inline_cut(X) :- var(X), !, p(X).
deep_cut(X) :- p(X), !, q(X).
p(a).
p(b).
q(a).
q(b).
