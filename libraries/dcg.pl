:- module(dcg, [
  phrase/2,
  phrase/3
]).

phrase(GRBody, S0, S) :-
  native:phrase(GRBody, S0, S).

phrase(GRBody, S0) :-
  phrase(GRBody, S0, []).
