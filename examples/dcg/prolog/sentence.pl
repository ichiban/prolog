:- use_module(library(prologue), [append/3]).
:- use_module(library(dcg)).
:- set_prolog_flag(double_quotes, atom).

sentence --> noun_phrase, verb_phrase.
verb_phrase --> verb.
noun_phrase --> article, noun.
noun_phrase --> article, adjective, noun.
article --> [the].
adjective --> [nice].
noun --> [dog].
noun --> [cat].
verb --> [runs].
verb --> [barks].
verb --> [bites].
