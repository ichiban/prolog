:- initialization(hello(world)). % After loading this file, `hello(world).` will be executed.
:- initialization(halt(0)).      % Then, `halt(0).` will be executed.

hello(Name) :-
  write('hello, '),
  write(Name),
  write('!'),
  nl.
