% A choice point must restore the cut barrier that was in effect when it was
% created. p/1's second clause is only reached by retrying, and the arithmetic
% in it pushes a cut barrier; if the retry restores that barrier as 0, the cut
% truncates the whole choice point stack, taking the caller's alternatives with
% it.
p(0).
p(N) :- N > 0, M is N - 1, p(M).
