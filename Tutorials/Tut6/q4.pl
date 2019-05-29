split(L, 0, [], L) :- !.
split([X|L], N, [X|L1], L2) :- N1 is N-1, split(L, N1, L1, L2).