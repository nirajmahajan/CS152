%makes a list containing 1, 2, ... N
makelist(1, [1]) :- !.
makelist(N, L) :- N1 is N-1, makelist(N1, L1), append(L1, [N], L).

% zips two lists
zip([], _, []) :- !.
zip(_, [], []) :- !.
zip([X | Xs], [M | Ms], L) :- zip(Xs, Ms, L1), append([(X, M)], L1, L).

% replicates N times X
replicate(0, _, []) :- !.
replicate(N, X, L) :- N1 is N-1, replicate(N1, X, L1), L = [X | L1].

% returns whether a position is safe or not
safepos((X, Y), (X1, Y1)):- not(=(X1, X)), not(=(Y1, Y)),
			     			Z is abs(X-X1), Z1 is abs(Y-Y1),
			     			not(=(Z, Z1)).

queens(1, [X]) :- !, member(X, [1, 2, 3, 4, 5, 6, 7, 8]).
queens(N, L) :- N1 is N-1,
				queens(N1, L1),
				makelist(N1, LN1),
				zip(LN1, L1, FL1),
				replicate(8, N, L8N), makelist(8, L8),
				zip(L8N, L8, Poses),
				member((Row, Col), Poses),
				maplist(safepos((Row, Col)), FL1),
				append(L1, [Col], L).

