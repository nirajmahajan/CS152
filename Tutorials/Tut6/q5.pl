my_mem(X, Y) :- member(Y, X).
unique([]).
unique([X|Xs]) :- not(member(X, Xs)), unique(Xs).

occupancy(B) :- B = [[sanket, S], [ankush, An], [ashwin, As], [umang, U], [krishna, K]],
				Floors = [1, 2, 3, 4, 5], Residents = [S, An, As, U, K],
				maplist(my_mem(Floors), Residents),
				unique(Residents),
				As \= 5, An \= 1, U \= 1, U \= 5,
				abs(U-K) > 1, abs(U-An) > 1,
				S > An, !.
