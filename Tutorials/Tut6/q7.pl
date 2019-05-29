inBoard([X1, Y1]) :- X1 > 0, X1 < 8, Y1 > 0, Y1 < 8.

validmove([X, Y], [X1, Y1]) :- X1 is X+1, Y1 is Y+2, inBoard([X1, Y1]).
validmove([X, Y], [X1, Y1]) :- X1 is X-1, Y1 is Y+2, inBoard([X1, Y1]).
validmove([X, Y], [X1, Y1]) :- X1 is X+1, Y1 is Y-2, inBoard([X1, Y1]).
validmove([X, Y], [X1, Y1]) :- X1 is X-1, Y1 is Y-2, inBoard([X1, Y1]).
validmove([X, Y], [X1, Y1]) :- X1 is X+2, Y1 is Y+1, inBoard([X1, Y1]).
validmove([X, Y], [X1, Y1]) :- X1 is X-2, Y1 is Y+1, inBoard([X1, Y1]).
validmove([X, Y], [X1, Y1]) :- X1 is X+2, Y1 is Y-1, inBoard([X1, Y1]).
validmove([X, Y], [X1, Y1]) :- X1 is X-2, Y1 is Y-1, inBoard([X1, Y1]).

ride(T, 49, T) :- !.
ride([X|Xs], N, T) :-   validmove(X, New),
						S = [X|Xs],
						not(member(New, S)),
						S1 = [New|S],
						N1 is N+1,
						ride(S1, N1, T).

tour(Res) :- ride([[1,1]], 1, Res1), reverse(Res1, Res), write(Res). 

reverse([], []).
reverse([X|Xs], S) :- reverse(Xs, S1), append(S1, [X], S).