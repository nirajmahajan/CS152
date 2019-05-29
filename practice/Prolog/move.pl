validmove([e,X,Y,Z,W], [X,e,Y,Z,W]).
validmove([e,X,Y,Z,W], [Y,X,e,Z,W]).

validmove([X,e,Y,Z,W], [e,X,Y,Z,W]).
validmove([X,e,Y,Z,W], [X,Y,e,Z,W]).
validmove([X,e,Y,Z,W], [X,Z,Y,e,W]).

validmove([X,Y,e,Z,W], [X,e,Y,Z,W]).
validmove([X,Y,e,Z,W], [X,Y,Z,e,W]).
validmove([X,Y,e,Z,W], [e,Y,X,Z,W]).
validmove([X,Y,e,Z,W], [X,Y,W,Z,e]).

validmove([X,Y,Z,e,W], [X,Y,e,Z,W]).
validmove([X,Y,Z,e,W], [X,Y,Z,W,e]).
validmove([X,Y,Z,e,W], [X,e,Z,Y,W]).

validmove([X,Y,Z,W,e], [X,Y,Z,e,W]).
validmove([X,Y,Z,W,e], [X,Y,e,W,Z]).

soln([p, p, e, d, d], S, S).

soln([X, Y, e, Z, W], S, T) :- validmove([X, Y, e, Z, W], M),
								not(member(M, S)),
								S1 = [M | S],
								soln(M, S1, T).

soln([X, e, Y, Z, W], S, T) :- validmove([X, e, Y, Z, W], M),
								not(member(M, S)),
								S1 = [M | S],
								soln(M, S1, T).

soln([X, Y, Z, e, W], S, T) :- validmove([X, Y, Z, e, W], M),
								not(member(M, S)),
								S1 = [M | S],
								soln(M, S1, T).

soln([e, X, Y, Z, W], S, T) :- validmove([e, X, Y, Z, W], M),
								not(member(M, S)),
								S1 = [M | S],
								soln(M, S1, T).

soln([X, Y, Z, W, e], S, T) :- validmove([X, Y, Z, W, e], M),
								not(member(M, S)),
								S1 = [M | S],
								soln(M, S1, T).