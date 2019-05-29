myLength([], 0).
myLength([_ | Xs], N) :- myLength(Xs, N1), N is N1+1.