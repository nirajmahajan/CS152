myLast([X|[]], X).
myLast([_|Xs], T) :- myLast(Xs, T).