myappend([], [], []) :- !.
myappend([], X, X).
myappend([X|Y], Z, [X|L]) :- myappend(Y, Z, L).

a(1).
b(1).
c(1).
b(2).
c(2).
d(2).
e(2).
f(3).
p(X) :- a(X).
p(X) :- b(X), c(X), !, d(X), e(X).
p(X) :- f(X).
