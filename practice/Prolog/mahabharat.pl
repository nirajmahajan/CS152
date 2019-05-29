son(bhim, pandu).
son(yudhishthir, pandu).
son(duryodhan, dhritarashtra).
brother(pandu, dhritarashtra).

brother(X,Y) :- brother(Y,X), !.
brother(X,Y) :- son(X,Z), son(Y,Z), \==(X, Y).

uncle(X,Y) :- son(Y,F), brother(F,X).
