flatten(leaf(X), [X]).
flatten(node(X, Lt, Rt), Ans) :- flatten(Lt, L), flatten(Rt, R),
			       append(L, [X], Lf), append(Lf, R, Ans).
