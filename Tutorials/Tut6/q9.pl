% subpart A
max(X,Y,Z) :- X >= Y,!, Z =:= X.
max(X,Y,Y).

% subpart B
myarrange([], R, W, B, L) :- append(R, W, L1), append(L1, B, L), !.
myarrange([r | Xs], R, W, B, L) :- R1 = [r | R], myarrange(Xs, R1, W, B, L).
myarrange([w | Xs], R, W, B, L) :- W1 = [w | W], myarrange(Xs, R, W1, B, L).
myarrange([b | Xs], R, W, B, L) :- B1 = [b | B], myarrange(Xs, R, W, B1, L). 
arrange(L, M) :- myarrange(L, [], [], [], M).

% subpart C
moves(1, A, B, _, [to(A, B)]) :- !.
moves(N, A, B, C, L) :- N1 is N-1,
						moves(N1, A, C, B, L1), moves(N1, C, B, A, L2),
						append(L1, [to(A, B)], L3),
						append(L3, L2, L).

% subpart D
% move(Wolf, Goat, Cabbage)
move(0, 0, 0).
move(1, 0, 0).
move(0, 1, 0).
move(0, 0, 1).

ismoved([0, 0, 0], single(farmer)).
ismoved([1, 0, 0], pair(wolf, farmer)).
ismoved([0, 1, 0], pair(goat, farmer)).
ismoved([0, 0, 1], pair(cabbage, farmer)).

safe([0, 0, 0]).
safe([1, 0, 0]).
safe([0, 1, 0]).
safe([0, 0, 1]).
safe([1, 0, 1]).

transfer([0, 0, 0], right, _, S, S) :- !.
transfer([W, G, C], left, A, S, T)  :-  move(Wt, Gt, Ct),
										W >= Wt, G >= Gt, C >= Ct,
										Wl is W-Wt, Gl is G-Gt, Cl is C-Ct,
										safe([Wl, Gl, Cl]),
										A1 = [Wl, Gl, Cl, left],
										not(member(A1, A)),
										Anew = [A1 | A],
										ismoved([Wt, Gt, Ct], NewEntry),
										S1 = [NewEntry | S],
										transfer([Wl, Gl, Cl], right, Anew, S1, T).

transfer([W, G, C], right, A, S, T)  :- move(Wt, Gt, Ct),
										WRt is 1-W, GRt is 1-G, CRt is 1-C, 
										WRt >= Wt, GRt >= Gt, CRt >= Ct,
										Wr is WRt-Wt, Gr is GRt-Gt, Cr is CRt-Ct,
										Wl is W+Wt, Gl is G+Gt, Cl is C+Ct, 
										A1 = [Wl, Gl, Cl, right],
										not(member(A1, A)),
										Anew = [A1 | A],
										safe([Wr, Gr, Cr]),
										ismoved([Wt, Gt, Ct], NewEntry),
										S1 = [NewEntry | S],
										transfer([Wl, Gl, Cl], left, Anew, S1, T).

crossings(C) :- transfer([1, 1, 1], left, [[1, 1, 1, left]], [], T), length(T, 7), reverse(T, C), write(C).