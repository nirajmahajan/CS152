% state of the form [M C B] where M = missionaries on left, C = cannibals on left, B = (left/right) position of Boat

% M, C are from the side where the boat is (safe side) 

noEat(0, _).
noEat(M, C) :- M >= C.

move(0, 1).
move(1, 0).
move(1, 1).
move(2, 0).
move(0, 2).

transfer(0, 0, right, T, T) :- !.
transfer(Ml, Cl, left, S, T) :-     move(M, C),
							      	Ml1 is Ml-M, Cl1 is Cl-C,
							      	Ml1 >= 0, Cl1 >= 0,
							      	MR1 is 3-Ml+M, CR1 is 3-Cl+C,
							      	noEat(Ml1, Cl1), noEat(MR1, CR1),
							      	NewState = [Ml1, Cl1, right],
							      	not(member(NewState, S)),
									S1 = [NewState|S],
									transfer(Ml1, Cl1, right, S1, T).

transfer(Ml, Cl, right, S, T) :-	move(M, C),
							      	MR is 3-Ml, CR is 3-Cl,
							      	MR1 is MR-M, CR1 is CR-C,
							      	MR1 >= 0, CR1 >= 0,
							      	Ml1 is 3-MR1, Cl1 is 3-CR1,
							      	noEat(Ml1, Cl1), noEat(MR1, CR1),
							      	NewState = [Ml1, Cl1, left],
							      	not(member(NewState, S)),
									S1 = [NewState|S],
									transfer(Ml1, Cl1, left, S1, T).

safe(Ans) :- transfer(3, 3, left, [[3, 3, left]], T), reverse(T, Ans), write(Ans).