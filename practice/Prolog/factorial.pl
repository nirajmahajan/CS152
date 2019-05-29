factorial(0, 1) :- !.
factorial(N, A) :- N1 is N-1, factorial(N1, A1),
				   A is A1*N.