goal(Combination):- Colours = [r, y, g], 
					member(A, Colours), member(B, Colours), member(C, Colours),
					 
					%A's statement
					A1 = [B, C],
					A1 \= [r, r], A1 \= [y, y],

					%B's Statement
					B1 = [A, C],
					B1 \= [r, r], B1 \= [y, y], C \= r, C \= y,

					%Final
					Combination = [A, B, C].