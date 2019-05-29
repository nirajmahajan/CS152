% There are five consecutive houses, each of a different
% color and inhabited by men of different nationalities. They each
% own a different pet, have a different favorite drink and drive a
% different vehicle.
%   1. The Englishman lives in the red house.
%   2. The Spaniard owns a dog.
%   3. Coffee is drunk in the green house.
%   4. The Ukrainian drinks tea.
%   5. The green house is immediately to the right of the ivory
%      house.
%   6. The motor cycle owner keeps snails.
%   7. Bike is driven by the man who lives in the yellow
%      house.
%   8. Milk is drunk in the middle house.
%   9. The Norwegian lives in the first house on the left.
%  10. The man rides a skateboard lives in the house next to the man
%      who owns a  fox.
%  11. The man who rides a bike lives next to  the man who owns a horse.

%  12. The man with the boat drinks orange juice.
%  13. The Japanese has a car
%  14. The Norwegian lives next to the blue house.

%Who owns the zebra, who drinks water.

% left_right(L,R,List) means L is to the left of R in the list List.
left_right(L,R,[L,R,_,_,_]).
left_right(L,R,[_,L,R,_,_]).
left_right(L,R,[_,_,L,R,_]).
left_right(L,R,[_,_,_,L,R]).

% next_to(A,B,L) means A is next to B in the List L.
next_to(A,B,L) :- left_right(A,B,L).
next_to(A,B,L) :- left_right(B,A,L).

arrangement(S) :- S = [[_, norwegian, _, _, _], [blue, _, _, _, _], [_, _, _, milk, _], _, _],
                  member([red, english, _, _, _], S),
                  member([_, spaniard, _, _, dog], S),
                  member([green, _, _, coffee, _], S),
                  member([_, ukrainian, _, tea, _], S),
                  left_right([ivory,_,_,_,_],[green,_,_,_,_],S),
				  member([_,_,motorcycle,_,snails], S),
                  member([yellow,_,bike,_,_], S),
				  next_to([_,_,skateboard,_,_],[_,_,_,_,fox],S),
		  		  next_to([_,_,bike,_,_],[_,_,_,_,horse],S),
				  member([_,_,boat,orange_juice,_],S),
				  member([_,japanese,car,_,_],S),
				  member([_,_,_,_,zebra], S),
				  member([_,_,_,water,_], S).		

goal1(Who,Who1) :- arrangement(S), member([_,Who,_,_,zebra], S),
				   member([_,Who1,_,water,_], S).
