:- dynamic enemyField/1.
:- dynamic myField/1.

printField(0) :- write(' U '). /* unknown */
printField(1) :- write(' W '). /* water */
printField(2) :- write(' X '). /* hit */
printField(3) :- write(' - '). /* destroyed */
printField(4) :- write(' error '). /* last ship destroyed */
printField(5) :- write(' eeeee '). /* missed */
printField(6) :- write(' o '). /* ship */

outputField([]).
	
outputField([9/_/State | T]) :-
	printField(State),
	nl,
	outputField(T).	

outputField([_/_/State | T]) :-
	printField(State),
	outputField(T).
	

printMyField :-
	myField(Field),
	outputField(Field).
	
printEnemyField :-
	enemyField(Field),
	outputField(Field).

	
	