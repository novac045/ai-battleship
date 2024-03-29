:- dynamic enemyField/1.
:- dynamic myField/1.

printField(0) :- write(' U '). /* unknown */
printField(1) :- write(' W '). /* water */
printField(2) :- write(' X '). /* hit */
printField(3) :- write(' X '). /* destroyed */
printField(4) :- write(' X '). /* last ship destroyed */
printField(5) :- write(' error '). /* missed */
printField(6) :- write(' o '). /* ship */

outputField([]) :- /*Output to Console*/
	flush_output.
	
outputField([9/_/State | T]) :-
	printField(State),
	nl,
	outputField(T).	

outputField([_/_/State | T]) :-
	printField(State),
	outputField(T).
	
printMyField :-
	myField(Field),
	outputField(Field),
	saveMyField(Field),
	!.
	
printEnemyField :-
	enemyField(Field),
	outputField(Field),
	!.

printOpenList :-
	openList(OpenList),
	write(OpenList), nl.
	
printOpenList.
	
saveMyField(Field) :-
	append('MyField.txt'),
	outputField(Field),nl,
	told
.