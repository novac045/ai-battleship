
doDefend(X, Y, RetVal) :-
	myField(MyField),
	member(X/Y/State, MyField), 
	/* update Field for hit at X/Y */
	updateMyField(X, Y, State),
	returnValue(X, Y, State, MyField, RetVal), !.

/* water - nothing to do */
updateMyField(_, _, 1).

/* ship - update ship */
updateMyField(X, Y, 6) :-
	myField(OldField),
	substitute(X/Y/6, OldField, X/Y/2, NewField),
	retractall(myField(_)),
	asserta(myField(NewField)).
	
/* water */
returnValue(_, _, 1, _, 1). 

/* lastShipDestroyed - if no State=6 (ship) in myField */
returnValue(_, _, 6, MyField, 4) :-
	\+ member(_/_/6, MyField). 


/* destroyed */
/* returnValue(X, Y, 6, _, 3). */

/* hit */
returnValue(_, _, 6, _, 2).

/* error, e.g. if same ship-spot was hit twice */
/* 0 - unknown!!! */
/*retrunValue(_, _, _, _, 0).*/

