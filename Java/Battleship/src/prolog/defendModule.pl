
/* ---------------------------------------------- */	
doDefend(X, Y, NewState) :-
	myField(MyField),
	member(X/Y/State, MyField), 
	/* update Field for hit at X/Y */
	updateMyField(X, Y, State, NewState),
	!.

/* ---------------------------------------------- */	
/* water was hit - nothing to do */
/*               - state MISSED??! */
updateMyField(_, _, 1, 1).

/* ship was hit - state hit:       2 */
updateMyField(X, Y, 6, NewState) :-
	myField(OldField),
	substitute(X/Y/6, OldField, X/Y/2, NewField),
	retractall(myField(_)),
	asserta(myField(NewField)),
	updateShips(X, Y, NewState).

/* ---------------------------------------------- */	
/* ship was hit - state last ship destroyed: 4    */
/*              - if no state 6 (ship) in myField */
updateShips(_, _, 4) :-
	myField(MyField),
	\+ member(_/_/6, MyField).

/* ship was hit - state whole destroyed: 3 */
updateShips(X, Y, 3) :-
	completelyDestroyed(X, Y, X, Y).
	
/* else         - just a normal hit */
updateShips(_, _, 2).

/* ---------------------------------------------- */	
/* do not regard field borders                    */
completelyDestroyed(X, Y, _, _) :-
	X < 0; X >= 10;
	Y < 0; Y >= 10.

/* not destroyed if there is State SHIP: 6        */
completelyDestroyed(X, Y, _, _) :-
	myField(MyField),
	member(X/Y/6, MyField),
	!,
	fail.

/* recursion if state is HIT: 2                   */
completelyDestroyed(X, Y, OldX, OldY) :-
	myField(MyField),
	member(X/Y/2, MyField),
	
	destroyedEast(X, Y, OldX, OldY),
	destroyedWest(X, Y, OldX, OldY),
	destroyedNorth(X, Y, OldX, OldY),
	destroyedSouth(X, Y, OldX, OldY).
	
/* ---------------------------------------------- */		
destroyedEast(X, _, OldX, _) :-
	NewX is X+1,
	NewX =:= OldX.
destroyedEast(X, Y, OldX, OldY) :-
	NewX is X+1,
	completelyDestroyed(NewX, Y, X, OldY).

destroyedWest(X, _, OldX, _) :-
	NewX is X-1,
	NewX =:= OldX.
destroyedWest(X, Y, OldX, OldY) :-
	NewX is X-1,
	completelyDestroyed(NewX, Y, X, OldY).

destroyedNorth( _, Y, _, OldY) :-
	NewY is Y-1,
	NewY =:= OldY.
destroyedNorth( X, Y, OldX, OldY) :-
	NewY is Y-1,
	completelyDestroyed(X, NewY, OldX, Y).
	
destroyedSouth( _, Y, _, OldY) :-
	NewY is Y+1,
	NewY =:= OldY.
destroyedSouth( X, Y, OldX, OldY) :-
	NewY is Y+1,
	completelyDestroyed(X, NewY, OldX, Y).
