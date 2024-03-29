/* for usage of substitute */
:- use_module(library(dialect/sicstus/lists)).
:- consult('initShips.pl').

/* ---------------------------------------------- */	
/* initialise prolog client                       */
initPrologClient :- 
	assert(openList([])),
	initMyField,
	initEnemyField.

/* praedikat initMyField() */
initMyField      :- 
	retractall(myField(_)),
	/* init myField with Water: 1 */
	generateField(99, 1, [], Field),
	/* currently with dummy ships */
	place(Ships),%SBog
	fillWithShips(Ships,Field,FilledField),%SBog
	%fillWithDummies(Field, DummyField),%SRit
	assert(myField(FilledField))%SBog
	%assert(myField(DummyField))%SRit
	.


/* praedikat initEnemyField() */
initEnemyField   :- 
	retractall(enemyField(_)),
	/* init enemyField with unknown: 0 */
	generateField(99, 0, [], Field),
	assert(enemyField(Field)).


/* ---------------------------------------------- */	
/* Logic for field generation                     */
generateField(-1, _, R, R) :- !.

generateField(Count, InitVal, OldField, NewField) :-
	X is Count mod 10,
	Y is Count //  10,
	append([X/Y/InitVal], OldField, TempField),
	Count2 is Count - 1,
	generateField(Count2, InitVal, TempField, NewField).

/* helper */
fillWithDummies(Field, DummyField) :-
	State is 6,
	substitute(5/2/1, Field, 5/2/State, F1),
	substitute(5/3/1, F1, 5/3/State, F2),

	substitute(6/9/1, F2, 6/9/State, F3),
	substitute(7/9/1, F3, 7/9/State, F4),
	substitute(8/9/1, F4, 8/9/State, F5),
	
	substitute(0/2/1, F5, 0/2/State, F6),
	substitute(0/3/1, F6, 0/3/State, F7),
	substitute(0/4/1, F7, 0/4/State, F8),
	substitute(0/5/1, F8, 0/5/State, DummyField).
	
/* Ships: _W/_W/X/Y */
/* Field X/Y/6 */     
fillWithShips([],A,A).
fillWithShips([_/_/Xi/Yi|Others], InitialField, FilledField):-
	fillWithShips(Others, InitialField, TmpField),
	State is 6,
	X is Xi-1,
	Y is Yi-1,
	substitute(X/Y/1, TmpField, X/Y/State, FilledField)
.