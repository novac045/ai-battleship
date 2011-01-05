:- dynamic enemyField/1.

/* for usage of substitute */
:- use_module(library(dialect/sicstus/lists)).

/* praedikat doAttack(X, Y) */
doAttack(X, Y) :-
	randValue(X),
	randValue(Y),
	enemyField(Field),
	member(X/Y/0, Field), !.

/* praedikat attackResponse( State ) */
attackResponse(X, Y, State) :-
	enemyField(OldField),
	substitute(X/Y/0, OldField, X/Y/State, NewField),
	retractall(enemyField(_)),
	asserta(enemyField(NewField)), !.

randValue(X) :-
	random(0, 10, X).