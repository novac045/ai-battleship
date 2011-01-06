/* module for attack decision */
:- consult('strategy.pl').

:- dynamic enemyField/1.

/* for usage of substitute */
:- use_module(library(dialect/sicstus/lists)).

/* ---------------------------------------------- */	
/* praedikat doAttack(X, Y) */
doAttack(X, Y) :-
	getPointOfAttack(A, B),
    /*write('Random: '), write(A), write(','), write(B), nl,*/
	doAttackCheck(A, B, XVerified, YVerified),
    X is XVerified,
    Y is YVerified,
    !.

/* ---------------------------------------------- */	
doAttackCheck(X, Y, XVerified, YVerified) :-
	enemyField(Field),
	member(X/Y/0, Field),
    XVerified is X,
    YVerified is Y,
    !.

doAttackCheck(X, Y, XVerified, YVerified) :-
	enemyField(Field),
	\+ member(X/Y/0, Field),
    doAttack(A, B),
    XVerified is A,
    YVerified is B,
    !.

/* ---------------------------------------------- */	
/* praedikat attackResponse(X, Y, State )         */
attackResponse(X, Y, State) :-
	updateEnemyField(X, Y, State),
	updateOpenList(X, Y, State).

/* if a ship was destroyed mark field as normal hit */
updateEnemyField(X, Y, 3) :-
	enemyField(OldField),
	substitute(X/Y/0, OldField, X/Y/2, NewField),
	retractall(enemyField(_)),
	asserta(enemyField(NewField)), !.

/* update field state                               */
updateEnemyField(X, Y, State) :-
	enemyField(OldField),
	substitute(X/Y/0, OldField, X/Y/State, NewField),
	retractall(enemyField(_)),
	asserta(enemyField(NewField)), !.

