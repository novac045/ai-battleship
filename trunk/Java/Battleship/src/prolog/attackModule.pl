:- dynamic enemyField/1.

/* for usage of substitute */
:- use_module(library(dialect/sicstus/lists)).

/* ---------------------------------------------- */	
/* praedikat doAttack(X, Y) */
doAttack(X, Y) :-
	randValues(A, B),
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
/* praedikat attackResponse(X, Y, State ) */
attackResponse(X, Y, State) :-
	enemyField(OldField),
	substitute(X/Y/0, OldField, X/Y/State, NewField),
	retractall(enemyField(_)),
	asserta(enemyField(NewField)), !.

randValues(X, Y) :-
	random(0, 10, X),
	random(0, 10, Y).