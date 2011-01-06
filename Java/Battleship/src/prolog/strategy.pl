/* for usage of substitute */
:- use_module(library(dialect/sicstus/lists)).

:- consult('outputModule.pl').

:- dynamic openList/1.
:- dynamic enemyField/1.

/* ---------------------------------------------- */
/* water was hit - nothing to do                  */
updateOpenList(_, _, 1).

/* last ship destroyed - nothing to do            */
updateOpenList(_, _, 4).

/* whole ship destroyed - clear open list         */
updateOpenList(X, Y, 3) :-
	newOpenList([]),
	surroundWithWater(X, Y, X, Y).

/* hit                                            */
updateOpenList(X, Y, 2) :-
	West is X - 1,
	appendFreeFieldToList(West, Y),
	East is X + 1,
	appendFreeFieldToList(East, Y),
	North is Y - 1,
	appendFreeFieldToList(X, North),
	South is Y + 1,
	appendFreeFieldToList(X, South),
	printOpenList,
	checkHitDirection(X, Y).

/* ---------------------------------------------- */
surroundWithWater(X, Y, _, _) :-
	X < 0; X >= 10;
	Y < 0; Y >= 10.

/* X, Y - hit, causes recursion                   */
surroundWithWater(X, Y, OldX, OldY) :-
	enemyField(EnemyField),
	member(X/Y/2, EnemyField),
	
	surroundEast(X, Y, OldX, OldY),
	surroundWest(X, Y, OldX, OldY),
	surroundNorth(X, Y, OldX, OldY),
	surroundSouth(X, Y, OldX, OldY).

/* X, Y - unknown, becomes water                  */
surroundWithWater(X, Y, _, _) :-
	enemyField(OldField),
	member(X/Y/0, OldField),
	substitute(X/Y/0, OldField, X/Y/1, NewField),
	retractall(enemyField(_)),
	asserta(enemyField(NewField)).

surroundWithWater(_, _, _, _).

/* ---------------------------------------------- */
/* surround with water - recursion                */
/* avoid endless loop with already checked values */		
surroundEast(X, _, OldX, _) :-
	NewX is X+1,
	NewX =:= OldX.
/* s East Neighbour                           */
surroundEast(X, Y, _, OldY) :-
	NewX is X+1,
	surroundWithWater(NewX, Y, X, OldY).

/* avoid endless loop with already checked values */		
surroundWest(X, _, OldX, _) :-
	NewX is X-1,
	NewX =:= OldX.
/* check West Neighbour                           */
surroundWest(X, Y, _, OldY) :-
	NewX is X-1,
	surroundWithWater(NewX, Y, X, OldY).

/* avoid endless loop with already checked values */		
surroundNorth( _, Y, _, OldY) :-
	NewY is Y-1,
	NewY =:= OldY.
/* check North Neighbour                          */
surroundNorth( X, Y, OldX, _) :-
	NewY is Y-1,
	surroundWithWater(X, NewY, OldX, Y).
	
/* avoid endless loop with already checked values */		
surroundSouth( _, Y, _, OldY) :-
	NewY is Y+1,
	NewY =:= OldY.
/* check South Neighbour                          */
surroundSouth( X, Y, OldX, _) :-
	NewY is Y+1,
	surroundWithWater(X, NewY, OldX, Y).

	
/* ---------------------------------------------- */
appendFreeFieldToList(X, Y) :-
	X < 0; X >= 10;
	Y < 0; Y >= 10.
	
appendFreeFieldToList(X, Y) :-
	enemyField(Field),
	openList(OpenList),
	member(X/Y/0, Field),
	append(OpenList, [X/Y], NewOpenList),
	newOpenList(NewOpenList).

appendFreeFieldToList(X, Y):-
	write('Skipped: '), write(X), write(', '), write(Y), nl.

/* ---------------------------------------------- */
/* Vertical ship                                  */
checkHitDirection(X, Y) :-
	YNorth is Y - 1,
	YSouth is Y + 1,
	enemyField(Field),
	(member(X/YNorth/2, Field); member(X/YSouth/2, Field)),
	ColLeft is X - 1,
	ColRight is X + 1,
	openList(OpenList),
	delete(OpenList, ColLeft/_,  OpenListNew),
	delete(OpenListNew, ColRight/_, OpenListOut),
	newOpenList(OpenListOut)
	.

/* Horizontal ship                                */
checkHitDirection(X, Y) :-
	XWest is X - 1,
	XEast is X + 1,
	enemyField(Field),
	(member(XWest/Y/2, Field); member(XEast/Y/2, Field)),
	RowUp   is Y - 1,
	RowDown is Y + 1,
	openList(OpenList),
	delete(OpenList, _/RowUp,  TempOpenList),
	delete(TempOpenList, _/RowDown, NewOpenList),
	newOpenList(NewOpenList)
	.

/* default - true                                 */
checkHitDirection(_, _).
	
/* ---------------------------------------------- */
/*                  */
getPointOfAttack(X, Y) :-
	openList([X/Y | T]),
	newOpenList(T).


/* if open list is empty, take random values      */
getPointOfAttack(X, Y) :-
	random(0, 10, X),
	random(0, 10, Y).
	
	
/* ---------------------------------------------- */
newOpenList(NewOpenList) :-
	retractall(openList(_)),
	asserta(openList(NewOpenList)).
		