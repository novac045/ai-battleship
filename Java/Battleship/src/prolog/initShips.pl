ship(S) :-
	between(1,10,S)
.
ship(S,P,X,Y):-
	true
.
ship_type(1,battleship):-
	true
. 
ship_type(S,cruiser):-
	between(2,3,S),
	!
.
ship_type(S,destroyer):-
	between(4,6,S),
	!
.
ship_type(S,submarine):- 
	between(7,10,S),
	!
.
slength(battleship,4):-
	true
.
slength(cruiser,3):-
	true
.
slength(destroyer,2):-
	true
.

slength(submarine,1):-
	true
.
/*
ship length is an auxiliary definition which defines for a particular ship its length.
*/
ship_length(S,L):-
	ship_type(S,Type), 
	slength(Type,L)
.
domx(X):-
	between(1,10,X)
.
domy(Y):-
	between(1,10,X)
.
test1:-
	findall(S,(ship(S),ship_length(S,Length), between(1,Length,Part)),Liste)
/*
	forall(
			(ship(S),ship_length(S,Length), between(1,Length,Part))
		, 
			(domx(X),domy(Y), ship(S,Part,X,Y))
		)
*/
.