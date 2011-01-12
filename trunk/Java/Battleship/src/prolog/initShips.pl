:-use_module(library(random)).

:-assert(template(
[
	1/5/X11/Y11,
	1/4/X12/Y12,
	1/3/X13/Y13,
	1/2/X14/Y14,
	1/1/X15/Y15,
	
	2/4/X21/Y21,
	2/3/X22/Y22,
	2/2/X23/Y23,
	2/1/X24/Y24,
	
	3/3/X31/Y31,
	3/2/X32/Y32,
	3/1/X33/Y33,
	
	4/3/X41/Y41,
	4/2/X42/Y42,
	4/1/X43/Y43,
	
	5/2/X51/Y51,
	5/1/X52/Y52])
).
connectedHV(S1,P1,X1,Y1,S2,P2,X2,Y2):-
	S1 == S2,
	P1 \== P2,
	X1-X2 =:= P1-P2,
	Y1 == Y2,
	!
.
connectedHV(S1,P1,X1,Y1,S2,P2,X2,Y2):-
	S1 == S2,
	P1 \== P2,
	Y1-Y2 =:= P1-P2,
	X1 == X2,
	!
.
distance(S1,S2,X1,_Y1,X2,_Y2):-
	S1 \== S2,
	abs(X1-X2)>1,
	!
.
distance(S1,S2,_X1,Y1,_X2,Y2):-
	S1 \== S2,
	abs(Y1-Y2)>1,
	!
.
initShips([],_,_).
initShips([S/P/X/Y|Others],Xseq,Yseq):-
	initShips(Others,Xseq,Yseq),
	member(X, Xseq),
	member(Y, Yseq),
	rules(S/P/X/Y,Others)
.
rules(_,[]).
rules(S1/P1/X1/Y1,[S2/P2/X2/Y2|Others]):-
	(connectedHV(S1,P1,X1,Y1,S2,P2,X2,Y2);
	distance(S1,S2,X1,Y1,X2,Y2)),
	rules(S1/P1/X1/Y1,Others)
.
itShips([]).
itShips([S|Others]):-
	% Fast rewind ...
	itShips(Others),
	% random X and Y Domain for one ship ...
	template(Template),
	randseq(10,10,Xseq),
	randseq(10,10,Yseq),
	% template for one ship ....
	findall((S/P/X/Y),member(S/P/X/Y,Template),OneShip),
	% all possible positions for one ship
	findall(OneShip,initShips(OneShip,Xseq,Yseq),Rpl),
	% random position for one ship
	length(Rpl,RplLength),
	RplRand is random(RplLength),
	nth0(RplRand,Rpl,Elem),
	% replacement of ships position in template
	subtract(Template,OneShip,Temp2),
	union(Elem,Temp2,Temp3),
	retractall(template(_)),
	assert(template(Temp3))
.


place(Position) :-
	randseq(5,5,Ships),
	%write(Ships),nl,
	itShips(Ships),
	template(Tmp),
	Position = Tmp,
	%write(Tmp),
	length(Tmp,X),
	write(Position),nl
	%nl,write(X)
.