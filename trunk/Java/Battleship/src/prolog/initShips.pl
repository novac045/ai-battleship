:-use_module(library(random)).
:-assert(template(
[
	1/5/_X11/_Y11,
	1/4/_X12/_Y12,
	1/3/_X13/_Y13,
	1/2/_X14/_Y14,
	1/1/_X15/_Y15,

	2/4/_X21/_Y21,
	2/3/_X22/_Y22,
	2/2/_X23/_Y23,
	2/1/_X24/_Y24,

	3/3/_X31/_Y31,
	3/2/_X32/_Y32,
	3/1/_X33/_Y33,

	4/3/_X41/_Y41,
	4/2/_X42/_Y42,
	4/1/_X43/_Y43,

	5/2/_X51/_Y51,
	5/1/_X52/_Y52])
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
distance(S1,S2,X1,Y1,X2,Y2):-
	S1 \== S2,
	((Y1 == Y2,
	abs(X1-X2)>1);
	(Y1 \== Y2,
	abs(X1-X2)>=1)),
	!
.
distance(S1,S2,X1,Y1,X2,Y2):-
	S1 \== S2,
	((X1 == X2,
	abs(Y1-Y2)>1);
	(X1 \== X2,
	abs(Y1-Y2)>=1)),
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
itShips([],_,[]).
itShips([Ship|OtherShips],Template,Positions):-
	itShips(OtherShips,Template,TmpPositions),

	randseq(10,10,Xseq),
	randseq(10,10,Yseq),
	
	findall((Ship/P/X/Y),member(Ship/P/X/Y,Template),OneRandomShip),
	append(OneRandomShip,TmpPositions,TMP),
	findall(TMP,initShips(TMP,Xseq,Yseq),AllPossiblePositions),
	length(AllPossiblePositions,RandomMax),
	RandomIndex is random(RandomMax),
	
	nth0(RandomIndex,AllPossiblePositions,Positions)
.
place(Positions):-
	template(Template),
	randseq(5,5,Ships),
	itShips(Ships,Template,Positions)
.