:-use_module(library(random)).
/*Template zur Abbildung der 
Verwendeten Schiffe.
Schiffs-ID / Teilenummer / X-Koordinate / Y-Koordinate*/
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
/* Teile des selben Schiffes m�ssen 
aneinander liegen (Horizontal) */
connectedHV(S1,P1,X1,Y1,S2,P2,X2,Y2):-
	S1 == S2,
	P1 \== P2,
	X1-X2 =:= P1-P2,
	Y1 == Y2,
	!
.
/* Teile des selben Schiffes m�ssen 
aneinander liegen (Vertikal) */
connectedHV(S1,P1,X1,Y1,S2,P2,X2,Y2):-
	S1 == S2,
	P1 \== P2,
	Y1-Y2 =:= P1-P2,
	X1 == X2,
	!
.
/*Ungleiche Schiffe m�ssen 
Ein Feld Abstand halten, 
ODER Diagonal versetzt stehen (Vertikal)*/
distance(S1,S2,X1,Y1,X2,Y2):-
	S1 \== S2,
	((Y1 == Y2,
	abs(X1-X2)>1);
	(Y1 \== Y2,
	abs(X1-X2)>=1)),
	!
.
/*Ungleiche Schiffe m�ssen 
Ein Feld Abstand halten, 
ODER Diagonal versetzt stehen (Horizontal)*/
distance(S1,S2,X1,Y1,X2,Y2):-
	S1 \== S2,
	((X1 == X2,
	abs(Y1-Y2)>1);
	(X1 \== X2,
	abs(Y1-Y2)>=1)),
	!
.
/*Wendet die Platzierungsregeln f�r eine 
Liste von Schiffen an, auf einem Spielfeld 
mit den Wertebereichen Xseq und Yseq*/
initShips([],_,_).
initShips([S/P/X/Y|Others],Xseq,Yseq):-
	initShips(Others,Xseq,Yseq),
	member(X, Xseq),
	member(Y, Yseq),
	rules(S/P/X/Y,Others)
.
/*Anwendung der Regeln auf ein Schiff. 
Ein Schiff muss 'in sich' legal platziert werden */
rules(_,[]).
rules(S1/P1/X1/Y1,[S2/P2/X2/Y2|Others]):-
	(connectedHV(S1,P1,X1,Y1,S2,P2,X2,Y2);
	distance(S1,S2,X1,Y1,X2,Y2)),
	rules(S1/P1/X1/Y1,Others)
.
/*Erzeugt eine Liste von Schiffen mit zuf�llig gew�hlten,
legalen Positionierungen.*/
itShips([],_,[]).
itShips([Ship|OtherShips],Template,Positions):-
	itShips(OtherShips,Template,TmpPositions),

	randseq(10,10,Xseq), %zuf�llige Reihenfolge der X-Koordinaten
	randseq(10,10,Yseq), %zuf�llige Reihenfolge der Y-Koordinaten
	
	findall((Ship/P/X/Y),member(Ship/P/X/Y,Template),OneRandomShip), % Lade Template f�r ein Schiff
	append(OneRandomShip,TmpPositions,TMP), % speichere Schiff in Tempor�re Liste
	findall(TMP,initShips(TMP,Xseq,Yseq),AllPossiblePositions), % finde alle legalen Positionierungen f�r Schiffe in Temp. Liste
	
	length(AllPossiblePositions,RandomMax),				% auswahl einer
	RandomIndex is random(RandomMax),					% zuf�lligen, legalen Positionierung
	nth0(RandomIndex,AllPossiblePositions,Positions)	% und speichern dieser Positionierung in der R�ckgabeliste
.
/*Erzeugt eine zuf�llige Platzierungsreihenfolge
f�r die Schiffe und st��t das platzieren nach dieser Reihenfolge an*/
place(Positions):-
	template(Template),
	randseq(5,5,Ships),
	itShips(Ships,Template,Positions)
.
