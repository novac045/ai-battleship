

doDefend(X, Y, State) :-
	myField(MyField),
	member(X/Y/State, MyField), !.
	