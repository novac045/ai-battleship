/* consults */
:- consult('initModule.pl').
:- consult('attackModule.pl').
:- consult('defendModule.pl').
:- consult('outputModule.pl').

/* ---------------------------------------------- */	
/* handling for multiple games                    */
/*numberOfGames(3).*/

decreaseNumberOfGames :-
	numberOfGames(Num),
	Num > 0,
	NewNum is Num -1,
	retractall(numberOfGames(_)),
	assert(numberOfGames(NewNum)).
	

/*numberOfWins(0).*/

increaseNumberOfWins :-
	numberOfWins(Num),
	NewNum is Num +1,
	retractall(numberOfWins(_)),
	assert(numberOfWins(NewNum)).

/*numberOfLosses(0).*/

increaseNumberOfLosses :-
	numberOfLosses(Num),
	NewNum is Num +1,
	retractall(numberOfLosses(_)),
	assert(numberOfLosses(NewNum)).

/* ---------------------------------------------- */	
/* connection handling                            */
connect(Port) :- 
    tcp_socket(Socket),
    gethostname(Host),  % local host
    tcp_connect(Socket,Host:Port),
    tcp_open_socket(Socket,INs,OUTs),
    assert(connectedReadStream(INs)),
    assert(connectedWriteStream(OUTs)),
	assert(connectedTcpSocket(Socket)),
    write('Connected'), nl.

/*:- connect(54321).*/

disconnect :-
	connectedTcpSocket(Socket),
	tcp_close_socket(Socket),
	retractall(connectedReadStream(_)),
	retractall(connectedWriteStream(_)),
	retractall(connectedTcpSocket(_)),
	write('disconnected'), nl.	

/* ---------------------------------------------- */	
/* defend handling */
defend(State) :-
    write('    KI is defending'), nl,
    connectedReadStream(IStream),
    connectedWriteStream(OStream),
    write('    - Waiting for input'), nl,
    read(IStream,(1,[X,Y])),
    doDefend(X, Y, State),
    write('    - KI got attacked at: '), write(X), write(', '), write(Y), write(' -> ') , write(State) ,nl,
    write(OStream,'('),
    write(OStream,(2,[X,Y,State])),
    write(OStream,').'),
    nl(OStream),
    flush_output(OStream),
    flush_output,
	!.

/* ---------------------------------------------- */	
/* attack handling */
attack(State) :-
    write('    KI is attacking'), nl,
    connectedReadStream(IStream),
    connectedWriteStream(OStream),
	/* angreifen */
	doAttack(X, Y),
    write(OStream,'('),
    write(OStream,(1,[X,Y])),
    write(OStream,').'),
    nl(OStream),
    flush_output(OStream),
    flush_output,
    write('    - Waiting for response'), nl,
    read(IStream,(2,[U,V,State])),
	attackResponse(U, V, State),
    write('    - KI attacked        : '), write('State: '), write(U), write(', '), write(V), write(' and hit: '), write(State), nl,
	printEnemyField,
	!.

/* ---------------------------------------------- */	
/* Checks for Win or Loss                         */
lost(4).
won(4).	

writeIfLost(4) :-

	increaseNumberOfLosses,
	write('KI looses.'), nl.
writeIfLost(_).

writeIfWon(4) :-
	increaseNumberOfWins,
	write('KI wins.'), nl.
writeIfWon(_).
	
/* ---------------------------------------------- */	
/* start game defending                           */
defendFirst :-
    /*write('Iterating ... '), nl,*/
    defend(MyState),
	writeIfLost(MyState),
	\+ lost(MyState),
    attack(EnemyState),
	writeIfWon(EnemyState),
	\+ won(EnemyState),
    defendFirst,
	!.
/* end game without warning */
defendFirst.

/* ---------------------------------------------- */	
/* start game attacking                           */
attackFirst :-
    /*write('Iterating ... '), nl,*/
    attack(EnemyState),
	writeIfWon(EnemyState),
	\+ won(EnemyState),
    defend(MyState),
	writeIfLost(MyState),
	\+ lost(MyState),
    attackFirst,
	!.
	
/* end game without warning */
attackFirst.

/* ---------------------------------------------- */	
/* main                                           */
mainInit(3) :-
    connectedReadStream(IStream),
    read(IStream,(5,[])),
    write('Received start signal'), nl,
    defendFirst,
	!.

mainInit(4) :-
    connectedReadStream(IStream),
    read(IStream,(5,[])),
    write('Received start signal'), nl,
    attackFirst,
	!.

/* in-game predicate                               */
main :-
	decreaseNumberOfGames,
	connect(54321),
	initPrologClient,
    connectedReadStream(IStream),
    read(IStream,(OPCODE,[])),
	printMyField,
    mainInit(OPCODE),
	disconnect,
	main.

/* predicate for end of game                      */
main :-
	write('End of game.'), nl,
	numberOfWins(X),
	numberOfLosses(Y),
	write('KI won '), write(X), write(' times and lost '), 
	write(Y), write(' times.'), nl.

firststart :- 	
	assert(numberOfGames(50)),
	assert(numberOfWins(0)),
	assert(numberOfLosses(0)),
	main.

:- firststart.	