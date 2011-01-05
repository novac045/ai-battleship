/* consults */
:- consult('initModule.pl').
:- consult('attackModule.pl').
:- consult('defendModule.pl').
:- consult('outputModule.pl').


/* connection handling */
connect(Port) :- 
    tcp_socket(Socket),
    gethostname(Host),  % local host
    tcp_connect(Socket,Host:Port),
    tcp_open_socket(Socket,INs,OUTs),
    assert(connectedReadStream(INs)),
    assert(connectedWriteStream(OUTs)),
    write('Connected'), nl.

:- connect(54321).

/* defend handling */
defend(State) :-
    write('    KI is defending'), nl,
    connectedReadStream(IStream),
    connectedWriteStream(OStream),
    write('    - Waiting for input'), nl,
    read(IStream,(1,[X,Y])),
    doDefend(X, Y, State),
    write('    - KI got attacked at: '), write(X), write(', '), write(Y), write(' -> ') , write(State) ,nl,
    write(OStream,(2,[X,Y,State])),
    nl(OStream),
    flush_output(OStream),
    flush_output,
	!.

/* attack handling */
attack(State) :-
    write('    KI is attacking'), nl,
    connectedReadStream(IStream),
    connectedWriteStream(OStream),
	/* angreifen */
	doAttack(X, Y),
    write(OStream,(1,[X,Y])),
    nl(OStream),
    flush_output(OStream),
    flush_output,
    write('    - Waiting for response'), nl,
    read(IStream,(2,[U,V,State])),
	attackResponse(U, V, State),
    write('    - KI attacked        : '), write('State: '), write(U), write(', '), write(V), write(' and hit: '), write(State), nl,
	printEnemyField,
	!.

lost(4).
won(4).	

writeIfLost(4) :-
	write('KI looses.'), nl.
writeIfLost(_).

writeIfWon(4) :-
	write('KI wins.'), nl.
writeIfWon(_).
	
/* startgame */
defendFirst :-
    write('Iterating ... '), nl,
    defend(MyState),
	writeIfLost(MyState),
	\+ lost(MyState),
    attack(EnemyState),
	\+ won(EnemyState),
    defendFirst,
	!.
/* end game without warning */
defendFirst.

attackFirst :-
    write('Iterating ... '), nl,
    attack(EnemyState),
	\+ won(EnemyState),
    defend(MyState),
	writeIfLost(MyState),
	\+ lost(MyState),
    attackFirst,
	!.
/* end game without warning */
attackFirst.

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

main :-
    connectedReadStream(IStream),
    read(IStream,(OPCODE,[])),
	initPrologClient,
	printMyField,
    mainInit(OPCODE),
	write('End of Game.'), nl.

:- main.
