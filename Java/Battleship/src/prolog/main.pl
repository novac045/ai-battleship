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
defend :-
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
    flush_output.

/* attack handling */
attack :-
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
	printEnemyField.

/* startgame */
defendFirst :-
    write('Iterating ... '), nl,
    defend,
    attack,
    defendFirst.

attackFirst :-
    write('Iterating ... '), nl,
    attack,
    defend,
    attackFirst.

mainInit(OPCODE) :-
    OPCODE =:= 3,
    connectedReadStream(IStream),
    read(IStream,(5,[])),
    write('Received start signal'), nl,
    defendFirst.

mainInit(OPCODE) :-
    OPCODE =:= 4,
    connectedReadStream(IStream),
    read(IStream,(5,[])),
    write('Received start signal'), nl,
    attackFirst.

main :-
    connectedReadStream(IStream),
    read(IStream,(OPCODE,[])),
	initPrologClient,
	printMyField,
    mainInit(OPCODE),
    defendFirst.

:- main.
