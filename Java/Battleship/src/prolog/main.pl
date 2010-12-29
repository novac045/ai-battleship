connect(Port) :- 
    tcp_socket(Socket),
    gethostname(Host),  % local host
    tcp_connect(Socket,Host:Port),
    tcp_open_socket(Socket,INs,OUTs),
    assert(connectedReadStream(INs)),
    assert(connectedWriteStream(OUTs)),
    write('Connected'), nl
    .

:- connect(54321).

defend :-
    write('    Defend'), nl,
    connectedReadStream(IStream),
    connectedWriteStream(OStream),
    write('    - Waiting for input'), nl,
    read(IStream,(1,[X,Y])),
    write('    - Received: '), write(X), write(', '), write(Y), nl,
    State is 8, /* Aufruf einer Attacke, R�ckgabe eines State */
    write(OStream,(2,[X,Y,State])),
    nl(OStream),
    flush_output(OStream),
    flush_output
    .

attack :-
    write('    Attack'), nl,
    connectedReadStream(IStream),
    connectedWriteStream(OStream),
	/* DoAttack(), r�ckgabe von X, Y */
    X is 1,
    Y is 2,
    write(OStream,(1,[X,Y])),
    nl(OStream),
    flush_output(OStream),
    flush_output,
    write('    - Waiting for response'), nl,
    read(IStream,(2,[U,V,State])),
	/* AttackResponse(State), keine r�ckgabe */
    write('    - Received: '), write('State: '), write(U), write(', '), write(V), write(', '), write(State), nl
    .

defendFirst :-
    write('Iterating ... '), nl,
    defend,
    attack,
    defendFirst
    .

attackFirst :-
    write('Iterating ... '), nl,
    attack,
    defend,
    attackFirst
    .

mainInit(OPCODE) :-
    OPCODE =:= 3,
    connectedReadStream(IStream),
    read(IStream,(5,[])),
    write('Received start signal'), nl,
    defendFirst
    .

mainInit(OPCODE) :-
    OPCODE =:= 4,
    connectedReadStream(IStream),
    read(IStream,(5,[])),
    write('Received start signal'), nl,
    attackFirst
    .

main :-
    connectedReadStream(IStream),
    read(IStream,(OPCODE,[])),
    mainInit(OPCODE),
    defendFirst.

:- main.
