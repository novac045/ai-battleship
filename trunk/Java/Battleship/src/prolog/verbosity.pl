/*
Abschalten der Ausgabe
*/
verbose(0):-
	open_null_stream(Stream),	%Null-Stream erzeugen
	set_output(Stream),			%Null-Stream als Ausgabe setzen
	assert(currentStream(Stream))%Null-Stream im globalen Prädikat currentStream setzen
.
/*
Umleiten der Ausgabe in Datei
*/
verbose(1):-
	open('GameLogs/Output.txt', append, Stream),	%File-Stream erzeugen, Datei wird fortgeschrieben (append)
	set_output(Stream),								%File-Stream als Ausgabe setzten
	write('------- NEW GAME SESSION -------'),nl,	%Zusätzliche Ausgabe um Spiele voneinander abzugrenzen
	assert(currentStream(Stream))					%File-Stream im globalen Prädikat currentStream setzen
.
/*
Konsole zur Ausgabe nutzen
*/
verbose(2):-
	set_output(user_output),			%Setzen des user_outputs als Ausgabe (=Konsole)
	assert(currentStream(user_output))	%user_output im globalen Prädikat currentStream setzen
.