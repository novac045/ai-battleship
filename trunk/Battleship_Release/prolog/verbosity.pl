verbose(0):-
	open_null_stream(Stream),
	set_output(Stream),
	assert(currentStream(Stream))
.
verbose(1):-
	open('GameLogs/Output.txt', append, Stream),
	set_output(Stream),
	write('------- NEW SILENT SESSION -------'),
	assert(currentStream(Stream))
.
verbose(2):-
	set_output(user_output),
	assert(currentStream(user_output))
.