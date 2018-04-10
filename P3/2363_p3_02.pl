pertenece(X, [X|_]).
pertenece(X, [_|Rs]) :- pertenece(X, Rs).

pertenece_m(X, [X|_]) :-
	X \= [_|_].

pertenece_m(X, [Y|_]) :-
	Y = [_|_],
	pertenece_m(X, Y).

pertenece_m(X, [_|Rs]) :-
	pertenece_m(X, Rs).
