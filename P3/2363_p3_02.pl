pertenece(X, [X|_]).
pertenece(X, [_|Rs]) :- pertenece(X, Rs).

pertenece_m(_, []).
pertenece_m(X, [_|Ls) :-
	pertenece_m(X, Ls).

