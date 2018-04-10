%Ejercicio 1
pertenece(X, [X|_]).
pertenece(X, [_|Rs]) :- pertenece(X, Rs).

pertenece_m(X, [X|_]) :-
	X \= [_|_].

pertenece_m(X, [Y|_]) :-
	Y = [_|_],
	pertenece_m(X, Y).

pertenece_m(X, [_|Rs]) :-
	pertenece_m(X, Rs).


%Ejercicio 2

concatena([], L, L).
concatena([X|L1], L2, [X|L3]) :-
	concatena(L1, L2, L3).

invierte([], []).

invierte([H|T], R) :-
	invierte(T, Ls),
	concatena(Ls, [H], R).

%Ejercicio 3

%TODO: ask

insert(X-P, [], [X-P]).

insert(X-P, [Y-P1|Ls], [X-P,Y-P1|Ls]) :-
	P =< P1.

insert(X-P, [Y-P1|Ls], R) :-
	P > P1,
	insert(X-P, Ls, T),
	concatena([Y-P1], T, R).


%Ejercicio 4

elem_count(_, [], 0).

elem_count(X, [X|Ls], Xn) :-
	elem_count(X, Ls, Xp),
	Xn is Xp + 1.

elem_count(X, [Y|Ls], Xn) :-
	Y \= X,
	elem_count(X, Ls, Xn).
