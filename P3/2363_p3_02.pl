/**/
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


%Ejercicio 4.1

elem_count(_, [], 0).

elem_count(X, [X|Ls], Xn) :-
	elem_count(X, Ls, Xp),
	Xn is Xp + 1.

elem_count(X, [Y|Ls], Xn) :-
	Y \= X,
	elem_count(X, Ls, Xn).


%Ejercicio 4.2

list_count([], _, []).

list_count([X|Xs], L2, [X-V|L3]) :-
	elem_count(X, L2, V),
	list_count(Xs, L2, L3).


%Ejercicio 5

sort_list(List,Sorted):-
	i_sort(List, [], Sorted).

i_sort([], Acc, Acc).

i_sort([H|T], Acc, Sorted):-
	insert(H, Acc, NAcc),
	i_sort(T, NAcc, Sorted).
	

%Ejercicio 6

build_tree([X-_, Y-_], tree(1, tree(X, nil, nil), tree(Y, nil, nil))).

build_tree([X-_|Ls], tree(1, tree(X, nil, nil), SubTree)) :-
	build_tree(Ls, SubTree).


%Ejercicio 7.1

encode_elem(X, [], tree(X, nil, nil)).

encode_elem(X, [0], tree(1, tree(X, _, _) , _)).

encode_elem(X, [1|R], tree(1, tree(Y, _, _), SubTree)) :-
	X \= Y,
	encode_elem(X, R, SubTree).
	

%Ejercicio 7.2

encode_list([X], [Y], T) :-
	encode_elem(X, Y, T).

encode_list([X|Ls], [Y|Rs], T) :-
	encode_elem(X, Y, T),
	encode_list(Ls, Rs, T).

%Ejercicio 8

dictionary([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).

encode(Text, Result) :-
	dictionary(X),
	list_count(X, Text, Occurences),
	sort_list(Occurences, OccurencesSorted),
	invierte(OccurencesSorted, OccurencesDesc),
	build_tree(OccurencesDesc, Tree),
	encode_list(Text, Result, Tree).
