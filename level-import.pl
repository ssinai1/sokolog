:- use_module(library(aggregate)).
:- use_module(library(pio)).
:- use_module(library('dcg/basics')).


% Load a level from file
load_level(FilePath) :-
    clear_level,
    lines_from_file(FilePath, Lines1),
    maplist(replace_front_back, Lines1, Lines),
    add_tops(Lines),
    add_rights_others(Lines).


% Loading lines of characters from file
lines_from_file(Path, Lines) :-
    phrase_from_file(lines(Lines1), Path),
    maplist(maplist(code_char), Lines1, Lines).


lines([])           --> eos, !.
lines([Line|Lines]) --> line(Line), lines(Lines).


line([])     --> eol, !.
line([C|Cs]) --> [C], line(Cs).


code_char(N,C) :- char_code(C,N).


% Assert the board facts

add_rights_others(Ls) :-
    length(Ls, Ny),
    findall(Y, between(1, Ny, Y), Ys),
    maplist(add_rights1, Ls, Ys),
    maplist(add_others, Ls, Ys).


add_rights1(Cs, Y) :-
    foldl(add_right(Y), Cs, 0-'#', _).


add_right(Y, C2, X-C1, X1-C2) :-
    X1 is X + 1,
    ((is_floor(C1), is_floor(C2)) ->
        X1 is X + 1,
        cxry(X, Y, P),
        cxry(X1, Y, Q),   
        assertz(right(P, Q))
    ; true).


add_others(Cs, Y) :-
    length(Cs, Nx),
    findall(X, between(1, Nx, X), Xs),
    maplist(add_other(Y), Xs, Cs).


add_other(Y, X, C) :-
    cxry(X, Y, P),
    (is_floor(C) ->
        assertz(coord(P,X,Y))
    ; true),
    add_items(C, P).


items(['@'-[sokoban], '$'-[box], '.'-[storage], '+'-[storage, sokoban], '*'-[storage, box]]).

add_items(I, Loc) :-
    items(Is),
    foreach(member(I-L, Is),
        foreach(member(A, L), add_item1(A, Loc))
    ).

add_item1(A, Loc) :-
    F =..[A, Loc], assertz(F).


add_tops(Ls) :-
    foldl(add_tops1, Ls, 0-[], _).


add_tops1(C2s, Y-C1s, Y1-C2s) :-
    length(C1s, N1),
    length(C2s, N2),
    Nx is min(N1, N2),
    nfirst(C1s, Nx, D1s),
    nfirst(C2s, Nx, D2s),
    findall(X, between(1, Nx, X), Xs),
    maplist(add_top(Y), Xs, D1s, D2s),
    Y1 is Y + 1.


add_top(Y, X, C1, C2) :-
    ((is_floor(C1), is_floor(C2)) ->
        Y1 is Y + 1,
        cxry(X, Y, P),
        cxry(X, Y1, Q),   
        assertz(top(Q, P))
    ; true).


% Converting empty spaces to walls outside the board
replace_front_back(R, T) :-
    replace_front(R, Q),
    reverse(Q, Qrev),
    replace_front(Qrev, Trev),
    reverse(Trev, T).


replace_front([], []).

replace_front([C|R], ['#'|T]) :-
    (C \== '#' -> replace_front(R, T); T = R).


is_floor(C) :- C \== '#'.


cxry(X, Y, P) :-
    atom_concat(c, X, P1),
    atom_concat(r, Y, P2),
    atom_concat(P1, P2, P).


% Fist N element of list
nfirst(L, N, L1) :-
    length(L1, N),
    append(L1, _, L).


clear_level :-
    retractall(top(_,_)),
    retractall(right(_,_)),
    retractall(coord(_,_,_)),
    retractall(sokoban(_)),
    retractall(box(_)),
    retractall(storage(_)).
