:- use_module(library(aggregate)).
:- use_module(library(pio)).
:- use_module(library('dcg/basics')).


board_symbol(wall, '#').
board_symbol(sokoban, '@').
board_symbol(sokoban, '+').
board_symbol(box, '$').
board_symbol(box, '*').
board_symbol(storage, '.').
board_symbol(storage, '*').
board_symbol(storage, '+').
board_symbol(empty_space, ' ').


% Load a level from file
load_level(FilePath) :-
    load_level(FilePath, _).

load_level(FilePath, Lines) :-
    clear_level,
    lines_from_file(FilePath, Lines),
    maplist(replace_front_back, Lines, Lines1),
    add_tops(Lines1),
    add_rights_others(Lines1).


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
    board_symbol(wall, W),
    foldl(add_right(Y), Cs, 0-W, _).


add_right(Y, C2, X-C1, X1-C2) :-
    X1 is X + 1,
    (   (\+ board_symbol(wall, C1), \+ board_symbol(wall, C2))
    ->  X1 is X + 1,
        cxry(X, Y, P),
        cxry(X1, Y, Q),   
        assertz(right(P, Q))
    ;   true
    ).


add_others(Cs, Y) :-
    length(Cs, Nx),
    findall(X, between(1, Nx, X), Xs),
    maplist(add_other(Y), Xs, Cs).


add_other(Y, X, C) :-
    cxry(X, Y, P),
    (   \+ board_symbol(wall, C)
    ->  assertz(coord(P,X,Y))
    ;   true
    ),
    add_items(C, P).


atom_symbols(AtomSyms) :-
    setof(Atom, Sym^board_symbol(Atom, Sym), Atoms),
    findall(Atom-Syms,
        (member(Atom, Atoms),
        findall(Sym,
            board_symbol(Atom, Sym),
            Syms
        )),
        AtomSyms
    ).


symbol_atoms(SymAtoms) :-
    setof(Sym, Atom^board_symbol(Atom, Sym), Syms),
    findall(Sym-Atoms,
        (member(Sym, Syms),
        findall(Atom,
            board_symbol(Atom, Sym),
            Atoms
        )),
        SymAtoms
    ).


add_items(I, Loc) :-
    symbol_atoms(Is),
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
    ((\+ board_symbol(wall, C1), \+ board_symbol(wall, C2)) ->
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

replace_front([C|R], [W|T]) :-
    board_symbol(wall, W),
    (C == W -> T = R; replace_front(R, T)).


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
    retractall(storage(_)),
    retractall(empty_space(_)),
    retractall(wall(_)).
