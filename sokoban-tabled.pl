% Based on the idea of a tabled sokoban solver in B-Prolog from this paper: http://ceur-ws.org/Vol-810/paper-l13.pdf

% Memory in bytes available for tabling - change it, if necessary
:- set_prolog_flag(table_space, 3000000000).

:- ['level-import.pl'].
:- ['sokoban-common.pl'].


% A solution with the shortest number of box moves - using "rook rule"
solution(FilePath, Plan, NumMoves):-
    abolish_all_tables,
    load_level(FilePath),
    setof(X, X^box(X), BoxLocs),
    sokoban(SokobanLoc),
    canonical_sokoban(SokobanLoc, NewSokobanLoc, BoxLocs),
    solve_tabled(NewSokobanLoc, BoxLocs, Plan),
    length(Plan, NumMoves),
    write('Plan (rook rule): '), write(Plan), nl,
    write('Number of moves: '), write(NumMoves), nl.


% Mode-directed tabling for finding shortest path in state space
:-table solve_tabled(+, +, lattice(shortest/3)).

solve_tabled(_, BoxLocs, []):-
    storages_sorted(BoxLocs), !.

solve_tabled(SokobanLoc, BoxLocs, [Move|Moves]):-
    next_state((SokobanLoc, BoxLocs), (NextSokobanLoc, NextBoxLocs), Move),
    solve_tabled(NextSokobanLoc, NextBoxLocs, Moves).


next_state((SokobanLoc, BoxLocs), (NextSokobanLoc, NextBoxLocs), (BoxLoc, Dir, DestLoc)) :-
    select(BoxLoc, BoxLocs, BoxLocs1),
    direction(Dir),
    next_to(PushLoc, Dir, BoxLoc),
    \+ memberchk(PushLoc, BoxLocs1),
    next_to(BoxLoc, Dir, FirstDestLoc),
    safe_location(FirstDestLoc, BoxLocs1),
    connected(PushLoc, SokobanLoc, BoxLocs),
    rook_movement(FirstDestLoc, DestLoc, Dir, BoxLocs1),
    insert_ordered(DestLoc, BoxLocs1, NextBoxLocs),
    canonical_sokoban(BoxLoc, NextSokobanLoc, NextBoxLocs).


canonical_sokoban(SokobanLoc, NextSokobanLoc, BoxLocs) :-
    (   (select(BoxLoc, BoxLocs, BoxLocs1),
        direction(Dir),
        next_to(NextSokobanLoc, Dir, BoxLoc),
        \+ memberchk(NextSokobanLoc, BoxLocs1),
        (   SokobanLoc @< NextSokobanLoc
        ->
            connected(SokobanLoc, NextSokobanLoc, BoxLocs)
        ;   connected(NextSokobanLoc, SokobanLoc, BoxLocs)
        ))
    ->  true
    ;   NextSokobanLoc = SokobanLoc).


% Finding path on the board
:-table connected/3.

connected(X, X, _).

connected(X, Y, BoxLocs):-
    connected(X, Z, BoxLocs),
    next_to(Y, _, Z),
    \+ memberchk(Z, BoxLocs).


rook_movement(Loc, Loc, _, _).

rook_movement(Loc, DestLoc, Dir, BoxLocs):-
    next_to(Loc, Dir, NextLoc),
    safe_location(NextLoc, BoxLocs),
    rook_movement(NextLoc, DestLoc, Dir, BoxLocs).
