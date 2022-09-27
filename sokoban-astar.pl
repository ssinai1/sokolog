
% Only transpose/2 was used
:- use_module(library(clpfd)).

:- ['level-import.pl'].
:- ['sokoban-common.pl'].
:- ['astar.pl'].


% A solution with the shortest number of one-step box moves
solution(FilePath, Plan, NumMoves) :-
    abolish_all_tables,
    load_level(FilePath),
    setof(X, X^box(X), BoxLocs),
    sokoban(SokobanLoc),
    canonical_sokoban(SokobanLoc, NewSokobanLoc, BoxLocs),
    astar_path((NewSokobanLoc, BoxLocs), P, next_state, puzzle_safe_state, h_puzzle2, puzzle_goal),
    states_to_plan(P, Plan1),
    simplify_plan(Plan1, Plan),
    length(Plan1, NumMoves),
    write('Plan (standard rule): '), write(Plan), nl,
    write('Number of moves: '), write(NumMoves), nl.


movement(Xs, Ys, (X, Dir, Y)) :-
    select(X, Xs, Xs1),
    \+ member(X, Ys),
    select(Y, Ys, _),
    \+ member(Y, Xs1),
    direction(Dir),
    nexts_to(X, Dir, Y).


states_to_plan([_], []).

states_to_plan([(_, BoxLocs), (_, NextBoxLocs)|R], [M|Ms]) :-
    movement(BoxLocs, NextBoxLocs, M),
    states_to_plan([(_, NextBoxLocs)|R], Ms), !.


simplify_plan(Ps, Ms) :-
    simplify_plan(Ps, [], Ms).


simplify_plan([], Ms, Ms1) :-
    reverse(Ms, Ms1).

simplify_plan([P|Ps], Ms, Qs) :-
    simplify_front([P|Ps], M, Rs),
    simplify_plan(Rs, [M|Ms], Qs).


simplify_front([], none, []).

simplify_front([(X, Dir, Y)], (X, Dir, Y), []).

simplify_front([(X, Dir, Y), (Y1, Dir1, Z)|Ps], M, Rs) :-
    ((Y \== Y1; Dir \== Dir1) ->
        M = (X, Dir, Y), Rs = [(Y1, Dir1, Z)|Ps]
    ; simplify_front([(X, Dir, Z)|Ps], M, Rs)).


puzzle_safe_state(_).


puzzle_goal((_, BoxLocs)) :-
    storages_sorted(BoxLocs).


h_puzzle1((_, BoxLocs), Sum) :-
    sum_min_moves(BoxLocs, Sum).


h_puzzle2((_, BoxLocs), C) :-
    assignment_cost_LB(BoxLocs, C).


next_state((SokobanLoc, BoxLocs)#G0, (NextSokobanLoc, NextBoxLocs)#G) :-
    select(BoxLoc, BoxLocs, BoxLocs1),
    next_to(PushLoc, Dir, BoxLoc),
    \+ member(PushLoc, BoxLocs1),
    next_to(BoxLoc, Dir, NewLoc),
    safe_location(NewLoc, BoxLocs1),
    connected(SokobanLoc, PushLoc, BoxLocs),
    insert_ordered(NewLoc, BoxLocs1, NextBoxLocs),
    canonical_sokoban(BoxLoc, NextSokobanLoc, NextBoxLocs),
    G is G0 + 1.


% If the two states are the same, then the sokoban position is the same
canonical_sokoban(SokobanLoc, NextSokobanLoc, BoxLocs) :-
    (select(BoxLoc, BoxLocs, BoxLocs1),
            direction(Dir),
            next_to(NextSokobanLoc, Dir, BoxLoc),
            \+ member(NextSokobanLoc, BoxLocs1),
            connected(SokobanLoc, NextSokobanLoc, BoxLocs) ->
        true
    ; NextSokobanLoc = SokobanLoc).


sokoban_next_loc(Loc#G0, NextLoc#G) :-
    next_to(Loc, _, NextLoc),
    G is G0 + 1.


sokoban_goal(DestLoc, DestLoc).


sokoban_safe_loc(BoxLocs, Loc) :-
    \+ member(Loc, BoxLocs).


% Manhattan distance heuristics
h_sokoban(DestLoc, Loc, H) :-
    coord(Loc, X1, Y1),
    coord(DestLoc, X2, Y2),
    H is abs(X2 - X1) + abs(Y2 - Y1).


connected(StartLoc, DestLoc, BoxLocs) :-
    SafeState1 =.. [sokoban_safe_loc, BoxLocs],
    Goal1 =.. [sokoban_goal, DestLoc],
    HFunction1 =.. [h_sokoban, DestLoc],
    astar_connected(StartLoc, sokoban_next_loc, SafeState1, HFunction1, Goal1).


% Trivial lower bound assignment cost estimation heuristics
:- table sum_min_moves/3.

sum_min_moves(BoxLocs, Sum) :-
    storages_sorted(Storages),
    findall(C,
        (   member(Loc, BoxLocs),
            min_moves(Loc, C, Storages)
        ),
        Cs
    ),
    sum_list(Cs, Sum).


:- table min_moves/3.

min_moves(BoxLoc, C, Storages) :-
    findall(N,
        (   member(Loc, Storages),
            move_count(BoxLoc, Loc, N)
        ),
        Ns
    ),
    min_list(Ns, C).


% A (potentially) greater lower bound assignment cost estimation heuristics
:- table box_costs/3.

box_costs(StorageLocs, BoxLoc, L) :-
    maplist(move_count(BoxLoc), StorageLocs, L).


boxes_costs(StorageLocs, BoxLocs, Ls) :-
    maplist(box_costs(StorageLocs), BoxLocs, Ls).


subtract_list_value(L, V, P) :-
    Y is - V,
    maplist(plus(Y), L, P).


subtract_lists_mins(Ls, Mins, Ps) :-
    maplist(min_list, Ls, Mins),
    maplist(subtract_list_value, Ls, Mins, Ps).


% Estimated lower bound cost
estimated_cost(Ls, Cost) :-
    subtract_lists_mins(Ls, MinsR, Ps),
    transpose(Ps, Ts),
    maplist(min_list, Ts, MinsC),
    sum_list(MinsR, SumR),
    sum_list(MinsC, SumC),
    Cost is SumR + SumC.


:- table assignment_cost_LB/2.

assignment_cost_LB(BoxLocs, Cost) :-
    storages_sorted(StorageLocs),
    boxes_costs(StorageLocs, BoxLocs, Ls),
    estimated_cost(Ls, Cost).


% Counting moves
:- table move_count/4.

move_count(StartLoc, DestLoc, N) :-
    SafeState1 =.. [safe_loc1, []],
    Goal1 =.. [sokoban_goal, DestLoc],
    NextLoc1 =.. [next_box_loc, []],
    HFunction1 =.. [h_sokoban, DestLoc],
    (astar_path(StartLoc, Path, NextLoc1, SafeState1, HFunction1, Goal1) ->
        length(Path, N1),
        N is N1 - 1
    ; max_moves(N)
    ).


max_moves(1000000).


next_box_loc(BoxLocs, Loc#G0, NextLoc#G) :-
    direction(Dir),
    next_to(Loc, Dir, NextLoc),
    \+ member(NextLoc, BoxLocs),
    G is G0 + 1.


safe_loc1(BoxLocs, Loc) :-
    \+ member(Loc, BoxLocs),
    (\+ corner(Loc); storage(Loc)),
    not_wall_trap(Loc).
