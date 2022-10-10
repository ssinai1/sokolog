
% Only transpose/2 was used
:- use_module(library(clpfd)).
:- use_module(library(predicate_options)).

:- ['level-import.pl'].
:- ['sokoban-common.pl'].
:- ['sokoban-astar-common.pl'].


:- predicate_options(solution/4, 4, [minimize(atom)]).

% A solution with the smallest number of box pushes
solution(FilePath, Plan, NumMoves) :-
    load_level(FilePath),
    solution(Plan, NumMoves, _, [minimize(box_pushes)]),
    write('Plan (push optimal): '), write(Plan), nl,
    write('Number of moves: '), write(NumMoves), nl.


solution(Plan, NumMoves, PlanOneStep, Options) :-
    (   option(minimize(sokoban_steps), Options, box_pushes)
    ->  NextState = next_state_s
    ;   NextState = next_state
    ),
    abolish_all_tables,
    setof(X, X^box(X), BoxLocs),
    sokoban(SokobanLoc),
    canonical_sokoban(SokobanLoc, NewSokobanLoc, BoxLocs),
    astar_path((NewSokobanLoc, BoxLocs), P, NextState, puzzle_safe_state, h_puzzle2, puzzle_goal),
    states_to_plan(P, PlanOneStep),
    simplify_plan(PlanOneStep, Plan),
    length(PlanOneStep, NumMoves).


puzzle_safe_state(_).


puzzle_goal((_, BoxLocs)) :-
    storages_sorted(BoxLocs).


h_puzzle1((_, BoxLocs), Sum) :-
    sum_min_moves(BoxLocs, Sum).


h_puzzle2((_, BoxLocs), C) :-
    assignment_cost_LB(BoxLocs, C).


% Edge weight is the number of box pushes (1)
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


% Edge weight is the number of sokoban moves
next_state_s((SokobanLoc, BoxLocs)#G0, (BoxLoc, NextBoxLocs)#G) :-
    select(BoxLoc, BoxLocs, BoxLocs1),
    next_to(PushLoc, Dir, BoxLoc),
    \+ member(PushLoc, BoxLocs1),
    next_to(BoxLoc, Dir, NewLoc),
    safe_location(NewLoc, BoxLocs1),
    insert_ordered(NewLoc, BoxLocs1, NextBoxLocs),
    connected_N(SokobanLoc, PushLoc, BoxLocs, N),
    G is G0 + N + 1.


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
    (   astar_path(StartLoc, Path, NextLoc1, SafeState1, HFunction1, Goal1)
    ->  length(Path, N1),
        N is N1 - 1
    ;   max_moves(N)
    ).


max_moves(1000000).


next_box_loc(BoxLocs, Loc#G0, NextLoc#G) :-
    direction(Dir),
    next_to(Loc, Dir, NextLoc),
    \+ member(NextLoc, BoxLocs),
    G is G0 + 1.


safe_loc1(BoxLocs, Loc) :-
    \+ member(Loc, BoxLocs),
    (   \+ corner(Loc)
    ->  true
    ;   storage(Loc)),
    not_wall_trap(Loc).
