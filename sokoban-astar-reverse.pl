
% Only transpose/2 was used
:- use_module(library(clpfd)).
:- use_module(library(predicate_options)).

:- ['level-import.pl'].
:- ['sokoban-common.pl'].
:- ['sokoban-astar-common.pl'].


% TODO: unify duplicated code parts somehow

:- predicate_options(solution/4, 4, [minimize(atom)]).


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
    storages_sorted(BoxLocs),
    % In general, we do not know what the allowed sokoban end positions are until we solve the puzzle
    sokoban_end_locations(BoxLocs, SokobanEndLocs),
    findall(
        Path,
        (
            member(SokobanLoc, SokobanEndLocs),
            astar_path((SokobanLoc, BoxLocs), Path, NextState, puzzle_safe_state, h_puzzle2, puzzle_goal)
        ),
        Paths
    ),
    [PFirst|_] = Paths,
    foldl(shortest, Paths, PFirst, P1),
    reverse(P1, P),
    states_to_plan(P, PlanOneStep),
    simplify_plan(PlanOneStep, Plan),
    length(PlanOneStep, NumMoves).


sokoban_end_locations(BoxLocs, SokobanEndLocs) :-
    findall(
        PullLoc,
        (
            select(BoxLoc, BoxLocs, BoxLocs1),
            next_to(BoxLoc, Dir, PullLoc),
            next_to(PullLoc, Dir, NewSokobanLoc),
            \+ member(PullLoc, BoxLocs1),
            \+ member(NewSokobanLoc, BoxLocs1)
        ),
        Locs
    ),
    disconnected_locations(Locs, BoxLocs, [], SokobanEndLocs1),
    maplist(canonical_sokoban_(BoxLocs), SokobanEndLocs1, SokobanEndLocs).


canonical_sokoban_(BoxLocs, SokobanLoc, NewSokobanLoc) :-
    canonical_sokoban(SokobanLoc, NewSokobanLoc, BoxLocs).


unreachable_from(StartLoc, BoxLocs, TestLocs, DestLocs) :-
    findall(
        DestLoc,
        (
            member(DestLoc, TestLocs),
            \+ connected(StartLoc, DestLoc, BoxLocs)
        ),
        DestLocs
    ).


disconnected_locations([], _, Q, Q).

disconnected_locations([P|Ps], BoxLocs, Qs, Ls) :-
    unreachable_from(P, BoxLocs, Ps, Us),
    disconnected_locations(Us, BoxLocs, [P|Qs], Ls).


:- table startlocs_sorted/1.

startlocs_sorted(StartLocs) :-
    setof(X, X^box(X), StartLocs).

 
puzzle_safe_state(_).


puzzle_goal((SokobanLoc, BoxLocs)) :-
    startlocs_sorted(BoxLocs),
    sokoban(SokobanLoc1),
    connected(SokobanLoc, SokobanLoc1, BoxLocs).


h_puzzle1((_, BoxLocs), Sum) :-
    sum_min_moves(BoxLocs, Sum).


h_puzzle2((_, BoxLocs), C) :-
    assignment_cost_LB(BoxLocs, C).

% Edge weight is the number of box pushes (1)
next_state((SokobanLoc, BoxLocs)#G0, (NextSokobanLoc, NextBoxLocs)#G) :-
    select(BoxLoc, BoxLocs, BoxLocs1),
    next_to(BoxLoc, Dir, PullLoc),
    next_to(PullLoc, Dir, NewSokobanLoc1),
    \+ member(PullLoc, BoxLocs1),
    \+ member(NewSokobanLoc1, BoxLocs1),
    insert_ordered(PullLoc, BoxLocs1, NextBoxLocs),
    not_dead_end(NewSokobanLoc1, BoxLocs, NextBoxLocs),
    connected(SokobanLoc, PullLoc, BoxLocs),
    canonical_sokoban(NewSokobanLoc1, NextSokobanLoc, NextBoxLocs),
    G is G0 + 1.

% Edge weight is the number of sokoban moves
next_state_s((SokobanLoc, BoxLocs)#G0, (NextSokobanLoc, NextBoxLocs)#G) :-
    select(BoxLoc, BoxLocs, BoxLocs1),
    next_to(BoxLoc, Dir, PullLoc),
    next_to(PullLoc, Dir, NextSokobanLoc),
    \+ member(PullLoc, BoxLocs1),
    \+ member(NextSokobanLoc, BoxLocs1),
    insert_ordered(PullLoc, BoxLocs1, NextBoxLocs),
    not_dead_end(NextSokobanLoc, BoxLocs, NextBoxLocs),
    connected_N(SokobanLoc, PullLoc, BoxLocs, N),
    G is G0 + N + 1.


% The case of a dead end
not_dead_end(NextSokobanLoc, BoxLocs, NextBoxLocs) :-
    (   next_to(NextSokobanLoc, _, Loc), \+ member(Loc, BoxLocs)
    ->  true
    ;   startlocs_sorted(NextBoxLocs)
    ).


% Trivial lower bound assignment cost estimation heuristics
:- table sum_min_moves/3.

sum_min_moves(BoxLocs, Sum) :-
    startlocs_sorted(StartLocs),
    findall(C,
        (   member(Loc, BoxLocs),
            min_moves(Loc, C, StartLocs)
        ),
        Cs
    ),
    sum_list(Cs, Sum).


:- table min_moves/3.

min_moves(BoxLoc, C, StartLocs) :-
    findall(N,
        (   member(Loc, StartLocs),
            move_count(BoxLoc, Loc, N)
        ),
        Ns
    ),
    min_list(Ns, C).


% A (potentially) greater lower bound assignment cost estimation heuristics
:- table box_costs/3.

box_costs(StartLocs, BoxLoc, L) :-
    maplist(move_count(BoxLoc), StartLocs, L).


boxes_costs(StartLocs, BoxLocs, Ls) :-
    maplist(box_costs(StartLocs), BoxLocs, Ls).


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
    startlocs_sorted(StartLocs),
    boxes_costs(StartLocs, BoxLocs, Ls),
    estimated_cost(Ls, Cost).


% Counting moves
:- table move_count/4.

move_count(StartLoc, DestLoc, N) :-
    Goal1 =.. [sokoban_goal, DestLoc],
    NextLoc1 =.. [next_box_loc, []],
    HFunction1 =.. [h_sokoban, DestLoc],
    (astar_path(StartLoc, Path, NextLoc1, safe_loc1, HFunction1, Goal1) ->
        length(Path, N1),
        N is N1 - 1
    ; max_moves(N)
    ).


max_moves(1000000).


next_box_loc(BoxLocs, Loc#G0, NextLoc#G) :-
    direction(Dir),
    next_to(Loc, Dir, NextLoc),
    next_to(NextLoc, Dir, NextNextLoc),
    \+ member(NextLoc, BoxLocs),
    \+ member(NextNextLoc, BoxLocs),
    G is G0 + 1.


safe_loc1(_).
