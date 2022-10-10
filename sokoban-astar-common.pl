:- ['astar.pl'].

states_to_plan([_], []).

states_to_plan([(_, BoxLocs), (_, NextBoxLocs)|R], [M|Ms]) :-
    movement(BoxLocs, NextBoxLocs, M),
    states_to_plan([(_, NextBoxLocs)|R], Ms), !.


movement(Xs, Ys, (X, Dir, Y)) :-
    select(X, Xs, Xs1),
    \+ member(X, Ys),
    select(Y, Ys, _),
    \+ member(Y, Xs1),
    direction(Dir),
    nexts_to(X, Dir, Y).


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
    (   (Y \== Y1; Dir \== Dir1)
    ->  M = (X, Dir, Y), Rs = [(Y1, Dir1, Z)|Ps]
    ;   simplify_front([(X, Dir, Z)|Ps], M, Rs)
    ).


% If the two states are the same, then the sokoban position is the same
canonical_sokoban(SokobanLoc, NextSokobanLoc, BoxLocs) :-
    (   (select(BoxLoc, BoxLocs, BoxLocs1),
        direction(Dir),
        next_to(NextSokobanLoc, Dir, BoxLoc),
        \+ member(NextSokobanLoc, BoxLocs1),
        connected(SokobanLoc, NextSokobanLoc, BoxLocs))
    ->  true
    ;   NextSokobanLoc = SokobanLoc
    ).


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


connected_N(StartLoc, DestLoc, BoxLocs, N) :-
    SafeState1 =.. [sokoban_safe_loc, BoxLocs],
    Goal1 =.. [sokoban_goal, DestLoc],
    HFunction1 =.. [h_sokoban, DestLoc],
    astar_path(StartLoc, Path, sokoban_next_loc, SafeState1, HFunction1, Goal1),
    length(Path, N1),
    N is N1 - 1.