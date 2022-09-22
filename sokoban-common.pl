:- use_module(library(aggregate)).


direction(up).
direction(down).
direction(left).
direction(right).


:- table next_to/3.

next_to(X, up, Y) :- top(X, Y).
next_to(X, down, Y) :- top(Y, X).
next_to(X, right, Y) :- right(X, Y).
next_to(X, left, Y) :- right(Y, X).


:- table storages_sorted/1.

storages_sorted(StorageLocs) :-
    setof(X, X^storage(X), StorageLocs).


safe_location(Loc, BoxLocs):-
    \+ member(Loc, BoxLocs),
    (corner(Loc) -> storage(Loc); true),
    not_wall_trap(Loc),
    foreach(member(BoxLoc, BoxLocs), \+ stuck(BoxLoc, Loc)).


opposite_of(up, down).
opposite_of(down, up).
opposite_of(left, right).
opposite_of(right, left).


right_turn(up, right).
right_turn(down, left).
right_turn(left, up).
right_turn(right, down).


:- table corner/1.

corner(X) :-
    (\+ top(_, X); \+ top(X, _)),
    (\+ right(_, X); \+ right(X, _)).


nexts_to(X, Dir, Y) :- 
    next_to(X, Dir, Y).

nexts_to(X, Dir, Z) :-
    next_to(X, Dir, Y),
    nexts_to(Y, Dir, Z).


wall_trap(X, Dir) :-
    \+ next_to(X, Dir, _),
    \+ storage(X),
    right_turn(Dir, SideDir),
    foreach(nexts_to(X, SideDir, Y), (\+ next_to(Y, Dir, _), \+ storage(Y))),
    opposite_of(SideDir, SideDir1),
    foreach(nexts_to(X, SideDir1, Y), (\+ next_to(Y, Dir, _), \+ storage(Y))).


:- table not_wall_trap/1.

not_wall_trap(X) :-
    foreach(direction(Dir), \+ wall_trap(X, Dir)).


:- table stuck/2.

stuck(X, Y):-
    (\+ right(X, Y) -> right(Y, X); true),
    (\+ storage(X); \+ storage(Y)),
    (\+ top(X, _); \+ top(_, X)),
    (\+ top(Y, _); \+ top(_, Y)).

stuck(X, Y):-
    (\+ top(X, Y) -> top(Y, X); true),
    (\+ storage(X); \+ storage(Y)),
    (\+ right(X, _); \+ right(_, X)),
    (\+ right(Y, _); \+ right(Y, _)).


insert_ordered(X, [], [X]) :- !.
insert_ordered(X, [Y|Ys], [X, Y|Ys]):-
    X @=< Y, !.
insert_ordered(X, [Y|Ys], [Y|Ordered]):-
    insert_ordered(X, Ys, Ordered).