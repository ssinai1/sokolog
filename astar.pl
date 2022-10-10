% A* search algorithm implementation

:- op(400,yfx,'#').    


astar_path(StartNode, Path, NextNode, SafeNode, HFunction, Goal) :-
    astar_solve(StartNode, (Node#FromNode, R), NextNode, SafeNode, HFunction, Goal),
    astar_reconstruct_path(Node-_#_#FromNode, R, [], Path).


astar_connected(StartNode, NextNode, SafeNode, HFunction, Goal) :-
    astar_solve(StartNode, _, NextNode, SafeNode, HFunction, Goal).


astar_solve(StartNode, (Node#FromNode, R), NextNode, SafeNode, HFunction, Goal) :-
    f_score(HFunction, StartNode, 0, F),
    empty_assoc(Visited),
    empty_assoc(RestOpen),
    put_assoc(StartNode, Visited, 0#F#none, Visited1),
    astar_search(F#StartNode-0#none, RestOpen, Visited1, Node#_#_#FromNode, R, NextNode, SafeNode, HFunction, Goal).


astar_reconstruct_path(Node-_#_#none, _, P, [Node|P]) :- !.

astar_reconstruct_path(Node-_#_#FromNode, Visited, S, FullPath) :-
    del_assoc(FromNode, Visited, _#_#P, Visited1),
    astar_reconstruct_path(FromNode-_#_#P, Visited1, [Node|S], FullPath), !.


astar_search(F#Node-G#FromNode, _, Visited, Node#G#F#FromNode, Visited, _, _, _, Goal) :-
    call(Goal, Node), !.

astar_search(_#Node-G#_, RestOpen, Visited, FinalState, FinalVisited, NextNode, SafeNode, HFunction, Goal) :-
    expand(Node#G, NextNode, SafeNode, Neighbor_Gs),
    process_neighbors(Neighbor_Gs, Node, HFunction, RestOpen, Visited, NewOpen, NewVisited),
    del_min_assoc(NewOpen, K, V, NewOpen1),
    nonvar(V),
    astar_search(K-V, NewOpen1, NewVisited, FinalState, FinalVisited, NextNode, SafeNode, HFunction, Goal).


expand(Node_G0, NextNode, SafeNode, Neighbor_Gs) :-
    findall(Neighbor#G,
        (   call(NextNode, Node_G0, Neighbor#G),
            call(SafeNode, Neighbor)
        ), Neighbor_Gs).


process_neighbors([], _, _, Open, Visited, Open, Visited).

process_neighbors([Node#G|R], FromNode, HFunction, Open, Visited, NewOpen, NewVisited) :-
    (   get_assoc(Node,Visited,G0#F0#_)
    ->  process_visited(Node#G, FromNode, HFunction, G0#F0, Open, Visited, NewOpen1, NewVisited1)
    ;   process_unvisited(Node#G, FromNode, HFunction, Open, Visited, NewOpen1, NewVisited1)
    ),
    process_neighbors(R, FromNode, HFunction, NewOpen1, NewVisited1, NewOpen, NewVisited).


process_visited(Node#G, FromNode, HFunction, G0#F0, Open, Visited, NewOpen1, NewVisited) :-
    (   G < G0
    ->  f_score(HFunction, Node, G, F),
        (   del_assoc(F0#Node, Open, _, Open1)
        ->  true
        ;   Open1 = Open
        ),
        put_assoc(F#Node, Open1, G#FromNode, NewOpen1),
        put_assoc(Node, Visited, G#F#FromNode, NewVisited)
    ;   NewOpen1 = Open,
        NewVisited = Visited
    ).


process_unvisited(Node#G, FromNode, HFunction, Open, Visited, NewOpen, NewVisited) :-
    f_score(HFunction, Node, G, F),
    put_assoc(F#Node, Open, G#FromNode, NewOpen),
    put_assoc(Node, Visited, G#F#FromNode, NewVisited).


f_score(HFunction, Node, G, F) :-
    call(HFunction, Node, H),
    F is G + H.
