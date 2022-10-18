% A* search algorithm implementation

:- op(400,yfx,'#').    


astar_path(StartNode#Metadata, Path, NextNode, SafeNode, HFunction, FScore, GZero, Goal) :-
    astar_solve(StartNode#Metadata, (Node#FromNode, R), NextNode, SafeNode, HFunction, FScore, GZero, Goal),
    astar_reconstruct_path(Node-_G#_F#_Metadata#FromNode, R, [], Path).


astar_connected(StartNode#Metadata, NextNode, SafeNode, HFunction, FScore, GZero, Goal) :-
    astar_solve(StartNode#Metadata, _, NextNode, SafeNode, HFunction, FScore, GZero, Goal).


astar_solve(StartNode#Metadata, (Node#FromNode, R), NextNode, SafeNode, HFunction, FScore, GZero, Goal) :-
    call(GZero, G0),
    call(FScore, HFunction, StartNode, G0, F),
    empty_assoc(Visited),
    empty_assoc(RestOpen),
    put_assoc(StartNode, Visited, G0#F#Metadata#none, Visited1),
    astar_search(F#StartNode-G0#Metadata#none, RestOpen, Visited1, Node#_G#_F#_Metadata#FromNode, R, NextNode, SafeNode, HFunction, FScore, Goal).


astar_reconstruct_path(Node-_G#_F#_Metadata#none, _, P, [Node|P]) :- !.

astar_reconstruct_path(Node-_G#_F#_Metadata#FromNode, Visited, S, FullPath) :-
    del_assoc(FromNode, Visited, _G2#_F2#_#P, Visited1),
    astar_reconstruct_path(FromNode-_G3#_F3#_#P, Visited1, [Node|S], FullPath), !.


astar_search(F#Node-G#Metadata#FromNode, _RestOpen, Visited, Node#G#F#Metadata#FromNode, Visited, _, _, _, _, Goal) :-
    call(Goal, Node), !.

astar_search(_F#Node-G#Metadata#_FromNode, RestOpen, Visited, FinalState, FinalVisited, NextNode, SafeNode, HFunction, FScore, Goal) :-
    expand(Node#Metadata#G, NextNode, SafeNode, Neighbor_Metadata_Gs),
    process_neighbors(Neighbor_Metadata_Gs, Node, HFunction, FScore, RestOpen, Visited, NewOpen, NewVisited),
    del_min_assoc(NewOpen, K, V, NewOpen1),
    nonvar(V),
    astar_search(K-V, NewOpen1, NewVisited, FinalState, FinalVisited, NextNode, SafeNode, HFunction, FScore, Goal).


expand(Node_Metadata_G0, NextNode, SafeNode, Neighbor_Metadata_Gs) :-
    findall(Neighbor#Metadata#G,
        (   call(NextNode, Node_Metadata_G0, Neighbor#Metadata#G),
            call(SafeNode, Neighbor)
        ), Neighbor_Metadata_Gs).


process_neighbors([], _FromNode, _HFunction, _FScore, Open, Visited, Open, Visited).

process_neighbors([Node#Metadata#G|R], FromNode, HFunction, FScore, Open, Visited, NewOpen, NewVisited) :-
    (   get_assoc(Node, Visited, G0#F0#_Metadata0#_FromNode0)
    ->  process_visited(Node#Metadata#G, FromNode, HFunction, FScore, G0#F0, Open, Visited, NewOpen1, NewVisited1)
    ;   process_unvisited(Node#Metadata#G, FromNode, HFunction, FScore, Open, Visited, NewOpen1, NewVisited1)
    ),
    process_neighbors(R, FromNode, HFunction, FScore, NewOpen1, NewVisited1, NewOpen, NewVisited).


process_visited(Node#Metadata#G, FromNode, HFunction, FScore, G0#F0, Open, Visited, NewOpen1, NewVisited) :-
    (   G @< G0
    ->  call(FScore, HFunction, Node, G, F),
        (   del_assoc(F0#Node, Open, _, Open1)
        ->  true
        ;   Open1 = Open
        ),
        put_assoc(F#Node, Open1, G#Metadata#FromNode, NewOpen1),
        put_assoc(Node, Visited, G#F#Metadata#FromNode, NewVisited)
    ;   NewOpen1 = Open,
        NewVisited = Visited
    ).


process_unvisited(Node#Metadata#G, FromNode, HFunction, FScore, Open, Visited, NewOpen, NewVisited) :-
    call(FScore, HFunction, Node, G, F),
    put_assoc(F#Node, Open, G#Metadata#FromNode, NewOpen),
    put_assoc(Node, Visited, G#F#Metadata#FromNode, NewVisited).
