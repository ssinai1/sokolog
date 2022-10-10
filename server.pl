:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_head)).
:- use_module(library(http/js_write)).
:- use_module(library(http/http_session)).

:- ['sokoban-astar.pl'].

:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(files, '/clientside', []).

:- html_resource(files('style.css'), []).
:- html_resource(files('client.js'), []).

:- http_handler(files(.), serve_files, [prefix]).


serve_files(Request) :-
    http_reply_from_files('clientside', [cache(false)], Request).

serve_files(Request) :-
	http_404([], Request).


:- http_handler(root(sokoban_gui), sokoban_gui(Method), [method(Method), methods([get, post])]).


% client state : current_sokoban/1, current_box/1, interactive/0, cached_solution/1.

:- dynamic   sessions_created/1.


server(Port) :-
    http_server(http_dispatch, [port(Port)]).


start_svr(Port, FilePath) :-
    retractall(board(_)),
    retractall(sessions_created(_)),
    assertz(sessions_created([])),
    load_level(FilePath, Lines0),
    reverse(Lines0, LinesR),
    remove_outer_lines(LinesR, LinesR1),
    reverse(LinesR1, Lines),
    maplist(maplist(keep_only_wall_floor_storage), Lines, Lines1),
    assertz(board(Lines1)),
    server(Port).


stop_svr(Port) :-
    http_stop_server(Port, []),
    ignore((
        sessions_created(Sessions),
        foreach(member(S, Sessions), http_close_session(S))
    )).


sokoban_gui(post, Request) :-
    http_in_session(SessionID),
    debug(post, 'post: ~p', [SessionID-Request]),
    http_read_json_dict(Request, _{
        action:StrAction,
        need_interactive:StrNeedInteractive
    }),
    string_to_atom(StrAction, Action),
    string_to_atom(StrNeedInteractive, NeedInteractive),
    with_mutex(SessionID, (
        (   \+ http_session_data(current_sokoban(_))
        ->  reset_client_state
        ;   true
        ),
        (   (NeedInteractive == true, \+ http_session_data(interactive))
        ->  reply_json_dict(_{
                status:failed
            }) 
        ;   (   Action == interactive_on
            ->  http_session_retractall(interactive),
                http_session_assert(interactive),
                Status = succeeded
            ;   direction(Action)
            ->  (   assert_move(Action)
                ->  Status = succeeded
                ;   Status = failed
                )
            ;   Action == reset
            ->  http_session_retractall(interactive),
                http_session_assert(interactive),
                reset_locs,
                Status = succeeded
            ;   Action == solve
            ->  http_session_retractall(interactive),
                reset_locs,
                assert_solution(Err),
                (   nonvar(Err)
                ->  http_session_assert(interactive),
                    Status = failed
                ;   Status = succeeded
                )
            ;   Status = succeed
            ),
            http_session_data(cached_solution(Solution)),
            curr_locs(SokobanLoc, BoxLocs),
            (   http_session_data(interactive)
            ->  Interactive = true
            ;   Interactive = false
            ),
            /* (direction(Action) -> format('Status: 404~n'), format('Content-type: text/plain~n~n'), format('Created object as ~q~n', [asd-123]); */
            reply_json_dict(_{
                sokoban:SokobanLoc,
                boxes:BoxLocs,
                solution:Solution,
                interactive:Interactive,
                status:Status
            })
            %)
        )
    )).

sokoban_gui(get, Request) :-
    http_in_session(SessionID),
    debug(get, 'get: ~p', [SessionID-Request]),
    with_mutex(server_mutex, (
        sessions_created(Sessions),
        (   \+ member(SessionID, Sessions)
        ->  retractall(sessions_created(_)),
            assertz(sessions_created([SessionID|Sessions]))
        ;   true
        )
    )),
    board(LBoard),
    reply_html_page(title('Sokoban solver'), [
        \html_requires(files('style.css')),
        h1('Sokoban Solver GUI'),
        div(id='content', [
            div(id='left', [
                div(class='timer hidden', span('Dummy')),
                div(button(onClick='solve()', 'Solve (push optimal)')),
                div(button(onClick='reset()', 'Reset'))
            ]),
            div(id='right', [
                div([id='timer', class='timer'], [
                    span(id='hours', '0:'),
                    span(id='minutes', '00:'),
                    span(id='seconds', '00')
                ]),
                table([id='board'], [\html_board(LBoard)])
            ])
        ]),
        script(src='/clientside/client.js', [])
    ]).


remove_outer_lines([], []).

remove_outer_lines([L|Ls], Out) :-
    board_symbol(wall, W),
    (   member(W, L)
    ->  Out = [L|Ls]
    ;   remove_outer_lines(Ls, Out)
    ).


keep_only_wall_floor_storage(In, Out) :-
    atom_symbols(AtomSyms),
    (   (member(storage-Ss, AtomSyms), member(In, Ss))
    ->  board_symbol(storage, Out), \+ board_symbol(sokoban, Out), \+ board_symbol(box, Out), ! 
    ;   (   board_symbol(wall, In)
        ->  Out = In
        ;   board_symbol(empty_space, Out)
        )
    ).


curr_locs(SokobanLoc, BoxLocs) :-
    http_session_data(current_sokoban(SokobanLoc)),
    findall(Loc, http_session_data(current_box(Loc)), BoxLocs).


reset_locs :-
    http_session_retractall(current_sokoban(_)),
    http_session_retractall(current_box(_)),
    findall(Loc,
        (box(Loc), http_session_assert(current_box(Loc))),
        _
    ),
    sokoban(SokobanLoc),
    http_session_assert(current_sokoban(SokobanLoc)).


reset_client_state :-
    http_session_retractall(interactive),
    http_session_assert(interactive),
    http_session_retractall(cached_solution(_)),
    http_session_assert(cached_solution([])),
    reset_locs.


assert_move(Dir) :-
    curr_locs(SokobanLoc, BoxLocs),
    next_to(SokobanLoc, Dir, NextLoc),
    (http_session_data(current_box(NextLoc))
    ->  (   (next_to(NextLoc, Dir, NextNextLoc), \+ member(NextNextLoc, BoxLocs))
        ->  (   http_session_retract(current_box(NextLoc)),
                http_session_assert(current_box(NextNextLoc)),
                NextLoc1 = NextLoc
            )
        ;   NextLoc1 = SokobanLoc
        )
    ;   NextLoc1 = NextLoc
    ),
    http_session_retract(current_sokoban(SokobanLoc)),
    http_session_assert(current_sokoban(NextLoc1)).


assert_solution(Err) :-
    ignore((http_session_data(cached_solution([])),
        http_session_retractall(cached_solution(_)),
        sokoban_moves(Dirs, Err),
        http_session_assert(cached_solution(Dirs))
    )).


sokoban_moves(Dirs, Err) :-
    (   catch(solution(_, _NumMoves, PlanOneStep, [minimize(box_pushes)]), error(Err, _), debug(err, 'Err:~p', [Err]))
    ->  setof(X, X^box(X), BoxLocs),
        sokoban(StartLoc),
        path_with_push(PlanOneStep, StartLoc, BoxLocs, [], Path),
        reverse(Path, Path1),
        path_to_dirs([StartLoc-false|Path1], Dirs1),
        reverse(Dirs1, Dirs)
    ;   Dirs = [], Err = cannot_solve
    ).


path_with_push([], _, _, Path, Path).

path_with_push([(BoxLoc, Dir, NewBoxLoc)|R], StartLoc, BoxLocs, Q, Path) :-
    next_to(DestLoc, Dir, BoxLoc),
    sokoban_path(StartLoc, DestLoc, BoxLocs, P),
    [_|P0] = P,
    reverse(P0, P1),
    maplist(with_false, P1, F),
    append([BoxLoc-true|F], Q, Q1),
    selectchk(BoxLoc, BoxLocs, BoxLocs1),
    insert_ordered(NewBoxLoc, BoxLocs1, NewBoxLocs),
    path_with_push(R, BoxLoc, NewBoxLocs, Q1, Path).


with_false(E, E-false).


sokoban_path(StartLoc, DestLoc, BoxLocs, Path) :-
    SafeState1 =.. [sokoban_safe_loc, BoxLocs],
    Goal1 =.. [sokoban_goal, DestLoc],
    HFunction1 =.. [h_sokoban, DestLoc],
    astar_path(StartLoc, Path, sokoban_next_loc, SafeState1, HFunction1, Goal1).


path_to_dirs([_], []) :- !.

path_to_dirs([Loc-_, NextLoc-NextFlag|R], [[Dir, NextFlag]|Dirs]) :-
    next_to(Loc, Dir, NextLoc),
    path_to_dirs([NextLoc-NextFlag|R], Dirs).


html_board(LBoard) -->
    { length(LBoard, NumRows), call(trs, Out, NumRows, LBoard, []), ! },
    html(Out).


trs([], _) --> [].

trs([tr(id=RowId, Es2)|Ess], NumRows) -->
    [Es],
    {
        length(Ess,N),
        N1 is NumRows - N,
        string_concat('r', N1, RowId)
    },
    { call(tds2, RowId, Es, Es2) },
    trs(Ess, NumRows).


tds([], _, _, _) --> [].
   
tds([td([id=CellId, Class], E)|Es], RowId, NumCols, Col) -->
    {
        Col1 is Col - 1,
        N1 is NumCols - Col1,
        string_concat('c', N1, ColId),
        string_concat(ColId, RowId, CellId),
        debug(dcg, 'dcg:~p', [NumCols-Col])
    },
    td1(Class, E, CellId),
    tds(Es, RowId, NumCols, Col1).


td1(class=wall, div(' '), _) --> { board_symbol(wall, W) }, [W].

td1(class=storage, div(' '), _) --> { only_storage(S) }, [S].

td1(class=empty, div(' '), CellId) --> { board_symbol(empty_space, S), string_to_atom(CellId, Loc), is_inside(Loc) }, [S].

td1(class=outer, div(' '), CellId) --> { board_symbol(empty_space, S), string_to_atom(CellId, Loc), \+ is_inside(Loc) }, [S].


only_storage(Sym) :-
    board_symbol(storage, Sym), \+ board_symbol(sokoban, Sym), \+ board_symbol(box, Sym), !.


is_inside(Loc) :-
    sokoban(StartLoc),
    connected(StartLoc, Loc, []).


tds2(RowId, In, Out) :-
    length(In, NumCols),
    debug(dcg, 'In:~p', [In]),
    phrase(tds(Out, RowId, NumCols, NumCols), In).
