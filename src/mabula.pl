askForNames(P1_Name, P2_Name) :- repeat,
                                 write('Hello Player1, please type in your name.'), nl,
                                 write('Name:'),
                                 read(P1_Name), atom(P1_Name), nl,
                                 nl,
                                 repeat,
                                 write('Hello Player2, please type in your name.'), nl,
                                 write('Name:'),
                                 read(P2_Name), atom(P2_Name), nl,
                                 nl.

%addPlayersToGame(P1_Name, P2_Name) :- assert(player(P1_Name)), assert(player(P2_Name)). % FIXME: Remove assert, add player names to gameState.


play_game :- initial_state(GameState-Player),
             display_game(GameState-Player),
             game_cycle(GameState-Player).

game_cycle(GameState-Player) :- game_over(GameState, Winner), !,
                                congratulate(Winner).

game_cycle(GameState-Player):- choose_move(GameState, Player, Move),
                               move(GameState, Move, NewGameState),
                               next_player(Player, NextPlayer),
                               display_game(NewGameState-NextPlayer), !,
                               game_cycle(NewGameState-NextPlayer).

congratulate(playerName) :- write('Congratulations, ', playerName, ' won.'), nl.

executeGame(1) :- cls,
                  askForNames(P1, P2),
%                 addPlayersToGame(P1, P2),
                  construct_board(_NewBoard),
                  cls,
                  display_board(8, 8, _NewBoard).
                  %write('Game not implemented.').

executeGame(2) :- write('Not implemented, option 2'), !.
executeGame(3) :- write('Not implemented, option 3'), !.


%game_over() :-
% Verify if the game has ended -> check if there are marbles on the perimeter
% If game has ended, add a visited attribute to each field and set it to false
% Iterate over board and check components and set fields to visited, check current max and corresponding color (BFS?)

/*
[1,2,3,4] _X4
[2,3,4] _X3
[3,4] _X2
[4] _X1
[] [] -> _X = []

_X1 = [4-false|[]]
_X2 = [3-false|[4-false]]
_X3 = [2-false|[3-false,4-false]]
_X2 = [1-false|[2-false,3-false,4-false]]
_X1 = [1-false|[1-false, 2-false,3-false,4-false]]
NewRow = [1-false, 2-false,3-false,4-false] ?
*/

add_visited_field_helper([], []) :- !.
add_visited_field_helper([Piece|T], NewRow) :- add_visited_field_helper(T, NewRowTail),
                                               NewRow = [Piece-false|NewRowTail].

add_visited_field([], []) :- !.
add_visited_field([Row|T], [NewRow|NT]) :- add_visited_field(T, NT),
                                           add_visited_field_helper(Row, NewRow).

% Check if top side empty.
top_side_all_null([TopRow | _]) :- all_null(TopRow).

% Check if right side empty.
right_side_all_null([]).
right_side_all_null([Row | Rest]) :- last(Row, null),
                                     right_side_all_null(Rest).

% Check if bottom side empty.
bottom_side_all_null(Board) :- last(Board, BottomRow),
                               all_null(BottomRow).

% Check if left side empty.
left_side_all_null([]).
left_side_all_null([[null | _] | Rest]) :- left_side_all_null(Rest).

check_if_game_ended(Board) :- top_side_all_null(Board), right_side_all_null(Board), bottom_side_all_null(Board), left_side_all_null(Board), !.


%check_black_max(Board, MaxBlackValue) :- largest_cluster(Board, 0, MaxBlackValue).

%check_white_max(Board, MaxWhiteValue) :- 

/*
    Get the element of the board, given an I and a J.
    Returns the color of that position (Can be 1, 0 or null).
*/
get_element(Board, I, J, Color) :-
    nth0(I, Board, Row),
    nth0(J, Row, Color).

/*
    Returns the adjacent position, given an I and a J
*/
adjacent(I, J, NI, Y) :- NI is I - 1, NI >= 0. % Up
adjacent(I, J, NI, Y) :- NI is I + 1.          % Down
adjacent(I, J, I, NJ) :- NJ is J - 1, NJ >= 0. % Left
adjacent(I, J, I, NJ) :- NJ is J + 1.          % Right

/*
    Iterate over Board and store maximum value. 
*/
% get_max_value(+Board, +Color, +Visited, -MaxValue)

/*
    Get a value of a component.
*/
% get_component_value(+Board, +Color, +Visited, +I, +J, -Value)
get_component_value(Board, Color, Visited, I, J, Acc, Value) :-
        findall((NI, NJ), (adjacent(I, J, NI, NJ), get_element(Board, NI, NJ, Value), \+ member((NI, NJ), Visited)), Neighbors), % Find the neighbors of the element in position (I, J)
        Acc1 is Acc + 1,
        append([(I, J)], Visited, Visited1),
        get_component_value_helper(Board, Color, Neighbors, Visited1, Acc1, Value).
        %write(Neighbors).

get_component_value_helper(_, _, [], _, Acc, Acc) :- !.
get_component_value_helper(Board, Color, [(NI, NJ)|RestNeigh], Visited, Acc, Value):-
        \+ member((NI, NJ), Visited),
        get_component_value(Board, Color, Visited, NI, NJ, Acc, Value),
        get_component_value_helper(Board, Color, RestNeigh, Visited, Acc, Value).
get_component_value_helper(Board, Color, [(NI, NJ)|RestNeigh], Visited, Acc, Value):-
        get_component_value_helper(Board, Color, RestNeigh, Visited, Acc, Value).


/*
    [[null,0   ,null,null],
     [0   ,0   ,0   ,null],
     [null,0   ,null,null],
     [null,null,null,null]]

     [[null,0],[null,0]]

    [[null,0,null,null],[0,0,0,null],[null,0,null,null],[null,null,null,null]] Connected components are (0,1)(1,0)(1,1)(1,2)(2,1)
*/
% 
% get_



% check_max_marbles() :- 

% initial_state() :-
% display_game() :-
% move() :-
% valid_moves() :-

% value() :-
% choose_move() :-
