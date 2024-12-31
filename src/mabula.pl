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
adjacent(I, J, NI, NJ) :- NI is I - 1, NJ is J, NI >= 0.  % Up
adjacent(I, J, NI, NJ) :- NI is I + 1, NJ is J.           % Down
adjacent(I, J, NI, NJ) :- NI is I, NJ is J - 1, NJ >= 0.  % Left
adjacent(I, J, NI, NJ) :- NI is I, NJ is J + 1.           % Right

/*
    Iterate over Board and store maximum value.
*/
% get_max_value(+Board, +Color, +Visited, +CurrentMax, -MaxValue)
get_max_value(Board, Color, Visited, CurrentMax, MaxValue) :-
    length(Board, NumRows),
    nth1(1, Board, Row), length(Row, NumCols),
    findall((I, J), (between(1, NumRows, I), between(1, NumCols, J)), Cells),
    get_max_value_helper(Board, Color, Cells, Visited, CurrentMax, MaxValue).

/*
    Helper function.
    Recursive function to get the maximum size of a color's components.
*/
get_max_value_helper(_, _, [], _, MaxValue, MaxValue) :- !.
get_max_value_helper(Board, Color, [(I, J) | RestCells], Visited, CurrentMax, MaxValue) :-
    \+ member((I, J), Visited), % Check if the cell is not visited
    get_element(Board, I, J, Color), % Check if the cell matches the specified color
    get_component_value(Board, Color, Visited, I, J, 0, ComponentValue), % Get the size of the component
    NewMax is max(CurrentMax, ComponentValue), % Update the maximum value
    append([(I, J)], Visited, NewVisited), % Mark the current cell as visited
    get_max_value_helper(Board, Color, RestCells, NewVisited, NewMax, MaxValue).
get_max_value_helper(Board, Color, [(I, J) | RestCells], Visited, CurrentMax, MaxValue) :-
    (member((I, J), Visited); \+ get_element(Board, I, J, Color)), % Skip invalid cells
    get_max_value_helper(Board, Color, RestCells, Visited, CurrentMax, MaxValue).

/*
    Get a value of a component.
*/
% get_component_value(+Board, +Color, +Visited, +I, +J, -Value)
get_component_value(Board, Color, Visited, I, J, Acc, Value) :-
    findall((NI, NJ), 
            (adjacent(I, J, NI, NJ), 
             get_element(Board, NI, NJ, ColorOfElem), 
             \+ member((NI, NJ), Visited), 
             ColorOfElem == Color), 
            Neighbors), % Find the neighbors of the element in position (I, J)
    Acc1 is Acc + 1,
    append([(I, J)], Visited, Visited1),
    get_component_value_helper(Board, Color, Neighbors, Visited1, Acc1, Value).

get_component_value_helper(_, _, [], _, Acc, Acc) :- !.
get_component_value_helper(Board, Color, [(NI, NJ)|RestNeigh], Visited, Acc, Value) :-
    \+ member((NI, NJ), Visited),
    get_component_value(Board, Color, Visited, NI, NJ, Acc, Value1),
    get_component_value_helper(Board, Color, RestNeigh, [ (NI, NJ) | Visited ], Value1, Value), !.
get_component_value_helper(Board, Color, [_|RestNeigh], Visited, Acc, Value) :-
    get_component_value_helper(Board, Color, RestNeigh, Visited, Acc, Value).


/*
    [[null,0   ,null,null],
     [0   ,0   ,0   ,null],
     [null,0   ,null,null],
     [null,null,null,null]]

     [[null,0],[null,0]]

    [[null,0,null,null],[0,0,0,null],[null,0,null,null],[null,null,null,null]] Connected components are (0,1)(1,0)(1,1)(1,2)(2,1)

    [[null,0   ,null,null],[0   ,0   ,null,null],[null,null,null,0   ],[null   ,null   ,null   ,null   ]]

    [[null,0   ,null,null],[0   ,0   ,null,null],[null,0 ,null,0   ],[0   ,0   ,0   ,0   ]]

    findall((NI, NJ), (adjacent(2, 1, NI, NJ), get_element([[null,0   ,null,null],[0   ,0   ,null,null],[null,null,null,0   ],[0   ,0   ,0   ,0   ]], NI, NJ, ColorOfElem), \+ member((NI, NJ), []), ColorOfElem == 0), Neighbors)
    [(1,1)]
*/

% check_max_marbles() :- 

% initial_state() :-
% display_game() :-
% move() :-
% valid_moves() :-

% value() :-
% choose_move() :-
