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

/*
    valid_moves(+GameState, -ListOfMoves)
    Receives the current game state and returns a list of all possible valid moves.
    Algorithm:
        1. Identify all marbles of the current player
        2. For each edge marble, simulate pushing it in all possible directions, and verify:
            - No marble is pushed off the edge
*/
valid_moves(Board-Color, ListOfMoves) :-
    get_edge_marbles(Board, Color, TopEdgeMarblesPos, RightEdgeMarblesPos, BottomEdgeMarblesPos, LeftEdgeMarblesPos),
    get_all_edge_moves(Board, Color, TopEdgeMarblesPos, RightEdgeMarblesPos, BottomEdgeMarblesPos, LeftEdgeMarblesPos, AllEdgeMoves).
    %get_valid_edge_moves(Board, Color, EdgeMarblesPos, AllEdgeMoves, ValidEdgeMoves).
    % TODO: For now this is just getting the edge moves

get_all_edge_moves(Board, CurrentPlayer, TopEdgeMarblesPos, RightEdgeMarblesPos, BottomEdgeMarblesPos, LeftEdgeMarblesPos, AllEdgeMoves) :-
    length(Board, N),
    get_all_top_edge_moves(N, TopEdgeMarblesPos, AllTopEdgeMoves),
    get_all_right_edge_moves(N, RightEdgeMarblesPos, AllRightEdgeMoves),
    get_all_bottom_edge_moves(N, BottomEdgeMarblesPos, AllBottomEdgeMoves),
    get_all_left_edge_moves(N, LeftEdgeMarblesPos, AllLeftEdgeMoves),
    append(AllTopEdgeMoves, AllRightEdgeMoves, TempMoves1),
    append(TempMoves1, AllBottomEdgeMoves, TempMoves2),     
    append(TempMoves2, AllLeftEdgeMoves, AllEdgeMoves).     

% Helper predicate to generate all moves for a single marble position along the top edge
generate_top_moves(N, (I, J), Moves) :-
    findall((I, J)-(NewI, J), (between(1, N, NewI), NewI > I), Moves).

get_all_top_edge_moves(N, TopEdgeMarblesPos, AllTopEdgeMoves) :-
    MaxJ is N-2,
    findall(Move, (member(Pos, TopEdgeMarblesPos), generate_top_moves(MaxJ, Pos, Moves), member(Move, Moves)), AllTopEdgeMoves).

% Helper predicate to generate all moves for a single marble position along the right edge
generate_right_moves(N, (I, J), Moves) :-
    findall((I, J)-(I, NewJ), (between(1, N, NewJ), NewJ < J), Moves).

get_all_right_edge_moves(N, RightEdgeMarblesPos, AllRightEdgeMoves) :-
    findall(Move, (member(Pos, RightEdgeMarblesPos), generate_right_moves(N, Pos, Moves), member(Move, Moves)), AllRightEdgeMoves).

% Helper predicate to generate all moves for a single marble position along the bottom edge
generate_bottom_moves(N, (I, J), Moves) :-
    MaxN is N-1,
    findall((I, J)-(NewI, J), (between(1, MaxN, NewI), NewI < I), Moves).

get_all_bottom_edge_moves(N, BottomEdgeMarblesPos, AllBottomEdgeMoves) :-
    findall(Move, (member(Pos, BottomEdgeMarblesPos), generate_bottom_moves(N, Pos, Moves), member(Move, Moves)), AllBottomEdgeMoves).

% Helper predicate to generate all moves for a single marble position along the left edge
generate_left_moves(N, (I, J), Moves) :-
    MaxN is N-2,
    findall((I, J)-(I, NewJ), (between(1, MaxN, NewJ), NewJ > J), Moves).

get_all_left_edge_moves(N, LeftEdgeMarblesPos, AllLeftEdgeMoves) :-
    findall(Move, (member(Pos, LeftEdgeMarblesPos), generate_left_moves(N, Pos, Moves), member(Move, Moves)), AllLeftEdgeMoves).

/*
    Returns all edge marble positions for the current player.
*/
get_edge_marbles(Board, CurrentPlayer, TopEdgeMarblesPos, RightEdgeMarblesPos, BottomEdgeMarblesPos, LeftEdgeMarblesPos) :-
    edge_positions(Board, TopEdgePositions, RightEdgePositions, BottomEdgePositions, LeftEdgePositions),  % Get all top edge positions
    findall((Row, Col), (member((Row, Col), TopEdgePositions), get_element(Board, Row, Col, Color), Color == CurrentPlayer), TopEdgeMarblesPos),
    findall((Row, Col), (member((Row, Col), RightEdgePositions), get_element(Board, Row, Col, Color), Color == CurrentPlayer), RightEdgeMarblesPos),                     
    findall((Row, Col), (member((Row, Col), BottomEdgePositions), get_element(Board, Row, Col, Color), Color == CurrentPlayer), BottomEdgeMarblesPos),                     
    findall((Row, Col), (member((Row, Col), LeftEdgePositions), get_element(Board, Row, Col, Color), Color == CurrentPlayer), LeftEdgeMarblesPos).                     

/*
    Helper function.
    Returns a lists of all edge positions in the board (Top edges, right edges, bottom edges, left edges).
*/
edge_positions(Board, TopEdgePositions, RightEdgePositions, BottomEdgePositions, LeftEdgePositions) :-
    length(Board, N), % Get the number of rows
    MaxRow is N - 1,  % Calculate maximum row index
    findall((0, Col), between(0, MaxRow, Col), TopEdgePositions), 
    findall((Row, MaxRow), between(0, MaxRow, Row), RightEdgePositions), 
    findall((MaxRow, Col), between(0, MaxRow, Col), BottomEdgePositions), 
    findall((Row, 0), between(0, MaxRow, Row), LeftEdgePositions).

% Generate a move by simulating a push
/* push_move(Board, MarblePos, CurrentPlayer, Move) :-
    direction(Dir), % Define all possible directions (e.g., up, down, left, right)
    simulate_push(Board, MarblePos, Dir, CurrentPlayer, Move).

% Ensure the resulting board is valid after the move
valid_board_after_move(Board, Move) :-
    apply_move(Board, Move, NewBoard),
    \+ violates_rules(NewBoard).

% Rule violations: no more than two adjacent same-color marbles
violates_rules(Board) :-
    adjacent_positions(Board, Pos1, Pos2, Pos3),
    marble_at(Board, Pos1, Color),
    marble_at(Board, Pos2, Color),
    marble_at(Board, Pos3, Color). */
