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


play_game :- initial_state(_, Board-P1-P2-Color),
             display_game(Board-P1-P2-Color).
             %game_cycle(Board-Player-Color).

game_cycle(GameState-Player) :- game_over(GameState, Winner), !,
                                congratulate(Winner).
game_cycle(GameState-Player):- choose_move(GameState, Player, Move),
                               move(GameState, Move, NewGameState),
                               next_player(Player, NextPlayer),
                               display_game(NewGameState-NextPlayer), !,
                               game_cycle(NewGameState-NextPlayer).

congratulate(playerName) :- write('Congratulations, ', playerName, ' won.'), nl.

executeGame(1) :- play_game, !.
executeGame(2) :- write('Not implemented, option 2'), !.
executeGame(3) :- write('Not implemented, option 3'), !.

initial_state(GameConfig, Board-P1-P2-Color) :-
    cls,
    askForNames(P1, P2),
    cls,
    construct_board(Board).

display_game(Board-P1-P2-Color) :- 
    display_board(8, 8, Board),
    display_player_turn(P1, Color).

display_player_turn(Player, Color) :-
    write(Player),
    write(', what marble do you want to push?'), nl, nl,
    write('Row of the marble:'), 
    repeat,
    read(Row),
    write('Column of the marble:'),
    repeat,
    read(Col), nl,
    write('How many cells do you want to push it?'), nl,
    write('Distance:'),
    repeat,
    read(Dist).

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
    % get_max_value(+Board, +Color, +Visited, +CurrentMax, -MaxValue)
    Iterate over Board and store maximum value.
*/
get_max_value(Board, Color, Visited, CurrentMax, MaxValue) :-
    length(Board, NumRows),
    nth1(1, Board, Row), length(Row, NumCols),
    findall((I, J), (between(1, NumRows, I), between(1, NumCols, J)), Cells),
    get_max_value_helper(Board, Color, Cells, Visited, CurrentMax, MaxValue).

/*
    Helper predicate for get_max_value.
    Recursive predicate to get the maximum size of a color's components.
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

% Push the next piece if it exists
pushNextPiece(Row, _, null, Row). % Stop if the target position is empty (null)
pushNextPiece(Row, TargetIndex, NextPiece, NewRow) :- NextTarget is TargetIndex + 1,          % Determine the next target position
                                                      pushPieces(Row, NextTarget, NextPiece, NewRow). % Recursively push the next piece

% Push pieces starting from TargetIndex
pushPieces(Row, TargetIndex, Piece, NewRow) :- nth0(TargetIndex, Row, NextPiece),  % Get the current piece at TargetIndex
                                               replace(Row, TargetIndex, NextPiece, Piece, TempRow), % Place the new piece at TargetIndex
                                               pushNextPiece(TempRow, TargetIndex, NextPiece, NewRow).

% Move a piece in a row, pushing other pieces if necessary
movePiece(Row, CurrentIndex, 0, Row).
movePiece(Row, CurrentIndex, Distance, NewRow) :- TargetIndex is CurrentIndex + Distance,
                                                  nth0(CurrentIndex, Row, Piece), % Get the piece at CurrentIndex
                                                  Piece \= null,                  % Ensure we are moving a valid piece
                                                  pushPieces(Row, TargetIndex, Piece, TempNewRow), % Push pieces as needed
                                                  replace(TempNewRow, CurrentIndex, Piece, null, NewRow).


moveRowPieces(Board, RowIndex, InitialIndex, 0, NewRow) :- nth0(RowIndex, Board, RowToEdit),
                                                           movePiece(RowToEdit, InitialIndex, 0, NewRow).
moveRowPieces(Board, RowIndex, InitialIndex, DistanceToTravel, NewRow) :- nth0(RowIndex, Board, RowToEdit),
                                                            movePiece(RowToEdit, InitialIndex, 1, TempRow), % We want to move piece with index 0 to index Distance.         
                                                            NextDistanceToTravel is DistanceToTravel - 1,
                                                            NextIndex is InitialIndex + 1,
                                                            replace(Board, RowIndex, RowToEdit, TempRow, NewBoard),
                                                            moveRowPieces(NewBoard, RowIndex, NextIndex, NextDistanceToTravel, NewRow), !.

/*
    DEBUG: Remove

    % Example where the first move if from top to bottom, moves 6 cases.
    applyMove(0-1-6, [[null,1,1,0,1,0,1,null],[0,null,null,null,null,null,null,0],[1,null,null,null,null,null,null,0],[0,null,null,null,null,null,null,1],[0,null,null,null,null,null,null,0],[1,null,null,null,null,null,null,1],[0,null,null,null,null,null,null,0],[null,1,1,0,1,0,1,null]], N).
    Result = [[null,null,1,0,1,0,1,null],[0,null,null,null,null,null,null,0],[1,null,null,null,null,null,null,0],[0,null,null,null,null,null,null,1],[0,null,null,null,null,null,null,0],[1,null,null,null,null,null,null,1],[0,1,null,null,null,null,null,0],[null,1,1,0,1,0,1,null]]

    For move 0-1-5

    Initial
    [[null,1,1,0,1,0,1,null],[0,1,null,null,null,null,null,0],[1,null,null,null,null,null,null,0],[0,null,null,null,null,null,null,1],[0,null,null,null,null,null,null,0],[1,null,null,null,null,null,null,1],[0,null,null,null,null,null,null,0],[null,1,1,0,1,0,1,null]]

    Final
    [[null,null,1,0,1,0,1,null],[0,null,null,null,null,null,null,0],[1,null,null,null,null,null,null,0],[0,null,null,null,null,null,null,1],[0,null,null,null,null,null,null,0],[1,1,null,null,null,null,null,1],[0,1,null,null,null,null,null,0],[null,1,1,0,1,0,1,null]]

    For move 0-1-1

    Initial
    [[null,1,1,0,1,0,1,null],[0,1,null,null,null,null,null,0],[1,1,null,null,null,null,null,0],[0,1,null,null,null,null,null,1],[0,1,null,null,null,null,null,0],[1,1,null,null,null,null,null,1],[0,null,null,null,null,null,null,0],[null,1,1,0,1,0,1,null]]

    Final
    [[null,null,1,0,1,0,1,null],[0,1,null,null,null,null,null,0],[1,1,null,null,null,null,null,0],[0,1,null,null,null,null,null,1],[0,1,null,null,null,null,null,0],[1,1,null,null,null,null,null,1],[0,1,null,null,null,null,null,0],[null,1,1,0,1,0,1,null]]
*/

% Moves from the top slice originate from index 0.
applyMove(0-OldJ-Distance, Board, NewBoard) :- transpose(Board, Columns),
                                               moveRowPieces(Columns, OldJ, 0, Distance, NewRow),
                                               nth0(OldJ, Columns, OldRow),
                                               replace(Columns, OldJ, OldRow, NewRow, CascadedColumns),
                                               transpose(CascadedColumns, NewBoard). %, print(NewBoard). %TODO: remove

% Moves from the bottom slice originate from max index.
applyMove(MaxIndex-OldJ-Distance, Board, NewBoard) :- length(Board, BoardSize),
                                                      MaxIndex is BoardSize - 1,
                                                      reverse(Board, UpsideDownBoard),
                                                      transpose(UpsideDownBoard, Columns),
                                                      moveRowPieces(Columns, OldJ, 0, Distance, NewRow),
                                                      nth0(OldJ, Columns, OldRow),
                                                      replace(Columns, OldJ, OldRow, NewRow, CascadedColumns),
                                                      transpose(CascadedColumns, NewUpsideDownBoard),
                                                      reverse(NewUpsideDownBoard, NewBoard). %, print(NewBoard). %TODO: remove

% Moves from the left slice originate from row index 0.
applyMove(MiddleSliceIndex-0-Distance, Board, NewBoard) :- length(Board, BoardSize),
                                                           moveRowPieces(Board, MiddleSliceIndex, 0, Distance, NewRow),
                                                           nth0(MiddleSliceIndex, Board, OldRow),
                                                           replace(Board, MiddleSliceIndex, OldRow, NewRow, NewBoard). %, print(NewBoard). %TODO: remove

% Moves from the right slice originate from max row index.
applyMove(MiddleSliceIndex-MaxIndex-Distance, Board, NewBoard) :- length(Board, BoardSize),
                                                                  reverseColumns(Board, BoardRowReversed),
                                                                  moveRowPieces(BoardRowReversed, MiddleSliceIndex, 0, Distance, NewRow),
                                                                  nth0(MiddleSliceIndex, BoardRowReversed, OldRow),
                                                                  replace(BoardRowReversed, MiddleSliceIndex, OldRow, NewRow, NewBoardRowReversed),
                                                                  reverseColumns(NewBoardRowReversed, NewBoard). %, print(NewBoard). %TODO: remove

%FIXME: Fix move ?
move(Board-Player-Color, -1-(-1)-(-1), Board).
move(Board-Player-Color, OldI-OldJ-Distance, NewBoard) :- valid_moves(Board-Color, PossibleMoves),
                                                          member(OldI-OldJ-Distance, PossibleMoves),
                                                          applyMove(OldI-OldJ-Distance, Board, NewBoard).

% check_max_marbles() :- 



% Wrapper Function - Needed so maplist can work properlly.
apply_move_to_board(Board, Move, NewBoard) :- applyMove(Move, Board, NewBoard).

% Wrapper Function - % get_max_value(+Board, +Color, +Visited, +CurrentMax, -MaxValue)
get_max_value_of_board(Color, Board, MaxValue) :- get_max_value(Board, Color, [], 0, MaxValue).


/*
    Helper function.
    This function allows the AI to get the best move.
    Algorithm:
        1. Check if there are valid moves (Otherwise we skip this turn)
        2. Using maplist, create all boards where each board is a permutation of the original one. (A move was applied to the board.)
        3. Using maplist we will now get the MaxValue of each board and store it in a new list (MaxValues).
        4. After obtaining MaxValues we will fetch the indexes that contain the biggest values.
        6. Since the max values may occour more than one time we will choose an index randomly.
        5. The index that was obtained gives us the max value position but it also gives the move position that created the board which contains the max value.
           So we get the Move by indexing into AllValidMoves with this index.
*/
get_best_move(Board-Color, [], -1-(-1)-(-1)).
get_best_move(Board-Color, AllValidMoves, Move) :- maplist(apply_move_to_board(Board), AllValidMoves, NewBoards),
                                                   maplist(get_max_value_of_board(Color), NewBoards, MaxValues),
                                                   get_max_values_indexes(MaxValues, MaxValueIndexes),
                                                   random_member(Index, MaxValueIndexes),
                                                   nth0(Index, AllValidMoves, Move).%, write(Index). % TODO: remove


% move() :-
% valid_moves() :-

% value() :-

/*
    choose_move(GameState-Player-Color, Level, Move)
    Receives the current game state and returns a move.
    Algorithm:
        1. First identify whether we are a CPU or a Human Player. This is done using the Level argument.
           If Level is 0 then we are dealing with a Human Player;
           If Level is 1 then we are dealing with a CPU of level one;
           If Level is 2 then we are dealing with a CPU of level two.
        2. For a Human we print the relevant information:
              The name of the player who will need to choose the next move.
              The color of the marbles that the player can move.
        3. For an Ai we will print the current CPU and the move that it chose.
*/
choose_move(Board-Player-Color, 1, Move) :- valid_moves(Board-Color, PossibleMoves),
                                            ((PossibleMoves \== [], random_member(Move, PossibleMoves)); (Move = -1-(-1)-(-1))), !.
choose_move(Board-Player-Color, 2, Move) :- valid_moves(Board-Color, PossibleMoves),
                                            get_best_move(Board-Color, PossibleMoves, Move), !.
choose_move(Board-Player-Color, 0, I-J-Distance) :- Player \== cpu1, Player \== cpu2,
                                                    write('Use -1 -1 -1 to skip this round.'),nl,
                                                    format('~w, what marble do you want to push?', [Player]), nl,nl,
                                                    write('Row of the marble: '), read(I), nl,
                                                    write('Column of the marble: '), read(J), nl, nl,
                                                    write('How far do you want to push it?'), nl,nl,
                                                    write('Number of squares to push the marble: '), read(Distance), nl, nl.

/*
    valid_moves(+GameState, -ListOfMoves)
    Receives the current game state and returns a list of all possible valid moves.
    Algorithm:
        1. Identify all marbles of the current player
        2. For each edge marble, simulate pushing it in all possible directions, and verify:
            - No marble is pushed off the edge
*/
valid_moves(Board-Color, ListOfMoves) :-
    length(Board, N),
    get_edge_marbles(Board, Color, EdgeMarblesPos),
    get_all_edge_moves(Board, Color, EdgeMarblesPos, AllEdgeMoves),
    write('AllEdgeMoves='),
    write(AllEdgeMoves), nl, nl, % TODO: Remove (DEBUG)
    get_valid_edge_moves(Board, N, AllEdgeMoves, ListOfMoves),
    write('ListOfMoves='),
    write(ListOfMoves).

move(Board, Move, NewBoard) :- true. % TODO: Remove (DEBUG) placeholder



/*
    get_valid_edge_moves(+Board, +N, +AllEdgeMoves, -ListOfMoves)
    Given a List of all possible moves, filter only the valid ones.
*/
get_valid_edge_moves(_, _, [], []). % No moves left
get_valid_edge_moves(Board, N, [Move | RestMoves], [Move | ValidMoves]) :- % Move from the top edge
    is_top_edge(Move), % Check if the move is from the top edge
    applyMove(Move, Board, BoardAfter), % Simulate move
    valid_move(bottom, Board, BoardAfter), 
    !,
    get_valid_edge_moves(Board, N, RestMoves, ValidMoves).
get_valid_edge_moves(Board, N, [Move | RestMoves], [Move | ValidMoves]) :- % Move from the right edge
    MaxIndex is N - 1,
    is_right_edge(Move, MaxIndex), % Check if the move is from the right edge
    applyMove(Move, Board, BoardAfter), % Simulate move
    valid_move(left, Board, BoardAfter),
    !, 
    get_valid_edge_moves(Board, N, RestMoves, ValidMoves).
get_valid_edge_moves(Board, N, [Move | RestMoves], [Move | ValidMoves]) :- % Move from the bottom edge
    MaxIndex is N - 1,
    is_bottom_edge(Move, MaxIndex), % Check if the move is from the bottom edge
    applyMove(Move, Board, BoardAfter), % Simulate move
    valid_move(top, Board, BoardAfter),
    !, 
    get_valid_edge_moves(Board, N, RestMoves, ValidMoves).
get_valid_edge_moves(Board, N, [Move | RestMoves], [Move | ValidMoves]) :- % Move from the left edge
    is_left_edge(Move), % Check if the move is from the left edge
    applyMove(Move, Board, BoardAfter), % Simulate move
    valid_move(right, Board, BoardAfter),
    !,
    get_valid_edge_moves(Board, N, RestMoves, ValidMoves).
get_valid_edge_moves(Board, N, [_ | RestMoves], ValidMoves) :- % Invalid move, skip it
    get_valid_edge_moves(Board, N, RestMoves, ValidMoves).


/*
    get_board_edge(+Board, +Dir, -TopEdge)
    Given a board and the edge we want (top, bottom, left or right) return the corresponding Edge.
*/
get_board_edge(Board, top, TopEdge) :- Board = [TopEdge | _].
get_board_edge(Board, bottom, BottomEdge) :- last(Board, BottomEdge).
get_board_edge(Board, left, LeftEdge) :- findall(FirstElement, member([FirstElement | _], Board), LeftEdge).
get_board_edge(Board, right, RightEdge) :- findall(RightElement, (member(Row, Board), last(Row, RightElement)), RightEdge).

/*
    is_top_edge(+Move)
    Checks if a given move is from the top edge.
*/
is_top_edge(0-_-_) :- true.

/*
    is_right_edge(+Move, +MaxIndex)
    Checks if a given move is from the right edge.
*/
is_right_edge(_-MaxIndex-_, MaxIndex) :- true.

/*
    is_bottom_edge(+Move, +MaxIndex)
    Checks if a given move is from the bottom edge.
*/
is_bottom_edge(MaxIndex-_-_, MaxIndex) :- true.

/*
    is_left_edge(+Move)
    Checks if a given move is from the left edge.
*/
is_left_edge(_-0-_) :- true.

/*
    count_nulls(+EdgeList, -Count)
    Helper predicate to count the number of nulls in a list.
*/
count_nulls([], 0) :- !. 
count_nulls([null | Tail], Count) :-
    count_nulls(Tail, TailCount),  
    Count is TailCount + 1, !. 
count_nulls([_ | Tail], Count) :-
    count_nulls(Tail, Count).

/*
    valid_move(+Dir, +BoardBefore, +BoardAfter)
    Checks if a move is valid by comparing the number of nulls on the opposite edge before and after the move.
    A marble cannot push or be pushed to the perimeter. Once it leaves the perimeter it cannot not go back.
*/
valid_move(Dir, BoardBefore, BoardAfter) :-
    get_board_edge(BoardBefore, Dir, BottomBefore),
    get_board_edge(BoardAfter, Dir, BottomAfter),
    count_nulls(BottomBefore, NullCountBefore),
    count_nulls(BottomAfter, NullCountAfter), !,
    NullCountBefore == NullCountAfter.

/*
    generate_moves(+N, +(I, J), -Moves)
    Helper predicate for get_all_edge_moves. This predicate constructs a moves with the structure I-J-Dist.
*/
generate_moves(N, (I, J), Moves) :-
    MaxN is N-2,
    findall(I-J-Dist, between(1, MaxN, Dist), Moves).

/*
    get_all_edge_moves(+Board, +CurrentPlayer, +EdgeMarblesPos, -AllEdgeMoves)
    Given all the edge marble positions of the current player, this predicate generates all possible moves (valid and invalid).
*/
get_all_edge_moves(Board, CurrentPlayer, EdgeMarblesPos, AllEdgeMoves) :-
    length(Board, N),
    findall(Move, (member(Pos, EdgeMarblesPos), generate_moves(N, Pos, Moves), member(Move, Moves)), AllEdgeMoves).

/*
    get_edge_marbles(+Board, +CurrentPlayer, -EdgeMarblesPos)
    Returns all edge marble positions for the current player.
*/
get_edge_marbles(Board, CurrentPlayer, EdgeMarblesPos) :-
    edge_positions(Board, EdgePositions),  % Get all top edge positions
    findall((Row, Col), (member((Row, Col), EdgePositions), get_element(Board, Row, Col, Color), Color == CurrentPlayer), EdgeMarblesPos).

/*
    edge_positions(+Board, -EdgePositions)
    Helper predicate for get_edge_marbles.
    Returns a lists of all edge positions in the board (Top edges, right edges, bottom edges, left edges).
*/
edge_positions(Board, EdgePositions) :-
    length(Board, N), % Get the number of rows
    MaxIndex is N - 1,  % Calculate maximum row index
    findall((Row, Col),
            (between(0, MaxIndex, Row), between(0, MaxIndex, Col), % Iterate over all positions
             (Row == 0; Row == MaxIndex; Col == 0; Col == MaxIndex) ),
            EdgePositions).
