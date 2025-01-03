play_game :- displayOptions(Mode),
             configure_game(Mode, GameConfig),
             initial_state(GameConfig, Board-L1-L2-P1-P2-P1-Color-Level),
             display_game(Board-L1-L2-P1-P2-P1-Color-L1),
             format('It\'s ~w\'s turn!', [P1]), nl, nl,
             game_cycle(Board-L1-L2-P1-P2-P1-Color-L1).

initial_state(L1-L2-P1-P2, Board-L1-L2-P1-P2-P1-0-L1) :- 
    nl, write('Constructing board...'),
    construct_board(Board),
    nl, cls.

display_game(Board-L1-L2-P1-P2-CurrentPlayer-Color-Level) :-
    cls,
    display_board(8, 8, Board).

game_cycle(Board-L1-L2-P1-P2-Player-Color-Level) :- game_over(Board-L1-L2-P1-P2-Player-Color-Level, Winner),
                                                    congratulate(Winner, P1, P2).
game_cycle(Board-L1-L2-P1-P2-Player-Color-Level):- 
                               repeat,
                               choose_move(Board-L1-L2-P1-P2-Player-Color-Level, Level, NewI-NewJ-Distance),
                               length(Board, BoardSize),
                               move(Board-L1-L2-P1-P2-Player-Color-Level, NewI-NewJ-Distance, NewBoard),
                               next_player(L1, L2, P1, P2, Color, NextColor-NextPlayer-NextLevel),
                               display_game(NewBoard-L1-L2-P1-P2-NextPlayer-NextColor-NextLevel), 
                               format('It\'s ~w\'s turn!', [NextPlayer]), nl, nl,
                               game_cycle(NewBoard-L1-L2-P1-P2-NextPlayer-NextColor-NextLevel), !. 

next_player(L1, L2, P1, P2, 0, 1-P2-L2).
next_player(L1, L2, P1, P2, 1, 0-P1-L1).

congratulate(-1, P1, P2) :- format('Tie between ~w and ~w.', [P1, P2]),nl.
congratulate(0, P1, P2) :- format('Congratulations, ~w won.', [P1]),nl.
congratulate(1, P1, P2) :- format('Congratulations, ~w won.', [P2]),nl.

game_over(Board-L1-L2-P1-P2-Player-Color-Level, Winner) :- valid_moves(Board-L1-L2-P1-P2-Player-0-Level, List),
                            valid_moves(Board-L1-L2-P1-P2-Player-1-Level, List2),
                            List == [], List2 == [],
                            check_max_marbles(Board, Winner).

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
    % get_component_value(+Board, +Color, +Visited, +I, +J, -Value)
    Get a value of a component.
*/
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
                                                  Piece \== null,                 % Ensure we are moving a valid piece % FIXME: \= or \==
                                                  pushPieces(Row, TargetIndex, Piece, TempNewRow), !, % Push pieces as needed
                                                  replace(TempNewRow, CurrentIndex, Piece, null, NewRow).


moveRowPieces(Board, RowIndex, InitialIndex, 0, NewRow) :- nth0(RowIndex, Board, RowToEdit),
                                                           movePiece(RowToEdit, InitialIndex, 0, NewRow).
moveRowPieces(Board, RowIndex, InitialIndex, DistanceToTravel, NewRow) :- nth0(RowIndex, Board, RowToEdit),
                                                            movePiece(RowToEdit, InitialIndex, 1, TempRow), % We want to move piece with index 0 to index Distance.         
                                                            NextDistanceToTravel is DistanceToTravel - 1,
                                                            NextIndex is InitialIndex + 1,
                                                            replace(Board, RowIndex, RowToEdit, TempRow, NewBoard),
                                                            moveRowPieces(NewBoard, RowIndex, NextIndex, NextDistanceToTravel, NewRow), !.

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
move(Board-L1-L2-P1-P2-Player-Color-Level, -1-(-1)-(-1), Board) :- !.
move(Board-L1-L2-P1-P2-Player-Color-0, OldI-OldJ-Distance, NewBoard) :- 
                                                          length(Board, N),
                                                          is_valid_move(Board, N, Color, OldI-OldJ-Distance),  % FIXME: An invalid move crashes the game %FIXME: Cut here ?
                                                          applyMove(OldI-OldJ-Distance, Board, NewBoard), !.
move(Board-L1-L2-P1-P2-Player-Color-Level, OldI-OldJ-Distance, NewBoard) :- valid_moves(Board-L1-L2-P1-P2-Player-Color-Level, PossibleMoves),
                                                          member(OldI-OldJ-Distance, PossibleMoves),
                                                          applyMove(OldI-OldJ-Distance, Board, NewBoard), !.

check_max_marbles(Board, WinnerColor) :- get_max_value_of_board(0, Board, MaxBlackValue),
                                         get_max_value_of_board(1, Board, MaxWhiteValue),
                                         ((MaxBlackValue =:= MaxWhiteValue, WinnerColor = -1);(max(MaxBlackValue, MaxWhiteValue, MaxBlackValue), WinnerColor = 0); WinnerColor = 1).

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
choose_move(Board-L1-L2-P1-P2-Player-Color-Level, 1, Move) :- valid_moves(Board-L1-L2-P1-P2-Player-Color-Level, PossibleMoves),
                                            ((PossibleMoves \== [], random_member(Move, PossibleMoves)); (Move = -1-(-1)-(-1))), !.
choose_move(Board-L1-L2-P1-P2-Player-Color-Level, 2, Move) :- 
                                            %format('It`s ~w`s turn!', [Player]), nl,nl,
                                            valid_moves(Board-L1-L2-P1-P2-Player-Color-Level, PossibleMoves),
                                            get_best_move(Board-Color, PossibleMoves, Move), !.
choose_move(Board-L1-L2-P1-P2-Player-Color-Level, 0, I-J-Distance) :- Player \== cpu1, Player \== cpu2,
                                                    length(Board, BoardSize),
                                                    write('Use -1 -1 -1 to skip this round.'),nl,
                                                    format('~w, what ~w marble do you want to push?', [Player, Color]), nl,
                                                    read_integer('Row: ', OldI),
                                                    read_integer('Column: ', OldJ), nl,
                                                    write('How far do you want to push it?'), nl,
                                                    read_integer('Distance: ', Distance), nl,
                                                    translate_coords(OldI, OldJ, I, J, BoardSize, BoardSize).

/*
    valid_moves(+GameState, -ListOfMoves)
    Receives the current game state and returns a list of all possible valid moves.
    Algorithm:
        1. Identify all marbles of the current player
        2. For each edge marble, simulate pushing it in all possible directions, and verify:
            - No marble is pushed off the edge
*/
valid_moves(Board-L1-L2-P1-P2-Player-Color-Level, ListOfMoves) :-
    length(Board, N),
    get_edge_marbles(Board, Color, EdgeMarblesPos),
    get_all_edge_moves(Board, Color, EdgeMarblesPos, AllEdgeMoves),
    %write('AllEdgeMoves='), % TODO: Remove (DEBUG)
    %write(AllEdgeMoves), nl, nl, % TODO: Remove (DEBUG)
    get_valid_edge_moves(Board, N, Color, AllEdgeMoves, ListOfMoves).
    %write('ListOfMoves='), % TODO: Remove (DEBUG)
    %write(ListOfMoves). % TODO: Remove (DEBUG)

is_valid_move(BoardBefore, N, Color, I-J-Distance) :-
    MaxIndex is N - 1,
    get_element(BoardBefore, I, J, Color),  % FIXME: This crashes the game, but has to be checked for human players
    (
        (is_top_edge(I-J-Distance), applyMove(I-J-Distance, BoardBefore, BoardAfter), valid_move(bottom, BoardBefore, BoardAfter));
        (is_right_edge(I-J-Distance, MaxIndex), applyMove(I-J-Distance, BoardBefore, BoardAfter), valid_move(left, BoardBefore, BoardAfter));
        (is_bottom_edge(I-J-Distance, MaxIndex), applyMove(I-J-Distance, BoardBefore, BoardAfter), valid_move(top, BoardBefore, BoardAfter));
        (is_left_edge(I-J-Distance), applyMove(I-J-Distance, BoardBefore, BoardAfter), valid_move(right, BoardBefore, BoardAfter))
    ), !.
is_valid_move(_, _, _, _) :- 
    %write('Invalid'), nl, % TODO: Remove (DEBUG)
    fail.

get_valid_edge_moves(_, _, _, [], []). % No moves left
get_valid_edge_moves(Board, N, Color, [Move | RestMoves], [Move | ValidMoves]) :-
    is_valid_move(Board, N, Color, Move),
    get_valid_edge_moves(Board, N, Color, RestMoves, ValidMoves), !.
get_valid_edge_moves(Board, N, Color, [_ | RestMoves], ValidMoves) :- % Invalid move
    get_valid_edge_moves(Board, N, Color, RestMoves, ValidMoves).

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
generate_moves_helper(_, _, 0, []).
generate_moves_helper(I, J, MaxN, [I-J-MaxN|ResTail]) :- I >= 0, J >= 0, MaxN > 0, MaxN1 is MaxN - 1, generate_moves_helper(I, J, MaxN1, ResTail).

generate_moves(N, (I, J), Moves) :-
    MaxN is N-2,
    findall(I-J-Dist, between(1, MaxN, Dist), Moves).

/*
    get_all_edge_moves(+Board, +CurrentPlayer, +EdgeMarblesPos, -AllEdgeMoves)
    Given all the edge marble positions of the current player, this predicate generates all possible moves (valid and invalid).
*/
get_all_edge_moves(Board, CurrentPlayer, EdgeMarblesPos, AllEdgeMoves) :-
    length(Board, N),
    maplist(generate_moves(N), EdgeMarblesPos, Moves), append(Moves, AllEdgeMoves).

wrapper_for_include(Board, Color, (Row, Col)) :- get_element(Board, Row, Col, Color).

/*
    get_edge_marbles(+Board, +CurrentPlayer, -EdgeMarblesPos)
    Returns all edge marble positions for the current player.
*/
get_edge_marbles(Board, CurrentPlayer, EdgeMarblesPos) :-
    edge_positions(Board, EdgePositions),  % Get all top edge positions
    include(wrapper_for_include(Board, CurrentPlayer), EdgePositions, EdgeMarblesPos).
    %findall((Row, Col), (member((Row, Col), EdgePositions), get_element(Board, Row, Col, Color), Color == CurrentPlayer), EdgeMarblesPos).

createTopIndex(0, []).
createTopIndex(MaxIndex, [(0,MaxIndex)|ResTail]) :- MaxIndex > 0, MaxIndex1 is MaxIndex - 1, createTopIndex(MaxIndex1, ResTail).

createBottomIndex(_, 0, []).
createBottomIndex(Row, MaxIndex, [(Row, MaxIndex)|ResTail]) :- Row > 0, MaxIndex > 0, MaxIndex1 is MaxIndex - 1, createBottomIndex(Row, MaxIndex1, ResTail).

createLeftIndex(0, []).
createLeftIndex(MaxIndex, [(MaxIndex, 0)|ResTail]) :- MaxIndex > 0, MaxIndex1 is MaxIndex - 1, createLeftIndex(MaxIndex1, ResTail).

createRightIndex(_, 0, []).
createRightIndex(Row, MaxIndex, [(MaxIndex, Row)|ResTail]) :-  Row > 0, MaxIndex > 0, MaxIndex1 is MaxIndex - 1, createRightIndex(Row, MaxIndex1, ResTail).

/*
    edge_positions(+Board, -EdgePositions)
    Helper predicate for get_edge_marbles.
    Returns a lists of all edge positions in the board (Top edges, right edges, bottom edges, left edges).
*/
edge_positions(Board, EdgePositions) :-
    length(Board, N), % Get the number of rows
    MaxIndex is N - 1,  % Calculate maximum row index
    createTopIndex(MaxIndex, TopEdges),
    createBottomIndex(MaxIndex, MaxIndex, BottomEdges),
    createLeftIndex(MaxIndex, LeftEdges),
    createRightIndex(MaxIndex, MaxIndex, RightEdges),
    append([TopEdges, BottomEdges, LeftEdges, RightEdges], EdgePositions).
