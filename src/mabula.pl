/*
    Helper function
    This function handles the boot up process of the game.
    It does the following things:
    
    1. displays the options for human vs human, human vs ai and ai vs ai.
    2. creates a GameConfig Tuple (L1-L2-P1-P2) using the configure_game/2 and the Mode variable previously unified.
    3. calls initial_state/2 which generates a board and returns a GameState Tuple (Board-L1-L2-P1-P2-P1-Color-Level).
    4. displays the current board using display_game/1 predicate.
    5. prints to stdout who's playing in this turn.
    6. calls game_cycle/1 where the game's execution continues.
*/
play_game :- display_options(Mode),
             configure_game(Mode, GameConfig),
             initial_state(GameConfig, Board-L1-L2-P1-P2-P1-Color-Level),
             display_game(Board-L1-L2-P1-P2-P1-Color-L1),
             format('It\'s ~w\'s turn!', [P1]), nl, nl,
             game_cycle(Board-L1-L2-P1-P2-P1-Color-L1).

/*
    Helper function
    The function receives a GameConfig (L1-L2-P1-P2), writes to standard output "Constructing board...", calls construct_board/1 which construct
    a new board (new board inside Board variable) and clears the screem by using the cls/0 predicate.

    Returns a GameState (Board-L1-L2-P1-P2-P1-0-L1) where Board is the newly generated board, L1 and L2 are the levels of P1 and P2 respectively.
        L1 and L2 can have 3 values:
            - 0 => If the respective player is a human.
            - 1 => If the respective player is an Ai of level 1.
            - 2 => If the respective player is an Ai of level 2.
        The last 3 variables may seem unnecessary but they serve a purpose:
            - Third-to-last variable (P1 in the beginning) => The player that is currently playing the game.
            - Second-to-last variable (0 in the beginning) => The color associated with the player that is currently playing the game.
            - Last variable (L1 in the beggining) => The level associated with the player that is currently playing the game.
*/
initial_state(L1-L2-P1-P2, Board-L1-L2-P1-P2-P1-0-L1) :- 
    nl, write('Constructing board...'),
    construct_board(Board),
    nl, cls.

/*
    Wrapper function
    Used to clear the screen and display the current board state. It uses the cls/0 predicate and also display_board/3 which is the main function that
    actually prints the board to standard output.
*/
display_game(Board-L1-L2-P1-P2-CurrentPlayer-Color-Level) :-
    cls,
    display_board(8, 8, Board).

/*
    Main Loop
    This function is responsible for the game cycle. It has two cases:

    1st case:
        Checks if the game is over. If is not then we backtrack and execute the second condition. If the game has ended then game_over returns
        the winner color inside Winner and then we congratulate the player.
        The variable Winner can have 3 different values:
        
        1. -1 if a tie occoured.
        2. 0 if the player who played with the black marbles won.
        3. 1 if the player who played with the white marbles won.

    2nd case:
        This second case is responsible for the game loop. Every time it executes it asks the player for a move using the choose_move/3 predicate
        whether that player is an Ai of level 1 or level 2 or a human, calls move/3 which first validates the move that the player did and if the move
        is valid it applies it to the board and returns a valid permutation of the board otherwise it fails and the repeat inside the game_cycle predicate
        makes the player choose a move yet again.
        After a valid permutation of the board is returned inside the New Board variable we switch players, display the new board, print to standard output
        whose turn it is (using the player's name) and call game_cycle/3 again but this time with the NewBoard and with the last three varaibles (the ones that represent the current player)
        also updated.
*/
game_cycle(Board-L1-L2-P1-P2-Player-Color-Level) :- game_over(Board-L1-L2-P1-P2-Player-Color-Level, Winner),
                                                    congratulate(Winner, P1, P2), !.
game_cycle(Board-L1-L2-P1-P2-Player-Color-Level):- 
                               repeat,
                               choose_move(Board-L1-L2-P1-P2-Player-Color-Level, Level, NewI-NewJ-Distance),
                               length(Board, BoardSize),
                               move(Board-L1-L2-P1-P2-Player-Color-Level, NewI-NewJ-Distance, NewBoard),
                               next_player(L1, L2, P1, P2, Color, NextColor-NextPlayer-NextLevel),
                               display_game(NewBoard-L1-L2-P1-P2-NextPlayer-NextColor-NextLevel), 
                               format('It\'s ~w\'s turn!', [NextPlayer]), nl, nl,
                               think(NextLevel), nl,
                               game_cycle(NewBoard-L1-L2-P1-P2-NextPlayer-NextColor-NextLevel), !. 


/*
    Wrapper function
    Prevents level 0 from having a Thinking... print in the standard output as this is only meant for Ai players.
*/
think(0) :- !.
think(BotLevel) :- write('Thinking...'), !.

/*
    Helper function
    Switches the current player for the next one.
    If the current player is Player1 (P1) then the old variables are L1, P1, 0 and the new player will be Player2 (P2) which will have variables L2, P2, 1.
    The opposite also occours when P2 is the current player.
*/
next_player(L1, L2, P1, P2, 0, 1-P2-L2).
next_player(L1, L2, P1, P2, 1, 0-P1-L1).

/*
    Wrapper function
    Helps write the correct congratulation message in case there was a Tie (First argument will be set to -1), P1 (First argument will be set to 0) won or P2 won (First argument will be set to 1).
*/
congratulate(-1, P1, P2) :- format('Tie between ~w and ~w.', [P1, P2]),nl.
congratulate(0, P1, P2) :- format('Congratulations, ~w won.', [P1]),nl.
congratulate(1, P1, P2) :- format('Congratulations, ~w won.', [P2]),nl.

/*
    Helper function
    The game_over/2 predicate helps check if the game has ended. It does the following calls:

    1. First it calls valid_moves for the black marbles. This allows us to check if the list of moves for black marbles is empty or not.
    2. Then it does the exact same thing but for white marbles.
    3. Since the game inly ends if no more player has moves then we need to check if both of the valid moves lists are empty. If they aren't then the game
       has not ended and thus we fail. Otherwise we move on to checking who's the winner because the game has ended.
    4. To help determine the winner we use the check_max_marbles/2 predicate. This will return the Winner which can either be:
        -1 if a tie occoured.
        0 if the player who played with the black marbles won.
        1 if the player who played with the white marbles won.
       This value is then unified with the value inside the game_over/2 predicate and can therefor be used in other functions.
*/
game_over(Board-L1-L2-P1-P2-Player-Color-Level, Winner) :- valid_moves(Board-L1-L2-P1-P2-Player-0-Level, List),
                            valid_moves(Board-L1-L2-P1-P2-Player-1-Level, List2),
                            List == [], List2 == [],
                            check_max_marbles(Board-L1-L2-P1-P2-Player-Color-Level, Winner).

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
adjacent(I, J, NI, NJ) :- NI is I - 1, NJ is J, NI >= 0.  % Check the position directly above (Up) and ensure it's within bounds.
adjacent(I, J, NI, NJ) :- NI is I + 1, NJ is J.           % Check the position directly below (Down).
adjacent(I, J, NI, NJ) :- NI is I, NJ is J - 1, NJ >= 0.  % Check the position directly to the left (Left) and ensure it's within bounds.
adjacent(I, J, NI, NJ) :- NI is I, NJ is J + 1.           % Check the position directly to the right (Right).

/*
    % get_max_value(+Board, +Color, +Visited, +CurrentMax, -MaxValue)
    Iterate over Board and store the maximum value of the specified color's connected components.
*/
get_max_value(Board, Color, Visited, CurrentMax, MaxValue) :-
    length(Board, NumRows),                                                   % Get the number of rows in the board.
    nth1(1, Board, Row), length(Row, NumCols),                                % Get the number of columns by examining the first row.
    findall((I, J), (between(1, NumRows, I), between(1, NumCols, J)), Cells), % Generate all cell coordinates (I, J).
    get_max_value_helper(Board, Color, Cells, CurrentMax, MaxValue). % Start recursive helper to find the max value.

/*
    Helper predicate for get_max_value.
    Recursive predicate to get the maximum size of a color's components.
*/
get_max_value_helper(_, _, [], MaxValue, MaxValue) :- !.                            % No cells left, return the current max value.
get_max_value_helper(Board, Color, [(I, J) | RestCells], CurrentMax, MaxValue) :-
    get_element(Board, I, J, Color),                                                % Check if it matches the specified color
    bfs_component(Board, Color, (I, J), ComponentValue),                            % Get the ComponentValue with the BFS.
    NewMax is max(CurrentMax, ComponentValue),                                      % Update the maximum value if the component size is larger.
    get_max_value_helper(Board, Color, RestCells, NewMax, MaxValue).            % Continue with the remaining cells.
get_max_value_helper(Board, Color, [(I, J) | RestCells], CurrentMax, MaxValue) :-
    \+ get_element(Board, I, J, Color),                                            % Skip cells that do not match the color.
    get_max_value_helper(Board, Color, RestCells, CurrentMax, MaxValue).           % Continue with the remaining cells.

/*
    % bfs_component(+Board, +Color, +Start, -Visited)
    Perform BFS to find all connected components of the same color as `Start`.
*/
bfs_component(Board, Color, Start, ComponentSize) :-
    bfs(Board, Color, [Start], [], Visited),
    length(Visited, ComponentSize).


/*
    % bfs(+Board, +Color, +Queue, +VisitedSoFar, -Visited)
    Helper predicate to perform BFS using a queue.
*/
bfs(_, _, [], Visited, Visited) :- !. % No more marbles to visit
bfs(Board, Color, [(I, J) | RestQueue], VisitedSoFar, Visited) :-
    \+ member((I, J), VisitedSoFar),               
    get_element(Board, I, J, Color),              
    findall((NI, NJ),
            (adjacent(I, J, NI, NJ),              
             get_element(Board, NI, NJ, Color),  
             \+ member((NI, NJ), VisitedSoFar)),
            Neighbors),
    append(RestQueue, Neighbors, NewQueue),       
    append([(I, J)], VisitedSoFar, NewVisited),   
    bfs(Board, Color, NewQueue, NewVisited, Visited), !. 
bfs(Board, Color, [_ | RestQueue], VisitedSoFar, Visited) :-
    bfs(Board, Color, RestQueue, VisitedSoFar, Visited). % Skip already visited nodes.

/*
    There's mutual recursion happening here (The comments for this function are also located in the push_pieces/4 predicate.)
*/
push_next_piece(Row, _, null, Row).                                                                     % Stop if the target position is empty (null)
push_next_piece(Row, TargetIndex, NextPiece, NewRow) :- NextTarget is TargetIndex + 1,                  % Determine the next target position
                                                      push_pieces(Row, NextTarget, NextPiece, NewRow). % Recursively push the next piece

/*
    This function allows us to push pieces.
     - We first gather the element that is inside the position that we want to place our new piece (NextPiece).
     - Then we call replace which replaces the old piece (NextPiece) pointed by the target index (TargetIndex) by the new piece (Piece).
     - And to make sure we are pushing the old marble (NextPiece) we call push_next_piece which does the following:
        - If the old piece was null TempRow will be the same as Row because the old marble didn't actually exist (null value found). (base case)
        - Or pushes any other pieces using this function until a null value is found. (recursive step)
*/
push_pieces(Row, TargetIndex, Piece, NewRow) :- nth0(TargetIndex, Row, NextPiece),  % Get the current piece at TargetIndex
                                               replace(Row, TargetIndex, NextPiece, Piece, TempRow), % Place the new piece at TargetIndex
                                               push_next_piece(TempRow, TargetIndex, NextPiece, NewRow).


/*
    Helper function
    This function consists of two cases:
    
    The Base Case:
        If the distance is zero then we return the Row with no modifications.

    The Recursive step:
        - We first calculate the target index (TargetIndex) which is a combination of the CurrentIndex of the marble plus one (step-by-step).
        - Then we retrive the piece that we want to move and check if it isn't a null piece.
        - After this we call push_pieces which moves any potential pieces that are inside the target index so that we can free up the target index
          for the piece we are trying to move.
        - After the call is done we just replace the element located at original index by null since we already moved the piece and the one that
          is inside the place pointed by CurrentIndex is just a ghost marble.
*/
move_piece(Row, CurrentIndex, 0, Row).
move_piece(Row, CurrentIndex, Distance, NewRow) :- TargetIndex is CurrentIndex + Distance,
                                                  nth0(CurrentIndex, Row, Piece), % Get the piece at CurrentIndex
                                                  Piece \== null,                 % Ensure we are moving a valid piece
                                                  push_pieces(Row, TargetIndex, Piece, TempNewRow), !, % Push pieces as needed
                                                  replace(TempNewRow, CurrentIndex, Piece, null, NewRow).

/*
    This predicate handles moving a piece in a specific row of the board by a given distance. 
    It consists of two cases:

    The Base Case:
       If the distance is zero then we return the Row with no modifications.

    The Recursive Step:
       To move a piece by a non-zero distance, the game rules require moving it step by step 
       rather than picking the marble up and placing it at the final destination. 
       - First, the function extracts the target row (RowToEdit) from the board.
       - Then, it calls move_piece to shift the piece at InitialIndex by one position to the left,
         resulting in a temporary row (TempRow) with this single move performed.
       - After updating the row, the function recursively calls move_row_pieces to continue moving
         the piece until the desired distance is covered. For each recursive step:
         - We decrement the distance to travel (DistanceToTravel) by one.
         - We increment the index of the piece to be moved (InitialIndex) because it has been shifted when we called move_piece.
         - We update the original row in the board row by the new temporary row (TempRow) which crates the new board (NewBoard).

    This process repeats until the distance to travel reaches zero, at which point the updated row is returned.
*/
move_row_pieces(Board, RowIndex, InitialIndex, 0, NewRow) :- nth0(RowIndex, Board, NewRow).
move_row_pieces(Board, RowIndex, InitialIndex, DistanceToTravel, NewRow) :- nth0(RowIndex, Board, RowToEdit),
                                                            move_piece(RowToEdit, InitialIndex, 1, TempRow), % We want to move piece with index 0 to index Distance.         
                                                            NextDistanceToTravel is DistanceToTravel - 1,
                                                            NextIndex is InitialIndex + 1,
                                                            replace(Board, RowIndex, RowToEdit, TempRow, NewBoard),
                                                            move_row_pieces(NewBoard, RowIndex, NextIndex, NextDistanceToTravel, NewRow), !.


/*
    Case where the move originated from the right slice.
    Moves from the top slice originate from index 0.
    Since the element is already in the Top side of the Row we need to transpose the Board so that a column now becomes a Row.
    After this we can call move_row_pieces which moves the piece at index 0 of the corresponding Row to the next position at index + Distance.
    Then we replace the Old Row by the Newly created Row and reverse the Row again. This last step also creates the NewBoard.
*/
apply_move(0-OldJ-Distance, Board, NewBoard) :- transpose(Board, Columns),
                                               move_row_pieces(Columns, OldJ, 0, Distance, NewRow),
                                               nth0(OldJ, Columns, OldRow),
                                               replace(Columns, OldJ, OldRow, NewRow, CascadedColumns),
                                               transpose(CascadedColumns, NewBoard).

/*
    Case where the move originated from the right slice.
    Moves from the bottom slice originate from max index.
    Since the element is already in the Bottom side of the Row we need to reverse the Board so the Bottom side now becomes the Top side and then
    transpose the Board so that a column now becomes a Row. After this we can call move_row_pieces which moves the piece at
    index 0 of the corresponding Row to the next position at index + Distance.
    Then we replace the Old Row by the Newly created Row and reverse the Row again. This last step also creates the NewBoard.
*/
apply_move(MaxIndex-OldJ-Distance, Board, NewBoard) :- length(Board, BoardSize),
                                                      MaxIndex is BoardSize - 1,
                                                      reverse(Board, UpsideDownBoard),
                                                      transpose(UpsideDownBoard, Columns),
                                                      move_row_pieces(Columns, OldJ, 0, Distance, NewRow),
                                                      nth0(OldJ, Columns, OldRow),
                                                      replace(Columns, OldJ, OldRow, NewRow, CascadedColumns),
                                                      transpose(CascadedColumns, NewUpsideDownBoard),
                                                      reverse(NewUpsideDownBoard, NewBoard).


/*
    Case where the move originated from the right slice.
    Moves from the left slice originate from row index 0.
    Since the element is already in the left side of the Row we do not need to apply any transformation to the board and we can just call
    move_row_pieces which moves the piece at index 0 of the corresponding Row to the next position at index + Distance.
    Then we replace the Old Row by the Newly created Row and reverse the Row again. This last step also creates the NewBoard.
*/
apply_move(MiddleSliceIndex-0-Distance, Board, NewBoard) :- length(Board, BoardSize),
                                                           move_row_pieces(Board, MiddleSliceIndex, 0, Distance, NewRow),
                                                           nth0(MiddleSliceIndex, Board, OldRow),
                                                           replace(Board, MiddleSliceIndex, OldRow, NewRow, NewBoard).


/*
    Case where the move originated from the right slice.
    Moves from the right slice originate from max row index.
    To use the move_row_pieces we need to invert each Row of the Board. This is done by the reverse_columns/2 predicate.
    After this is done we can call move_row_pieces which moves the piece at index 0 of the corresponding Row to the next position at index + Distance.
    Then we replace the Old Row by the Newly created Row and reverse the Row again. This last step also creates the NewBoard.
*/
apply_move(MiddleSliceIndex-MaxIndex-Distance, Board, NewBoard) :- length(Board, BoardSize),
                                                                  reverse_columns(Board, BoardRowReversed),
                                                                  move_row_pieces(BoardRowReversed, MiddleSliceIndex, 0, Distance, NewRow),
                                                                  nth0(MiddleSliceIndex, BoardRowReversed, OldRow),
                                                                  replace(BoardRowReversed, MiddleSliceIndex, OldRow, NewRow, NewBoardRowReversed),
                                                                  reverse_columns(NewBoardRowReversed, NewBoard).

/*
    Helper Function
    This function has 3 cases.
    The first one represents the case where a player does not want to play. It the move provided is -1-(-1)-(-1) then this case gets executed and returns the board exactly has before.
    The second one represents the move predicate for human players. If level is 0 then we are performing a validation on the move first and only then we apply it.
    The third one represent the move predicate for non human players (Ai). This only calls apply_move as the moves that the Ai players execute come from the list returned by the valid_moves predicate.
*/
move(Board-L1-L2-P1-P2-Player-Color-Level, -1-(-1)-(-1), Board) :- !.
move(Board-L1-L2-P1-P2-Player-Color-0, OldI-OldJ-Distance, NewBoard) :- 
                                                          length(Board, N),
                                                          is_valid_move(Board, N, Color, OldI-OldJ-Distance),
                                                          apply_move(OldI-OldJ-Distance, Board, NewBoard), !.
move(Board-L1-L2-P1-P2-Player-Color-Level, OldI-OldJ-Distance, NewBoard) :- Level =\= 0,
                                                          apply_move(OldI-OldJ-Distance, Board, NewBoard), !.

/*
    Helper predicate
    Uses value/3
*/
check_max_marbles(Board-L1-L2-P1-P2-Player-Color-Level, WinnerColor) :- 
                                         value(Board-L1-L2-P1-P2-Player-0-Level, P1, MaxBlackValue), 
                                         value(Board-L1-L2-P1-P2-Player-1-Level, P2, MaxWhiteValue),
                                         ((MaxBlackValue =:= MaxWhiteValue, WinnerColor = -1);(max(MaxBlackValue, MaxWhiteValue, MaxBlackValue), WinnerColor = 0); WinnerColor = 1).

/*
    Wrapper Function
    Allows maplist to work properlly.
*/
apply_move_to_board(Board, Move, NewBoard) :- apply_move(Move, Board, NewBoard).

/*
    Returns a value measuring how good or bad the current game state is.
*/
value(Board-L1-L2-P1-P2-Player-Color-Level, PlayerToCheck, MaxValue) :- get_max_value(Board, Color, [], 0, MaxValue).

/*
    Wrapper for value to use with maplist
*/
wrapper_value(Board-L1-L2-P1-P2-Player-Color-Level, NewBoard, MaxValue) :- value(NewBoard-L1-L2-P1-P2-Player-Color-Level, Player, MaxValue).

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
get_best_move(Board-L1-L2-P1-P2-Player-Color-Level, [], -1-(-1)-(-1)).
get_best_move(Board-L1-L2-P1-P2-Player-Color-Level, AllValidMoves, Move) :- 
                                                   maplist(apply_move_to_board(Board), AllValidMoves, NewBoards),
                                                   maplist(wrapper_value(Board-L1-L2-P1-P2-Player-Color-Level), NewBoards, MaxValues),
                                                   get_max_values_indexes(MaxValues, MaxValueIndexes),
                                                   random_member(Index, MaxValueIndexes),
                                                   nth0(Index, AllValidMoves, Move).

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
                                            valid_moves(Board-L1-L2-P1-P2-Player-Color-Level, PossibleMoves),
                                            get_best_move(Board-L1-L2-P1-P2-Player-Color-Level, PossibleMoves, Move), !.
choose_move(Board-L1-L2-P1-P2-Player-Color-Level, 0, I-J-Distance) :- length(Board, BoardSize),
                                                    write('Use -1 -1 -1 to skip this round.'),nl,
                                                    ((Color =:= 0, ColorName = black);(ColorName = white)), !,
                                                    format('~w, what ~w marble do you want to push?', [Player, ColorName]), nl,
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
    get_valid_edge_moves(Board, N, Color, AllEdgeMoves, ListOfMoves).

/*
    Helper function.
    This function allows for a better time complexity when checking if the move performed by a human player is correct.
    It first checks if the element that the user want to move is the color that he can move by using the get_element/3 predicate. If it isn't then we fail
    and say that the move I-J-Distance is invalid otherwise, we move on.
    Now that we know that the element is the correct color we still need to check if the move is valid. For this we use 4 conditions.
    With the help of the predicates is_top_edge/1, is_right_edge/2, is_bottom_edge/2 and is_left_edge/1 we check where the move originated from, perform the move
    with apply_move and check if this move is valid by using the valid_move/3 predicate.
*/
is_valid_move(BoardBefore, N, Color, I-J-Distance) :-
    MaxIndex is N - 1,
    get_element(BoardBefore, I, J, Color),
    (
        (is_top_edge(I-J-Distance), apply_move(I-J-Distance, BoardBefore, BoardAfter), valid_move(bottom, BoardBefore, BoardAfter));
        (is_right_edge(I-J-Distance, MaxIndex), apply_move(I-J-Distance, BoardBefore, BoardAfter), valid_move(left, BoardBefore, BoardAfter));
        (is_bottom_edge(I-J-Distance, MaxIndex), apply_move(I-J-Distance, BoardBefore, BoardAfter), valid_move(top, BoardBefore, BoardAfter));
        (is_left_edge(I-J-Distance), apply_move(I-J-Distance, BoardBefore, BoardAfter), valid_move(right, BoardBefore, BoardAfter))
    ), !.

/*
    Helper function
    Helps determine the valid edge moves. It has 3 cases.
        The first is the base case where if the list of moves is empty then there are no more valid moves.

        The second is a recursive case where we first check if the move is a valid one by calling the is_valid_move/4 predicate.
        If we fail we move on to the next case where we will deal with invalid moves. Otherwise we call get_valid_edge_moves again so that we check the rest of the moves.
        On the way up we add the valid move (Move) that we found to the list of ValidMoves.

        The third case is a recursive case. It gets called when the move is not valid and just calls the function again but ignores the invalid Move.
*/
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

/*
    Helper function
    Function that generates all possible moves inside a board.
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
    maplist(generate_moves(N), EdgeMarblesPos, Moves), append(Moves, AllEdgeMoves).

/*
    Wrapper function
    This wrapper allows the correct position of arguments so that the include/3 predicate can work correctly by extracting the element inside the list
    which are tuples in this case (Row, Col) and execute this function.
*/
wrapper_for_include(Board, Color, (Row, Col)) :- get_element(Board, Row, Col, Color).

/*
    get_edge_marbles(+Board, +CurrentPlayer, -EdgeMarblesPos)
    Returns all edge marble positions for the current player.
*/
get_edge_marbles(Board, CurrentPlayer, EdgeMarblesPos) :-
    edge_positions(Board, EdgePositions),  % Get all top edge positions
    include(wrapper_for_include(Board, CurrentPlayer), EdgePositions, EdgeMarblesPos).

/*
    Helper function
    Creates all possible indexes for the top slice of a board of size N.
*/
create_top_index(0, []).
create_top_index(MaxIndex, [(0,MaxIndex)|ResTail]) :- MaxIndex > 0, MaxIndex1 is MaxIndex - 1, create_top_index(MaxIndex1, ResTail).

/*
    Helper function
    Creates all possible indexes for the bottom slice of a board of size N.
*/
create_bottom_index(_, 0, []).
create_bottom_index(Row, MaxIndex, [(Row, MaxIndex)|ResTail]) :- Row > 0, MaxIndex > 0, MaxIndex1 is MaxIndex - 1, create_bottom_index(Row, MaxIndex1, ResTail).

/*
    Helper function
    Creates all possible indexes for the left slice of a board of size N.
*/
create_left_index(0, []).
create_left_index(MaxIndex, [(MaxIndex, 0)|ResTail]) :- MaxIndex > 0, MaxIndex1 is MaxIndex - 1, create_left_index(MaxIndex1, ResTail).

/*
    Helper function
    Creates all possible indexes for the right slice of a board of size N.
*/
create_right_index(_, 0, []).
create_right_index(Row, MaxIndex, [(MaxIndex, Row)|ResTail]) :-  Row > 0, MaxIndex > 0, MaxIndex1 is MaxIndex - 1, create_right_index(Row, MaxIndex1, ResTail).

/*
    edge_positions(+Board, -EdgePositions)
    Helper predicate for get_edge_marbles.
    Returns a lists of all edge positions in the board (Top edges, right edges, bottom edges, left edges).
*/
edge_positions(Board, EdgePositions) :-
    length(Board, N), % Get the number of rows
    MaxIndex is N - 1,  % Calculate maximum row index
    create_top_index(MaxIndex, TopEdges),
    create_bottom_index(MaxIndex, MaxIndex, BottomEdges),
    create_left_index(MaxIndex, LeftEdges),
    create_right_index(MaxIndex, MaxIndex, RightEdges),
    append([TopEdges, BottomEdges, LeftEdges, RightEdges], EdgePositions).
