/*
    construct_board(-FilledBoard)

    This predicate helps build the board of the game.
    White pieces are represented by a 1 while Black pieces are represented by a 0.
    The board is a list of lists (matrix).
*/
construct_board(FilledBoard) :- replicate(8, null, Row),
                                replicate(8, Row, Board),
                                repeat,
                                generate_list(0, Res, true),
                                validate_pieces(Res), 
                                partition_list(Res, 4, 6, BoardSides),
                                place_initial_pieces(Board, BoardSides, FilledBoard), !.

/*
    fill_sides(+Idx, +MaxSize, +RightList, +LeftListReversed, +BoardMiddle, -NewBoardMiddle).

    Helper predicate for place_initial_pieces.
    This predicate receives the board, without the top and bottom list and fills the left and right sides and returns it.
*/
fill_sides(MaxSize, MaxSize, _, _, _, []) :- !.
fill_sides(Idx, MaxSize, [RH|RT], [LH|LT], [H|T], NewBoard) :- Idx1 is Idx + 1,
                                                              fill_sides(Idx1, MaxSize, RT, LT, [H|T], NewBoardTail),           
                                                              replace_first_last(H, LH, RH, NewList),
                                                              NewBoard = [NewList|NewBoardTail].

/*
    place_initial_pieces(+Board, +FourSides, -FilledBoard)

    Fills the Mabula board with the constructed perimeter.
    Given a list divided into four sublists (sides), each side of the board is filled.
*/
place_initial_pieces(Board, [Top, Right, Bottom, Left | []], FilledBoard) :- append([null|Top], [null], TopSlice), % Fill top half of Mabula board.
                                                                           length(Board, BoardSize),             % Board is a square
                                                                           ElementToDelete is BoardSize - 1,     % Delete the last element so we only work with the middle part.
                                                                           delete_elem(ElementToDelete, Board, [null,null,null,null,null,null,null,null], [NewBoardHead | NewBoardMiddle]), % Continuation of upper line (Actually removes the element)
                                                                           length(NewBoardMiddle, BoardMiddleSize),
                                                                           reverse(Left, LeftReversed), % Calculate how many lists are in the middle.
                                                                           fill_sides(0, BoardMiddleSize, Right, LeftReversed, NewBoardMiddle, FilledBoardMiddle), % Fill the right side of the Mabula board.
                                                                           append([null|Bottom], [null], BottomHalfTemp),
                                                                           reverse(BottomHalfTemp, BottomHalf), % Fill bottom half of Mabula board.
                                                                           append([TopSlice|FilledBoardMiddle], [BottomHalf], FilledBoard).% Return the filled Mabula board.                                                                  

/*
    validate_whites(+PieceArrangementList, +NumWhitePieces)

    Helper predicate that checks if we have a total of 12 white pieces.
    
    Arguments:
        [Piece|T] -> The list that contains all pieces.
        NumWhitePieces -> Number of white pieces.

    Description:
        This predicate recursively goes down the list and counts only white pieces.
        If more or less than 12 white pieces are found the predicate returns no.
*/
validate_whites([], 12) :- !.
validate_whites([], NumWhitePieces) :- NumWhitePieces =\= 12, !, fail.
validate_whites([Piece | T], NumWhitePieces) :- Piece =:= 1, 
                                               NumWhitePieces1 is NumWhitePieces + 1, 
                                               validate_whites(T, NumWhitePieces1);
                                               Piece =:= 0,
                                               validate_whites(T, NumWhitePieces).

/*
    validate_blacks(+PieceArrangementList, +NumBlackPieces)

    Helper predicate that checks if we have a total of 12 black pieces.
    
    Arguments:
        [Piece|T] -> The list that contains all pieces.
        NumBlackPieces -> Number of black pieces.

    Description:
        This predicate recursively goes down the list and counts only black pieces.
        If more or less than 12 black pieces are found the predicate returns no.
*/
validate_blacks([], 12) :- !.
validate_blacks([], NumBlackPieces) :- NumBlackPieces =\= 12, !, fail.
validate_blacks([Piece | T], NumBlackPieces) :- Piece =:= 0, 
                                               NumBlackPieces1 is NumBlackPieces + 1, 
                                               validate_blacks(T, NumBlackPieces1);
                                               Piece =:= 1,
                                               validate_blacks(T, NumBlackPieces).

/*
    check_last_link(+PieceArrangementList)

    Helper predicate to check if the last link (where the left side and the top side of the Mabula board meet) do not break the rules.
    
    Arguments:
        [H1, H2 | Tail] -> This is the list that contains all pieces. We start by checking the first two elements, if they are the same
                           then that means that the last element cannot be the same as the first two otherwise 3 pieces would be "touching".
                           The same is verified if the last two elements are the same.
*/
check_last_link([H1, H2 | Tail]) :- list_nth(23, [H1, H2 | Tail], Last), ((H1 =\= H2);(H1 =:= H2, H1 =\= Last, H2 =\= Last)),
                   list_nth(22, [H1, H2 | Tail], SencondLast), ((Last =\= SencondLast);(Last =:= SencondLast, H1 =\= Last)).

/*
    validate_pieces(+PieceArrangementList)

    This predicate checks if a given arrangement of marbles is valid or not. It uses the following predicates to check:
    - validate_whites
    - validate_blacks
    - check_last_link
*/
validate_pieces([H1, H2 | Tail]) :- validate_whites([H1, H2 | Tail], 0), validate_blacks([H1, H2 | Tail], 0), check_last_link([H1, H2 | Tail]).

/*
    generate_list(+Size, -Res, +GenWhite)

    Generates the marbles to place around the Mabula Board.
    Also ensures that in each list there are no more than 2 consecutive elements of the same color. 
    This validation has to be done in the end too, for when we connect the beginning of the piece list to its end (not done in this predicate).
*/
generate_list(24, [], _) :- !.
generate_list(Size, Res, GenWhite) :- GenWhite, % Generate white pieces
                                     repeat,
                                     random(1, 3, RandValue), % At max 2 pieces of the same color can be connected in the beginning.
                                     NextSize is Size + RandValue,
                                     NextSize =< 24, !,
                                     replicate(RandValue, 1, WhitePiecesLists),
                                     sw_bool_value(GenWhite, GenBlack),
                                     generate_list(NextSize, ResTail, GenBlack),
                                     append(WhitePiecesLists, ResTail, Res);

                                     sw_bool_value(GenWhite, GenBlack), % Generate black pieces
                                     GenBlack,
                                     repeat,
                                     random(1, 3, RandValue),
                                     NextSize is Size + RandValue,
                                     NextSize =< 24, !,
                                     replicate(RandValue, 0, BlackPiecesList),
                                     generate_list(NextSize, ResTail, GenBlack),
                                     append(BlackPiecesList, ResTail, Res).

/*
    display_row_line(+M)

    Helper predicate. Draws the lines that delimit the fields where the player can place a marble.
*/
display_row_line(0) :- write('|'), nl, !.
display_row_line(M) :- 
            write('|---'),
            M1 is M-1,
            display_row_line(M1).

/*
    display_row_empty(+M, +BoardRow)
    
    Helper predicate. Draws the fields where the player can place a marble.
*/
display_row_empty(0, _) :- write('|'), nl, !.
display_row_empty(M, [1|RT]) :- write('| W '),
                                M1 is M-1,
                                display_row_empty(M1, RT).
display_row_empty(M, [0|RT]) :- write('| B '),
                                M1 is M-1,
                                display_row_empty(M1, RT).
display_row_empty(M, [null|RT]) :- write('|   '),
                                M1 is M-1,
                                display_row_empty(M1, RT).

/*
    display_col_numbers(+M, +MaxCol)

    Helper predicate. Displays the column numbers.
*/
display_col_numbers(MaxCol, MaxCol) :- !.
display_col_numbers(M, MaxCol) :-
            M1 is M + 1 ,
            write('   '),
            write(M1),
            display_col_numbers(M1, MaxCol).

/*
    display_row_number(+N)

    Helper predicate. Displays the number of each row.
*/
display_row_number(0) :- !.
display_row_number(N) :- write(N).

/*
    display_row(+N, +M, +Row)

    Helper predicate. Draws the rows of the board.
*/
display_row(N, M, Row) :-
            write(' '),
            display_row_line(M),
            display_row_number(N),
            display_row_empty(M, Row), !.

/*
    display_board(+N, +M, +Board)

    Draws the board on the screen. Receives the board and its length and height.
*/
display_board(N, M, [Row|Rows]) :-
            display_col_numbers(0, M), nl,
            display_board_cells(N, M, [Row|Rows]).

display_board_cells(0, M, []) :- 
            write(' '), 
            display_row_line(M), nl, !.
display_board_cells(N, M, [Row|Rows]) :-
            N1 is N-1,
            display_row(N, M, Row),
            display_board_cells(N1, M, Rows).

/*
    translate_coords(?DisplayCoordsI, ?DisplayCoordsJ, ?MatrixCoordsI, ?MatrixCoordsJ, +Rows, +Cols)

    Translates between displayed coordinates and the coordinates we use in the matrix.
*/
translate_coords(-1, -1, -1, -1, _, _).
translate_coords(DisplayCoordsI, DisplayCoordsJ, MatrixCoordsI, MatrixCoordsJ, Rows, Cols) :-
    MatrixCoordsI is Rows - DisplayCoordsI,
    MatrixCoordsJ is DisplayCoordsJ - 1.
