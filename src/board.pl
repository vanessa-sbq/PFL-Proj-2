:- use_module(library(random)).
:- include('./helpers.pl').

/*
    Helper function. Helps build the board of the game,

    White pieces are represented by a 1 while Black pieces are represented by a 0.

    The board is a list of lists (matrix).
    After replicate our board looks like:
    [[], [], [], [], [], [], [], []]
*/
construct_board(FilledBoard) :- replicate(8, null, Row),
                                replicate(8,Row,Board),
                                repeat,
                                generateList(0, Res, true),
                                validatePieces(Res), 
                                partitionList(Res, 4, 6, BoardSides),
                                placeInitialPieces(Board, BoardSides, FilledBoard), !.
                          %write(FilledBoard), nl. % TODO: Remove Debug



/* % TODO: Remove
[[null,1,1,0,1,0,1,null],
[0,null,null,null,null,null,null,0],
[1,null,null,null,null,null,null,0],
[0,null,null,null,null,null,null,1],
[0,null,null,null,null,null,null,0],
[1,null,null,null,null,null,null,1],
[0,null,null,null,null,null,null,0],
[null,1,1,0,1,0,1,null]]

DEBUG: Game ended
[[null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null]]

DEBUG: Left side empty
[[null,1,1,0,1,0,1,null],[null,null,null,null,null,null,null,0],[null,null,null,null,null,null,null,0],[null,null,null,null,null,null,null,1],[null,null,null,null,null,null,null,0],[null,null,null,null,null,null,null,1],[null,null,null,null,null,null,null,0],[null,1,1,0,1,0,1,null]]

FOR DEBUG:
[[null,1,1,0,1,0,1,null],[0,null,null,null,null,null,null,0],[1,null,null,null,null,null,null,0],[0,null,null,null,null,null,null,1],[0,null,null,null,null,null,null,0],[1,null,null,null,null,null,null,1],[0,null,null,null,null,null,null,0],[null,1,1,0,1,0,1,null]]
*/


fillSides(MaxSize, MaxSize, _, _, _, []) :- !.
fillSides(Idx, MaxSize, [RH|RT], [LH|LT], [H|T], NewBoard) :- Idx1 is Idx + 1,
                                                              fillSides(Idx1, MaxSize, RT, LT, [H|T], NewBoardTail),           
                                                              replace_first_last(H, LH, RH, NewList),
                                                              NewBoard = [NewList|NewBoardTail].

placeInitialPieces(Board, [Top, Right, Bottom, Left | []], FilledBoard) :- append([null|Top], [null], TopSlice),                                                                            % Fill top half of Mabula board.
                                                                           length(Board, BoardSize),                                                                                        % Board is a square
                                                                           ElementToDelete is BoardSize - 1,                                                                                % Delete the last element so we only work with the middle part.
                                                                           delete_elem(ElementToDelete, Board, [null,null,null,null,null,null,null,null], [NewBoardHead | NewBoardMiddle]), % Continuation of upper line (Actually removes the element)
                                                                           length(NewBoardMiddle, BoardMiddleSize),
                                                                           reverse(Left, LeftReversed),                                                                          % Calculate how many lists are in the middle.
                                                                           fillSides(0, BoardMiddleSize, Right, LeftReversed, NewBoardMiddle, FilledBoardMiddle),                                   % Fill the right side of the Mabula board.
                                                                           append([null|Bottom], [null], BottomHalfTemp),
                                                                           reverse(BottomHalfTemp, BottomHalf),                                                                       % Fill bottom half of Mabula board.
                                                                           append([TopSlice|FilledBoardMiddle], [BottomHalf], FilledBoard).                                                   % Return the filled Mabula board.
                                                                           

/*
    Brief: Helper function that checks if we have a total of 12 white pieces.
    
    Arguments:
        [Piece|T] -> The list that contains all pieces.
        NumWhitePieces -> Number of white pieces.

    Description:
        This function recursively goes down the list and counts only white pieces.
        It's deterministic in the sense that if more than 12 pieces are found the function returns no.
*/
validateWhites([], 12) :- !.
validateWhites([], NumWhitePieces) :- NumWhitePieces =\= 12, !, fail.
validateWhites([Piece | T], NumWhitePieces) :- Piece =:= 1, 
                                               NumWhitePieces1 is NumWhitePieces + 1, 
                                               validateWhites(T, NumWhitePieces1);
                                               Piece =:= 0,
                                               validateWhites(T, NumWhitePieces).

/*
    Brief: Helper function that checks if we have a total of 12 black pieces.
    
    Arguments:
        [Piece|T] -> The list that contains all pieces.
        NumBlackPieces -> Number of black pieces.

    Description:
        This function recursively goes down the list and counts only black pieces.
        It's deterministic in the sense that if more than 12 pieces are found the function returns no.
*/
validateBlacks([], 12) :- !.
validateBlacks([], NumBlackPieces) :- NumBlackPieces =\= 12, !, fail.
validateBlacks([Piece | T], NumBlackPieces) :- Piece =:= 0, 
                                               NumBlackPieces1 is NumBlackPieces + 1, 
                                               validateBlacks(T, NumBlackPieces1);
                                               Piece =:= 1,
                                               validateBlacks(T, NumBlackPieces).

/*
    Brief: Helper function to check if the last link (where the left side and the top side of the Mabula board meet)
           do not break the rules.
    
    Arguments:
        [H1, H2 | Tail] -> This is the list that contains all pieces. We start by checking the first two elements, if they are the same
                           then that means that the last element cannot be the same as the first two otherwise 3 pieces would be "touching".
                           The same is verified if the last two elements are the same.
*/
checkLastLink([H1, H2 | Tail]) :- list_nth(23, [H1, H2 | Tail], Last), ((H1 =\= H2);(H1 =:= H2, H1 =\= Last, H2 =\= Last)),
                   list_nth(22, [H1, H2 | Tail], SencondLast), ((Last =\= SencondLast);(Last =:= SencondLast, H1 =\= Last)).

/*
    Wrapper Function.
    Helps call the following functions:
        validateWhites
        validateBlacks
        checkLastLink
    
    In the end this function should say if a given arregement of the marbels are correct or not.
*/
validatePieces([H1, H2 | Tail]) :- validateWhites([H1, H2 | Tail], 0), validateBlacks([H1, H2 | Tail], 0), checkLastLink([H1, H2 | Tail]).

/*
    Generates the marbles to place around the Mabula Board.
    Also ensures that the middle lists and that in each lists there is no more than 2 elements that are the same color. A verification for the
    last link is necessary (see checkLastLink function).
*/
generateList(24, [], _) :- !.
generateList(Size, Res, GenWhite) :- GenWhite, % Generate white pieces
                                     repeat,
                                     random(1, 3, RandValue), % At max 2 pieces of the same color can be connected in the beginning.
                                     NextSize is Size + RandValue,
                                     NextSize =< 24, !,
                                     replicate(RandValue, 1, WhitePiecesLists),
                                     sw_bool_value(GenWhite, GenBlack),
                                     generateList(NextSize, ResTail, GenBlack),
                                     append(WhitePiecesLists, ResTail, Res);

                                     sw_bool_value(GenWhite, GenBlack), % Generate white pieces
                                     GenBlack,
                                     repeat,
                                     random(1, 3, RandValue), % At max 2 pieces of the same color can be connected in the beginning.
                                     NextSize is Size + RandValue,
                                     NextSize =< 24, !,
                                     replicate(RandValue, 0, BlackPiecesList),
                                     generateList(NextSize, ResTail, GenBlack),
                                     append(BlackPiecesList, ResTail, Res).


/*
    Helper function.
    Draws the lines that delimit the fields where the player can place a marble.
*/
display_row_line(0) :- write('|'), nl, !.
display_row_line(M) :- 
            write('|---'),
            M1 is M-1,
            display_row_line(M1).

/*
    Helper function.
    Draws the fields where the player can place a marble.
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
    Displays the column numbers
*/
display_col_numbers(MaxCol, MaxCol) :- !.
display_col_numbers(M, MaxCol) :-
            M1 is M + 1 ,
            write('   '),
            write(M1),
            display_col_numbers(M1, MaxCol).

/*
    Displays the number of each row
*/
display_row_number(0) :- !.
display_row_number(N) :- write(N).

/*
    Helper function.
    Draws the rows of the board.
*/
display_row(N, M, Row) :-
            write(' '),
            display_row_line(M),
            display_row_number(N),
            display_row_empty(M, Row), !.

/*
    Draws the board on the screen.
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
translate_coords(DisplayCoordsI, DisplayCoordsJ, MatrixCoordsI, MatrixCoordsJ, Rows, Cols) :-
    MatrixCoordsI is Rows - DisplayCoordsJ,
    MatrixCoordsJ is DisplayCoordsI - 1.

%move() :-