:- use_module(library(random)).
:- include('./helpers.pl').

/*
    Helper function. Helps build the board of the game,

    White pieces are represented by a 1 while Black pieces are represented by a 0.

    The board is a list of lists (matrix).
    After replicate our board looks like:
    [[], [], [], [], [], [], [], []]
*/
construct_board(Board) :- replicate(8, null, Row),
                          replicate(8,Row,Board),
                          repeat,
                          generateList(0, Res, true),
                          validatePieces(Res), 
                          partitionList(Res, 4, 6, BoardSides),
                          placeInitialPieces(Board, BoardSides, FilledBoard), !,
                          write(FilledBoard), nl. % TODO: Remove Debug

/*
[[null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null]]
*/

constrctRight(List, Element, NewList) :- length(List, Size),
                                         Size1 is Size - 1,
                                         replace(List, Size1, null, Element, NewList).

fillRight(MaxSize, MaxSize, _, _, []) :- !.
fillRight(Idx, MaxSize, [RH|RT], [H|T], NewBoard) :- Idx1 is Idx + 1,
                                                     fillRight(Idx1, MaxSize, RT, [H|T], NewBoardTail),           
                                                     constrctRight(H, RH, NewList), NewBoard = [NewList|NewBoardTail].

%fillBottom(Bottom, FilledBoard) :-
%fillLeft(Left, FilledBoard) :-
%a :- placeInitialPieces([[null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null]], [[1,1,1,1,1,1],[1,1,1,1,1,1],[1,1,1,1,1,1],[1,1,1,1,1,1]], FilledBoardA), write(FilledBoardA). % TODO:FIXME: remove.

placeInitialPieces(Board, [Top, Right, Bottom, Left | []], FilledBoard) :- append([null|Top], [null], TopSlice), % Fill top half of Mabula board.
                                                                           length(Board, BoardSize), % Board is a square
                                                                           ElementToDelete is BoardSize - 1, % Delete the last element so we only work with the middle part.
                                                                           delete_elem(ElementToDelete, Board, [null,null,null,null,null,null,null,null], [NewBoardHead | NewBoardMiddle]), % Continuation of upper line (Actually removes the element)
                                                                           length(NewBoardMiddle, BoardMiddleSize), % Calculate how many lists are in the middle.
                                                                           fillRight(0, BoardMiddleSize, Right, NewBoardMiddle, FilledBoardMiddle), % Fill the right side of the Mabula board.
                                                                           append([TopSlice|FilledBoardMiddle], [[null,null,null,null,null,null,null,null]], FilledBoard). % FIXME: Temporary, remove, left and down are not complete.
                                                                           %fillBottom(Bottom, FilledBoard),
                                                                           %fillLeft(Left, FilledBoard).
                                                                           

/*
    Brief: Helper function that checks if we have a total of 12 white pieces.
    
    Arguments:
        [Piece|T] -> The list that contains all pieces.
        NumWhitePieces -> Number of white pieces.

    Description:
        This function recursively goes down the list and counts only white pieces.
        It's deterministic in the sense that if more than 12 pieces are found the function returns no.
*/
validateWhites([], 12).
validateWhites([Piece | T], NumWhitePieces) :- Piece =:= 1, 
                                               NumWhitePieces1 is NumWhitePieces + 1, 
                                               validateWhites(T, NumWhitePieces1);
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
validateBlacks([], 12).
validateBlacks([Piece | T], NumBlackPieces) :- Piece =:= 0, 
                                               NumBlackPieces1 is NumBlackPieces + 1, 
                                               validateBlacks(T, NumBlackPieces1);
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
                   list_nth(23, [H1, H2 | Tail], SencondLast), ((Last =\= SencondLast);(Last =:= SencondLast, H1 =\= Last)).

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
display_row_empty(0) :- write('|'), nl, !.
display_row_empty(M) :- 
            write('|   '),
            M1 is M-1,
            display_row_empty(M1).

/*
    Helper function.
    Draws the rows of the board.
*/
display_row(M) :-
            display_row_line(M),
            display_row_empty(M), !.

/*
    Draws the board on the screen.
*/
display_board(0, M) :- display_row_line(M), nl, !.
display_board(N, M) :-
            display_row(M),
            N1 is N-1,
            display_board(N1, M).
