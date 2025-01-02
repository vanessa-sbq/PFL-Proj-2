:- include('./game.pl').

/****************** Boards for testing *******************/
board1([[null,1   ,1   ,0   ,1   ,0   ,1   ,null],
        [0   ,null,null,null,null,null,null,0   ],
        [1   ,null,null,null,null,null,null,0   ],
        [0   ,null,null,null,null,null,null,1   ],
        [0   ,null,null,null,null,null,null,0   ],
        [1   ,null,null,null,null,null,null,1   ],
        [0   ,null,null,null,null,null,null,0   ],
        [null,1   ,1   ,0   ,1   ,0   ,1   ,null]]).

board2([[null,1   ,1   ,0   ,1   ,0   ,1   ,null],
        [0   ,null,null,null,null,null,null,0   ],
        [1   ,null,null,null,null,null,null,0   ],
        [0   ,null,null,null,null,null,null,1   ],
        [null,0   ,null,null,null,null,null,0   ],
        [1   ,null,null,null,null,null,null,1   ],
        [0   ,null,null,null,null,null,null,0   ],
        [null,1   ,1   ,0   ,1   ,0   ,1   ,null]]).

board3([[null,1   ,1   ,0   ,1   ,0   ,1   ,null],
        [0   ,null,null,null,null,null,null,0   ],
        [null,1   ,null,null,null,null,null,0   ],
        [0   ,null,null,null,null,null,null,1   ],
        [null,0   ,null,null,null,null,null,0   ],
        [1   ,null,null,null,null,null,null,1   ],
        [0   ,1   ,null,null,null,null,null,0   ],
        [null,null,1   ,0   ,1   ,0   ,1   ,null]]).

board4([[null,null,1   ,0   ,1   ,0   ,1   ,null],
        [0   ,null,null,null,null,null,null,0   ],
        [null,null,null,null,null,null,null,0   ],
        [0   ,null,null,null,null,null,null,1   ],
        [null,1   ,null,null,null,null,null,0   ],
        [1   ,1   ,null,null,null,null,null,1   ],
        [0   ,0   ,null,null,null,null,null,0   ],
        [null,1   ,1   ,0   ,1   ,0   ,1   ,null]]).

board5([[null,1   ,1   ,0   ,1   ,0   ,1   ,null],
        [0   ,null,null,null,null,null,null,0   ],
        [null,1   ,null,null,null,null,null,0   ],
        [0   ,null,null,null,null,null,null,1   ],
        [null,0   ,null,null,null,null,null,0   ],
        [1   ,null,null,null,null,null,null,1   ],
        [0   ,1   ,1   ,null,null,null,null,0   ],
        [null,null,null,0   ,1   ,0   ,1   ,null]]).

% Printing the boards
board1_before :- board1(Board), display_board(8, 8, Board).
board2_before :- board2(Board), display_board(8, 8, Board).
board3_before :- board3(Board), display_board(8, 8, Board).
/*********************************************************/


/********* All edge moves tests (including invalid) **********/    
% Get all the valid moves for player 0
valid_moves_test1 :- board1(Board),
                     valid_moves(Board-0, ListOfMoves),
                     write(ListOfMoves).

% Get all the valid moves for player 0
get_valid_edge_moves_test1 :- board1(Board),
                              get_valid_edge_moves(Board, [0-3-1,0-3-2], ValidMoves),
                              write(ValidMoves).

% get_edge_marbles(Board, Color, TopEdgeMarblesPos, RightEdgeMarblesPos, BottomEdgeMarblesPos, LeftEdgeMarblesPos)
get_edge_marbles_test1 :- board1(Board),
                          get_edge_marbles(Board, 0, EdgeMarblesPos),
                          write('EdgeMarblesPos='),
                          write(EdgeMarblesPos).

% get_all_edge_moves(Board, CurrentPlayer, TopEdgeMarblesPos, RightEdgeMarblesPos, BottomEdgeMarblesPos, LeftEdgeMarblesPos, AllEdgeMoves) 
get_all_edge_moves_test1 :- board1(Board),
                           get_all_edge_moves(Board, 0, [(0,3),(0,5),(1,7),(2,7),(4,7),(6,7),(7,3),(7,5),(1,0),(3,0),(4,0),(6,0)], AllEdgeMoves),
                           write(AllEdgeMoves).
/*************************************************************/    

/******************* Moves tests *********************/    
% Tests for board1
test1_apply_move_6 :- board1(Board), applyMove(0-1-6, Board, _N), display_board(8, 8, _N).
test1_apply_move_5 :- board1(Board), applyMove(0-1-5, Board, _N), display_board(8, 8, _N).
test1_apply_move_4 :- board1(Board), applyMove(0-1-4, Board, _N), display_board(8, 8, _N).
test1_apply_move_3 :- board1(Board), applyMove(0-1-3, Board, _N), display_board(8, 8, _N).
test1_apply_move_2 :- board1(Board), applyMove(0-1-2, Board, _N), display_board(8, 8, _N).
test1_apply_move_1 :- board1(Board), applyMove(0-1-1, Board, _N), display_board(8, 8, _N).
test1_apply_move_0 :- board1(Board), applyMove(0-1-0, Board, _N), display_board(8, 8, _N).

% Tests for board2
test2_apply_move_6 :- board2(Board), applyMove(0-1-6, Board, _N), display_board(8, 8, _N).
test2_apply_move_5 :- board2(Board), applyMove(0-1-5, Board, _N), display_board(8, 8, _N).
test2_apply_move_4 :- board2(Board), applyMove(0-1-4, Board, _N), display_board(8, 8, _N).
test2_apply_move_3 :- board2(Board), applyMove(0-1-3, Board, _N), display_board(8, 8, _N).
test2_apply_move_2 :- board2(Board), applyMove(0-1-2, Board, _N), display_board(8, 8, _N).
test2_apply_move_1 :- board2(Board), applyMove(0-1-1, Board, _N), display_board(8, 8, _N).
test2_apply_move_0 :- board2(Board), applyMove(0-1-0, Board, _N), display_board(8, 8, _N).

% Tests for board3
test3_apply_move_6 :- board3(Board), applyMove(0-1-6, Board, _N), display_board(8, 8, _N).
test3_apply_move_5 :- board3(Board), applyMove(0-1-5, Board, _N), display_board(8, 8, _N).
test3_apply_move_4 :- board3(Board), applyMove(0-1-4, Board, _N), display_board(8, 8, _N). % invalid
test3_apply_move_3 :- board3(Board), applyMove(0-1-3, Board, _N), display_board(8, 8, _N).
test3_apply_move_2 :- board3(Board), applyMove(0-1-2, Board, _N), display_board(8, 8, _N). % valid
test3_apply_move_1 :- board3(Board), applyMove(0-1-1, Board, _N), display_board(8, 8, _N).
test3_apply_move_0 :- board3(Board), applyMove(0-1-0, Board, _N), display_board(8, 8, _N).
/*****************************************************/    

/************ get a board edge ************/

test3_get_board_top :- board3(Board), get_board_edge(Board, top, TopEdge), write(TopEdge).
test3_get_board_right :- board3(Board), get_board_edge(Board, right, RightEdge), write(RightEdge).
test3_get_board_bottom :- board3(Board), get_board_edge(Board, bottom, BottomEdge), write(BottomEdge).
test3_get_board_left :- board3(Board), get_board_edge(Board, left, LeftEdge), write(LeftEdge).

/******************************************/

/************* valid board moves *************/
test3_valid_top_move4 :- board3(BoardBefore),
                         board4(BoardAfter),
                         valid_move(bottom, BoardBefore, BoardAfter). % invalid

test3_valid_bottom_move4 :- board3(BoardBefore),
                            board5(BoardAfter),
                            valid_move(top, BoardBefore, BoardAfter). % invalid

test3_get_board_bottom_before :- board3(_Board), 
                                 get_board_edge(_Board, bottom, BottomEdge), 
                                 write(BottomEdge).
test3_get_board_bottom_after2 :- board3(_Board), 
                                 applyMove(0-1-2, _Board, _N), 
                                 get_board_edge(_Board, bottom, BottomEdge), 
                                 write(BottomEdge).
test3_get_board_bottom_after4 :- board3(_Board), 
                                 applyMove(0-1-4, _Board, BoardAfter), 
                                 get_board_edge(BoardAfter, bottom, BottomEdge), 
                                 write(BottomEdge).

test3_get_valid_edge_moves_top :- board3(Board),
                                  get_valid_edge_moves(Board, 8, [0-1-2, 0-1-4], ValidMoves),
                                  write(ValidMoves).

test3_valid_moves :- board3(Board),
                     valid_moves(Board-0, ListOfMoves).

test3_applyMove1 :- board3(Board), applyMove(0-1-1, Board, BoardAfter), display_board(8, 8, BoardAfter), valid_move(bottom, Board, BoardAfter). 
test3_applyMove2 :- board3(Board), applyMove(0-1-2, Board, BoardAfter), display_board(8, 8, BoardAfter), valid_move(bottom, Board, BoardAfter). 
test3_applyMove3 :- board3(Board), applyMove(0-1-3, Board, BoardAfter), display_board(8, 8, BoardAfter), valid_move(bottom, Board, BoardAfter). 
test3_applyMove4 :- board3(Board), applyMove(0-1-4, Board, BoardAfter), display_board(8, 8, BoardAfter), valid_move(bottom, Board, BoardAfter). 
test3_applyMove5 :- board3(Board), applyMove(0-1-5, Board, BoardAfter), display_board(8, 8, BoardAfter), valid_move(bottom, Board, BoardAfter). 
test3_applyMove6 :- board3(Board), applyMove(0-1-6, Board, BoardAfter), display_board(8, 8, BoardAfter), valid_move(bottom, Board, BoardAfter). 
test3_applyMove7 :- board3(Board), applyMove(0-2-3, Board, BoardAfter), display_board(8, 8, BoardAfter), valid_move(bottom, Board, BoardAfter). 



/* ab :- board3(BoardBefore), 
      board4(BoardAfter), 
      valid_top_move(BoardBefore, 1,0-1-4 , BoardAfter),
      write(BoardBefore),
      write(BoardAfter).

cd :- board3(BoardBefore), 
      board4(BoardAfter),
      get_board_edge(BoardBefore, bottom, BottomBefore),
      get_board_edge(BoardAfter, bottom, BottomAfter),
      count_nulls([null,null,1,0,1,0,1,null], NullCountBefore),
      count_nulls([null,1,1,0,1,0,1,null], NullCountAfter), !,
      write(NullCountBefore), nl,
      write(NullCountAfter), nl,
      NullCountBefore == NullCountAfter. */

ef :- board3(_Board), get_valid_edge_moves(_Board, 8, [7-2-1], ValidMoves), write(ValidMoves).


/*********************************************/

/*********************AI-Level-1-Tests************************/
ai11 :- board1(X), choose_move(X-cpu1-0, 1, Move), nl, format('Performed move ~w', [Move]). % Test for board 1 where level 1 AI will move a black marble.
ai12 :- board1(X), choose_move(X-cpu1-1, 1, Move), nl, format('Performed move ~w', [Move]). % Test for board 1 where level 1 AI will move a white marble.

ai13 :- board2(X), choose_move(X-cpu1-0, 1, Move), nl, format('Performed move ~w', [Move]). % Test for board 2 where level 1 AI will move a black marble.
ai14 :- board2(X), choose_move(X-cpu1-1, 1, Move), nl, format('Performed move ~w', [Move]). % Test for board 2 where level 1 AI will move a white marble.

ai15 :- board3(X), choose_move(X-cpu1-0, 1, Move), nl, format('Performed move ~w', [Move]). % Test for board 3 where level 1 AI will move a black marble.
ai16 :- board3(X), choose_move(X-cpu1-1, 1, Move), nl, format('Performed move ~w', [Move]). % Test for board 3 where level 1 AI will move a white marble.

ai17 :- board4(X), choose_move(X-cpu1-0, 1, Move), nl, format('Performed move ~w', [Move]). % Test for board 4 where level 1 AI will move a black marble.
ai18 :- board4(X), choose_move(X-cpu1-1, 1, Move), nl, format('Performed move ~w', [Move]). % Test for board 4 where level 1 AI will move a white marble.

ai19 :- board5(X), choose_move(X-cpu1-0, 1, Move), nl, format('Performed move ~w', [Move]). % Test for board 5 where level 1 AI will move a black marble.
ai110 :- board5(X), choose_move(X-cpu1-1, 1, Move), nl, format('Performed move ~w', [Move]). % Test for board 5 where level 1 AI will move a white marble.
/*************************************************************/

/*********************AI-Level-2-Tests************************/
ai21 :- board1(X), choose_move(X-cpu1-0, 2, Move), nl, format('Performed move ~w', [Move]). % Test for board 1 where level 2 AI will move a black marble.
ai22 :- board1(X), choose_move(X-cpu1-1, 2, Move), nl, format('Performed move ~w', [Move]). % Test for board 1 where level 2 AI will move a white marble.

ai23 :- board2(X), choose_move(X-cpu1-0, 2, Move), nl, format('Performed move ~w', [Move]). % Test for board 2 where level 2 AI will move a black marble.
ai24 :- board2(X), choose_move(X-cpu1-1, 2, Move), nl, format('Performed move ~w', [Move]). % Test for board 2 where level 2 AI will move a white marble.

ai25 :- board3(X), choose_move(X-cpu1-0, 2, Move), nl, format('Performed move ~w', [Move]). % Test for board 3 where level 2 AI will move a black marble.
ai26 :- board3(X), choose_move(X-cpu1-1, 2, Move), nl, format('Performed move ~w', [Move]). % Test for board 3 where level 2 AI will move a white marble.

ai27 :- board4(X), choose_move(X-cpu1-0, 2, Move), nl, format('Performed move ~w', [Move]). % Test for board 4 where level 2 AI will move a black marble.
ai28 :- board4(X), choose_move(X-cpu1-1, 2, Move), nl, format('Performed move ~w', [Move]). % Test for board 4 where level 2 AI will move a white marble.

ai29 :- board5(X), choose_move(X-cpu1-0, 2, Move), nl, format('Performed move ~w', [Move]). % Test for board 5 where level 2 AI will move a black marble.
ai210 :- board5(X), choose_move(X-cpu1-1, 2, Move), nl, format('Performed move ~w', [Move]). % Test for board 5 where level 2 AI will move a white marble.
/*************************************************************/