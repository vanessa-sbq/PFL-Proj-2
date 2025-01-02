:- include('./board.pl').

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
        [0   ,null,null,null,null,null,null,0   ],
        [null,1   ,1   ,0   ,1   ,0   ,1   ,null]]).

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
                              get_valid_edge_moves(Board, 0, [0-3-1,0-3-2], ValidMoves),
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
test3_apply_move_4 :- board3(Board), applyMove(0-1-4, Board, _N), display_board(8, 8, _N).
test3_apply_move_3 :- board3(Board), applyMove(0-1-3, Board, _N), display_board(8, 8, _N).
test3_apply_move_2 :- board3(Board), applyMove(0-1-2, Board, _N), display_board(8, 8, _N).
test3_apply_move_1 :- board3(Board), applyMove(0-1-1, Board, _N), display_board(8, 8, _N).
test3_apply_move_0 :- board3(Board), applyMove(0-1-0, Board, _N), display_board(8, 8, _N).
/*****************************************************/    
