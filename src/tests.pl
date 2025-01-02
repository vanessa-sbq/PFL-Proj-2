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
% get_edge_marbles(Board, Color, TopEdgeMarblesPos, RightEdgeMarblesPos, BottomEdgeMarblesPos, LeftEdgeMarblesPos)
get_edge_marbles_test1 :- board1(Board),
                         get_edge_marbles(Board, 0, TopEdgeMarblesPos, RightEdgeMarblesPos, BottomEdgeMarblesPos, LeftEdgeMarblesPos),
                         write('TopEdgeMarblesPos='),
                         write(TopEdgeMarblesPos), nl,
                         write('RightEdgeMarblesPos='),
                         write(RightEdgeMarblesPos), nl, 
                         write('BottomEdgeMarblesPos='),
                         write(BottomEdgeMarblesPos), nl,
                         write('LeftEdgeMarblesPos='),
                         write(LeftEdgeMarblesPos).

% get_all_edge_moves(Board, CurrentPlayer, TopEdgeMarblesPos, RightEdgeMarblesPos, BottomEdgeMarblesPos, LeftEdgeMarblesPos, AllEdgeMoves) 
get_all_edge_moves_test1 :- board1(Board),
                           get_all_edge_moves(Board, 0, [(0,3),(0,5)], [(1,7),(2,7),(4,7),(6,7)], [(7,3),(7,5)], [(1,0),(3,0),(4,0),(6,0)], AllEdgeMoves),
                           write(AllEdgeMoves).

% get_all_top_edge_moves(N, TopEdgeMarblesPos, AllTopEdgeMoves)
get_all_top_edge_moves_test1 :- get_all_top_edge_moves(8, [(0,3), (0,5)], AllTopEdgeMoves), format('~w', [AllTopEdgeMoves]).

% get_all_right_edge_moves(N, RightEdgeMarblesPos, AllRightEdgeMoves)
get_all_right_edge_moves_test1 :- get_all_right_edge_moves(8, [(1,7),(2,7),(4,7),(6,7)], AllRightEdgeMoves), format('~w', [AllRightEdgeMoves]).

% get_all_bottom_edge_moves(N, BottomEdgeMarblesPos, AllBottomEdgeMoves)
get_all_bottom_edge_moves_test1 :- get_all_bottom_edge_moves(8, [(7,3),(7,5)], AllBottomEdgeMoves), format('~w', [AllBottomEdgeMoves]).

% get_all_left_edge_moves(N, LeftEdgeMarblesPos, AllLeftEdgeMoves)
get_all_left_edge_moves_test1 :- get_all_left_edge_moves(8, [(1,0), (3,0), (4,0), (6,0)], AllLeftEdgeMoves), format('~w', [AllLeftEdgeMoves]).
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
