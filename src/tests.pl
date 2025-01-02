/******************* TESTS *********************/    

/*
    [[null,1   ,1   ,0   ,1   ,0   ,1   ,null],
     [0   ,null,null,null,null,null,null,0   ],
     [1   ,null,null,null,null,null,null,0   ],
     [0   ,null,null,null,null,null,null,1   ],
     [0   ,null,null,null,null,null,null,0   ],
     [1   ,null,null,null,null,null,null,1   ],
     [0   ,null,null,null,null,null,null,0   ],
     [null,1   ,1   ,0   ,1   ,0   ,1   ,null]]
*/

% get_edge_marbles(Board, Color, TopEdgeMarblesPos, RightEdgeMarblesPos, BottomEdgeMarblesPos, LeftEdgeMarblesPos)
get_edge_marbles_test :- get_edge_marbles([[null,1   ,1   ,0   ,1   ,0   ,1   ,null],[0   ,null,null,null,null,null,null,0   ],[1   ,null,null,null,null,null,null,0   ],[0   ,null,null,null,null,null,null,1   ],[0   ,null,null,null,null,null,null,0   ],[1   ,null,null,null,null,null,null,1   ],[0   ,null,null,null,null,null,null,0   ],[null,1   ,1   ,0   ,1   ,0   ,1   ,null]], 0, TopEdgeMarblesPos, RightEdgeMarblesPos, BottomEdgeMarblesPos, LeftEdgeMarblesPos),
                         write(TopEdgeMarblesPos),
                         write(RightEdgeMarblesPos), 
                         write(BottomEdgeMarblesPos), 
                         write(LeftEdgeMarblesPos).

% get_all_edge_moves(Board, CurrentPlayer, TopEdgeMarblesPos, RightEdgeMarblesPos, BottomEdgeMarblesPos, LeftEdgeMarblesPos, AllEdgeMoves) 
get_all_edge_moves_test :- get_all_edge_moves([[null,1   ,1   ,0   ,1   ,0   ,1   ,null],[0   ,null,null,null,null,null,null,0   ],[1   ,null,null,null,null,null,null,0   ],[0   ,null,null,null,null,null,null,1   ],[0   ,null,null,null,null,null,null,0   ],[1   ,null,null,null,null,null,null,1   ],[0   ,null,null,null,null,null,null,0   ],[null,1   ,1   ,0   ,1   ,0   ,1   ,null]], 0, [(0,3),(0,5)], [(1,7),(2,7),(4,7),(6,7)], [(7,3),(7,5)], [(1,0),(3,0),(4,0),(6,0)], AllEdgeMoves),
                           write(AllEdgeMoves).

% get_all_top_edge_moves(N, TopEdgeMarblesPos, AllTopEdgeMoves)
get_all_top_edge_moves_test :- get_all_top_edge_moves(8, [(0,3), (0,5)], AllTopEdgeMoves), format('~w', [AllTopEdgeMoves]).

% get_all_right_edge_moves(N, RightEdgeMarblesPos, AllRightEdgeMoves)
get_all_right_edge_moves_test :- get_all_right_edge_moves(8, [(1,7),(2,7),(4,7),(6,7)], AllRightEdgeMoves), format('~w', [AllRightEdgeMoves]).

% get_all_bottom_edge_moves(N, BottomEdgeMarblesPos, AllBottomEdgeMoves)
get_all_bottom_edge_moves_test :- get_all_bottom_edge_moves(8, [(7,3),(7,5)], AllBottomEdgeMoves), format('~w', [AllBottomEdgeMoves]).

% get_all_left_edge_moves(N, LeftEdgeMarblesPos, AllLeftEdgeMoves)
get_all_left_edge_moves_test :- get_all_left_edge_moves(8, [(1,0), (3,0), (4,0), (6,0)], AllLeftEdgeMoves), format('~w', [AllLeftEdgeMoves]).

/***********************************************/