:- include('./helpers.pl'). % TODO: Remove from this file and put inside of mabula.pl

/*
    Helper function. Helps build the board of the game,

    The board is a list of lists (matrix).
    After replicate our board looks like:
    [[], [], [], [], [], [], [], []]
*/
construct_board(Board) :- replicate(8,[],Board).