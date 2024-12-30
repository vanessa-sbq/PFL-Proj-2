askForNames(P1_Name, P2_Name) :- repeat,
                                 write('Hello Player1, please type in your name.'), nl,
                                 write('Name:'),
                                 read(P1_Name), atom(P1_Name), nl,
                                 nl,
                                 repeat,
                                 write('Hello Player2, please type in your name.'), nl,
                                 write('Name:'),
                                 read(P2_Name), atom(P2_Name), nl,
                                 nl.

askForNames(P1_Name) :- repeat,
                        write('Hello Player1, please type in your name.'), nl,
                        write('Name:'),
                        read(P1_Name), atom(P1_Name), nl,
                        nl.

%addPlayersToGame(P1_Name, P2_Name) :- assert(player(P1_Name)), assert(player(P2_Name)). % FIXME: Remove assert, add player names to gameState.


play_game :- initial_state(GameState-Player),
             display_game(GameState-Player),
             game_cycle(GameState-Player).

game_cycle(GameState-Player) :- game_over(GameState, Winner), !,
                                congratulate(Winner).

game_cycle(GameState-Player):- choose_move(GameState, Player, Move),
                               move(GameState, Move, NewGameState),
                               next_player(Player, NextPlayer),
                               display_game(NewGameState-NextPlayer), !,
                               game_cycle(NewGameState-NextPlayer).

congratulate(playerName) :- write('Congratulations, ', playerName, ' won.'), nl.

executeGame(1) :- cls,
                  askForNames(P1, P2),
%                 addPlayersToGame(P1, P2),
                  write('Game not implemented.').

executeGame(2) :- write('Not implemented, option 2'), !.
executeGame(3) :- write('Not implemented, option 3'), !.


% initial_state() :-
% display_game() :-
% move() :-
% valid_moves() :-
% game_over() :-
% value() :-
% choose_move() :-
