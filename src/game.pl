:- dynamic player/1.

askForNames(P1_Name, P2_Name) :- repeat,
                                 write('Hello Player1, please type in your name.'), nl,
                                 write('Name:'),
                                 read(P1_Name), nonvar(P1_Name), nl,
                                 nl,
                                 repeat,
                                 write('Hello Player2, please type in your name.'), nl,
                                 write('Name:'),
                                 read(P2_Name), nonvar(P2_Name), nl,
                                 nl.

askForNames(P1_Name) :- repeat,
                        write('Hello Player1, please type in your name.'), nl,
                        write('Name:'),
                        read(P1_Name), nonvar(P1_Name), nl,
                        nl.

addPlayersToGame(P1_Name, P2_Name) :- assert(player(P1_Name)), assert(player(P2_Name)).

executeGame(1) :- cls,
                  askForNames(P1, P2),
                  addPlayersToGame(P1, P2),
                  write('Game not implemented.').

executeGame(2) :- write('Not implemented, option 2'), !.
executeGame(3) :- write('Not implemented, option 3'), !.