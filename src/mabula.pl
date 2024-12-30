:- include('./menu.pl').
:- include('./game.pl').

play :- cls,
        displayMenu,
        repeat,
        displayOptions(_GameType), integer(_GameType), _GameType >= 1, _GameType =< 3,
        executeGame(_GameType).