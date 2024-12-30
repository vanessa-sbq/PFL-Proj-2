:- include('./menu.pl').
:- include('./mabula.pl').
:- include('./helpers.pl').
:- use_module(library(random)).

play :- cls,
        displayMenu,
        repeat,
        displayOptions(_GameType), integer(_GameType), _GameType >= 1, _GameType =< 3,
        executeGame(_GameType).