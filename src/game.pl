:- include('./menu.pl').
:- include('./mabula.pl').
:- include('./helpers.pl').
:- include('./board.pl').
:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(between)).

play :- cls,
        displayMenu,
        repeat,
        displayOptions(_GameType), integer(_GameType), _GameType >= 1, _GameType =< 3,
        executeGame(_GameType).