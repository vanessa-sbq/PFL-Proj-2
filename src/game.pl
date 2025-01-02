:- include('./menu.pl').
:- include('./mabula.pl').
:- include('./helpers.pl').
:- include('./board.pl').
:- use_module(library(random)).
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(clpfd)).

play :- cls,
        displayMenu,
        play_game, !.
