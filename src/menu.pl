/*
    Clears the screen.
*/
cls :- write('\33\[2J').

/*
    read_string(+Prompt, -InputAtom)

    Helper predicate to read a user input as a string.
*/
read_string(Prompt, InputAtom) :-
    write(Prompt),
    flush_output, 
    read_line(InputCodes),
    atom_codes(InputAtom, InputCodes). % Convert character codes to an atom

/*
    read_integer(+Prompt, -Number)

    Helper predicate to read a user input as an integer.
*/
read_integer(Prompt, Number) :-
    write(Prompt),
    flush_output,
    read_line(InputCodes), % Read input as a list of character codes
    (catch(number_codes(Number, InputCodes), _, fail) -> % Convert to integer
     true;   
     write('Invalid input, please try again.'), nl,
     read_integer(Prompt, Number)  % Retry on invalid input
    ), !.

/*
    Displays the title.
*/
display_menu :- write('                                                 '), nl,
               write('8b    d8    db    88""Yb 88   88 88        db    '), nl,
               write('88b  d88   dPYb   88__dP 88   88 88       dPYb   '), nl,
               write('88YbdP88  dP__Yb  88""Yb Y8   8P 88  .o  dP__Yb  '), nl,
               write('88 YY 88 dP""""Yb 88oodP YbodP88 88ood8 dP""""Yb '), nl,
               write('                                                 '), nl.

/*
    display_options(-X)

    Displays the main screen options and reads the user's selection.
*/
display_options(X) :- write('1 - Human vs. Human'), nl,
                     write('2 - Human vs. Computer'), nl,
                     write('3 - Computer vs. Computer'), nl, nl,
                     repeat,
                     read_integer('Game mode: ', X),
                     (X >= 1, X =< 3 -> !;   
                      write('Invalid option, please try again.'), nl, fail).

/*
    configure_game(+Option, -GameConfig)

    Displays the options for the game configurations and reads the user's selection.
*/
configure_game(1, 0-0-P1-P2) :- % Human vs. Human
    cls,
    ask_for_names(P1, P2),
    cls, !.
configure_game(2, 0-L2-P1-'CPU1') :- % Human vs. Computer
    cls,
    repeat,
    write('Hello Player1, please type in your name.'), nl,
    read_string('Name: ', P1), nl, nl,
    ask_cpu_level(L2, 'CPU1').
configure_game(3, L1-L2-'CPU1'-'CPU2') :- % Computer vs. Computer
    cls,
    ask_cpu_level(L1, 'CPU1'),
    ask_cpu_level(L2, 'CPU2').

/*
    ask_for_names(-P1, -P2)

    Menu that asks for both user's names in Human vs. Human.
*/
ask_for_names(P1, P2) :- repeat,
                       write('Hello Player1, please type in your name.'), nl,
                       read_string('Name: ', P1), nl, !, nl, 
                       repeat,
                       write('Hello Player2, please type in your name.'), nl,
                       read_string('Name: ', P2), nl, !, nl. 

/*
    ask_cpu_level(-X, +CpuName) 

    Menu that asks for the CPU level (Human vs. Computer and Computer vs. Computer).
*/
ask_cpu_level(X, CpuName) :-
    format('Choose the level for  ~a:', [CpuName]), nl,
    write('1 - Random'), nl,
    write('2 - Greedy'), nl,
    repeat,
    read_integer('CPU Level: ', X), nl,
    (X >= 1, X =< 2 -> !;   
     write('Invalid option, please try again.'), nl, fail).
