cls :- write('\33\[2J').

read_string(Prompt, InputAtom) :-
    write(Prompt),
    flush_output, 
    read_line(InputCodes),
    atom_codes(InputAtom, InputCodes), % Convert character codes to an atom
    format('You entered: ~w~n', [InputAtom]).

read_integer(Prompt, Number) :-
    write(Prompt),
    flush_output,
    read_line(InputCodes), % Read input as a list of character codes
    (catch(number_codes(Number, InputCodes), _, fail) -> % Convert to integer
     true;   
     write('Invalid input, please try again.'), nl,
     read_integer(Prompt, Number)  % Retry on invalid input
    ), !.

displayMenu :- write('                                                 '), nl,
               write('8b    d8    db    88""Yb 88   88 88        db    '), nl,
               write('88b  d88   dPYb   88__dP 88   88 88       dPYb   '), nl,
               write('88YbdP88  dP__Yb  88""Yb Y8   8P 88  .o  dP__Yb  '), nl,
               write('88 YY 88 dP""""Yb 88oodP YbodP88 88ood8 dP""""Yb '), nl,
               write('                                                 '), nl.


displayOptions(X) :- write('1 - Human vs. Human'), nl,
                     write('2 - Human vs. Computer'), nl,
                     write('3 - Computer vs. Computer'), nl, nl,
                     repeat,
                     read_integer('Game mode: ', X),
                     (X >= 1, X =< 3 -> !;   
                      write('Invalid option, please try again.'), nl, fail).



