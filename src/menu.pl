cls :- write('\33\[2J').

displayMenu :- write('                                                 '), nl,
               write('8b    d8    db    88""Yb 88   88 88        db    '), nl,
               write('88b  d88   dPYb   88__dP 88   88 88       dPYb   '), nl,
               write('88YbdP88  dP__Yb  88""Yb Y8   8P 88  .o  dP__Yb  '), nl,
               write('88 YY 88 dP""""Yb 88oodP YbodP88 88ood8 dP""""Yb '), nl,
               write('                                                 '), nl.


displayOptions(X) :- repeat,
                     write('1 - Human vs. Human'), nl,
                     write('2 - Human vs. Computer'), nl,
                     write('3 - Computer vs. Computer'), nl, nl,
                     write('Game mode: '),
                     read(X),
                     integer(X), 
                     X >= 1, 
                     X =< 3,
                     !.
