/*
    Brief: Used to create a list with an Amount number of Elem elements.
    
    Inputs for normal usage:
        Amount - The number of elements we want our list to contain
        Elem - The element that we want our list to contain.

    Returns: List with an Amount number of Elem elements.
*/
replicate(0, _, []).
replicate(Amount, Elem, List) :- Amount > 0,
                                 Amount1 is Amount - 1,
                                 replicate(Amount1, Elem, L1),
                                 List = [Elem|L1].