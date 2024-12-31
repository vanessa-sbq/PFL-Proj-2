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

list_append([], L2, L2).
list_append([H|T], L2, [H|L3Tail]) :- list_append(T, L2, L3Tail).

list_nth(0, List, Elem) :- list_append([Elem|_], _, List).
list_nth(N, [H|T], Elem) :- N > 0,
                            length([H|T], Size),
                            N < Size,
                            N1 is N - 1,
                            list_nth(N1, T, Elem), !.

delete_elem(0, [Elem|T], Elem, T).
delete_elem(Index, [H|T], Elem, Res) :- Index > 0,
                                        length([H|T], Size),
                                        Index < Size,
                                        Index1 is Index - 1,
                                        delete_elem(Index1, T, Elem, NewTail), !,
                                        Res = [H|NewTail].

replace([Old|T], 0, Old, New, [New|T]).
replace([H|T], Index, Old, New, Res) :- Index > 0,
                                        length([H|T], Size),
                                        Index < Size,
                                        Index1 is Index - 1,
                                        replace(T, Index1, Old, New, NewTail), !,
                                        Res = [H|NewTail].

replace_first_last([_|T], A, B, [A|NewList]) :- replace_last(T, B, NewList).

replace_last([X], B, [B]) :- !.
replace_last([H|T], B, [H|NewList]) :- replace_last(T, B, NewList).

partitionListHelper(0, NewList, NewList, []) :- !.
partitionListHelper(PartitionSize, [H|T], NewList, NewParttion) :- PartitionSize > 0,
                                                                   PartitionSize1 is PartitionSize - 1,
                                                                   partitionListHelper(PartitionSize1, T, NewList, NewPartitionTail),
                                                                   NewParttion = [H|NewPartitionTail].

partitionList([], 0, _, []).
partitionList(List, NumberOfPartitions, PartitionSize, Res) :- partitionListHelper(PartitionSize, List, NewList, NewParttion),
                                                               NumberOfPartitions1 is NumberOfPartitions - 1,
                                                               partitionList(NewList, NumberOfPartitions1, PartitionSize, ResTail),
                                                               Res = [NewParttion|ResTail].

popFromList([], []).
popFromList([H|T], T).

sw_bool_value(true, false).
sw_bool_value(false, true).

% Check if all elements of a list are null
all_null([]).
all_null([null | Rest]) :- all_null(Rest).
