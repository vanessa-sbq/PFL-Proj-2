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

/*
    Helper function
    An implementation of append where L3 is the concatenation of lists L1 ([H|T]) and L2.
*/
list_append([], L2, L2).
list_append([H|T], L2, [H|L3Tail]) :- list_append(T, L2, L3Tail).

/*
    Helper function
    Returns the maximum element between Elem1 and Elem2.
*/
max(Elem1, Elem2, Res) :- Elem1 > Elem2, Res is Elem1;
                          Elem1 < Elem2, Res is Elem2.

/*
    Helper function
    Unifies Elem with the Nth element of List, using only the append and length predicates.
*/
list_nth(0, List, Elem) :- list_append([Elem|_], _, List).
list_nth(N, [H|T], Elem) :- N > 0,
                            length([H|T], Size),
                            N < Size,
                            N1 is N - 1,
                            list_nth(N1, T, Elem), !.

/*
    Helper function
    Removes the element at position Index from List1 (which is unified with Elem), resulting in List2.
*/
delete_elem(0, [Elem|T], Elem, T).
delete_elem(Index, [H|T], Elem, Res) :- Index > 0,
                                        length([H|T], Size),
                                        Index < Size,
                                        Index1 is Index - 1,
                                        delete_elem(Index1, T, Elem, NewTail), !,
                                        Res = [H|NewTail].

/*
    Helper function
    Replaces the Old element, located at position Index in List1, by New, resulting in List2.
*/
replace([Old|T], 0, Old, New, [New|T]).
replace([H|T], Index, Old, New, Res) :- Index > 0,
                                        length([H|T], Size),
                                        Index < Size,
                                        Index1 is Index - 1,
                                        replace(T, Index1, Old, New, NewTail), !,
                                        Res = [H|NewTail].

/*
    Helper function
    Helps replace the first and last elements of a list. The first element will get substituted by A and the last element will be susbstituted by B.
*/
replace_first_last([_|T], A, B, [A|NewList]) :- replace_last(T, B, NewList).

/*
    Helper function
    Helps replace the last value of a list with the value B.
*/
replace_last([X], B, [B]) :- !.
replace_last([H|T], B, [H|NewList]) :- replace_last(T, B, NewList).

/*
    Helper function for partitionList
    Extracts the elements from a List and creates a new sublist (Partition).
*/
partitionListHelper(0, NewList, NewList, []) :- !.
partitionListHelper(PartitionSize, [H|T], NewList, NewParttion) :- PartitionSize > 0,
                                                                   PartitionSize1 is PartitionSize - 1,
                                                                   partitionListHelper(PartitionSize1, T, NewList, NewPartitionTail),
                                                                   NewParttion = [H|NewPartitionTail].

/*
    Helper function
    Helps with the partition of a List. NumberOfPartitions * PartitionSize must be the same as the total elements inside that List. The partitions will
    get outputed inside the Res variable as a list of lists.
*/
partitionList([], 0, _, []).
partitionList(List, NumberOfPartitions, PartitionSize, Res) :- partitionListHelper(PartitionSize, List, NewList, NewParttion),
                                                               NumberOfPartitions1 is NumberOfPartitions - 1,
                                                               partitionList(NewList, NumberOfPartitions1, PartitionSize, ResTail),
                                                               Res = [NewParttion|ResTail].

/*
    Helper function
    Helps reverse each Row inside the Board.
*/
reverseColumns([], []).
reverseColumns([Row|Rows], [RowReverse|ReversedColumnsTail]) :- reverse(Row, RowReverse),
                                               reverseColumns(Rows, ReversedColumnsTail).

/*
    Helper function
    Returns the indexes of the max values inside a List. These indexes get returned as a list inside MaxValueIndexes.
*/
get_max_values_indexes([], []).
get_max_values_indexes(List, MaxValueIndexes) :- max_member(MaxValue, List), findall(Index, nth0(Index, List, MaxValue), MaxValueIndexes), !.

/*
    Helper function.
    Switches boolean values.
    true becomes false and false becomes true.
*/
sw_bool_value(true, false).
sw_bool_value(false, true).
