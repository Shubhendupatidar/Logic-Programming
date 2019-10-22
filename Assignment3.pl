edges(1,5,4).
edges(2,5,6).
edges(3,5,8).
edges(4,5,9).
edges(1,6,10).
edges(2,6,9).
edges(3,6,3).
edges(4,6,5).
edges(5,7,3).
edges(5,10,4).
edges(5,11,6).
edges(5,12,7).
edges(5,6,7).
edges(5,8,9).
edges(6,8,2).
edges(6,12,3).
edges(6,11,5).
edges(6,10,9).
edges(6,7,10).
edges(7,10,2).
edges(7,11,5).
edges(7,12,10).
edges(7,8,10).
edges(8,9,3).
edges(8,12,3).
edges(8,11,4).
edges(8,10,8).
edges(10,15,5).
edges(10,11,2).
edges(10,12,5).
edges(11,15,4).
edges(11,13,3).
edges(11,12,4).
edges(12,13,7).
edges(12,14,8).
edges(15,13,3).
edges(13,14,4).
edges(14,17,5).
edges(14,18,4).
edges(17,18,8).

path([B | Rest], B, [B | Rest], Length, Length).
path([A | Rest], B, Path, CurrentLength, Length) :-
    edges(A, C, X),
    \+member(C, [A | Rest]),
    NewLength is CurrentLength + X,
    path([C, A | Rest], B, Path, NewLength, Length).
find_paths(A, B) :-
    path([A], B, Path, 0, Length),
    reverse(Path, DirectPath),
    printPath(DirectPath),
%    L1=[],
%    append(DirectPath, L1, L),
%    writef(' with evaluation %d\n', [Length]),
    nl,
    fail.
printPath([]).
printPath([X]) :-
    !, write(X).
printPath([X|T]) :-
    write(X),
    write(', '),
    printPath(T).
all_paths() :-
    (find_paths(1,17), true);
    (find_paths(2,17), true);
    (find_paths(3,17), true);
    (find_paths(4,17), true);
%cle    bagof(X, find_paths(1,17), Bag),
    true.

valid([]).
valid([X]) :- X is 17.
%    N is 17.
valid([X, Y| Tail]) :-
    edges(X,Y,_),
    !,
    valid([Y|Tail]);
    edges(Y,X,_),
    !,
    valid([Y|Tail]);
    false.
