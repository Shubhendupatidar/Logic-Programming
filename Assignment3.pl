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
print_all_paths() :-
    (find_paths(1,17), true);
    (find_paths(2,17), true);
    (find_paths(3,17), true);
    (find_paths(4,17), true);
    true.

valid([]).
valid([X]) :- X is 17.
valid([X, Y| Tail]) :-
    edges(X,Y,_),
    !,
    valid([Y|Tail]);
    edges(Y,X,_),
    !,
    valid([Y|Tail]);
    false.
findapath(X, Y, W, [X,Y], _) :- 
    edges(X, Y, W).
findapath(X, Y, W, [X|P], V) :- 
    \+ member(X, V),
    edges(X, Z, W1),
    findapath(Z, Y, W2, P, [X|V]),
    W is W1 + W2.
:-dynamic(solution/2).                            
findminpath(X, Y, W, P) :- 
    \+ solution(_, _),
    findapath(X, Y, W1, P1, []),
    assertz(solution(W1, P1)),
    !,
    findminpath(X,Y,W,P).

findminpath(X, Y, _, _) :- 
    findapath(X, Y, W1, P1, []),
    solution(W2, P2),
    W1 < W2,
    retract(solution(W2, P2)),
    asserta(solution(W1, P1)),
    fail.

findminpath(_, _, W, P) :- solution(W,P), retract(solution(W,P)).
optimal(X) :-
    findminpath(X,17, W, P), print(W), write(" is the length of path "), print(P).
%    findminpath(2,17, W2, P2), print(W2), print(" ,"), print(P2), nl,
%    findminpath(3,17, W3, P3), print(W3), print(" ,"),print(P3), nl,
%    findminpath(4,17, W4, P4), print(W4), print(" ,"),print(P4), nl.