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

path(17, Path, Path) :- !.
path(S,[L1|L2], Path) :-
    (edges(S, T, _) ; edges(T, S, _)),
    not(member(T, [L1|L2])),
    path(T, [T,L1|L2], Path).
allpossiblepaths(S, Lf) :-
    findall(Path, path(S, [S], Path), Lf).
allpaths() :-
    allpossiblepaths(1, Lf1),
    allpossiblepaths(2, Lf2),
    allpossiblepaths(3, Lf3),
    allpossiblepaths(4, Lf4),
    append(Lf1,Lf2,Tf1),
    append(Lf3,Lf4,Tf2),
    append(Tf1,Tf2,Lf),
    reverselists(Lf, [], Lcf),
    printresults(Lcf),
    length(Lcf, Ls),
    write(Ls).
reverselists([],L, L):- !.
reverselists([H|Tail], L, Fl) :- 
    reverse(H, Hf),
%    writef(Hf),
    append([Hf], L, Lf),
    reverselists(Tail, Lf, Fl).
printresults([]):- !. 
printresults([H|Tail]) :-
    writeln(H),
    printresults(Tail).
   
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
optimal() :-
    % findminpath(1,17, W1, P1),
    % findminpath(2,17, W2, P2),
    % findminpath(3,17, W3, P3),
    % findminpath(4,17, W4, P4),
    % W1<W2, W1<W3, W1<W4, !, write(P1);
    % W2<W1, W2<W3, W2<W4, !, write(P2);
    % W3<W1, W3<W2, W3<W4, !, write(P3);
    % W4<W1, W4<W2, W4<W3, !, write(P4).
    
   write("Optimal Path :"),
   findminpath(1,17, W1, P1),
%    print(W1), write(" is the length of path "), print(P1), nl,
   findminpath(2,17, W2, P2),
    % print(W2), write(" is the length of path "), print(P2), nl,
   findminpath(3,17, W3, P3),
    % print(W3), write(" is the length of path "),print(P3), nl,
   findminpath(4,17, W4, P4),
    % print(W4), write(" is the length of path "),print(P4), nl,
   W3<W1, W3<W2, W3<W4,!,
    % print(W3), write(" is the length of path "),
    print(P3), nl;
   W2<W1, W2<W3, W2<W4,!,
    % print(W2), write(" is the length of path "),
    print(P2), nl;
   W1<W2, W1<W3, W1<W4,!,
    % print(W1), write(" is the length of path "),
    print(P1), nl;
   W4<W1, W4<W2, W4<W3,!,
    % print(W4), write(" is the length of path "),
    print(P4), nl;
   false.   