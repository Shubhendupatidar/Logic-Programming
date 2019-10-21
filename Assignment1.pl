decode(S) :-
    %    write(S),
    %    nl,
        string_chars(S,L),
        decodeutil(L,N),
        write(N).
    
    decodeutil([],1).
    decodeutil(['0'|L],0).
    decodeutil([X|L],N) :-
        atom_number(X,M),
        M is 1,
        [_|R]=L,
        !,
        decodeutil(R,N2),
        decodeutil(L,N1),
        N is N1+N2;
        atom_number(X,M),
        M is 2,
        [Y|R]=L,
        atom_number(Y,K),
        K<7,
        !,
        decodeutil(R,N2),
        decodeutil(L,N1),
        N is N1+N2;
        decodeutil(L,N).
    
    
    
    
    
    