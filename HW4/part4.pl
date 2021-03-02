element(E,S) :- 
    member(E,S),!.

equalivalent(X, Y) :- 
    subtract(X, Y, []), 
    subtract(Y, X, []).

intersect([],L1,L2,L3).
intersect([H|T],L2,L3,[H|L4]):- element(H,L2),intersect(T,L3,L3,L4).

union([X|Y],Z,W) :- 
    element(X,Z),  
    union(Y,Z,W).

union([X|Y],Z,[X|W]) :- 
    \+ element(X,Z), 
    union(Y,Z,W).

union([],Z,Z).