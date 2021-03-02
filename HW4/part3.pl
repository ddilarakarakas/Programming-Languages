when(102,10).
when(108,12).
when(341,14).
when(455,16).
when(452,17).
when(402,14).

where(102,z23).
where(108,z11).
where(341,z06).
where(455,207).
where(452,207).
where(402,z06).

enroll(a,102).
enroll(a,108).
enroll(b,102).
enroll(c,108).
enroll(d,341).
enroll(e,455).

schedule(S,P,T):- 
    where(X,P), 
    enroll(S,X), 
    when(X,T).

usage(P,T):- 
    where(C,P), 
    when(C,T).

isSameClassRoom(X,Y) :- 
    where(X,R1), 
    where(Y,R2), 
    R1==R2.

isSameTime(X,Y):- 
    when(X,T1), 
    when(Y,T2), 
    T1==T2.

conflict(X,Y) :- 
    isSameClassRoom(X,Y),
    isSameTime(X,Y).

meet(X,Y) :- 
    enroll(X,C1), 
    enroll(Y,C2), 
    C1==C2.