flight(istanbul,izmir).
flight(istanbul,antalya).
flight(istanbul,gaziantep).
flight(istanbul,ankara).
flight(istanbul,van).
flight(istanbul,rize).

flight(izmir,istanbul).
flight(izmir,isparta).

flight(antalya,istanbul).
flight(antalya,konya).
flight(antalya,gaziantep).

flight(gaziantep,antalya).
flight(gaziantep,istanbul).

flight(ankara,istanbul).
flight(ankara,konya).
flight(ankara,van).

flight(van,istanbul).
flight(van,ankara).
flight(van,rize).

flight(rize,istanbul).
flight(rize,van).

flight(isparta,izmir).
flight(isparta,burdur).

flight(konya,antalya).
flight(konya,ankara).

flight(burdur,isparta).

flight(edirne,edremit).

flight(edremit,edirne).
flight(edremit,erzincan).

flight(erzincan,edremit).

distance(istanbul,izmir,328).
distance(istanbul,antalya,482).
distance(istanbul,gaziantep,847).
distance(istanbul,ankara,351).
distance(istanbul,van,1262).
distance(istanbul,rize,967).

distance(izmir,istanbul,328).
distance(izmir,isparta,308).

distance(antalya,istanbul,482).
distance(antalya,konya,192).
distance(antalya,gaziantep,592).

distance(gaziantep,antalya,592).
distance(gaziantep,istanbul,847).

distance(ankara,istanbul,351).
distance(ankara,konya,227).
distance(ankara,van,920).

distance(van,istanbul,1262).
distance(van,ankara,920).
distance(van,rize,373).

distance(rize,istanbul,967).
distance(rize,van,373).

distance(isparta,izmir,308).
distance(isparta,burdur,24).

distance(konya,antalya,192).
distance(konya,ankara,227).

distance(burdur,isparta,24).

distance(edirne,edremit,243).

distance(edremit,edirne,243).
distance(edremit,erzincan,1026).

distance(erzincan,edremit,1026).

get_min([F|R],M) :- 
    min(R,F,M). 

min([],M,M). 

min([[P,L]|R],[_,M],Min) :- 
    L < M, !, 
    min(R,[P,L],Min). 

min([_|R],M,Min) :- 
    min(R,M,Min).

distancet_(X,Y,L) :- 
    travel(X,Y,[X],C,L).

travel(X,Y,P,[Y|P],L) :- 
    distance(X,Y,L).
    
travel(X,Y,Visited,Path,L) :-
    distance(X,A,D),
    A \== Y,
    \+member(A,Visited),
    travel(A,Y,[A|Visited],Path,L1),
    L is D + L1.

sroute(A,B,Length) :- 
    setof([P,L],distancet_(A,B,L),Set), 
    get_min(Set,[P,Length]).