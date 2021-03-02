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

route(X,Y) :- 
    flight(X,Y),
    flight(Y,X),
    X\=Y.
route(X,Y) :- 
    flight(X,Z),
    flight(Z,Y),
    X\=Y.