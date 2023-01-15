horseman(Heads,Legs,Horses,Riders):-add(Horses,Riders,Heads), riders(Riders,RiderLegs),horses(Horses,HorseLegs), add(RiderLegs,HorseLegs,Legs).

riders(o,o).
riders(s(Heads),s(s(Legs))):-riders(Heads,Legs).

horses(o,o).
horses(s(Heads),s(s(s(s(Legs))))):-horses(Heads,Legs).

add(o,Y,Y) .
add(s(X),Y,s(Z)) :- add(X,Y,Z).