eq(o,o).
eq(s(X),s(Y)) :- eq(X,Y).

fisch(o).
fisch(s(o)):- fisch(o),!.
mobile(fisch(Gewicht),Gewicht).
mobile(bruecke(Links,Rechts),Gewicht) :- breuckenGewicht(Links,Rechts,Gewicht).

breuckenGewicht(Links,Rechts,Gewicht):-  add(GewichtRL,s(o),Gewicht),!, add(GewichtLinks,GewichtRechts,GewichtRL), eq(GewichtLinks,GewichtRechts),mobile(Links,GewichtLinks),mobile(Rechts,GewichtRechts).
add(o,Y,Y) .
add(s(X),Y,s(Z)) :- add(X,Y,Z).