% Definition der Ehefrau-Ehemann-Relation:
ehemann(christine, heinz).
ehemann(maria,     fritz).
ehemann(monika,    herbert).
ehemann(angelika,  hubert).
ehemann(claudia,   karl).

% Definition der Kind-Mutter-Relation:
mutter(herbert,  christine).
mutter(angelika, christine).
mutter(hubert,   maria).
mutter(karl,     maria).
mutter(susanne,  monika).
mutter(norbert,  monika).
mutter(andreas,  angelika).
mutter(anna,     claudia).

vater(Kind,Vater) :- ehemann(Mutter,Vater), mutter(Kind,Mutter).

grossvater(E,G) :- vater(E,V), vater(V,G).
grossvater(E,G) :- mutter(E,M), vater(M,G).

grossmutter(Person, Grossmutter) :- ehemann(Grossmutter, Grossvater), grossvater(Person, Grossvater).

geschwister(Person, Geschwister) :- mutter(Person, Mutter), mutter(Geschwister, Mutter), Person \= Geschwister.

eltern(Person,Eltern) :- vater(Person, Eltern).
eltern(Person, Eltern) :- mutter(Person, Eltern).

tante(Person, Tante) :- eltern(Person, Eltern), geschwister(Eltern, Tante), ehemann(Tante, _).
tante(Person, Tante) :- eltern(Person, Eltern), geschwister(Eltern, Onkel), ehemann(Tante, Onkel).
