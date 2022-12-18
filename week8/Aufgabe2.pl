kandidat(malfoy).
kandidat(potter).
kandidat(weasley).
kandidat(granger).

notInVorstand(Person, Vorsitzender, Kassenwart, Sekretaer) :- kandidat(Vorsitzender) \= kandidat(Person), kandidat(Kassenwart) \= kandidat(Person), kandidat(Sekretaer) \= kandidat(Person).
inVorstand(Person, Vorsitzender, _, _) :- Vorsitzender = Person.
inVorstand(Person, _, Kassenwart, _) :- Kassenwart = Person.
inVorstand(Person,_,_, Sekretaer) :- Sekretaer = Person.

rule1(Vorsitzender, Kassenwart, Sekretaer) :- notInVorstand(malfoy, Vorsitzender,Kassenwart,Sekretaer).
rule1(Vorsitzender, Kassenwart, Sekretaer) :- notInVorstand(potter, Vorsitzender, Kassenwart, Sekretaer).

rule2(Vorsitzender, Kassenwart, Sekretaer) :- notInVorstand(malfoy, Vorsitzender, Kassenwart, Sekretaer).
rule2(Vorsitzender, Kassenwart, Sekretaer) :- Vorsitzender = granger, inVorstand(malfoy,Vorsitzender,Kassenwart, Sekretaer).

rule3(Vorsitzender, Kassenwart, Sekretaer) :- notInVorstand(weasley, Vorsitzender, Kassenwart, Sekretaer).
rule3(Vorsitzender, Kassenwart, Sekretaer) :- inVorstand(potter, Vorsitzender, Kassenwart, Sekretaer), inVorstand(weasley,Vorsitzender,Kassenwart,Sekretaer).

rule4(Vorsitzender, Kassenwart, Sekretaer) :- notInVorstand(potter, Vorsitzender, Kassenwart, Sekretaer).
rule4(Vorsitzender, Kassenwart, Sekretaer) :- Sekretaer \= granger, inVorstand(potter, Vorsitzender,Kassenwart,Sekretaer).

rule5(Vorsitzender, Kassenwart, Sekretaer) :- notInVorstand(granger, Vorsitzender,Kassenwart,Sekretaer).
rule5(Vorsitzender, Kassenwart, Sekretaer) :- Vorsitzender \= weasley, inVorstand(granger, Vorsitzender,Kassenwart,Sekretaer).

vorstand(Vorsitzender,Kassenwart,Sekretaer) :- kandidat(Vorsitzender), kandidat(Kassenwart), kandidat(Sekretaer),Vorsitzender \= Kassenwart, Kassenwart \=Sekretaer, Vorsitzender \= Sekretaer, rule1(Vorsitzender,Kassenwart,Sekretaer), rule2(Vorsitzender,Kassenwart,Sekretaer), rule3(Vorsitzender,Kassenwart,Sekretaer), rule4(Vorsitzender,Kassenwart,Sekretaer), rule5(Vorsitzender,Kassenwart,Sekretaer).