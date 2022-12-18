and(true,true,true).
and(true,false,false).
and(false,true,false).
and(false,false,false).

or(true,_,true).
or(false,false,false).
or(false,true,true).


not(true,false).
not(false,true).

ex1(X,Y,Z,Result)     :- and(X,Y,Value), or(Value, Z, Result).
ex2(X,Y,Z,Result)    :- and(X,Y,Value1), and(Y,Z,Value2), or(Value2, Z, Value3), or(Value1, Value3, Result).
ex3(X,Y,Z,Result)        :- not(Y,Value1), and(X,Value1,Value2), and(Value2,Z,Value3), and(Z,Y,Value4), or(Value4,Z,Value5), or(Value3,Value5, Result).

Wir stellen folgende Anfragen:
?- ex1(true,false,true,Result).
Result = true.

?- ex2(true,false,true,Result).
Result = true.

?- ex3(true,false,true,Result).
Result = true.

Wie ist die Belegung um true zu erhalten?
?- ex1(X,Y,Z,true).
X = Y, Y = true ;
X = Z, Z = true,
Y = false ;
X = false,
Y = Z, Z = true ;
X = Y, Y = false,
Z = true.


?- ex2(X,Y,Z,true).
X = Y, Y = Z, Z = true ;
X = Y, Y = true,
Z = false ;
X = Z, Z = true,
Y = false ;
X = false,
Y = Z, Z = true ;
X = Y, Y = false,
Z = true .

?- ex3(X,Y,Z,true).
X = Y, Y = Z, Z = true ;
X = false,
Y = Z, Z = true ;
X = Z, Z = true,
Y = false ;
X = Y, Y = false,
Z = true .


