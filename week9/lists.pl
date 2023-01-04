append([],L,L).
append([E|R],L,[E|RL]) :- append(R,L,RL).

last(L,E) :- append(_,[E],L).

member(E,L) :- append(_,[E|_],L).

delete(E,L,R) :- append(L1,[E|L2],L), append(L1,L2,R).

sublist(T,L) :- append(_,L2,L), append(T,_,L2).

lookup(K,KVs,V) :- append(_,[(K,V)|_],KVs).

member2(E,L) :- append(_,[E|LR],L), append(_,[E|_],LR).

reverseNaive([],[]).
reverseNaive([X|Xs],Ys) :- reverseNaive(Xs,Y),!, append(Y,[X],Ys).

reverseAcc(Xs,Ys) :- reverseAccRun(Xs,[],Ys).
reverseAccRun([],Y,Y).
reverseAccRun([X|Xs],A,Y) :- reverseAccRun(Xs,[X|A],Y).