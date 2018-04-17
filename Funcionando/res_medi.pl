
miembro(E, L):- member(E,L), !.

resolucion([]).
resolucion([C|L]):- res(C,L).


res([],[]).
res([X|Xs],[[~X]|Ys]:-res(Xs,Ys).
res([~X|Xs],[[X]|Ys]:-res(Xs,Ys).
res([X|Xs],[Y|Ys]):- res([X],Ys),res(Xs,[Y]).
