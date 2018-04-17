
%%CASO DE ESTUDIO PROGRAMACION DECLARATIVA

%%Javier Monescillo Buitrón
%%Alejandro Medina Jimenez
%%Julian García Sanchez

%% ENTRADA SALIDA; 
%% PUEDE SER UN ARCHIVO

%% TRES PARTES
%% E/S
%% PASO A CLAUSULAS
%% RESOLUCION SLD
%%

%%%DEFINICIÓN DE LOS OPERADORES%%%
:- op(1200,yfx,'=>').
:- op(100,fx,'~').
:- op(900,xfx,'\\/').
:- op(900,xfx,'/\\').


%%%FUNCIONES DE LOS OPERADORES%%%
~(X) :- \+X.
/\(X,Y) :- X,Y.
\/(X,Y) :- X;Y.
==>(X,Y):- ~X\/Y.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%FUNCIONAMIENTO%%%

%%%%%% FUNCIONES AUXILIARES

concatenar([],L,L).
concatenar([X|M],L,[X|Z]):-concatenar(M,L,Z).

aplanar([],[]).
aplanar([[H|T]|R],S):- aplanar([H|T],T1),
                       aplanar(R,R1),
                       append(T1,R1,S), !.

aplanar([[]|T],S):- aplanar(T,S),!.
aplanar([H|T],[H|S]):- aplanar(T,S).


invertir([X],[X]).
invertir([X|M],Z):-invertir(M,S), concatenar(S,[X],Z).

%%%%%%%

%%%%%%%%%%%%  FUNCION PRINCIPAL
% SE INTRUDUCEN FORMULAS DE ESTE ESTILO [p=>q\/r, r/\q, etc] NO VALEN PARENTESIS CON IMPLICADORES PORQUE NO ESTA HECHO ES DECIR (P=>Q)=>Q va a petar
% funcion tiene de parametro de entrada, una funcion del tipo p v q => r y te devuelte G, que es la lista de literales
%aplanar, convierte las lista de listas en una unica lista, despues se invierte dos veces y se hace el paso 5a
funcion(L,G):- listas(L,[],E), aplanar(E,F), invertir(F, F2), write('Formulas = '), writeln(F2), invertir(F2,F3), paso5a(F3,[],G), !.

% la funcion listas como parametro de entrada tenemos, la formula anterior, y en el metodo anterior le pasamos la lista vacia, que en el caso
% de esta funcion es S, y la salida es la E, empezando desde la segunda llamada recursiva, es decir si esta la lista con la cabeza L y el cuerpo C, % siendo S otra lista vacia, (que probablemente sea un parámetro de acumulación), llamamos a ejecución, de L y devolvemos P, volvemos a llamar a 
% listas para que haga lo mismo, pero con el resto de la lista, y en el segundo parametro que es el parametro de acumulación, introducimos la lista 
% formada por P, que seria la P y el anterior parametro de acumulación que es la lista vacía []

listas([X], S, E):- ejecucion(~X,P), E=[P|S], !.
listas([L|C], S, E):- ejecucion(L,P),listas(C,[P|S],E).

% lo que le pasamos a ejecución es un literal de la formula

ejecucion(L,F):-paso1(L, C), paso2(C, D), paso3(D, E), paso4(E, F), !.


%%%%%%%%%PASO 1: DEFINICION DEL IMPLICADOR
% La definición del implicador es basicamente transformar de implicación a forma conjuntiva

paso1((A=>(B=>C),D) :- paso1(~A,D1), paso1(~(B=>C),D2), D=(D1\/D2).
paso1((A=>B)=>C,D) :- paso1(~(A=>B),D1), paso1(C,D2), D=(D1\/D2).
paso1(~(A=>B),C) :- paso1(~(~A),C1), paso1(~B,C2), C=(C1\/C2).
paso1(A=>B,C) :- paso1(~A,C1), paso1(B,C2), C = (C1\/C2).
paso1(A\/B,C) :- paso1(A,C1), paso1(B,C2), C = (C1\/C2).
paso1(A/\B,C) :- paso1(A,C1), paso1(B,C2), C = (C1/\C2).
paso1(~A,C):- paso1(A,C1), C = (~C1), !.
paso1(A,C) :- atom(A), C = A, !.


%%%%%%%%%PASO 2: LEYES DE DEMORGAN, Y DOBLE NEGADOR
%% como parametro tenemos al principio el doble negador
paso2(~A,~A):- atom(A), !.
paso2(~(~A),C):- paso2(A,C1),C=C1.
paso2(~(A\/B),C):- paso2(~A,C1), paso2(~B,C2), C = (C1/\C2).
paso2(~(A/\B),C):- paso2(~A,C1), paso2(~B,C2), C = (C1\/C2).
paso2(A\/B, C\/D):- paso2(A, C), paso2(B,D) .
paso2(A/\B, C/\D):- paso2(A, C), paso2(B,D) .
paso2(A,C):- atom(A), C = A, !.


%%%%%%%%%PASO 3: APLICACION DE LA DISTRIBUTIVA DE LA DISYUNCION
% ditributiva
paso3(A\/(B/\C) , S):- S = ((A\/B) /\ (A\/C)), !.
paso3((A/\B)\/C , S):- S = ((A\/C) /\ (B\/C)), !.
paso3(A,C):- C=A, !.


%%%%%%%%%PASO 4: ELIMINACION DE CONJUNCIONES
% eliminamos las conjunciones 
paso4(A/\B,C):- paso4(A, C1), paso4(B, C2), C = [C2, C1], !.
paso4(A,C):- C=A, !.


%%%%%%%%%PASO 5: CONVERSION DE CLAUSULAS EN LISTAS DE LITERALES
% ahora si, tenemos que poner las clausulas en listas de literales
% para ello contamos con el caso base que es una lista con la cabeaa A y cuerpo B, al cual le pasamos a que si es negado, lo introduce en s negado y % y si no es negado no lo introduce negado, y al igual con la tercera llamada recursiva si es A o B entonces tenemos que se convierte en un literal, % en s3= [S1,S2]
paso5a([A|B], S, E):- paso5b(A, S1), paso5a(B, [S1|S], E).
paso5a([], S, E):- E = S, !.
paso5b(A\/B, S):- paso5b(A, S1), paso5b(B, S2), S3 = [S1,S2], aplanar(S3, S), !.
paso5b(~A, S):- atom(A), S = [~A], !.
paso5b(A, S):-  atom(A), S = [A], !.



resolucion([]).
resolucion([C|L]):- res(C,L).


res([],[]).
res([X|Xs],[[~X]|Ys]):-res(Xs,Ys).
res([~X|Xs],[[X]|Ys]):-res(Xs,Ys).
res([X|Xs],[Y|Ys]):- res([X],Ys),res(Xs,[Y]).


demostradorBasadoResolucion(L):-funcion(L,G), write('Lista de literales = '), writeln(G),resolucion(G).



