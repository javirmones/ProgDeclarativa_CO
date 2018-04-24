
%%CASO DE ESTUDIO PROGRAMACION DECLARATIVA

%%Javier Monescillo Buitrón
%%Alejandro Medina Jimenez
%%Julian García Sanchez

%% ENTRADA SALIDA;
%% PUEDE SER UN ARCHIVO

%% TRES PARTES
%% E/S
%% PASO A CLAUSULAS, yo me encargo de cambiar esto
%% RESOLUCION Sld
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
funcion(L,G):- listas(L,[],E), aplanar(E,F), invertir(F, F2), write('F = '), writeln(F2), invertir(F2,F3), paso5a(F3,[],G), !.

listas([X], S, E):- ejecucion(~X,P), E=[P|S], !.
listas([L|C], S, E):- ejecucion(L,P),listas(C,[P|S],E).

% lo que le pasamos a ejecución es un literal de la formula

ejecucion(L,F):-paso1(L, C),
                paso2(C, D),
                paso3(D, E),
                paso4(E, F), !.


%%%%%%%%%PASO 1: DEFINICION DEL IMPLICADOR


paso1(A=>(B=>C),D) :- paso1(~A,D1), paso1(~(B=>C),D2), D=(D1\/D2).
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
paso3(A\/(B/\C) , ((A1\/B1) /\ (A1\/C1))):- paso3(A,A1), paso3(B,B1), paso3(C,C1).
paso3((A/\B)\/C , ((A\/C) /\ (B\/C))):- S = ((A\/C) /\ (B\/C)), !.
paso3(A,C):- C=A, !.

paso3(A,A).
paso3(A\/B, A\/B).

%%%%%%%%%PASO 4: ELIMINACION DE CONJUNCIONES
% eliminamos las conjunciones
paso4(A/\B,C):- paso4(A, C1), paso4(B, C2), C = [C2, C1], !.
paso4(A,C):- C=A, !.


%%%%%%%%%PASO 5: CONVERSION DE CLAUSULAS EN LISTAS DE LITERALES
paso5a([A|B], S, E):- paso5b(A, S1), paso5a(B, [S1|S], E).
paso5a([], S, E):- E = S, !.
paso5b(A\/B, S):- paso5b(A, S1), paso5b(B, S2), S3 = [S1,S2], aplanar(S3, S), !.
paso5b(~A, S):- atom(A), S = [~A], !.
paso5b(A, S):-  atom(A), S = [A], !.


% Resolucion
sonComplementarios(X,~X).
sonComplementarios(~X,X).

derivacion([C|Cs]):-
    resolucion(C, Cs).

resolucion(R, [C|L]):-
    paso_res(R,C,R11),
    writeln('Clausula 1:'), writeln(C),
    writeln('Clausula 2:'), writeln(R),
    writeln('----------------------'),
    eliminarComplementarios(R11,R1),
    writeln('resolvente --> '), writeln(R1),writeln(' '),
    writeln('||||||||||||||||||||||'),
    (R1 = [] -> true ; (append([R1],L,Raux), derivacion(Raux))).

paso_res(C1,C2,R):-
    append(C1,C2,Raux),
    eliminarComplementarios(Raux,R).


eliminarComplementarios([X|Xs],Xs2):-
    member(C1,Xs),
    (
         sonComplementarios(C1,X) ->
        (
        eliminarElemento(C1,Xs,Xs2)
         ) ; (Xs2 = [X|Xs])).

eliminarElemento(X, [X|Xs], Xs).
eliminarElemento(X, [Y|Ys], [Y|Zs]):- eliminarElemento(X, Ys, Zs).
