
%%CASO DE ESTUDIO PROGRAMACION DECLARATIVA

%%Javier Monescillo Buitrón
%%Alejandro Medina Jimenez
%%Julian García Sanchez
%   Leer Fichero2

read_file(File, CharList):- see(File), read_list(CharList), seen.

read_list([Char | List]) :- get0(Char), Char =\= -1,!, put(Char), read_list(List).
read_list([]).

%%LECTURA FICHERO
leer_fichero(fichero):-open(fichero,read, entrada), read_string(entrada,"","",S,String), close(entrada).
%%LECTURA FICHERO
%%leer_fichero(fichero):-open(fichero,read, entrada), read_string(entrada,"","",S,String), close(entrada).

%%%DEFINICIÓN DE LOS OPERADORES%%%

:- op(600,yfx,'=>').
:- op(300,fx,'~').
:- op(500,xfx,'\\/').
:- op(400,xfx,'/\\').


%%%FUNCIONES DE LOS OPERADORES%%%

~(X) :- \+X.
/\(X,Y) :- X,Y.
\/(X,Y) :- X;Y.
=>(X,Y):- ~X\/Y.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%FUNCIONAMIENTO%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%% FUNCIONES AUXILIARES %%%%%
%% L lista, X cabeza etc

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

%%%%%%%%%%%%  FUNCION PRINCIPAL %%% L sea una lista de formulas tales como p\/q=>s

funcion_principal(L,G):- to_lista(L,[],E), 
						 	aplanar(E,F), 
						 	invertir(F, F2), write('F = '), 
						 	writeln(F2), invertir(F2,F3), 
						 	paso5a(F3,[],G), !.


to_lista([X], S, E):- ejecucion(~X,P), 
							E=[P|S], !.
to_lista([L|C], S, E):- ejecucion(L,P), to_lista(C,[P|S],E).


ejecucion(L,F):-paso_1(L, C),
                paso_2(C, D),
                paso_3(D, E),
                paso_4(E, F), !.


%%%%%%%%%PASO 1: DEFINICION DEL IMPLICADOR
paso_1(A=>B,C) :- paso_1(~A,C1), paso_1(B,C2), C = (C1\/C2).
paso_1(A\/B,C) :- paso_1(A,C1), paso_1(B,C2), C = (C1\/C2).
paso_1(A/\B,C) :- paso_1(A,C1), paso_1(B,C2), C = (C1/\C2).
paso_1(~A,C):- paso_1(A,C1), C = (~C1), !.
paso_1(A,C) :- atom(A), C = A, !.


%%%%%%%%%PASO 2: LEYES DE DEMORGAN, Y DOBLE NEGADOR
paso_2(~A,~A):- atom(A), !.
paso_2(~(~A),C):- paso_2(A,C1),C=C1.
paso_2(~(A\/B),C):- paso_2(~A,C1), paso_2(~B,C2), C = (C1/\C2).
paso_2(~(A/\B),C):- paso_2(~A,C1), paso_2(~B,C2), C = (C1\/C2).
paso_2(A\/B, C\/D):- paso_2(A, C), paso_2(B,D) .
paso_2(A/\B, C/\D):- paso_2(A, C), paso_2(B,D) .
paso_2(A,C):- atom(A), C = A, !.


%%%%%%%%%PASO 3: APLICACION DE LA DISTRIBUTIVA DE LA DISYUNCION
paso_3(A\/(B/\C) , S):- S = ((A\/B) /\ (A\/C)), !.
paso_3((A/\B)\/C , S):- S = ((A\/C) /\ (B\/C)), !.
paso_3(A,C):- C=A, !.


%%%%%%%%%PASO 4: ELIMINACION DE CONJUNCIONES
paso_4(A/\B,C):- paso_4(A, C1), paso_4(B, C2), C = [C2, C1], !.
paso_4(A,C):- C=A, !.


%%%%%%%%%PASO 5: CONVERSION DE CLAUSULAS EN LISTAS DE LITERALES
paso_5a([A|B], S, E):- paso5b(A, S1), paso5a(B, [S1|S], E).
paso_5a([], S, E):- E = S, !.
paso_5b(A\/B, S):- paso_5b(A, S1), paso_5b(B, S2), S3 = [S1,S2], aplanar(S3, S), !.
paso_5b(~A, S):- atom(A), S = [~A], !.
paso_5b(A, S):-  atom(A), S = [A], !.


















