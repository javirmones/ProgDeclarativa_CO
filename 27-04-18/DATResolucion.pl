
%%CASO DE ESTUDIO PROGRAMACION DECLARATIVA

%%Javier Monescillo Buitrón
%%Alejandro Medina Jimenez
%%Julian García Sanchez

%% ENTRADA/SALIDA
%% PUEDE SER UN ARCHIVO

%% TRES PARTES
%% E/S
%% PASO A CLAUSULAS
%% RESOLUCION SLD
%%

%%%DEFINICIÓN DE LOS OPERADORES%%%
:- op(1200,yfx,'<=>').
:- op(1200,yfx,'=>').
:- op(100,fx,'~').
:- op(900,xfx,'\\/').
:- op(900,xfx,'/\\').


%%%FUNCIONES DE LOS OPERADORES%%%
~(X) :- \+X.
/\(X,Y) :- X,Y.
\/(X,Y) :- X;Y.
==>(X,Y):- ~X\/Y.


%%%% LECTURA DE ARCHIVO %%%

leer(Entrada,L):- open(Entrada, read, Str), read_file(Str,L), !, close(Str).

read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    read_file(Stream,L).


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

my_last_element([Y], Y).

my_last_element([_|Xs], Y):-
          my_last_element(Xs, Y).

addend(X, [], [X]).
addend(X, [C|R], [C|R1]):-
addend(X, R, R1). 

deleteend(L,L1):-addend(_,L1,L).

insertar(X, Lista, [X|Lista]). 

negar(X,~(Y)):- my_last_element(X,Y).

retornar_lista(Lista,Lista_negada):-negar(Lista,X),
                                    deleteend(Lista,Z),
                                    insertar(X,[],L),
                                    concatenar(Z,L,Lista_negada), !.

%%%%%%%%%%%%  FUNCION PRINCIPAL  %%%%%%%

ejecutar_demostrador(Entrada):- leer(Entrada,Lista), principal(Lista,Literales), derivacion3(Literales), !.

principal(L,Z):- retornar_lista(L,L1), funcion(L1,Z), !.

funcion([],[]):-!.
funcion([X|Xs],Y):- ejecucion(X,A),
                    funcion(Xs,Ys),
                    concatenar(A,Ys,Y).


ejecucion(L,Z1):- eliminar_implicador(L,L1),
                 tratar_neg(L1,L2),
                 distributiva(L2,Z),
                 extraer_clausulas(Z,Z1), !.
                

%%%%%%%%% PASO 1: Eliminación del doble implicador y el implicador

eliminar_implicador((A<=>B),((A1/\B1)\/(~A1/\~B1))):-
    !, eliminar_implicador(A,A1), eliminar_implicador(B,B1).
eliminar_implicador((A=>B),(~A1\/B1)):-
    !, eliminar_implicador(A,A1), eliminar_implicador(B,B1).
eliminar_implicador((A/\B),(A1/\B1)):-
    !, eliminar_implicador(A,A1),eliminar_implicador(B,B1).
eliminar_implicador((A\/B),(A1\/B1)):-
    !, eliminar_implicador(A,A1),eliminar_implicador(B,B1).
eliminar_implicador((~A),(~A1)):-!, eliminar_implicador(A,A1).
eliminar_implicador(A,A).

%%%%%%%%%PASO 2: Eliminación del negador

tratar_neg(~A,~A):- atom(A), !.
tratar_neg(~(~A),C1):- tratar_neg(A,C1).
tratar_neg(~(A\/B),(C1/\C2)):- tratar_neg(~A,C1), tratar_neg(~B,C2).
tratar_neg(~(A/\B),(C1\/C2)):- tratar_neg(~A,C1), tratar_neg(~B,C2).
tratar_neg(A\/B, C\/D):- tratar_neg(A, C), tratar_neg(B,D) .
tratar_neg(A/\B, C/\D):- tratar_neg(A, C), tratar_neg(B,D) .
tratar_neg(A,C):- atom(A), C = A, !.

%%%%%%%%%PASO 3: Propiedad distributiva

distributiva((A\/B),C):-
    !, distributiva(A,A1), distributiva(B,B1),
    distributiva1((A1\/B1),C).
distributiva((A/\B),(A1/\B1)):- 
    !, distributiva(A,A1), distributiva(B,B1).
distributiva(A,A).

distributiva1((A/\B)\/C,A1/\B1):-
    !, distributiva((A\/C),A1),distributiva((B\/C),B1).
distributiva1(A\/(B/\C),(A1/\B1)):-
    !, distributiva((A\/B),A1), distributiva((A\/C),B1).
distributiva1(A,A).

%%%%PASO 4: A clausulas

extraer_clausulas(A /\ B, L) :-
  !,
  extraer_clausulas(A, L1),
  extraer_clausulas(B, L2),
  append(L1,L2,L).

extraer_clausulas(A,L) :-
   atom(A),
   L = [[A]].

extraer_clausulas(~(A),L) :-
   atom(A),
   L = [[~(A)]].


extraer_clausulas(A \/ B, L) :-
  !,
  extraer_literales(A, L1),
  extraer_literales(B, L2),
  append(L1, L2, Inter),
  L = [Inter].


extraer_literales(A \/ B,L) :-
  !,
  extraer_literales(A, X),
  extraer_literales(B, Y),
  append(X, Y, L).

extraer_literales(~(A), [~(A)]).

extraer_literales(A,[A]).

%%%%%%%%%%%%%%%%%%% Resolucion %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tras identificar en el metodo paso_res dos clausulas a las que podemos
% aplicar el algoritmo de resolucion, procedemos a eliminar los
% elementos repetidos en las dos clausulas, aplicando asi, la regla de
% inferencia de la idempotencia
elimRepetidos([],[]).
elimRepetidos([H|T],S):-esMiembro(H,T),!,elimRepetidos(T,S).
elimRepetidos([H|T],[H|S]):-elimRepetidos(T,S).

%Predicado auxiliar para eliminar repetidos
esMiembro(X,[X|_]).
esMiembro(X,[_|T]):-esMiembro(X,T).

% predicado auxiliar para identificar si se puede aplicar resolucion a
% dos clausulas
sonComplementarios(X,~X).
sonComplementarios(~X,X).

% Aqui evaluamos los literales de dos clausulas [p,q,r],[~p,r], miembro
% a miembro, si el algoritmo de resolucion es aplicable sobre las dos
% clausulas, si es asi, primero concatenamos las dos clausulas generando
% nuestro futuro resolvente, eliminamos los elementos repetidos, y
% despues eliminamos los dos elementos complementarios, generando el
% resolvente que añadiremos a nuestra lista.
paso_res(C1,C2,R):-
    member(C11,C1),
    member(C22,C2),
    (	sonComplementarios(C11,C22) ->
       (
        append(C1,C2,R3),
	elimRepetidos(R3,R4),
	eliminarElemento(C11,R4,R5),
	eliminarElemento(C22,R5,R),!
	)).

% Metodo auxiliar para eliminar complementarios
eliminarElemento(X,[X|Xs],Xs).
eliminarElemento(X,[Y|Xs],[Y|Zs]) :- eliminarElemento(X,Xs,Zs).

% Primera llamada al metodo, seleccionando una clausula de nuestra lista
derivacion3([C|Clausulas]):-
    derivacion2([C|Clausulas], [C], [], C).

% Elegimos un elemento de la lista, y con el resolvente previamente
% seleccionado, aplicamos resolucion, generando un nuevo resolvente, si
% ese nuevo resolvente es [] paramos, si no, volvemos a llamar a
% derivacion añadiendo el resolvente previamente obtenido.
derivacion2(Cs, Camino_ant, Camino, R) :-
    member(C,Cs),
    paso_res(R, C, R1),
    write('clausula 1:'),
    writeln(R),
    write('clausula 2:'),
    writeln(C),
    write('Resolvente'),
    writeln(R1),
    (R1 = [] -> (!, writeln('solucion encontrada')); (
    writeln('---Siguiente Iteracion----'),
    derivacion2([R1|Cs], [R1|Camino_ant], Camino,R1))
    ).



