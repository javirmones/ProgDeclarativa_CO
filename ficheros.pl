%%LECTURA

aniadir_lista(X,[],[X]).
aniadir_lista(X,L,[X|L]).

%% LEEMOS DEL FICHERO haciendo leer('pruebaformulaEntrada.txt')
%% y tenemos que guardarlo en una lista para que quede tal que asi [p=>q, r, t] lo que sea.

leer(Entrada):- open(Entrada, read, In), 
read_string(In, "","",S, String),aniadir_lista(String,[],Z), writeln(Z),close(In). 



%%ESTE PARA ESCRIBIR LOS RESOLVENTES EN UN FICHERO
escribir(Salida, String):-
   open(Salida,write,Stream), 
   tell(Stream),
   write(Stream,String), 
   close(Stream). 
