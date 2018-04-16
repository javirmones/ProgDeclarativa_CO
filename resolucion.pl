resolver(Lista):-
	once(obtener_resolvente(Lista, Resolvente)),
	once(obtener_escritura_fichero(Lista, Resolvente)),
	(Resolvente = [] -> true ; resolver([Resolvente|Lista])).

obtener_resolvente(Lista, Resolvente):-
	miembro(A, Lista),
	miembro(B, Lista),
	A \= B,
	res(A, B, X),
	(X = false -> false);
	(miembro(X, Lista))-> false;
	Resolvente = X),
	write('Clausula 1 : '),writeln(A),
	write('Clausula 2 : '),writeln(B),
	write('Resolvente : '),writeln(Resolvente),writeln('')).

miembro(E, L):- member(E,L), !.
% AQUI HAY QUE IR A PASCUAL A QUUE NOS LO EXPLIQUE LA RESOLUCION