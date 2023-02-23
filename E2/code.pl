
:- module(_,_,[]).

author_data('Serrano', 'Arrese', 'Julia', '200119').


%Definicion numero natural (Teoria Peano y terminos recursivos)

% definicion del número natural cero
natural(0). 

% definicion del sucesor de un número natural
natural(s(X)) :-
    natural(X).


%%%%%%%%%%% Ejercicio 1 %%%%%%%%%%%

    %a.suma(X,Y,Z) <-> Z = X + Y
%caso base.  Y+0 = Y
suma(0,Y,Y) :- natural(Y).
%caso recursivo
suma(s(X),Y,s(Z)) :- suma(X,Y,Z).

    %b. par(X)
%caso base
par(0).

%caso recursivo
par(s(s(X))) :- par(X).

    %c. impar(X)
%caso base
impar(s(0)).

%caso recursivo
impar(s(s(X))) :- impar(X).


%%%%%%%%%%% Ejercicio 2 %%%%%%%%%%%

    %a. suma_a_lista(L,N,SL)

%caso base
suma_a_lista([],_,[]).

%caso recursivo
suma_a_lista([Head|Tail],N,[NewHead|NewTail]) :-
    suma(Head,N,NewHead),
    suma_a_lista(Tail,N,NewTail).


    %b.pares_lista(L,Ps)

%caso base
pares_lista([],[]).

%caso recursivo

pares_lista([Head|Tail],[Head|NewTail]) :-
    par(Head),
    !,
    pares_lista(Tail,NewTail).

pares_lista([_|Tail],NewTail) :-
    pares_lista(Tail,NewTail).




%%%%%%%%%%% Ejercicio 3 %%%%%%%%%%%
%extrae_elemento(I,L,E,NL)

% Caso base: si I es cero, entonces E es el primer elemento de L y NL es el resto de L
extrae_elemento(0, [E|NL], E, NL).

% Caso recursivo: si I es mayor que cero, se extrae el primer elemento de L y se llama recursivamente con I-1
extrae_elemento(s(I), [X|L], E, [X|NL]) :-
    extrae_elemento(I, L, E, NL).

