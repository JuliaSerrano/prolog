


:- module(_,_,[]).

author_data('Serrano', 'Arrese', 'Julia', '200119').


%%%%%%%%%%% Ejercicio 1 %%%%%%%%%%%

% Hechos

  %5.
controla(solozzo, drogas).
controla(corleone, apuestas).

% Reglas
  %3.
controla(solozzo,bronx):-
  apoya(roth,solozzo).
controla(solozzo,harlem):-
  apoya(roth,solozzo).

controla(corleone,manhattan):-
  apoya(roth,corleone).
controla(corleone,brooklyn):-
  apoya(roth,corleone).

  %6.
controla(X,policia):-
  controla(X,apuestas).

  %1.
elimina(corleone, solozzo) :-
  controla(corleone, manhattan),
  controla(corleone, brooklyn).

elimina(solozzo, corleone) :-
  controla(solozzo, drogas),
  apoya(roth, solozzo).


  %7.
garantiza_impunidad(X) :-
  controla(X,policia).

  %4.
apoya(roth,X):-
  garantiza_impunidad(X).

%%%%%%%%%%% Ejercicio 2 %%%%%%%%%%%

% Hechos
hermano(homer,tio_fester).
hermano(homer,tio_cosa).
hermano(tio_fester,tio_cosa).
hermano(pugsley,wednesday).




% Reglas
hermano(X,Y) :-
  hermano(Y,X).

% hermano(X,Y):-
%   padre(Z,X),
%   padre(Z,Y).

padre(homer,pugsley).
padre(homer,wednesday).

madre(abuela_addams,homer).
madre(abuela_addams,tio_fester).
madre(abuela_addams,tio_cosa).
madre(morticia,pugsley).
madre(morticia,wednesday).




cunyado(tio_fester,morticia).
cunyado(tio_cosa,morticia).

hijo(X,Y):-
  padre(Y,X).

hijo(X,Y):-
  madre(Y,X).

abuela(X,Z):-
  padre(Y,Z),
  madre(X,Y).

tio(tio_cosa,wednesday).
tio(tio_fester,wednesday).
tio(tio_cosa,pugsley).
tio(tio_fester,pugsley).
% tio(X,Y):-
%   padre(Z,Y),
%   hermano(Z,X).




