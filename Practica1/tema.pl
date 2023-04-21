
:- module(_,_,[]).

author_data('Serrano', 'Arrese', 'Julia', '200119').

%AUXILIAR METHODS FOR CHECKING
% checks if the arg is a list 
list([]).
list([_|Y]) :- list(Y).

%checks if the first arg is an element of the list
% in the second arg
member(X,[X|Y]):- 
    list(Y).
member(X,[_|T]):- 
    member(X,T).

% ---------------------------------------------



%Ejercicios para practicar

%1)PREFIX
%  Prefix(X,Y) X es prefijo de Y
%ejemplo: prefix([a,b],[a,b,c,d])

% prefix([Phead|PTail],[Lhead|LTail]):-
%     member(Phead,[Lhead]).

%recursive way:
prefix([], _).
prefix([X|Xs], [X|Ys]) :- prefix(Xs, Ys).

% ---------------------------------------------
%2) SUFFIX 
% Suffix(X,Y): siendo X un sufijo de Y
%ejemplo: suffix([a,c],[d,e,a,c])

% suffix(X,[_|Ys]) :-
%     suffix(X,Ys).

% suffix([X|Xs],[X|Ys]) :-
%     prefix(Xs,Ys).

suffix( A, B) :- append( _X, B, A).   

% ---------------------------------------------
%3)SUBLIST
% sublist(X,Y) X is a sublist of Y
%caso base, primera lista vacia -> es sublist
sublist([],_).

%se recorre la lista mientras no haya coincidencias
sublist(X,[_|Ys]):-
    sublist(X,Ys).

%si hay coincidencias, se recorren las listas
sublist([X|Xs],[X|Ys]):-
    sublist(Xs,Ys).

% ---------------------------------------------

%4) CONCATENATE
%concatenate([1,2],[3,4,5],C)
% C = [1,2,3,4,5]
append([ ],Ys ,Ys) :- list(Ys).
append([X|Xs],Ys ,[X|Zs]) :- append(Xs ,Ys ,Zs).

% ---------------------------------------------

%5) REVERSE

%first approach
reverse([],[]).


reverse([X|Xs],Ys ) :-
    reverse(Xs ,Zs),
    append(Zs ,[X],Ys).