
:- module(_,_,[assertions]).

%Portada
:- doc(title, "Aut@'{o}matas Celulares Reversibles").
:- doc(author, "Julia Serrano Arrese. C200119").
:- doc(usage, "use_module('code.pl')").

%Autor
:- prop author_data/4
    #"Nombre y matr@'{i}cula del autor de la pr@'{a}ctica. @includedef{author_data/4}".
author_data('Serrano', 'Arrese', 'Julia', '200119').

%-------------------------------------------------------
            % PROVIDED CODE

%COLOR/1
:- prop color/1 
     # "Los colores v@'{a}lidos para las c@'{e}lulas
     son:
     - blanco -> o
     - negro -> x  
    
    @includedef{color/1}".

%valid colors
color(o).
color(x).

%RULE/5
:- pred rule(X1,X2,X3,R,Y)
    #"@var{Y} es la c@'{e}lula resultante de aplicar el conjunto
    de reglas @var{R} de tipo r/7 a las células: @var{X1},
    @var{X2} y @var{X3}.@includedef{rule/5}".
%evolution rules
rule(o,o,o,_,o). 
rule(x,o,o,r(A,_,_,_,_,_,_),A):- color(A).
rule(o,x,o,r(_,B,_,_,_,_,_),B):- color(B).
rule(o,o,x,r(_,_,C,_,_,_,_),C):- color(C).
rule(x,o,x,r(_,_,_,D,_,_,_),D):- color(D).
rule(x,x,o,r(_,_,_,_,E,_,_),E):- color(E).
rule(o,x,x,r(_,_,_,_,_,F,_),F):- color(F).
rule(x,x,x,r(_,_,_,_,_,_,G),G):- color(G).
%test de rule
:- test rule(X1,X2,X3,R,Y) : 
    (X1 = o , X2 = x , X3 = o , R = r(x,o,x,x,x,x,o)) 
    => (Y = o) + not_fails.
%-------------------------------------------------------
            % AUXILIAR PREDICATES

%MY_LIST/1
:- prop my_list(L)
    #"Predicado que comprueba si el argumento @var{L} 
    es una lista. @includedef{my_list/1}".

% checks if the arg is a list 
my_list([]).
my_list([_|Y]) :- my_list(Y).

% test de my_list
:- test my_list(L) : 
    (L = [o,x,o]) 
    => (R = yes) + not_fails.

:- test my_list(L) : 
    (L = o) 
    => (R = no) + not_fails.

%MY_APPEND/3 
:- pred my_append(Xs, Ys, Zs)
    :: my_list * my_list * my_list
    # "Concatena las listas @var{Xs} e @var{Ys} y devuelve el resultado en
    @var{Zs}. Se espera que @var{Xs} y @var{Ys} sean listas.@includedef{my_append/3}".
%caso base
my_append([ ],Ys ,Ys) :- my_list(Ys).
%caso recursivo
my_append([X|Xs],Ys ,[X|Zs]) :- my_append(Xs ,Ys ,Zs).

%TESTS de my_append
% 2 empty lists -> empty list
:- test my_append([Xs], [Ys], [Zs]) : 
    (Xs = [], Ys = [])  => (Zs = []) + not_fails #"Caso base:".
% empty list, non-empty -> non-empty list = non-empty
:- test my_append([Xs], [Ys], [Zs]) : 
    (Xs = [], Ys = [o,x,o])  => (Zs = [o,x,o]) + not_fails #"Caso base:".

% non-empty list, empty list -> non-empty list = non-empty
:- test my_append([Xs], [Ys], [Zs]) : 
    (Xs = [o,x,o], Ys = [])  => (Zs = [o,x,o]) + not_fails.

% 2 non-empty, concatanate both in order -> non-empty
:- test my_append([Xs], [Ys], [Zs]) : 
    (Xs = [o,x,o], Ys = [x,x,o])  => (Zs = [o,x,o,x,x,o]) + not_fails.


%ADD_OS/2
:- pred add_os(List, Result)
#"Agrega el elemento 'o' al comienzo y al final de la lista @var{List},
produciendo la lista resultante @var{Result}. @includedef{add_os/2}".

%adds o to beginning and end of list
add_os(List,[o|Result]):- 
    my_append(List,[o],Result). 

% Tests de add_os
:- test add_os(L, R) :
    (L = [x, x, x]) 
    => (R = [o, x, x, x, o]) + not_fails.

%LAST_O/2
:- pred last_o(X, L)
#"Predicado que comprueba si el c@'{u}ltimo elemento de la lista
@var{L} es @var{X}. @includedef{last_o/2}".

%last element = X
last_o(X,[_|Tail]):- last_o(X,Tail).
last_o(X,[X|[]]).

% tests for last_o
:- test last_o(X, L) :
    (X = o, L = [x,o,x,o]) => (R = yes)+ not_fails.

:- test last_o(X, L) :
    (X = o, L = [x,o,x]) => (R = no)+ fails.

%FIRST_O/2
:- pred first_o(X, L)
#"Predicado que comprueba si el primer elemento de la lista @var{L}
es @var{X}. @includedef{first_o/2}".
%first element = X
first_o(X, [X|_]).

% tests for first_o
:- test first_o(X, L) :
    (X = o, L = [o,o,x,o]) => (R = yes) + not_fails.

:- test first_o(X, L) :
    (X = o, L = [x,o,x]) => (R = no)+ fails.


%CHECK_FIRST_AND_LAST/1
:- pred check_first_and_last(L)
#"Predicado que verifica si el primer y @'{u}ltimo elemento de la lista @var{L}
son 'o'. @includedef{check_first_and_last/1}".

%check first and last element of list = o
check_first_and_last(I) :- 
    last_o(o,I),
    first_o(o,I).

% test de check_first_and_last
:- test check_first_and_last(L) :
(L = [o, x, o])
=> (R = yes) + not_fails.

:- test check_first_and_last(L) :
(L = [x, o, x])
=> (R = no) + not_fails.

%AT_LEAST_3_ELEMS/1
:- pred at_least_3_elems(L)
    #"Predicado que comprueba si una lista tiene al menos 
    tres elementos.@includedef{at_least_3_elems/1}".

% Predicate that checks if a list has at least 3 elements
at_least_3_elems([_, _, _ | _]).

% Tests de at_least_3_elems
:- test at_least_3_elems(L) :
    (L = [o,x,o]) => (R=yes) + not_fails.

:- test at_least_3_elems(L) :
    (L = [o,x]) => (R=no) + fails.


%NATURAL/1
%Definicion numero natural (Teoria Peano y terminos recursivos)
:- prop natural/1 
    #"N@'{u}mero natural. @includedef{natural/1}".

% definicion del numero natural cero
natural(0). 

% definicion del sucesor de un número natural
natural(s(X)) :-
    natural(X).

%SUM/3 
:- pred sum(A,B,C)
   #"@var{C} es la suma de @var{A} y @var{B}
    en Peano. @includedef{sum/3}".
%Get addition of 2 nums
sum(0,Y,Y) :- natural(Y).
sum(s(X),Y,s(Z)) :- sum(X,Y,Z).

% test de sum
:- test sum(A,B,C) : 
    (A = 0, B = s(0)) => 
        (C = s(0)) + not_fails #"Caso base:".

:- test sum(A,B,C) : 
    (A = s(0), B = s(s(0))) 
    => (C = s(s(s(0)))) + not_fails.


%LEN/2
:- pred len(L,N)
   #"@var{N} es la logitud de la lista @var{L}. @includedef{len/2}".

%Get length of list
len([] ,0).
len([_|T],s(LT)) :- len(T,LT).

%Tests de len/2
:- test len(L,N) : (L = []) => (N = 0) + not_fails #"Caso base:".

:- test len(L,N) : (L = [o,x,o,o]) => (N = s(s(s(s(0))))) + not_fails.


%LENGTH_DIFFERENCE/2
:- pred length_difference(I, F)
    #"Verifica si la longitud de la lista @var{F} es dos más que la 
    longitud de la lista @var{I}.@includedef{length_difference/2}".


% Check if the length of F is two more than the length of I
length_difference(I, F) :-
    len(I, LI),             % Calcula la longitud de I
    len(F, LF),             % Calcula la longitud de F
    sum(LI, s(s(0)), LF).   % Verifica que la diferencia sea de dos

%TESTS de length_difference/2
:- test length_difference(I, F) : 
    (I = [], F = [o,x,x,o]) => (R=no) + fails. 

:- test length_difference(I, F) : (I = [o,x,x,o], F = [x,x,o,o,x,o])
     => (R=yes) + not_fails.

%IS_STATE/1
:- prop is_state/1

    #"Comprueba que el argumento @var{L} es un estado 
    v@'{a}lido:
    - Estado es una lista
    - Estado m@'{i}nimo formado por 3 c@'{e}lulas -> [o,x,o]
    - Estados deben empezar y terminar por c@'{e}lulas blancas
    @includedef{is_state/1}".

%checks if the list is a valid state
is_state(L):-
    my_list(L),
    at_least_3_elems(L),
    check_first_and_last(L).

% tests for is_state
:- test is_state(L) :
    (L = x) => (R = no) + fails.

:- test is_state(L) :
    (L = [o,x]) => (R = no) + fails.

:- test is_state(L) :
    (L = [o,x,x]) => (R = no) + fails.

:- test is_state(L) :
    (L = [o,x,o]) => (R = yes)+ not_fails.

:- test is_state(L) :
    (L = [o,x,x,o,x,x,x,x,o]) => (R = yes) + not_fails.
%-------------------------------------------------------
    % PREDICADOS PRINCIPALES

            % CELLS/3

:- pred cells_aux(InitialState,RuleSet,FinalState)
    #"Predicado auxiliar utilizado por 'cells/3', para realizar la 
    recursividad\. 
    
    En la implementaci@'{o}n, se divide el estado inicial @var{InitialState}
    en tripletes de c@'{e}lulas consecutivas y se aplica la regla correspondiente (definida
    por @var{Ruleset}) para cada tripleta, lo que da lugar al siguiente estado (@var{FinalState}).
    Este proceso se repite hasta llegar al caso base, cuando quedan solo
    2 c@'{e}lulas del estado inicial, finaliza el proceso recursivo
    @includedef{cells_aux/3}".

%caso base
cells_aux([_,_], _,Fs):- Fs = [].

%caso recursivo
cells_aux([X1,X2,X3|Xs], RuleSet, [F1|Fs]):-
    rule(X1,X2,X3,RuleSet,F1),
    cells_aux([X2,X3|Xs],RuleSet,Fs).

:- pred cells(InitialState,RuleSet,FinalState)
    #"Realiza un paso de evoluci@'{o}n desde @var{InitialState}
    a @var{FinalState} mediante la aplicaci@'{o}n de 
    la codificaci@'{o}n de reglas @var{RuleSet}. @includedef{cells/3}".

%InitialState, RuleSet, FinalState
cells(I,RuleSet,F):-
    is_state(I),
    add_os(I,I1),               %add o, beginning and end of I
    cells_aux(I1,RuleSet,F1),   %call recursive predicate
    add_os(F1,F),               %add o, beginning and end of F
    is_state(I1),
    is_state(F),
    length_difference(I,F).    %check that I1 has 2 elems more than F1               


%-------------------------------------------------------
            % EVOL/3

%caso base
evol(0,_,[o,x,o]).

%caso recursivo
evol(s(N),RuleSet,Cells):-
    at_least_3_elems(Cells),
    evol(N,RuleSet,PrevCells),
    cells(PrevCells,RuleSet,Cells).





%-------------------------------------------------------
            % STEPS/2

steps(Cells,N):-
    is_state(Cells),
    natural(N),
    evol(N,_,Cells).


%-------------------------------------------------------
            % RULESET/2
ruleset(RuleSet, Cells) :-
    steps(Cells, N),
    is_state(Cells),
    evol(N, RuleSet, Cells).



    
