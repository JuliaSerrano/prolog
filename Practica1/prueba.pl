
:- module(_,_,[]).

author_data('Serrano', 'Arrese', 'Julia', '200119').


%-------------------------------------------------------
            % PROVIDED CODE
%valid colors
color(o).
color(x).


%evolution rules
rule(o,o,o,_,o). 
rule(x,o,o,r(A,_,_,_,_,_,_),A) :- color(A).
rule(o,x,o,r(_,B,_,_,_,_,_),B) :- color(B).
rule(o,o,x,r(_,_,C,_,_,_,_),C) :- color(C).
rule(x,o,x,r(_,_,_,D,_,_,_),D) :- color(D).
rule(x,x,o,r(_,_,_,_,E,_,_),E) :- color(E).
rule(o,x,x,r(_,_,_,_,_,F,_),F) :- color(F).
rule(x,x,x,r(_,_,_,_,_,_,G),G) :- color(G).

%-------------------------------------------------------
            % AUXILIAR PREDICATES

% checks if the arg is a list 
list([]).
list([_|Y]) :- list(Y).

%append/3
append([ ],Ys ,Ys) :- list(Ys).
append([X|Xs],Ys ,[X|Zs]) :- append(Xs ,Ys ,Zs).


%adds o to beginning and end of list
add_os(List,[o|Result]):- 
    append(List,[o],Result). 


%last element = X
last_o(X,[_|Tail]):- last_o(X,Tail).
last_o(X,[X|[]]).

%first element = X
first_o(X, [X|_]).

%check first and last element of list = o
check_first_and_last(I) :- 
    last_o(o,I),
    first_o(o,I).

%-------------------------------------------------------
            % CELLS/3

%caso base
cells_aux([_,_], _,Fs):- Fs = [].

%caso recursivo
cells_aux([X1,X2,X3|Xs], RuleSet, [F1|Fs]):-
    rule(X1,X2,X3,RuleSet,F1),
    cells_aux([X2,X3|Xs],RuleSet,Fs).


%InitialState, RuleSet, FinalState
cells(I,RuleSet,F):-
    add_os(I,I1),               %add o, beginning and end of S
    cells_aux(I1,RuleSet,F1),   %call recursive predicate
    add_os(F1,F),               %add o, beginning and end of F
    list(I),!,                  %check if I is a list
    check_first_and_last(I),!,  %check I starts and ends with o
    list(F),!.                  %check if F is a list

%-------------------------------------------------------
            % EVOL/3

%caso base
evol(0,_,[o,x,o]).

%caso recursivo
evol(s(N),RuleSet,Cells):-
    evol(N,RuleSet,PrevCells),
    cells(PrevCells,RuleSet,Cells).
    
%-------------------------------------------------------
            % STEPS/2

% Caso base
steps([o,x,o], 0).

% Caso recursivo
steps(Cells, N) :-
    evol(N, _, [o,x,o]),
    evol(N, RuleSet, [o,x,o]),
    steps_aux(RuleSet, Cells, N),
    list(Cells), !.

steps_aux(_, [o,x,o], 0).
steps_aux(RuleSet, Cells, N) :-
    N > 0,
    N1 is N - 1,
    evol(1, RuleSet, [o,x,o], ResultSet),
    steps_aux(RuleSet, NextCells, N1),
    Cells = [o | NextCells],
    ResultSet = [_,_|_].






%-------------------------------------------------------
            % RULESET/2
ruleset(_,_).