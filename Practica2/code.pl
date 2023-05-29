
:- module(_,_,[classic,assertions]).


%Portada
:- doc(title, "Recorridos de valor m@'{i}nimo en tableros").
:- doc(author, "Julia Serrano Arrese. C200119").
:- doc(usage, "use_module('code.pl')").

%Autor
:- prop author_data/4
    #"Nombre y matr@'{i}cula del autor de la pr@'{a}ctica. @includedef{author_data/4}".
author_data('Serrano', 'Arrese', 'Julia', '200119').

%-------------------------------------------------------
            % PREDICADOS AUXILIARES

%%%%%%Predicado auxiliar para EFECTUAR/MOVIMIENTO/3 %%%%%%

% MUEVE/3
% Predicado auxiliar para calcular la nueva posicion

:- pred mueve(Dir,Pos,NewPos)
    #"@var{NewPos} es la posici@'{o}n resultante de moverse desde @var{Pos}
    en la direcci@'{o}n marcada por @var{Dir}, que puede ser una de las siguientes:
    - Norte (n)
    - Sur (s)
    - Este (e)
    - Oeste (o)
    - Noroeste (no)
    - Noreste (ne)
    - Suroeste (so)
    - Sureste (se)
    @includedef{mueve/3}".
mueve(n, pos(Row, Col), pos(RowNew, Col)) :- RowNew is Row - 1.
mueve(s, pos(Row, Col), pos(RowNew, Col)) :- RowNew is Row + 1.
mueve(e, pos(Row, Col), pos(Row, ColNew)) :- ColNew is Col + 1.
mueve(o, pos(Row, Col), pos(Row, ColNew)) :- ColNew is Col - 1.
mueve(no, pos(Row, Col), pos(RowNew, ColNew)) :- RowNew is Row - 1, ColNew is Col - 1.
mueve(ne, pos(Row, Col), pos(RowNew, ColNew)) :- RowNew is Row - 1, ColNew is Col + 1.
mueve(so, pos(Row, Col), pos(RowNew, ColNew)) :- RowNew is Row + 1, ColNew is Col - 1.
mueve(se, pos(Row, Col), pos(RowNew, ColNew)) :- RowNew is Row + 1, ColNew is Col + 1.

% test de mueve
:- test mueve(Dir,Pos,NewPos) :
    (Dir = n, Pos = pos(2,2)) => 
    (NewPos = pos(1,2)) + not_fails.

:- test mueve(Dir,Pos,NewPos) :
    (Dir = s, Pos = pos(2,2)) => 
    (NewPos = pos(3,2)) + not_fails.


%%%%%% Predicado auxiliar para MOVIMIENTO_VALIDO/3 %%%%%%

% CHECK_POS/2
% predicado para verificar posicion valida

:- pred check_pos(Pos,N)
    #"Verifica si la posici@'{o}n @var{Pos} es v@'{a}lida en un tablero 
    de @var{N}x@var{N}.@includedef{check_pos/2}".

check_pos(pos(Row,Col),N):-
    integer(Row),   % Verificar si Row es un entero
    integer(Col),   % Verificar si Col es un entero
    integer(N),
    Row > 0,        % Verificar si Row es mayor que 0
    Col > 0,        % Verificar si Col es mayor que 0
    Row =< N,
    Col =< N.

%tests de check_pos
:- test check_pos(Pos,N):
    (Pos = pos(2,3), N = 3)  => 
    (R = yes) + not_fails.

:- test check_pos(Pos,N):
    (Pos = pos(2,3), N = 2)  => 
    (R = no) + fails.


%%%%%% Predicado auxiliar para SELECT_CELL/4 && SELECT_DIR/3 %%%%%%

% MY_SELECT/3
:- pred my_select(X,List,NewList)
    #"Elimina la primera aparici@'{o}n del elemento @var{X} en la lista @var{List},
      generando la lista resultante @var{NewList}.@includedef{my_select/3}".

% auxiliar select
my_select(X, [X|Tail], Tail).
my_select(X, [Y|Tail], [Y|NewTail]) :-
  my_select(X, Tail, NewTail).

%tests de my_select
:- test my_select(X,List,NewList):
    (X = 2, List = [1, 2, 3], NewList = [1, 3]) =>
    (R = yes, NewList = [1, 3]) + not_fails.

:- test my_select(X, List, NewList) :
    (X = 4, List = [1, 2, 3], NewList = [1, 2, 3]) =>
    (R = no) + fails.

:- test my_select(X, List, NewList) :
    (X=2, List = [3, 4, 5, 2]) =>
    (NewList = [3, 4, 5]) + not_fails.


%%%%%% Predicados auxiliares para GENERAR_RECORRIDO/6 %%%%%%

% DIRECCION_VALIDA/2
:- pred direccion_valida(Board, Pos)
    #"Verifica si la posición @var{Pos} pertenece al tablero @var{Board}.
      @var{Board} es una lista de celdas representadas como:
       `cell(pos(Row,Col),op(Operador,Operando))`.@includedef{direccion_valida/2}".

% predicado auxiliar para comprobar que una posicion pertenece a un tablero dado
direccion_valida(Board, Pos) :-
    member(cell(Pos, _), Board).

%tests de direccion_valida
:- test direccion_valida(Board, Pos):
    (Board = [cell(pos(1, 1), op(*,-3)), cell(pos(2, 2), op(+,20)), 
    cell(pos(3, 3), op(//,-23))], Pos = pos(1,1)) => 
    (R = yes) + not_fails.

:- test direccion_valida(Board, Pos):
    (Board = [cell(pos(1, 1), op(*,-3)), cell(pos(2, 2), op(+,20)), 
    cell(pos(3, 3), op(//,-23))], Pos = pos(1,2)) => 
    (R = no) + fails.


%MY_REVERSE/2
:- pred my_reverse(List, Reversed)
    #"Invierte una lista @var{List}, devolviendo la lista invertida en @var{Reversed}.
    @includedef{my_reverse/2}".

% reverse a list
my_reverse(List, Reversed) :-
    reverse_aux(List, [], Reversed).


:- pred reverse_aux(List, Acc, Reversed)
    #"Predicado auxiliar utilizado por 'my_reverse/2' para realizar la recursividad.
      Realiza la inversi@'{o}n de una lista mediante recursi@'{o}n, acumulando los elementos
      invertidos en la variable de acumulaci@'{o}n @var{Acc} y devolviendo la lista
      invertida final en @var{Reversed}.@includedef{reverse_aux/3}".

reverse_aux([], Acc, Acc).
reverse_aux([H|T], Acc, Reversed) :-
    reverse_aux(T, [H|Acc], Reversed).

%tests my_reverse
:- test my_reverse(List, Reversed):
    (List = [1, 2, 3, 4, 5], Reversed = [5, 4, 3, 2, 1]) =>
    (R = yes) + not_fails.

:- test my_reverse(List, Reversed):
    (List = [1,2,3], Reversed = [2,3,1]) =>
    (R = no) + fails.

:- test my_reverse(List, Reversed):
    (List = [1, 2, 3, 4, 5]) =>
    (Reversed = [5, 4, 3, 2, 1]) + not_fails.


%%%%%% Predicados auxiliares para TABLERO/5 %%%%%%

% OBTENER_VALOR_MINIMO/2
:- pred obtener_valor_minimo(Recorridos, ValorMinimo)
    #"Predicado utilizado para encontrar el valor m@'{i}nimo en una lista 
    de recorridos. Toma una lista de recorridos representados como tuplas de la 
    forma `(Recorrido, Valor)` y devuelve en `ValorMinimo` el valor m@'{i}nimo 
    encontrado en dicha lista.@includedef{obtener_valor_minimo/2}".

% predicado auxiliar para encontrar el valor minimo en una lista de recorridos
obtener_valor_minimo([(_Recorrido, Valor) | RestoRecorridos], ValorMinimo) :-
    obtener_valor_minimo_aux(RestoRecorridos, Valor, ValorMinimo).


:- pred obtener_valor_minimo_aux(Recorridos, ValorActual, ValorMinimo)
    #"Predicado auxiliar utilizado por 'obtener_valor_minimo/2' para encontrar el 
    valor m@'{i}nimo en una lista de recorridos. Realiza la comparaci@'{o}n de 
    valores y la recursividad necesaria para encontrar el valor m@'{i}nimo.
    @includedef{obtener_valor_minimo_aux/3}".

%caso base, lista vacia (no hay mas recorridos)
obtener_valor_minimo_aux([], ValorMinimo, ValorMinimo).

%valor menor, actualizar valor
obtener_valor_minimo_aux([(_Recorrido, Valor) | RestoRecorridos], ValorActual, ValorMinimo) :-
    Valor < ValorActual,
    obtener_valor_minimo_aux(RestoRecorridos, Valor, ValorMinimo).

%valor igual o mayor, continuar recursividad
obtener_valor_minimo_aux([(_Recorrido, _) | RestoRecorridos], ValorActual, ValorMinimo) :-
    obtener_valor_minimo_aux(RestoRecorridos, ValorActual, ValorMinimo).

%tests obtener_valor_minimo
:- test obtener_valor_minimo(Recorridos, ValorMinimo):
    (Recorridos = [], ValorMinimo = 0) => (R = yes) + not_fails.

:- test obtener_valor_minimo(Recorridos, ValorMinimo):
    (Recorridos = [(_Recorrido1, 10), (_Recorrido2, 5), (_Recorrido3, 8)], 
    ValorMinimo = 5) => (R = yes) + not_fails.

:- test obtener_valor_minimo(Recorridos, ValorMinimo):
    (Recorridos = [(_Recorrido1, 10), (_Recorrido2, 5), (_Recorrido3, 8)], 
    ValorMinimo = 0) => (R = no) + fails.

:- test obtener_valor_minimo(Recorridos, ValorMinimo):
    (Recorridos = [(_Recorrido1, 8), (_Recorrido2, -5), (_Recorrido3, 0)])
     => (ValorMinimo = -5) + not_fails.


% recorridos_min/3
:- pred recorridos_min(Recorridos, ValorMinimo, NumRecorridosMin)
    #"Cuenta el n@'{u}mero de rutas en la lista de @var{Recorridos} que tienen el 
    @var{ValorMinimo} especificado. El resultado se unifica con el 
    @var{NumRecorridosMin}.@includedef{recorridos_min/3}".

% predicado auxiliar para contar el numero de rutas que tienen el valor minimo
recorridos_min(Recorridos, ValorMinimo, NumRecorridosMin) :-
    recorridos_min_aux(Recorridos, ValorMinimo, 0, NumRecorridosMin).


:- pred recorridos_min_aux(Recorridos, ValorMinimo, ContadorActual, NumRecsMin)
    #"Predicado auxiliar utilizado por 'recorridos_min/4' para contar el @'{u}mero de
     recorridos (@var{Recorridos}) que tienen el valor m@'{i}nimo especificado por @var{ValorMinimo}.
      Realiza la recursividad sobre la lista de @var{Recorridos} y lleva el 
      @var{ContadorActual} de recorridos m@'{i}nimos encontrados, obteniendo finalmente 
      el n@'{u}mero total de recorridos m@'{i}nimos encontrados unificado en  @var{NumRecsMin}.
      @includedef{recorridos_min_aux/4}".

%caso base, lista vacia (no hay mas recorridos)
recorridos_min_aux([], _ValMin, NumRecsMin, NumRecsMin).

%Valor encontrado == minimo buscado, contador++
recorridos_min_aux([(_Recorrido, Val) | RestoRec], ValMin, ContAct, NumRecsMin) :-
    Val =:= ValMin,
    NuevoCont is ContAct + 1,
    recorridos_min_aux(RestoRec, ValMin, NuevoCont, NumRecsMin).

% valor encontrado != buscado, continuar recursividad
recorridos_min_aux([_Recorrido| RestoRec], ValMin, ContAct, NumRecsMin) :-
    recorridos_min_aux(RestoRec, ValMin, ContAct, NumRecsMin).


% %tests recorridos_min
:- test recorridos_min(Recorridos, ValorMinimo, NumRecorridosMin) :
    (Recorridos = ([[(pos(1,1),2), (pos(1,2),0), (pos(2,1),-3)],
                   [(pos(1,1),-3), (pos(1,2),0), (pos(2,1),-6)],
                   [(pos(1,1),5), (pos(1,2),-1), (pos(2,1),5)]]),
    ValorMinimo = -6,
    NumRecorridosMin = 1) =>
    (R = yes) + not_fails.

:- test recorridos_min(Recorridos, ValorMinimo, NumRecorridosMin) :
    (Recorridos = ([[(pos(1,1),2), (pos(1,2),0), (pos(2,1),-10)],
                   [(pos(1,1),-3), (pos(1,2),0), (pos(2,1),-6)],
                   [(pos(1,1),5), (pos(1,2),-1), (pos(2,1),-10)]]),
    ValorMinimo = -10,
    NumRecorridosMin = 2) =>
    (R = yes) + not_fails.


:- test recorridos_min(Recorridos, ValorMinimo, NumRecorridosMin) :
    (Recorridos = ([[(pos(1,1),2), (pos(1,2),0), (pos(2,1),-10)],
                   [(pos(1,1),-3), (pos(1,2),0), (pos(2,1),-6)],
                   [(pos(1,1),5), (pos(1,2),-1), (pos(2,1),-10)]]),
    ValorMinimo = 0,
    NumRecorridosMin = 2) =>
    (R = no) + fails.


%-------------------------------------------------------
    % PREDICADOS PRINCIPALES

            % APARTADO 1

% efectuar_movimiento/3
:- pred efectuar_movimiento(Pos, Dir, Pos2)
    #"Predicado que realiza un movimiento desde la posici@'{o}n @var{Pos} en la direcci@'{o}n 
    @var{Dir}, obteniendo la nueva posición @var{Pos2}.@includedef{efectuar_movimiento/3}".

% se mueve desde Pos en direccion Dir, obteniendo Pos2
efectuar_movimiento(Pos,Dir,Pos2):-
    mueve(Dir, Pos, Pos2).  % Calcular la posicion 

%tests de efectuar_movimiento
:- test efectuar_movimiento(Pos, Dir, Pos2):
    (Pos = pos(1, 1), Dir = e, Pos2 = pos(1, 2)) => (R = yes) + not_fails.

:- test efectuar_movimiento(Pos, Dir, Pos2):
    (Pos = pos(1, 4), Dir = n, Pos2 = pos(0, 1)) => (R = no) + fails.


% movimiento_valido/3
:- pred movimiento_valido(N,Pos,Dir)
    #"Predicado que verifica si el movimiento desde la posición @var{Pos} en la direcci@'{o}n 
    @var{Dir} es v@'{a}lido en un tablero de tamaño @var{N}x@var{N}.
    @includedef{movimiento_valido/3}".

% el movimiento desde Pos en direccion Dir es valido
% en un tablero de NxN
movimiento_valido(N,Pos,Dir):-
    efectuar_movimiento(Pos,Dir,Pos2),
    check_pos(Pos2,N).

%tests de movimeinto_valido
:- test movimiento_valido(N, Pos, Dir):
    (N = 3, Pos = pos(1, 1), Dir = e) => (R = yes) + not_fails.

:- test movimiento_valido(N, Pos, Dir):
    (N = 3, Pos = pos(1, 1), Dir = n) => (R = no) + fails.


%select_cell/4
:- pred select_cell(IPos, Op, Board, NewBoard)
    #"Extrae la celda con la posición @var{IPos} del tablero @var{Board}, obteniendo
    @var{NewBoard} sin dicha celda y unificando @var{Op} con la operaci@'{o}n  asociada
    a la respectiva celda.@includedef{select_cell/4}".

% extrae la celda con pos IPos del tablero Board, obteniendo
% NewBoard sin dicha celda y unificando Op con la operacion
% asociada a la respectiva celda
select_cell(IPos, Op, Board, NewBoard) :-
    my_select(cell(IPos, Op), Board, NewBoard).

%tests de select_cell
:- test select_cell(IPos, Op, Board, NewBoard):
    (IPos = pos(2, 2), 
    Board = [cell(pos(1, 1), op(*,-3)), cell(pos(2, 2), op(+,-6)), cell(pos(3, 3), op(//,-23))])
     => 
    (Board = [cell(pos(1, 1), op(*,-3)), cell(pos(3, 3), op(//,-23))],
    Op = op(+,-6)) + not_fails.

:- test select_cell(IPos, Op, Board, NewBoard):
    (IPos = pos(4, 4),
    Board = [cell(pos(1, 1), op(*,-3)), cell(pos(2, 2), op(+,-6)), cell(pos(3, 3), op(//,-23))])
    => (R = no) + fails.


%select_dir/3
:- pred select_dir(Dir, Dirs, NewDirs)
    #"Resta una direcci@'{o}n @var{Dir} de las direcciones permitidas en @var{Dirs}, obteniendo
    @var{NewDirs} con la direcci@'{o}n restada.@includedef{select_dir/3}".

% extrae una direccion Dir de las permitidas en Dirs
% obteniendo NewDirs
select_dir(Dir, Dirs, NewDirs) :-
    my_select(dir(Dir, Num), Dirs, DirsWithoutDir),
    (Num > 1 -> NewNum is Num - 1, 
    NewDirs = [dir(Dir, NewNum) | DirsWithoutDir] ; NewDirs = DirsWithoutDir).

%tests de select_dir
:- test select_dir(Dir, Dirs, NewDirs):
    (Dir = n, Dirs = [dir(n, 3), dir(e, 2), dir(s, 1), dir(so, 4)]) => 
    NewDirs = [dir(n, 2), dir(e, 2), dir(s, 1), dir(so, 4)] + not_fails.

:- test select_dir(Dir, Dirs, NewDirs):
    (Dir = s, Dirs = [dir(n, 3), dir(e, 2), dir(s, 1), dir(so, 4)]) => 
    NewDirs = [dir(n, 3), dir(e, 2), dir(so, 4)] + not_fails.

:- test select_dir(Dir, Dirs, NewDirs):
    (Dir = s, Dirs = [dir(n, 3), dir(e, 2), dir(s, 1), dir(so, 4)],
    NewDirs = [dir(n, 4), dir(e, 2), dir(s, 1), dir(so, 4)]) => 
    (R = no) + fails.



%aplicar_op/3
:- pred aplicar_op(Op, Valor, Valor2)
    #"Dada una @var{Op} representada por `op(Operador,Operando)`, se aplica la operaci@'{o}n 
    especificada por el operador de la siguiente forma:
    - Operando izquierdo -> @var{Valor}
    - Operando derecho -> Operando
    - Resultado -> @var{Valor2}
    @includedef{aplicar_op/3}".


% aplica la operacion: Valor-Operador-Operando=Valor2
aplicar_op(op(+, Operando), Valor, Valor2) :-
    Valor2 is Valor + Operando.

aplicar_op(op(-, Operando), Valor, Valor2) :-
    Valor2 is Valor - Operando.

aplicar_op(op(*, Operando), Valor, Valor2) :-
    Valor2 is Valor * Operando.

aplicar_op(op(//, Operando), Valor, Valor2) :-
    Operando \= 0,
    Valor2 is Valor // Operando.


%tests de aplicar_op
:- test aplicar_op(Op, Valor, Valor2):
    (Op = op(+, 5), Valor = 10) => 
    (Valor2 = 15) + not_fails.

:- test aplicar_op(Op, Valor, Valor2):
    (Op = op(+, 5), Valor = 10,Valor2=7) => 
    (R = no) + fails.

:- test aplicar_op(Op, Valor, Valor2):
    (Op = op(*, 3), Valor = 8) => 
    (Valor2 = 24) + not_fails.

:- test aplicar_op(Op, Valor, Valor2):
    (Op = op(//, 4), Valor = 27) => 
    (Valor2 = 6) + not_fails.

:- test aplicar_op(Op, Valor, Valor2):
    (Op = op(//, 0), Valor = 10) => 
    (R = no) + fails.



%generar_recorrido/6
:- pred generar_recorrido(Ipos, N, Board, DirPerm, Recorrido, Valor)
    #"Obtiene un @var{Recorrido} del tablero @var{Board}, que tiene el tamaño @var{N}x@var{N}, 
    iniciado en la posici@'{o}n @var{Ipos}, teniendo en cuenta las direcciones permitidas en 
    @var{DirPerm}, y obteniendo un valor final @var{Valor}.
    Realiza la llamada al predicado auxiliar 'gen_rec_aux/8' para generar el recorrido.
    @includedef{generar_recorrido/6}".

% Se obtiene un recorrido en el tablero Board (de NxN), iniciado en Ipos
% teniendo en cuenta las direccionesPermitidas y obteniendo un Valor final
generar_recorrido(Ipos, N, Board, DirPerm, Recorrido, Valor) :-
    gen_rec_aux(Ipos, N, Board, DirPerm, [], Recorrido,0,Valor).


%gen_rec_aux/8 
:- pred gen_rec_aux(Pos, N, Board, DirPerm, Visitadas, Recorrido, ValorActual, ValorFinal)
    #"Predicado recursivo auxiliar utilizado por 'generar_recorrido/6' para generar un 
    @var{Recorrido} en un @var{Board} de tamaño @var{N}x@var{N}.

    El predicado realiza la recursividad sobre el @var{Board} y las @var{DirPerm}, 
    manteniendo una lista de celdas visitadas, @var{Visitadas}, y el valor actual del recorrido,
    @var{ValorActual}.

    Al finalizar, unifica el recorrido obtenido en la variable @var{Recorrido} y el valor final en 
    @var{ValorFinal}.@includedef{gen_rec_aux/8}".
    
% predicado recursivo auxiliar a generar_recorrido

% caso base, queda 1 celula
gen_rec_aux(Pos, _N,[Last|[]], _DirPerm, Visit,Rec,ValorActual,ValorFinal):-
    direccion_valida([Last],Pos),
    select_cell(Pos, op(Operador, Operando), [Last], _NewBoard),  % comprobar que existe la celda
    aplicar_op(op(Operador, Operando), ValorActual, ValorFinal),    %obtener nuevo valor
    my_reverse([(Pos, ValorFinal) | Visit], Rec).


% caso recursivo
gen_rec_aux(Pos, N, Board, DirPerm, Visit, Rec, ValorActual, ValorFinal) :-
    movimiento_valido(N, Pos, Dir), %comprobar que el movimiento es valido
    direccion_valida(Board,Pos),
    efectuar_movimiento(Pos, Dir, NewP),
    select_dir(Dir, DirPerm, NewDirs),  %seleccionar dir de la lista
    select_cell(Pos, op(Operador, Operando), Board, NewB),  %comprobar que existe la cell y extraer
    aplicar_op(op(Operador, Operando), ValorActual, NewVal),    %obtener nuevo valor
    gen_rec_aux(NewP, N, NewB, NewDirs, [(Pos, NewVal) | Visit], Rec, NewVal, ValorFinal). %llamada recursiva aumentando la lista de visitadas



%tests de generar_recorrido
:- test generar_recorrido(Ipos, N, Board, DirPerm, Recorrido, Valor):
    (Ipos = pos(1,2),
    N = 2,
    Board = [cell(pos(1,1),op(*,-3)),cell(pos(1,2),op(-,1)),cell(pos(2,1),
    op(-,3)),cell(pos(2,2),op(+,2000))],
    DirPerm = [dir(n,5),dir(s,6),dir(e,7),dir(o,4)]) => 
    (Recorrido = [(pos(1,2),-1),(pos(2,2),1999),(pos(2,1),1996),(pos(1,1),-5988)],
    Valor = -5988) + not_fails.

:- test generar_recorrido(Ipos, N, Board, DirPerm, Recorrido, Valor):
    (Ipos = pos(4,2),
    N = 2,
    Board = [cell(pos(1,1),op(*,-3)),cell(pos(1,2),op(-,1)),cell(pos(2,1),
    op(-,3)),cell(pos(2,2),op(+,2000))],
    DirPerm = [dir(n,5),dir(s,6),dir(e,7),dir(o,4)]) => 
    (R = no) + fails.

:- test generar_recorrido(Ipos, N, Board, DirPerm, Recorrido, Valor):
    (N = 3,
    Board = [cell(pos(1,1),op(*,-3)),cell(pos(1,2),op(-,1)),cell(pos(2,1),
    op(-,3)),cell(pos(2,2),op(+,2000))],
    DirPerm = [dir(n,1),dir(s,6),dir(e,7),dir(o,3)]) => 
    (R = no) + fails.


%---------------------------------

            % APARTADO 2
%generar_recorridos/5
:- pred generar_recorridos(N, Board, DirPerm, Recs, Valor)
    #"Genera todos los recorridos (@var{Recs}) posibles en un tablero de tamaño @var{N}x@var{N}.
    El predicado recibe el tamaño del tablero @var{N}, el tablero @var{Board} que contiene las 
    celdas del tablero, la lista de direcciones permitidas @var{DirPerm}, y unifica los 
    recorridos generados en @var{Recs} y el valor final en @var{Valor}.
    Para generar los recorridos, se obtiene una posici@'{o}n del tablero utilizando el predicado 
    'member/2' y luego se utiliza el predicado 'generar_recorrido/6' para generar un recorrido 
    iniciando desde esa posici@'{o}n. @includedef{generar_recorridos/5}".

% genera todos los recorridos posibles de un tablero
generar_recorridos(N, Board, DirPerm, Recs, Valor) :-
    member(cell(Pos, _Valor), Board), % Obtener una posicion del tablero
    generar_recorrido(Pos, N, Board, DirPerm, Recs, Valor).



%tests de generar_recorridos
:- test generar_recorridos(N, Board, DirPerm, Recs, Valor):
    (N = 3,
    Board = [cell(pos(1,1),op(*,-3)),cell(pos(1,2),op(-,1)),cell(pos(1,3),op(-,1)),
    cell(pos(2,1),op(-,3)),cell(pos(2,2),op(+,2000)),cell(pos(2,3),op(-,3)),
    cell(pos(3,1),op(+,2000)),cell(pos(3,2),op(+,2000)),cell(pos(3,3),op(+,2000))],
    DirPerm = [dir(n,2),dir(s,2),dir(e,2),dir(o,6)]) =>
    (Recorridos = [(pos(1,3),-1),(pos(2,3),-4),(pos(3,3),1996),(pos(3,2),3996),(pos(3,1),5996),
    (pos(2,1),5993),(pos(2,2),7993),(pos(1,2),7992),(pos(1,1),-23976)],
    Valor = -23976 ) + not_fails.

:- test generar_recorridos(N, Board, DirPerm, Recs, Valor):
    (N = 3,
    Board = [cell(pos(1,1),op(*,-3)),cell(pos(1,2),op(-,1)),cell(pos(1,3),op(-,1)),
    cell(pos(2,1),op(-,3)),cell(pos(2,2),op(+,2000)),cell(pos(2,3),op(-,3)),
    cell(pos(3,1),op(+,2000)),cell(pos(3,2),op(+,2000)),cell(pos(3,3),op(+,2000))],
    DirPerm = [dir(n,2),dir(s,2),dir(e,2),dir(o,6)], Valor = -23) =>
    (R = no) + fails.

:- test generar_recorridos(N, Board, DirPerm, Recs, Valor):
    (N = 2,
    Board = [cell(pos(1,1),op(*,-3)),cell(pos(1,2),op(-,1)),cell(pos(1,3),op(-,1)),
    cell(pos(2,1),op(-,3)),cell(pos(2,2),op(+,2000)),cell(pos(2,3),op(-,3)),
    cell(pos(3,1),op(+,2000)),cell(pos(3,2),op(+,2000)),cell(pos(3,3),op(+,2000))],
    DirPerm = [dir(n,2),dir(s,2),dir(e,2),dir(o,6)]) =>
    (R = no) + fails.

:- test generar_recorridos(N, Board, DirPerm, Recs, Valor):
    (N = 3,
    Board = [cell(pos(1,1),op(*,-3)),cell(pos(1,2),op(-,1)),cell(pos(1,3),op(-,1)),
    cell(pos(2,1),op(-,3)),cell(pos(2,2),op(+,2000)),cell(pos(2,3),op(-,3)),
    cell(pos(3,1),op(+,2000)),cell(pos(3,2),op(+,2000)),cell(pos(3,3),op(+,2000))],
    DirPerm = [dir(so,2),dir(n,3),dir(s,1),dir(no,1),dir(e,2),dir(o,6)]) =>
    (Recs = [(pos(1,2),-1),(pos(1,3),-2),(pos(2,3),-5),(pos(3,2),1995),(pos(3,3),3995),
    (pos(2,2),5995),(pos(3,1),7995),(pos(2,1),7992),(pos(1,1),-23976)],
    Valor = -23976) + not_fails.


            % APARTADO 3
%tablero/5
:- pred tablero(N, Tablero, DirPerm, ValorMinimo, NumeroDeRutasConValorMinimo)
    #"Predicado que utiliza el predicado anterior 'generar_recorridos/5' para generar 
    todos los recorridos posibles del @var{Tablero} de tamaño
    @var{N}, teniendo en cuenta las direcciones permitidas @var{DirPerm}.

    Luego, obtiene el @var{ValorMinimo} entre todos los recorridos y 
    cuenta el n@'{u}mero de rutas que tienen dicho valor. Los resultados se unifican con 
    los argumentos @var{ValorMinimo} y @var{NumeroDeRutasConValorMinimo} respectivamente.
    @includedef{tablero/5}".



% sobre todos los recorridos posibles de un tablero, se obtiene el valor minimo y 
% el numero de rutas que lo poseen
tablero(N, Tablero, DirPerm, ValorMinimo, NumeroDeRutasConValorMinimo) :-
    findall((Rec, Valor), generar_recorridos(N, Tablero, DirPerm, Rec, Valor), Recs),
    obtener_valor_minimo(Recs, ValorMinimo),
    recorridos_min(Recs, ValorMinimo, NumeroDeRutasConValorMinimo).


%tests de tablero
:- test tablero(N, Tablero, DirPerm, ValorMinimo, NumeroDeRutasConValorMinimo):
    (N = 4,
    Tablero = [cell(pos(1,1),op(*,-3)),cell(pos(1,2),op(-,1)),cell(pos(1,3),op(-,4)),
    cell(pos(1,4),op(-,555)),cell(pos(2,1),op(-,3)),cell(pos(2,4),op(-,444)),cell(pos(3,1),op(*,0)),
    cell(pos(3,4),op(+,20)),cell(pos(4,1),op(-,2)),cell(pos(4,2),op(-,1000)),cell(pos(4,3),op(-,9)),
    cell(pos(4,4),op(*,4))],
    DirPerm = [dir(n,2),dir(s,2),dir(e,2),dir(o,6)]) =>
    (R = no) + fails.

:- test tablero(N, Tablero, DirPerm, ValorMinimo, NumeroDeRutasConValorMinimo):
    (N = 4,
    Tablero =[cell(pos(1,1),op(*,-3)),cell(pos(1,2),op(-,1)),cell(pos(1,3),op(-,4)),
    cell(pos(1,4),op(-,555)),cell(pos(2,1),op(-,3)),cell(pos(2,4),op(-,444)),cell(pos(3,1),op(*,0)),
    cell(pos(3,4),op(+,20)),cell(pos(4,1),op(-,2)),cell(pos(4,2),op(-,1000)),cell(pos(4,3),op(-,9)),
    cell(pos(4,4),op(*,4))],
    DirPerm = [dir(n,5),dir(s,6),dir(e,7),dir(o,4)]) =>
    (ValorMinimo = -5028, NumeroDeRutasConValorMinimo = 1).









