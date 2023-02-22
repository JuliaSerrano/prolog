% Este programa es un ejemplo simple de Ciao Prolog
% que imprime "Hola, mundo!" en la consola.

:- initialization(main).

main :-
    write('Hola, mundo!'),
    nl,
    halt.
