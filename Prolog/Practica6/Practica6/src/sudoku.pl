  /*

       - Logica Computacional 2020-2

       - Practica 6

       - Alumno: Toledo Reyes Jose Miguel

       - Numero de cuenta: 316164069

       - Alumno: Gramer Muloz Omar Fernando

       - Numero de cuenta: 419003698

  */



:-use_module(library(clpfd)).


sudoku(A1,A2,A3,A4,A5,A6,A7,A8,A9,
    B1,B2,B3,B4,B5,B6,B7,B8,B9,
    C1,C2,C3,C4,C5,C6,C7,C8,C9,
    D1,D2,D3,D4,D5,D6,D7,D8,D9,
    E1,E2,E3,E4,E5,E6,E7,E8,E9,
    F1,F2,F3,F4,F5,F6,F7,F8,F9,
    G1,G2,G3,G4,G5,G6,G7,G8,G9,
    H1,H2,H3,H4,H5,H6,H7,H8,H9,
    I1,I2,I3,I4,I5,I6,I7,I8,I9):-
                                sudoku([[A1,A2,A3,A4,A5,A6,A7,A8,A9],
                                        [B1,B2,B3,B4,B5,B6,B7,B8,B9],
                                        [C1,C2,C3,C4,C5,C6,C7,C8,C9],
                                        [D1,D2,D3,D4,D5,D6,D7,D8,D9],
                                        [E1,E2,E3,E4,E5,E6,E7,E8,E9],
                                        [F1,F2,F3,F4,F5,F6,F7,F8,F9],
                                        [G1,G2,G3,G4,G5,G6,G7,G8,G9],
                                        [H1,H2,H3,H4,H5,H6,H7,H8,H9],
                                        [I1,I2,I3,I4,I5,I6,I7,I8,I9]]),

                                printSudoku([A1,A2,A3,A4,A5,A6,A7,A8,A9,
                                             B1,B2,B3,B4,B5,B6,B7,B8,B9,
                                             C1,C2,C3,C4,C5,C6,C7,C8,C9,
                                             D1,D2,D3,D4,D5,D6,D7,D8,D9,
                                             E1,E2,E3,E4,E5,E6,E7,E8,E9,
                                             F1,F2,F3,F4,F5,F6,F7,F8,F9,
                                             G1,G2,G3,G4,G5,G6,G7,G8,G9,
                                             H1,H2,H3,H4,H5,H6,H7,H8,H9,
                                             I1,I2,I3,I4,I5,I6,I7,I8,I9]).

sudoku( Filas ) :-
        % Verifica que el sudoku sea un tablero de 9x9.
        length(Filas, 9),
        maplist(same_length(Filas), Filas),

        % Une todas las filas en una lista llamada Tablero.
        append(Filas, Tablero),

        % Establecemos el dominio de los elementos del tablero a naturales entre 1 a 9.
        Tablero ins 1..9,

        % Verifica para fila que esta no tenga algun elemento repetido.
        maplist(all_distinct, Filas),

        % Saca la trasnpuesta  de Filas para poder manejar las columnas.
        transpose(Filas, Columnas),

        % Verifica para columna que esta no tenga algun elemento repetido.
        maplist(all_distinct, Columnas),

        % Se nombra individualmente a cada fila.
        Filas = [F1,F2,F3,F4,F5,F6,F7,F8,F9],

        % Primera sección de bloques, conformados por los elementos de las primeras tres filas.
        bloques(F1, F2, F3),
        % Segunda sección de bloques, conformados por los elementos de las tres filas del medio.
        bloques(F4, F5, F6),
        % Tercera sección de bloques, conformados por los elementos de las últimas tres filas.
        bloques(F7, F8, F9),

        maplist(label,Filas).


% Verifica que los elementos no se repitan en los bloques conformados por elementos de las filas recibidas.

% Caso base: Un bloque vacío se considera correcto para terminar la recursión.
bloques([], [], []).

% 
bloques([Num1,Num2,Num3| Fila1 ], [Num4,Num5,Num6| Fila2 ], [Num7,Num8,Num9| Fila3 ]) :-
        all_distinct([Num1,Num2,Num3,Num4,Num5,Num6,Num7,Num8,Num9]), 
        % Recursión sobre el siguiente bloque en el mismo sector de filas.
        bloques(Fila1, Fila2, Fila3).


printSudoku([A1,A2,A3,A4,A5,A6,A7,A8,A9,
             B1,B2,B3,B4,B5,B6,B7,B8,B9,
             C1,C2,C3,C4,C5,C6,C7,C8,C9,
             D1,D2,D3,D4,D5,D6,D7,D8,D9,
             E1,E2,E3,E4,E5,E6,E7,E8,E9,
             F1,F2,F3,F4,F5,F6,F7,F8,F9,
             G1,G2,G3,G4,G5,G6,G7,G8,G9,
             H1,H2,H3,H4,H5,H6,H7,H8,H9,
             I1,I2,I3,I4,I5,I6,I7,I8,I9]):-

        write("╔═══════╦═══════╦═══════╗\n"),
        write("║ ") ,write(A1),write(" "),write(A2),write(" "),write(A3),
        write(" ║ "),write(A4),write(" "),write(A5),write(" "),write(A6),
        write(" ║ "),write(A7),write(" "),write(A8),write(" "),write(A9),write(" ║\n"),

        write("║ ") ,write(B1),write(" "),write(B2),write(" "),write(B3),
        write(" ║ "),write(B4),write(" "),write(B5),write(" "),write(B6),
        write(" ║ "),write(B7),write(" "),write(B8),write(" "),write(B9),write(" ║\n"),

        write("║ ") ,write(C1),write(" "),write(C2),write(" "),write(C3),
        write(" ║ "),write(C4),write(" "),write(C5),write(" "),write(C6),
        write(" ║ "),write(C7),write(" "),write(C8),write(" "),write(C9),write(" ║\n"),
        write("╠═══════╬═══════╬═══════╣\n"),

        write("║ ") ,write(D1),write(" "),write(D2),write(" "),write(D3),
        write(" ║ "),write(D4),write(" "),write(D5),write(" "),write(D6),
        write(" ║ "),write(D7),write(" "),write(D8),write(" "),write(D9),write(" ║\n"),

        write("║ ") ,write(E1),write(" "),write(E2),write(" "),write(E3),
        write(" ║ "),write(E4),write(" "),write(E5),write(" "),write(E6),
        write(" ║ "),write(E7),write(" "),write(E8),write(" "),write(E9),write(" ║\n"),

        write("║ ") ,write(F1),write(" "),write(F2),write(" "),write(F3),
        write(" ║ "),write(F4),write(" "),write(F5),write(" "),write(F6),
        write(" ║ "),write(F7),write(" "),write(F8),write(" "),write(F9),write(" ║\n"),
        write("╠═══════╬═══════╬═══════╣\n"),

        write("║ ") ,write(G1),write(" "),write(G2),write(" "),write(G3),
        write(" ║ "),write(G4),write(" "),write(G5),write(" "),write(G6),
        write(" ║ "),write(G7),write(" "),write(G8),write(" "),write(G9),write(" ║\n"),

        write("║ ") ,write(H1),write(" "),write(H2),write(" "),write(H3),
        write(" ║ "),write(H4),write(" "),write(H5),write(" "),write(H6),
        write(" ║ "),write(H7),write(" "),write(H8),write(" "),write(H9),write(" ║\n"),
        write("║ ") ,write(I1),write(" "),write(I2),write(" "),write(I3),
        write(" ║ "),write(I4),write(" "),write(I5),write(" "),write(I6),
        write(" ║ "),write(I7),write(" "),write(I8),write(" "),write(I9),write(" ║\n"),
        write("╚═══════╩═══════╩═══════╝\n").




