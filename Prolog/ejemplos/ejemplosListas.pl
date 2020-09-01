% Permutación de una lista
permutacion([],[]).
permutacion(L1,[X|L2]) :- select(X,L1,L3), permutacion(L3,L2).

% Función que verifica si una lista está ordenada
ordenada([]).
ordenada([_]).
ordenada([X,Y|L]) :- X @=< Y, ordenada([Y|L]).

% Función que ordena una lista
ordenacion(L,L1) :- permutacion(L,L1), ordenada(L1).

% Cuadrados mágicos 

cuadradomagico([A,B,C,D,E,F,G,H,I]) :- permutacion([1,2,3,4,5,6,7,8,9],[A,B,C,D,E,F,G,H,I]), 
A+B+C =:= 15, D+E+F =:= 15,
G+H+I =:= 15, A+D+G =:= 15,
B+E+H =:= 15, C+F+I =:= 15,
A+E+I =:= 15, C+E+G =:= 15.

% Autómatas finitos no deterministas

% Estado final
final(e3).

% Transiciones 
trans(e1,a,e1). 
trans(e1,a,e2). 
trans(e1,b,e1).
trans(e2,b,e3).
trans(e3,b,e4).

% Transiciones epsilon
nulo(e2,e4).
nulo(e3,e1).

% Aceptación de una cadena
acepta(E,[]) :- final(E).
acepta(E,[X|L]) :- trans(E,X,E1), acepta(E1,L).
acepta(E,L) :- nulo(E,E1), acepta(E1,L).

% El problema de la cebra

% Función que nos indica dentro de una lista la casa que está a la izquierda de otra
aLaIzq([Izq|[Der|_]],Izq,Der).
aLaIzq([_|Resto],Izq,Der) :- aLaIzq(Resto,Izq,Der).

% Función que nos dice si una casa determinada está a lado de otra ya sea izquierda o derecha.
alLado(Calle,X,Y) :- aLaIzq(Calle,X,Y).
alLado(Calle,X,Y) :- aLaIzq(Calle,Y,X).

% La informacion de cada casa es la siguiente 
% [Número de casa,Color,Nacionalidad,Bebida,Mascota,Marca de cigarillos]

respuesta(Calle) :- Calle = [[1,_,noruego,_,_,_],[2,azul,_,_,_,_],[3,_,_,leche,_,_],[4,_,_,_,_,_],[5,_,_,_,_,_]],
                   member([_,roja,britanico,_,_,_],Calle),
                   member([_,_,sueco,_,perro,_],Calle),
                   member([_,_,danes,te,_,_],Calle),
                   member([_,verde,_,cafe,_,_],Calle),
                   member([_,_,_,_,pajaros,pallmall],Calle),
                   member([_,amarilla,_,_,_,dunhill],Calle),
                   member([_,_,_,cerveza,_,bluemaster],Calle),
                   member([_,_,aleman,_,_,prince],Calle),
                   member([_,_,_,_,cebra,_],Calle),
                   aLaIzq(Calle,[_,verde,_,_,_,_],[_,blanca,_,_,_,_]),
                   alLado(Calle,[_,_,_,_,_,blends],[_,_,_,_,gatos,_]),
                   alLado(Calle,[_,_,_,_,caballos,_],[_,_,_,_,_,dunhill]),
                   alLado(Calle,[_,_,_,_,_,blends],[_,_,_,agua,_,_]).

mascota(Mascota,Persona):- respuesta(Calle), member([_,_,Persona,_,Mascota,_],Calle).
