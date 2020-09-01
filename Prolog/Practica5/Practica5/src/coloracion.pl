/*
	{-
	- Logica Conmputacional 2020-2 
	- Practica 5, Problemas Logicos 
	- Alumno: Toledo Reyes JOse Miguel
	- Correo:josemigueltr@ciencias.unam.mx
	-}
*/

/*
Coloracion:

El teorema de los cuatro colores establece que cualquier mapa puede ser coloreado con cuatro colores
diferentes, de tal forma que no queden regiones adyacentes con el mismo color.
El cual se cumple si se puede asignar un color a cada regi ́on tal que se forme una coloraci ́on correcta,
es decir, ninguna regi ́on comparte el mismo color con sus regiones adyacentes y adem ́as s ́olo se utilizan
4 colores para colorear cada regi ́on.

la representacion y coloracion de los mapas sera de la siguiente manera:

Ejemplo:

 m1:
         ╔══════════╦══════════╗          
         ║    a     ║     b    ║        
         ╠════╦═════╩═════╦════╣       
         ║    ║           ║    ║        
         ║  c ║     d     ║ e  ║           
         ║    ║           ║    ║            
         ╠════╩═════╦═════╩════╣                      
         ║    f     ║     g    ║
         ╚══════════╩══════════╝     

mapa(m1,[a-[b,c,d],b-[a,d,e],c-[a,d,f],d-[a,b,c,e,f,g],e-[b,d,g],f-[c,d,g],g-[d,e,f]]).

En donde se representara un mapa con una lista de adyacencias la cual indicara cuales regiones 
del mapa son adyacentes entre si.

La salida que se espera es una lista de pares en donde se muestre de que color se pintaran las regiones del mapa
En caso  de que no se halla podido colorear el mapa siguiendo el teorema de los 4 colores, se imprimira falso.

*/

mapa(m1,[a-[b,c,d],b-[a,d,e],c-[a,d,f],d-[a,b,c,e,f,g],e-[b,d,g],f-[c,d,g],g-[d,e,f]]).

%Mapa de la ciudad de mexico, basado en el dibujo que venia en la practica5
mapa(cdmx,[r1-[r2,r4,r5],
           r2-[r1,r3,r4],
           r3-[r2,r4,r6,r7,r8],
           r4-[r1,r2,r3,r5,r8,r9],
           r5-[r1,r4,r9],
           r6-[r3,r7],
           r7-[r3,r6,r8,r10,r12,r13],
           r8-[r3,r4,r7,r9,r10,r11],
           r9-[r4,r5,r8,r11],
           r10-[r7,r8,r11,r13,r14],
           r11-[r8,r9,r10,r14,r15],
           r12-[r7,r13],
           r13-[r7,r10,r12,r14,r16],
           r14-[r10,r11,r13,15,r16],
           r15-[r11,r14,r16],
           r16-[r13,r14,r15]
           ]).

        
        
        
%Función que recibe un mapa,colores y responde si es posible colorearlo.

%En caso de que sea posible y se cumpla el teorema de los 4 colores,se regresara una
%lista de pares formados por una region del mapa y uno de los colores que se ingresaron
%Los pares de las regiones que son adyacentes, tendran un color distinto.                
coloracion(Mapa,Colores,S) :-
mapa(Mapa,Lista), 
coloracion_aux(Lista,Colores,[],S).


%Función que recibe una lista de adyacencias,colores,Conjunto de pares,y trata de colorear cada region del
%mapa con los colores que se proporcionaron,siguiendo el teorema de los 4 colores.
coloracion_aux([],_,S,S).
coloracion_aux([Reg-Ady|Resto],Colores,Conj,S) :-
%Se obtiene un color perteneciente a la lista de colores.
member(C,Colores),
%Se revisa que las regiones que son adyacentes a la region con la que se esta trabajando
%si esta ya se encuentra en el conjunto de pares con el mismo color que la region.
%entonces se desecha esa rama y se hace el backtraking.
not((member(Vec,Ady), member(Vec-C,Conj))),  
%Llamada recursiva para colorear las demas regiones del mapa
coloracion_aux(Resto,Colores,[Reg-C|Conj],S).






        


        