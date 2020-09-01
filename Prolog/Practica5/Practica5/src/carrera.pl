/*
	{-
	- Logica Conmputacional 2020-2 
	- Practica 5, Problemas Logicos 
	- Alumno: Toledo Reyes JOse Miguel
	- Correo:josemigueltr@ciencias.unam.mx
	-}
*/

	
	/*
	Representacion de los elementos de carrera:
    [posicion,nombre,auto,color,pais]
	*/

	respuesta(Carrera):-Carrera=[ [1,_,_,_,_],
								  [2,_,_,_,_],
								  [3,_,_,_,_],
								  [4,_,_,_,_],
								  [5,_,_,_,_]],


	/*a) El conductor del auto rojo es italiano.*/
	member([_,_,_,rojo,italia],Carrera),

	
	/* b) El Monza clasifico delante del de Argentina.*/  
	delanteDe(Carrera,[_,_,monza,_,_],[_,_,_,_,argentina]), 

    
	/*c) El Mazda clasifico en tercer lugar.*/
	/*d) jorge es de alemania*/
	/*j) Jorge se clasifico en tercer lugar y su auto no es azul.*/
	member([3,jorge,mazda,_,alemania],Carrera),


    /*e) El Monza es de color amarillo.*/
	/*g) El auto frances es de color amarillo.*/
    /*m) Pablo es piloto del auto amarillo.*/
	member([_,pablo,monza,amarillo,francia],Carrera),
	

	/*f) El auto de Pablo se clasifico delante del auto de Mario.*/
	delanteDe(Carrera,[_,pablo,_,_,_],[_,mario,_,_,_]),

	
	/*h) Mario corrio un Ferrari.*/
	member([_,mario,ferrari,_,_],Carrera),
	
	
	/*i) El Susuzi clasifico despues del auto de Raul y no llego en segundo lugar.*/
	member([P,_,susuzi,_,_],Carrera),P\=2,


	/* i) El Susuzi clasifico despues del auto de Raul y no llego en segundo lugar.*/  
	delanteDe(Carrera,[_,raul,_,_,_],[_,_,susuzi,_,_]),



	/*k) El Trooper se clasifico luego del de Alemania y antes del rojo.*/
	delanteDe(Carrera,[_,_,_,_,alemania],[_,_,trooper,_,_]),


    /*k) El Trooper se clasifico luego del de Alemania y antes del rojo.*/
	delanteDe(Carrera,[_,_,trooper,_,_],[_,_,_,rojo,_]),


    /*l) El auto negro no llego ni en primero ni en ultimo.*/
	member([T,_,_,negro,_],Carrera),(T\=1 , T\=5),


	/*n) Carlos termino despues del de Brasil.*/
	delanteDe(Carrera,[_,_,_,_,brasil],[_,carlos,_,_,_]),
    
	/*o) El auto de Argentina es blanco.*/
	member([_,_,_,blanco,argentina],Carrera),

	/*j) Jorge se clasifico en tercer lugar y su auto no es azul.*/
	member([3,_,_,X,_],Carrera),X\=azul,
    

	%Datos necesarios
	member([_,_,_,azul,_],Carrera),


	member([_,_,trooper,_,_],Carrera),
	
	
	member([_,_,_,_,brasil],Carrera),
    
	
	member([_,_,_,negro,_],Carrera).

    /*Función que nos dice si un corredor llego despues de otro.*/
	delanteDe([Izq|[Der|_]],Izq,Der).
	delanteDe([_|Resto],Izq,Der):-delanteDe(Resto,Izq,Der).





	   
	



	


	
