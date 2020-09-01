{-
- Logica Conmputacional 2020-2 
- Práctica 4, Implementación del algoritmo dpll.
- Creador: Pedro Juan Salvador Sánchez Pérez

- Alumno: José Miguel Toledo Reyes
- Número de cuenta: 316164069
- Correo: josemigueltr@ciencias.unam.mx

- Alumno: Omar Fernando Gramer Muñoz
- Número de cuenta: 419003698
- Correo: grameromar@ciencias.unam.mx
-}

module DPLL where

import LProp
import Data.List

type Literal  = Prop
type Clausula = [Literal]
type Formula  = [Clausula]
type Modelo   = [Literal]
type Solucion = (Modelo, Formula)



{- ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
   ┃     Seccion de funciones para la regla de la clausula unitaria          ┃ 
   ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ -}
-- unit -> Dada una solución (m,f) verficia si la formula f tiene clausulas unitarias u,
-- y de ser así agrega cuidadosamente las literales de dichas clausulas al modelo m.
unit :: Solucion -> Solucion
unit ( m, f ) = ( units, diferencia f ([[a]| a<-units]) )
   where units = ( filtraUnit m ([head(u) | u <- (unitarias f) ]) )


-- filtraUnit -> Dado un modelo m y una lista de literales ls, regresa el modelo 
-- resultante de agregar de ls las literales que no figuraban en m. Evitando así
-- agregar posibles contradicciones al modelo. 
filtraUnit:: Modelo -> [Literal] -> Modelo
filtraUnit m []= m
filtraUnit m (l:ls)
   | ( meteNeg(Neg l ) `elem` m || l `elem` m ) = filtraUnit m ls 
   | otherwise = filtraUnit ( l:m ) ls


-- esUnitaria -> Dada una clausula c verifica si c es unitaria, es decir,
-- si c está únicamente compuesta por una proposición atómica ( una 
-- constante, una variable o la negación de una variable ).
esUnitaria :: Clausula -> Bool
esUnitaria [_] = True
esUnitaria _   = False


-- unitarias -> Dada una fórmula f regresa una fórmula que 
-- contiene únicamente las clausulas unitarias de f.
unitarias :: Formula -> [Clausula]
unitarias f = [ c | c <- f, esUnitaria (c) ]




{- ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
   ┃     Seccion de funciones para la regla de eliminacion                   ┃ 
   ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ -}
-- elim -> Dada una solución (m,f) regresa la solución resultante tras eliminar
-- todas las clausulas que contienen a alguna de las literales del modelo.
elim :: Solucion -> Solucion
elim ( m, f ) = ( m, diferencia f ( [ c| l <-m, c<-f, l `elem` c] ) )




{- ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
   ┃     Seccion de funciones para la regla de reduccion                     ┃ 
   ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ -}
-- red -> Dada una solución (m,f) regresa la solución resultante tras reducir
-- en todas las clausulas las literales del modelo.
red :: Solucion -> Solucion
red  ( m, f ) = ( m, [ (reduce m c) | c<-f ] )


-- reduce -> Dada una lista de literales ls y una clausula c regresa la clausula resultante
-- de haber reducido c respecto a cada literal l en ls. 
reduce :: [Literal] -> Clausula -> Clausula
reduce ls c = diferencia c ( [ x | x <- c, l <- ls, x == (meteNeg (Neg l)) ] )




{- ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
   ┃     Seccion de funciones para la regla de separacion                    ┃ 
   ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ -}
-- split -> Dada una lista de soluciones regresa la lista resultante al aplicar
-- split individualmente a cada solución ( usando splitIndividual en cada una ).
split :: [Solucion] -> [Solucion]
split [] = []
split (s:ls) = union (splitMe s) (split ls)


-- splitMe -> Aplica split en la solución dada con la primer literal no 
-- contenida en el modelo que encuentre en la formula.
splitMe :: Solucion -> [Solucion]
splitMe s
   |( (buscaLits s ) == [] ) = [s] 
   |otherwise = ( splitMeAux( head( buscaLits s ) ) s )


-- splitMeAux -> Dada una literal l y una solución regresa una lista con dos soluciones, donde
-- la primer solución s1 une a la literal l a al modelo y la segunda solución s2 une a la
-- negación de la literal l al modelo. 
splitMeAux :: Literal -> Solucion -> [Solucion]
splitMeAux l (m,f) = [ s1, s2 ]
    where s1 = ( union m [l], f)
          s2 = ( union m [ meteNeg (Neg l) ], f)




{- ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
   ┃    Seccion de funciones para la regla de conflicto                      ┃ 
   ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ -}
conflict :: Solucion -> Bool
conflict (m,[[]]) = True
conflict (m,f)    = False




{- ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
   ┃    Seccion de funciones para la regla de exito                          ┃ 
   ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ -}
success :: Solucion -> Bool
success (m,[]) = True
success (m,f)  = False 





-- Seccion de las funciones principales de DPLL

dpllsearch :: [Solucion] -> [Solucion]
dpllsearch ls = [ (red $ elim s) | s <- ls, success( red $ elim s )]


dpll :: Solucion -> [Solucion]
dpll s = dpllsearch ( generaModelos s )


-- generaModelos -> Dada una solución, genera todos los posibles modelos a
-- probar usando consecutivamente la regla de split.
generaModelos :: Solucion -> [Solucion]
generaModelos s = generaModelosAux ( ls ) ( length ( buscaLits ( unit s ) ) )
    where ls = [ unit s ]

-- generaModelosAux -> Dada una solución y un entero n, realiza
-- split n veces consecutivas.
generaModelosAux :: [Solucion] -> Int -> [Solucion]
generaModelosAux ls 0 = split ls
generaModelosAux ls n = generaModelosAux ( split ls ) ( n-1 ) 


main :: Solucion -> [Solucion]
main s
    | soluciones == []  = error "La fórmula es insatisfacible"
    | otherwise         = soluciones
    where soluciones = (dpll s)


-- Ejemplos

bueno = [[Neg (V "P"), V "R", Neg (V "T")],[Neg (V "Q"), Neg (V "R")],[V "P",Neg (V "S")],[Neg (V "P"), V "Q", Neg (V "R"), Neg (V "S")]]
exe1 = [[V "p", V "q"],[Neg (V "q")],[Neg (V "p"), V "q", Neg (V "r")]]
exe2 = [[V "p", V "q"],[V "p", Neg (V "q")],[V "r", V "q"],[V "r", Neg (V "q")]]    
exe3 = [[V "p", Neg (V "q")],[Neg (V "p"), V "q"],[V "q", Neg (V "r")],[Neg (V "q"), Neg (V "r")]]
exe4 = [[V "p", V "q"], [V "r", Neg (V "q"), Neg (V "s")], [Neg (V "p"), V "s"], [Neg (V "r")]]
exe5 = [[V "p", V "q", V "r"], 
        [V "p", Neg (V "q"), Neg (V "r")],
        [V "p", Neg (V "w")],
        [Neg (V "q"), Neg (V "r"), Neg (V "w")],
        [Neg (V "p"), Neg (V "q"), V "r"],
        [V "u", Neg (V "x")],
        [V "u", V "x"],
        [V "q", Neg (V "u")],
        [Neg (V "r"), Neg (V "u")]]
exe6 = [[V "p"], [Neg (V "p")]]        

ejemplo1 = main ([], exe1 )
ejemplo2 = main ([], exe2 )
ejemplo3 = main ([], exe3 )
ejemplo4 = main ([], exe4 )
ejemplo5 = main ([], exe5 )   
ejemplo6 = main ([], bueno)   
ejemplo7 = main ([], exe6 )




{- ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
   ┃             Funciones auxiliares             ┃ 
   ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ -}

-- diferencia -> Dadas dos listas l1 y l2 regresa una lista de la diferencia de los
-- elementos de l1 con los elementos de l2.
diferencia::Eq a => [a] -> [a] -> [a]
diferencia l1 l2 = [ x | x <- l1, x `notElem` l2 ]


-- eliminaDuplicados -> Regresa una copia de la lista eliminando los elementos duplicados.
eliminaDuplicados :: Eq a => [a] -> [a]
eliminaDuplicados [] = [] 
eliminaDuplicados (x:xs) 
    | x `elem` xs =   ( eliminaDuplicados xs )
    | otherwise   = x:( eliminaDuplicados xs )


--buscaLits -> Dado un modelo y una formula, regresa una lista de las literales en la formula 
--que no figuran en el modelo.
buscaLits:: Solucion -> Clausula
buscaLits (_,[]) = []
buscaLits (m, f) = [l |cl <- f , l <- cl , l `notElem` m ,  meteNeg(Neg l) `notElem` m ]