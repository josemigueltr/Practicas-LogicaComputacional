{-
- Logica Conmputacional 2020-2 
- Practica 3, Implementación del algoritmo dpll.
- Creación: Pedro Juan Salvador Sánchez Pérez
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
-- y de ser así agrega las literales de dichas clausulas al modelo m.
unit :: Solucion -> Solucion
unit ( m, f ) = ( ( union m [ head(u) | u <- (unitarias f) ] ), diferencia f (unitarias f) )

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
elim :: Solucion -> Solucion
elim ( m, f ) = ( m, diferencia f ( [ c| l <-m, c<-f, l `elem` c] ) )



{- ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
   ┃     Seccion de funciones para la regla de reduccion                     ┃ 
   ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ -}
red :: Solucion -> Solucion
red  ( m, f ) = ( m, [ (reduce m c) | c<-f ] )

-- reduce -> Dada una lista de literales ls y una clausula c regresa la clausula resultante
-- de haber reducido c respecto a cada literal l en ls. 
reduce :: [Literal] -> Clausula -> Clausula
reduce ls c = diferencia c ( [ x | x <- c, l <- ls, x == (meteNeg (Neg l)) ] )



{- ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
   ┃     Seccion de funciones para la regla de separacion                    ┃ 
   ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ -}
split :: Solucion -> [Solucion]
split ( m , f ) = splitAux m [(m,f)]

splitAux :: Modelo -> [Solucion] -> [Solucion]
splitAux []     ss = []
splitAux (l:ms) ss =  union ( splitAux ms (splitAll l ss)) (splitAll l ss) 

splitAll :: Literal -> [Solucion] -> [Solucion]
splitAll l []         = []
splitAll l ((m,f):ss) = union ( splitme l (m,f) ) ( splitAll l ss)




{- ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
   ┃     Seccion de funciones para la regla de separacion                    ┃ 
   ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ -}

split :: Solucion -> [Solucion]
split (m,f)
   |(buscaMod m f) == [] = [(m,f)] 
   |otherwise = (splitme  (head(buscaMod m f)) (m,f) )


--buscaMod -> Dado un modelo y una formula, regresa una lista de las variables de la formula 
--que no figuran en el modelo, incluyendo sus negaciones. 
buscaMod::Modelo->Formula->Clausula
buscaMod _ []=[]
buscaMod m l = [e |cl <- l , e <- cl , e `notElem` m ,  meteNeg(Neg e) `notElem` m ]


-- splitme -> Dada una literal l y una solución regresa una lista con dos soluciones, donde
-- la primer solución s1 une a la literal l a al modelo y la segunda solución s2 une a la
-- negación de la literal l al modelo. 
splitme :: Literal -> Solucion -> [Solucion]
splitme l (m,f) = [ s1, s2 ]
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
success (m,[]) =True
success  (m,f)   =False 



-- Ejemplos

exa0 = [[Neg p, r, Neg t],[Neg q, Neg r],[p,Neg s],[Neg p, q, Neg r, Neg s]]
exa1 = [[p, q],[Neg q],[Neg p, q, Neg r]]
exa2 = [[p, q],[ p, Neg q],[r, q],[r, Neg q]]    
exa3 = [[p, Neg q],[Neg p, q],[q, Neg r],[Neg q, Neg r]]
exa4 = [[p, q], [r, Neg q, Neg s], [Neg p, s], [Neg r]]
exa5 = [[p, q, r], 
        [p, Neg q, Neg r],
        [p, Neg w ],
        [Neg q, Neg r, Neg w ],
        [Neg p, q, r ],
        [u, Neg x],
        [u, x ],
        [q, Neg u ],
        [Neg r, Neg u ]]
exa6 = [[p], [Neg p]]        


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