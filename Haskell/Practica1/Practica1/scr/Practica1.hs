{-
- Lógica computacional 2020-2
- Practica 1

- Alumno: José Miguel Toledo Reyes
- Número de cuenta: 316164069
- Correo: josemigueltr@ciencias.unam.mx

- Alumno: Omar Fernando Gramer Muñoz
- Número de cuenta: 419003698
- Correo: grameromar@ciencias.unam.mx
-}

module Practica1 where

{-  ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
    ┃               Tipos definidos                ┃ 
    ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ -}

-- Complejo -> Define el tipo de los números complejos mediante una dupla de flotantes.
data Complejo = Z Float Float deriving(Show,Eq)


-- Nat -> Define la implementación recursiva de los números naturales iniciando desde cero.
data Nat = Cero | Suc Nat deriving(Show,Eq)


-- Lista -> Define la implementación personalizada de listas recursivas, la lista más pequeña es 
-- la lista Nula.
data Lista a = Nula | Cons a (Lista a) deriving(Show,Eq)


-- Arbol -> Define la implementación recursiva de árboles binarios, el árbol más pequeño es 
-- el árbol vacío.
data Arbol = Vacio | Nodo Arbol Int Arbol deriving(Show,Eq)




{-  ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
    ┃           Funciones principales              ┃ 
    ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ -}

-- puntoMedio -> Dados dos puntos en el plano encuentra el punto medio entre los dos.
puntoMedio :: (Float,Float) -> (Float,Float) -> (Float,Float)
puntoMedio (a,b) (c,d) = ( (a+c)/2,(b+d)/2 )


-- raices -> Dados los coeficientes de una ecuación de segundo grado encuentra las raices
-- de esta en una pareja ordenada de números complejos.
raices :: Float -> Float -> Float -> (Complejo,Complejo)
raices a b c       
    | d >=0 = ( Z ((-b+e)/(2*a)) (0), Z (-(-b+e)/(2*a)) (0) )
    | d <0  = ( Z ( b/(2*a) ) ( sqrt(-d)/(2*a) ), Z ( -b/(2*a) ) ( -sqrt(-d)/(2*a) ) )
    where d = b^2-4*a*c
          e = sqrt d 


-- segmento -> Regresa una lista de los elementos de xs comprendidos entre las posiciones m y n.
segmento :: Int -> Int -> [a] -> [a]
segmento 1 1 (x:xs) = [x]
segmento m n (x:xs)
    | m > 1 = segmento (m-1) (n-1) xs
   where size = length xs
segmento 1 n (x:xs) = x:( segmento 1 (n-1) xs )


-- extremos -> Regresa la lista formada por los n primeros elementos de xs y los n finales 
-- elementos de xs.
extremos :: Int -> [a] -> [a]
extremos n xs 
    | ( n > size `div` 2 ) = xs
    | otherwise = ( (segmento 1 n xs) ++ (segmento (size-n+1) size xs) )
   where size = length xs


-- dIntervalos ->  Dados dos números enteros y una lista, elimina los elementos que se encuentren
-- en el intervalo de esos dos numeros.
dIntervalos :: Int -> Int -> [a] -> [a]
dIntervalos m n xs = ( (segmento 1 (m-1) xs ) ++ (segmento (n+1) size xs) )
   where size = length xs


-- numerosAbundantes -> Regresa una lista de números abundantes menores o iguales que n.
numerosAbundantes :: Int -> [Int]
numerosAbundantes 0 = []
numerosAbundantes n  
    | n < suma  = n : numerosAbundantes(n-1)
    | otherwise =     numerosAbundantes(n-1)
  where suma = sum( tail( divisores n ) )


-- eliminaDuplicados -> Regresa una copia de la lista eliminando los elementos duplicados.
eliminaDuplicados :: Eq a => [a] -> [a]
eliminaDuplicados [] = [] 
eliminaDuplicados (x:xs) 
    | x `elem` xs =   ( eliminaDuplicados xs )
    | otherwise   = x:( eliminaDuplicados xs )


-- primitivo -> Calcula el primitivo del número recibido. 
primitivo :: Integer -> Integer
primitivo n
    | n < 10    = n
    | otherwise = primitivo ( product ( digitos n ) )


-- sipLis -> Dadas dos listas y un Natural j, regresa una lista tal que, se encuentran 
-- concatenados el i-ésimo elemento de la primer Lista con el i-ésimo elemento de la 
-- segunda Lista; a partir del elemento j de cada una de las listas.
sipLis :: Nat -> Lista a -> Lista a -> Lista a
sipLis n xs ys = combina ( omitir ( (int n)-1) xs ) ( omitir ( (int n)-1) ys ) 



--a planaArbolPre -> Dado un arbol, regresa una lista de los elementos del árbol en recorrido preorden.
aplanaArbolPre :: Arbol -> [Int]
aplanaArbolPre Vacio         = []
aplanaArbolPre (Nodo izq int der) = int : (aplanaArbolPre izq ++ aplanaArbolPre der)





{- ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
   ┃             Funciones auxiliares             ┃ 
   ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ -}

-- divisores -> Dado n un número entero, regresa una lista con los divisores propios de n, del mayor
-- al menor, incluyendo a n.
divisores :: Int -> [Int]
divisores n = divisoresAux n n

divisoresAux:: Int ->Int->[Int]
divisoresAux n 0 = []
divisoresAux n p
    | ( n `mod` p == 0 ) = p:( divisoresAux n (p-1) )
    | otherwise          =   ( divisoresAux n (p-1) )


-- digitos -> Regresa la descomposición un número en una lista de sus dígitos en orden.
digitos :: Integer -> [Integer]
digitos 0 = []
digitos n = digitos (n `div` 10) ++ [n `mod` 10]


-- int -> Regresa una representación númerica entera del Nat ( natural ) recibido.
int :: Nat -> Integer
int Cero    = 0
int (Suc n) = 1 + (int n)


-- omitir -> Regresa una Lista ( del tipo personalizado ) omitiendo los primeros n elementos.
omitir :: Integer -> Lista a -> Lista a
omitir 0 xs = xs 
omitir n ( Cons x (xs) ) = omitir (n-1) xs


-- combina -> Dadas dos Listas ( del tipo personalizado ) xs y ys, regresa una lista donde se 
-- insertan de manera intercalada ( uno y uno ) los elementos de xs y ys hasta que una sea Nula.
combina:: Lista a -> Lista a -> Lista a
combina xs Nula = Nula
combina Nula ys = Nula    
combina (Cons a (xs)) (Cons b (ys)) =  Cons a  ( (Cons b ( combina xs ys) ) )




{-  ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
    ┃     Funciones propias de Haskell usadas      ┃ 
    ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ -}

-- suma      -> Suma todos los elementos de una lista de enteros.
-- product   -> Multiplica cada uno de los elemetos de una lista de enteros.
-- elem      -> Verifica si un elemento ya se encuentra contenido en una lista.
-- length    -> Calcula el numero de elementos de una lista.