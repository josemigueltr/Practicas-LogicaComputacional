{-

- Lógica computacional 2020-2
- Practica 2

- Alumno: José Miguel Toledo Reyes
- Número de cuenta: 316164069
- Correo: josemigueltr@ciencias.unam.mx

- Alumno: Omar Fernando Gramer Muñoz
- Número de cuenta: 419003698
- Correo: grameromar@ciencias.unam.mx

-}

module Practica2 where

{-  ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
    ┃               Tipos definidos                ┃ 
    ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ -}

-- Prop -> Representa las fórmulas proposicionales usando los constructores
-- constructores T, F, Var, Neg, Conj, Disy, Impl y Equi para las fórmulas
-- atómicas, negaciones, conjunciones, implicaciones y equivalencias,
-- respectivamente.

data Prop = T | F | Var String | Neg Prop | Conj Prop Prop | Disy Prop Prop | Impl Prop Prop | Equi Prop Prop deriving Eq

type Estado = [String]

instance Show Prop where
         show T = "Verdadero"
         show F = "Falso"
         show (Var p) = p
         show (Neg p) = "¬" ++ show p
         show (Disy p q) = "(" ++ show p ++ " ∨ "  ++ show q ++ ")"
         show (Conj p q) = "(" ++ show p ++ " ∧ "  ++ show q ++ ")"
         show (Impl p q) = "(" ++ show p ++ " ⟶ "  ++ show q ++ ")"
         show (Equi p q) = "(" ++ show p ++ " ⟷ "  ++ show q ++ ")"


-- Definimos las siguientes fórmulas proposicionales
-- como variables atómicas: p, q, r, s, t, u.
p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"





{-  ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
    ┃           Funciones principales              ┃ 
    ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ -}

-- variables -> Devuelve el conjunto formado por todos los símbolos
-- proposicionales que aparecen en la fórmula.
variables :: Prop -> Estado
variables F            = []
variables T            = []
variables ( Var  p )   = [p]
variables ( Neg  f )   = variables f
variables ( Conj f g ) = eliminaDuplicados (variables f ++ variables g)
variables ( Disy f g ) = eliminaDuplicados (variables f ++ variables g)
variables ( Impl f g ) = eliminaDuplicados (variables f ++ variables g)
variables ( Equi f g ) = eliminaDuplicados (variables f ++ variables g)



-- conjPotencia -> Regresa una lista de todos los subconjuntos del conjunto recibido.
conjPotencia :: [a] -> [[a]]
conjPotencia []     = [[]]
conjPotencia [x]    = [[],[x]]
conjPotencia (x:xs) = [ x:px | px <- conjPotencia(xs) ] ++ conjPotencia(xs)



-- interpretacion -> Interpreta (evalúa) la fórmula recibida f dado un estado e.
interpretacion :: Prop -> Estado -> Bool 
interpretacion F            e = False
interpretacion T            e = True
interpretacion ( Var  p )   e = p `elem` e
interpretacion ( Neg  f )   e = not ( interpretacion f e )
interpretacion ( Conj f g ) e = ( interpretacion f e ) && ( interpretacion g e )
interpretacion ( Disy f g ) e = ( interpretacion f e ) || ( interpretacion g e )
interpretacion ( Equi f g ) e = ( interpretacion f e ) == ( interpretacion g e )
interpretacion ( Impl f g ) e = ( interpretacion (Neg f ) e ) || ( interpretacion g e )



-- estadosPosibles -> Dada una fórmula f devuelve una lista con todos los estados 
-- con los que podemos evaluar la fórmula.
estadosPosibles :: Prop -> [Estado]
estadosPosibles f = conjPotencia ( variables f )



-- tautologia -> Dada una fórmula f devuelve si f es una tautogía.
tautologia :: Prop -> Bool
tautologia f = ( modelos f ) == ( estadosPosibles f )



-- contradiccion -> Dada una fórmula f devuelve si f es una contradiccion.
contradiccion :: Prop -> Bool
contradiccion f = ( modelos f ) == []



-- esModelo -> Dada un estado e y una fórmula proposicional f,
-- verifica si el estado es un modelo ( la interpretación de e en f se evalúa a verdadero ).
esModelo :: Estado -> Prop -> Bool
esModelo e f = ( interpretacion f e ) == True



-- modelos -> Dada una fórmula f regresa una lista con todos los modelos de f
modelos :: Prop -> [Estado]
modelos f = [ e | e <- (estadosPosibles f), (esModelo e f) == True ]



-- esValida -> Dada una fórmula f determina si es válida,
-- ( es decir, si la fórmula es una tautología ).
esValida :: Prop -> Bool
esValida f = tautologia f



-- esInsatisfacible -> Dada una fórmula f determina si no es sastifacible,
-- ( es decir, si la fórmula es una contradicción ).
esInsatisfacible :: Prop -> Bool
esInsatisfacible f = contradiccion f



-- esSatisfacible -> Dada una fórmula f determina si es sastifacible,
-- ( es decir, si la fórmula no es una contradicción ).
esSatisfacible :: Prop -> Bool
esSatisfacible f = not ( contradiccion f )





{- ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
   ┃             Funciones auxiliares             ┃ 
   ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ -}

-- eliminaDuplicados -> Regresa una copia de la lista eliminando los elementos duplicados.
eliminaDuplicados :: Eq a => [a] -> [a]
eliminaDuplicados [] = [] 
eliminaDuplicados (x:xs) 
    | x `elem` xs =   ( eliminaDuplicados xs )
    | otherwise   = x:( eliminaDuplicados xs )