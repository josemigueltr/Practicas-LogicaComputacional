{-
- Logica Conmputacional 2020-2 
- Practica 3, LP en Haskell y FNC
- Creador: Pedro Juan Salvador Sánchez Pérez
-}

module LProp where

-- | VarP. Tipo que representa el conjunto de variables proposicionales.
type VarP = String

-- | Prop. Tipo que representa el conjunto de fórmulas de la lógica
-- proposicional.
data Prop = TTrue
          | FFalse
          | V VarP
          | Neg Prop
          | Conj Prop Prop
          | Disy Prop Prop
          | Imp Prop Prop
          | Equiv Prop Prop 

p, q, r, s, t, u :: Prop
a = V "a"
b = V "b"
c = V "c"
d = V "d"
p = V "p"
q = V "q"
r = V "r"
s = V "s"
t = V "t"
u = V "u"
v = V "v"
x = V "x"
y = V "y"
z = V "z"
w = V "w"


-- / se intancia la clase Show para poder especificar como queremos que se impriman la logica proposicional 
-- / TTrue en terminal se vera como "T", Neg (V "p") se vera como ~(p)
instance Show Prop where
    show TTrue       = "T"
    show FFalse      = "F"
    show (V x)       = show x
    show (Neg p)     = "¬("++ show p ++")"
    show (Conj p q)  = "(" ++ show p ++ " ^ "   ++ show q ++")"
    show (Disy p q)  = "(" ++ show p ++ " v "   ++ show q ++")"
    show (Imp p q)   = "(" ++ show p ++ " -> "  ++ show q ++")"
    show (Equiv p q) = "(" ++ show p ++ " <-> " ++ show q ++")"

-- / se insatancia la clase Eq para poder comparar formulas proposicionales
-- TTrue == False = False
instance Eq Prop where
    (==) TTrue TTrue = True
    (==) FFalse FFalse = True
    (==) (V x) (V y) = x == y
    (==) (Neg p) (Neg q)          =  (==) p q
    (==) (Conj  p q) (Conj  r s)  =  (==) p r && (==) q s
    (==) (Disy  p q) (Disy  r s)  =  (==) p r && (==) q s
    (==) (Imp   p q) (Imp   r s)  =  (==) p r && (==) q s
    (==) (Equiv p q) (Equiv r s)  = ((==) p r && (==) q s) || ((==) p s && (==) q r)
    (==) p q = False

    
-- / se instancia la clase Ord para poder dar orden a las formulas proposicionales 
-- ~p > p = False
instance Ord Prop where
    (<)  p q = peso p < peso q
    (>)  p q = peso p > peso q 
    (<=) p q = peso p <= peso q 
    (>=) p q = peso p >= peso q 
    min  p q = if peso p <= peso q then p else q
    max  p q = if peso p >= peso q then p else q

-- | peso. Función que dada una fórmula devuelve el número de sus conectivos.
--
-- --> peso (Conj (V 1) (Disy (V 2) (FFalse))) = 2
-- --> peso (Conj (V 1) (Disy (V 2) (Neg (V 3)))) = 3
peso :: Prop -> Int
peso (Neg p)     = 1 + peso p
peso (Conj  p q) = 1 + peso p + peso q
peso (Disy  p q) = 1 + peso p + peso q
peso (Imp   p q) = 1 + peso p + peso q
peso (Equiv p q) = 1 + peso p + peso q
peso _ = 0 

-- --> elimEquiv (Equiv (V p) (V q)) = (Conj (Imp (V p) (V q)) (Imp (V q) (V p)))
elimEquiv :: Prop -> Prop
elimEquiv FFalse = FFalse
elimEquiv TTrue  = TTrue
elimEquiv (V p)  = V p
elimEquiv (Neg phi)       = Neg  ( elimEquiv phi) 
elimEquiv (Conj  phi rho) = Conj ( elimEquiv phi) (elimEquiv rho) 
elimEquiv (Disy  phi rho) = Disy ( elimEquiv phi) (elimEquiv rho)   
elimEquiv (Imp   phi rho) = Imp  ( elimEquiv phi) (elimEquiv rho) 
elimEquiv (Equiv phi rho) = Conj (Imp p q) (Imp q p) 
    where  
        p = (elimEquiv phi)
        q = (elimEquiv rho)


--(Equiv (Equiv (V "p") (V "q")) (Conj (V "r") (Disy (V "s") (V "T"))) ) 


-- | elimImp. Funció que dada una fórmula devuelve su equivalente que no contiene implicaciones.
--
-- --> elimEquiv (Imp (V p) (Disy (V q) (FFalse))) = Disy (Neg (V p)) (Disy (V q) FFalse)
elimImp :: Prop -> Prop
elimImp FFalse = FFalse
elimImp TTrue  = TTrue
elimImp (V p)  = V p
elimImp (Neg   phi)     = Neg   (elimImp phi) 
elimImp (Conj  phi rho) = Conj  (elimImp phi) (elimImp rho)
elimImp (Disy  phi rho) = Disy  (elimImp phi) (elimImp rho)
elimImp (Equiv phi rho) = Equiv (elimImp phi) (elimImp rho)
elimImp (Imp   phi rho) = Disy  (Neg p) (q) 
    where  
        p = (elimImp phi)
        q = (elimImp rho)

--(Neg (Neg (Neg (V "e"))))

-- | elimIE. Función que dada una fórmula devuelve su equivalente que no contiene implicaciones,
-- ni equivalencias.
--
elimIE :: Prop -> Prop
elimIE p = elimImp $ elimEquiv p



-- / funcion que recibe una formula de la logica proposicional y que devuelve otra formula de la logica proposicional que es logicamente
-- equivalente pero las negaciones que existen solo aplican a formulas atomicas. tambien elimina la doble negacion. la funcion supone
-- que la fomula ya no tiene implicaciones ni equivalencias.

meteNeg :: Prop -> Prop
meteNeg FFalse = FFalse
meteNeg TTrue  = TTrue
meteNeg (V p)  = V p
meteNeg (Neg  phi)     = auxNeg(phi)
meteNeg (Conj phi rho) = Conj (meteNeg phi) (meteNeg rho)
meteNeg (Disy phi rho) = Disy (meteNeg phi) (meteNeg rho)

auxNeg :: Prop -> Prop
auxNeg FFalse = TTrue
auxNeg TTrue  = FFalse
auxNeg (V p)  = (Neg (V p))
auxNeg (Neg phi)      = meteNeg phi
auxNeg (Conj phi rho) = Disy (meteNeg(Neg phi)) (meteNeg(Neg rho)) 
auxNeg (Disy phi rho) = Conj (meteNeg(Neg phi)) (meteNeg(Neg rho)) 



-- / funcion que reciba una formula de la logica proposicional y devuelva una formula
-- equivalente tal que este en forma normal negativa,
fnn :: Prop -> Prop 
fnn p = meteNeg (elimIE p)


-- / funcion que reciba una formula de la logica proposicional y devuelva una formula
-- equivalente pero que distribuye la disyuncion sobre la conjuncion (Ej. p v (q ^ r) = (p v q) ^ (p v r)). la funcion supone que la formula 
-- esta en forma normal negativa.
dist :: Prop -> Prop
dist ( Neg p )    = Neg  ( dist p )
dist ( Conj p q ) = Conj ( dist p ) ( dist q )
dist ( Disy p q ) = distA p q
dist prop  = prop


-- Sobrecarga el operador de disyunción en la cola.
distA :: Prop -> Prop -> Prop
distA  p (Conj q r) = Conj ( dist (Disy p q) ) ( dist (Disy p r) )
distA (Conj q r) p  = Conj ( dist (Disy q p) ) ( dist (Disy r p) ) 
distA p q = if( tieneConj p || tieneConj q )
    then dist ( Disy (dist p) (dist q) )
    else Disy p q


-- / funcion que reciba una formula de la logica proposicional y devuelva una formula
-- equivalente tal que este en forma normal conjuntiva,
cnf :: Prop -> Prop
cnf p = dist $ fnn p 




{- ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
   ┃             Funciones auxiliares             ┃ 
   ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛ -}

-- tieneConj -> Dada una fórmula proposicional revisa si en dicha
-- formula existe alguna disyunción.
tieneConj :: Prop -> Bool
tieneConj ( Neg ( p) ) = tieneConj p
tieneConj ( Conj p q ) = True
tieneConj ( Disy p q ) = tieneConj p || tieneConj q
tieneConj prop         = False