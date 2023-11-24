module Queue (Cola, colaVacia, enColar, deColar, colaEsVacia, front) where 

colaVacia :: Cola a
enColar:: a -> Cola a -> Cola a
deColar :: Cola a -> Cola a
front :: Cola a -> a
colaEsVacia:: Cola a -> Bool


data Cola a = ColaVacia | Qo a (Cola a) deriving Show

colaVacia = ColaVacia

enColar x s = s Qo x 

deColar ColaVacia = error "Cola vacía"
deColar (Qo _ s) = s

front ColaVacia = error "Cola vacía"
front (Qo x _) =  x

colaEsVacia ColaVacia = True
colaEsVacia _         = False


{-
newtype Cola a = Q [a] deriving Show

colaVacia = Q[]

enColar x (Q s) = Q (s ++ [x])

deColar (Q []) = error "ColaVacia"
deColar (Q(x:t)) = Q t

front (Q []) = error "ColaVacia"
front (Q(x:t)) = x

colaEsVacia (Q []) = True
colaEsVacia (Q _ ) = False
-}