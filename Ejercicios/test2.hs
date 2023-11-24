newtype ColaPrioridad a = Cp [a] deriving Show

mkqpr :: (Ord a) => ColaPrioridad a
mkqpr = Cp []

addqpr :: (Ord a) => a -> ColaPrioridad a -> ColaPrioridad a
addqpr x (Cp []) = Cp [x]
addqpr x (Cp s)  = Cp (s ++ [x])

nextqpr :: (Ord a) => ColaPrioridad a -> a
nextqpr (Cp []) = error "Cola vacia"
nextqpr (Cp (x:xs)) = x

popqpr :: (Ord a) => ColaPrioridad a -> ColaPrioridad a
popqpr (Cp []) = error "Cola vacia"
popqpr (Cp (x:xs)) = Cp xs

data Bintree a = BintreeVacio | Nodo a (Bintree a) (Bintree a) deriving Show

newBintree :: (Ord a) => Bintree a
newBintree = BintreeVacio

addTree :: (Ord a) => a -> Bintree a -> Bintree a
addTree x BintreeVacio = Nodo x BintreeVacio BintreeVacio
addTree x (Nodo y izq der)
                        | x == y = Nodo y izq der
                        | x <  y = Nodo y (addTree x izq) der
                        | x >  y = Nodo y izq (addTree x der)

minTree :: (Ord a) => Bintree a -> (a,Bintree a)
minTree (Nodo y BintreeVacio der) = (y,der)
minTree (Nodo y izq der) = (x,Nodo y newIzq der)
                            where
                                (x,newIzq) = minTree izq

newtype ColaPrioridad2 a = Cp2 (Bintree a) deriving Show

mkqpr2 = Cp2 (BintreeVacio)
addqpr2 x (Cp2 arbol) = Cp2 (addTree x arbol)
nextqpr2 (Cp2 arbol)  = let (valorMenor,_) = minTree arbol in valorMenor
popqpr2 (Cp2 arbol)   = let (_,nuevoArbol) = minTree arbol in Cp2 nuevoArbol

