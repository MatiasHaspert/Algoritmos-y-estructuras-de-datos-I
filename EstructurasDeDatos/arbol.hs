data ArbolBin a = ArbolBinVacio | Nodo a (ArbolBin a) (ArbolBin a) deriving Show 

newArbolBin :: (Ord a) => ArbolBin a

newArbolBin = ArbolBinVacio

addTree :: (Ord a) => a -> ArbolBin a -> ArbolBin a

addTree x ArbolBinVacio = Nodo x ArbolBinVacio ArbolBinVacio
addTree x (Nodo y izq der)  
                        | x == y = Nodo y izq der
                        | x <  y  = Nodo y (addTree x izq) der
                        | x >  y  = Nodo y izq (addTree x der)

inTree :: (Ord a) => a -> ArbolBin a -> Bool

inTree x ArbolBinVacio = False
inTree x (Nodo y izq der)
                        | x == y = True
                        | x <  y = inTree x izq
                        | x >  y = inTree x der

inOrderTree :: (Ord a) => ArbolBin a -> [a]

inOrderTree ArbolBinVacio = []
inOrderTree (Nodo y izq der) = inOrderTree izq ++ [y] ++ inOrderTree der

inPreOrderTree :: (Ord a) => ArbolBin a -> [a]

inPreOrderTree ArbolBinVacio = []
inPreOrderTree (Nodo y izq der) = [y] ++ inPreOrderTree izq ++ inPreOrderTree der

inPostOrderTree :: (Ord a) => ArbolBin a -> [a]

inPostOrderTree ArbolBinVacio = []
inPostOrderTree (Nodo y izq der) = inPostOrderTree izq ++ inPostOrderTree der ++ [y]`` 