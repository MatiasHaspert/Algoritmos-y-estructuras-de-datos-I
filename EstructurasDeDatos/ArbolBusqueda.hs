data Bintree a = ArbolVacio | Nodo a (Bintree a) (Bintree a) deriving Show 

mkNewTree ::(Ord a)=> Bintree a
inTree ::(Ord a)=> a -> Bintree a -> Bool
addTree ::(Ord a)=> a -> Bintree a -> Bintree a
delTree ::(Ord a)=> a -> Bintree a -> Bintree a

mkNewTree = ArbolVacio

inTree x ArbolVacio = False 
inTree x (Nodo y izq der)
                        | x == y = True
                        | x <  y = inTree x izq 
                        | x >  y = inTree x der 

addTree x ArbolVacio = Nodo x ArbolVacio ArbolVacio
addTree x (Nodo y izq der)
                        | x == y = Nodo y izq der 
                        | x <  y = Nodo y (addTree x izq) der 
                        | x >  y = Nodo y izq (addTree x der)

minTree :: (Ord a) => Bintree a -> (a, Bintree a)
minTree (Nodo y ArbolVacio der) = (y,der)  
minTree (Nodo y izq der) = let (x,newizq) = minTree izq 
                            in (x,Nodo y newizq der)

delTree x ArbolVacio = ArbolVacio
delTree x (Nodo y izq ArbolVacio)
                                | x == y = izq 
delTree x (Nodo y ArbolVacio der)
                                | x == y = der
delTree x (Nodo y izq der) 
                         |x <  y = Nodo y (delTree x izq) der 
                         |x >  y = Nodo y izq (delTree x der) 
                         |x == y = let (k,newder) = minTree der 
                                   in (Nodo k izq newder)

inOrder :: (Ord a)=> Bintree a -> [a]
inOrder ArbolVacio = [] 
inOrder (Nodo y izq der) = inOrder izq ++ [y] ++ inOrder der 

{-newtype Dict a = Dicc (Bintree a) deriving Show

mkDict:: (Ord a)=> Dict a
mkDict = Dicc (mkNewTree)

insertDict :: (Ord a)=> a -> Dict a -> Dict a
insertDict x (Dicc t) = Dicc (addTree x t)

inDict::(Ord a) => a -> Dict a -> Bool
inDict x (Dicc t) = inTree x t

delDict::(Ord a)=> a -> Dict a -> Dict a
delDict x (Dicc t) = Dicc ( delTree x t)
-}