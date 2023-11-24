data Arbol a = ArbolVacio | Nodo a (Arbol a) (Arbol a) deriving Show 

nuevoArbol :: (Ord a) => Arbol a 
inArbol  :: (Ord a) => a -> Arbol a -> Bool
addArbol :: (Ord a) => a -> Arbol a -> Arbol a 
eliArbol :: (Ord a) => a -> Arbol a -> Arbol a 

nuevoArbol = ArbolVacio

inArbol x ArbolVacio = False
inArbol x (Nodo y izq der)
                        | x == y = True
                        | x <  y = inArbol x izq
                        | x >  y = inArbol x der

addArbol x ArbolVacio = Nodo x ArbolVacio ArbolVacio
addArbol x (Nodo y izq der)
                        | x == y = Nodo y izq der 
                        | x <  y = Nodo y (addArbol x izq) der 
                        | x >  y = Nodo y izq (addArbol x der)

minimoArbol :: (Ord a) => Arbol a -> (a, Arbol a)
minimoArbol (Nodo y ArbolVacio der) = (y,der)
minimoArbol (Nodo y izq der) = let (x, newIzq) = minimoArbol izq
                               in (x, Nodo y newIzq der) 

eliArbol x ArbolVacio = ArbolVacio
eliArbol x (Nodo y izq ArbolVacio)
                                | x == y = izq
eliArbol x (Nodo y ArbolVacio der)
                                | x == y = der 
eliArbol x (Nodo y izq der)
                                | x < y = Nodo y (eliArbol x izq) der 
                                | x > y = Nodo y izq (eliArbol x der)
                                | x == y = let (s,t) = minimoArbol der 
                                           in (Nodo s izq t)

inOrder :: (Ord a) => Arbol a -> [a]
inOrder (Nodo y izq der) = inOrder izq ++ [y] ++ inOrder der 