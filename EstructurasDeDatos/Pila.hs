module Stack (Pila, push, pop, top, pilaVacia, pilaEsVacia) where

push :: a -> Pila a -> Pila a
pop  :: Pila a -> Pila a
top  :: Pila a -> a
pilaVacia :: Pila a   
pilaEsVacia:: Pila a -> Bool 

{-data Pila a = PilaVacia | Stk a (Pila a) deriving Show

pilaVacia = PilaVacia

push x s = Stk x s 

pop PilaVacia = error "Pila Vacía"
pop (Stk _ s) = s

top PilaVacia = error "Pila Vacía"
top (Stk x _) = x

pilaEsVacia PilaVacia = True

pilaEsVacia _         = False
-}
newtype Pila a = Stk [a] deriving Show

pilaVacia = Stk []

push x (Stk xs) = Stk (x:xs)

pop (Stk []) = error "Pila Vacía"
pop (Stk (_:xs)) = Stk xs

top (Stk []) = error "Pila Vacía"
top (Stk (x:_)) = x

pilaEsVacia (Stk []) = True
pilaEsVacia (Stk _ ) = False