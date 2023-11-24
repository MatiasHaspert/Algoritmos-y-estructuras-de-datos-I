module Set (Set, emptySet, setEmpty, inSet, addSet,delSet, unionSet) where

newtype Set a = St [a] deriving Show

emptySet :: Set a 
emptySet = St []

setEmpty :: (Eq a) => Set a -> Bool
setEmpty (St []) = True
setEmpty (St [x]) = False 


inSet :: (Eq a) => a -> Set a -> Bool 
inSet x (St []) = False
inSet x (St (y:ys)) = x == y || inSet x (St ys)

addSet :: (Eq a) => a -> Set a -> Set a
addSet x (St xs) = if inSet x (St xs) then St (xs) else St (x:xs)  

delSet :: (Eq a) => a -> Set a -> Set a
delSet x (St []) = St []
delSet x (St (y:ys)) = if x /= y then addSet y (delSet x (St ys)) else delSet x (St (ys))

unionSet :: (Eq a) => Set a -> Set a -> Set a
unionSet (St []) (St (x)) = St (x)
unionSet (St (x:xs)) (St y) = if inSet x (St y) then unionSet (St xs) (St y) else unionSet (St xs) (St (x:y))