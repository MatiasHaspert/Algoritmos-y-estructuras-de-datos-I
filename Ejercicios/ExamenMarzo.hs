module Set (Set, emptySet, setEmpty, inSet, addSet,delSet, unionSet) where

newtype Set a = St [a] deriving Show

emptySet = St []

setEmpty (St []) = True
setEmpty (St [x]) = False

inSet x (St []) = False
inSet x (St (y:ys)) = x == y || inSet x (St ys)

addSet x (St y) = if inSet x (St y) then St (y) else St (x:y)

delSet x (St []) = St []
delSet x (St (y:ys)) = if x /= y then addSet y (delSet x (St ys)) else delSet x (St ys)

unionSet (St []) (St (x)) = St (x)
unionSet (St (x:xs)) (St (y)) = if inSet x (St y) then unionSet (St xs) (St y) else unionSet (St xs) (St (x:y))