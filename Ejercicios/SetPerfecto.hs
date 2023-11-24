module Set(Set, emptySet, setEmpty, addSet, inSet) where

   newtype Set a = St [a] deriving Show

   emptySet :: Set a
   emptySet = St []

   setEmpty :: Set a -> Bool
   setEmpty (St []) = True
   setEmpty (St _) = False

   inSet :: (Eq a) => a -> Set a -> Bool
   inSet x (St []) = False
   inSet x (St (y:ys)) = x == y || inSet x (St (ys))

   addSet :: (Eq a) => a-> Set a -> Set a
   addSet x (St xs) = if inSet x (St xs) then St (xs) else St (x:xs)

   delSet :: (Eq a) => a -> Set a -> Set a
   delSet x (St []) = St []
   delSet x (St (y:ys)) = if x /= y then addSet y (delSet x (St (ys))) else delSet x (St (ys))

   unionSet :: (Eq a) => Set a -> Set a -> Set a
   unionSet (St []) (St (ys)) = St (ys)
   unionSet (St (x:xs)) (St (ys)) = if inSet x (St (ys)) then unionSet (St (xs)) (St (ys)) else unionSet (St (xs)) (St (x:ys))
