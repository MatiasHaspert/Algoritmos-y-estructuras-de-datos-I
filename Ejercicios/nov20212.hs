particion :: Ord a => a -> [a] -> ([a],[a])
particion x [] = ([],[])
particion x (y:ys) = if y <= x then (y:men,may) else (men,y:may)
			where (men,may) = particion x ys

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort (men) ++ [x] ++ qsort (may)
		where (men,may) = particion x xs
