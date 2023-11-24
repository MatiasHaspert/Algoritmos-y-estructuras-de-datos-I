juntar :: (Ord a) => [a] -> [a] -> [a]

juntar xs [] = xs
juntar [] xs = xs
juntar (x:xs) (y:ys) = if x<y then x : (juntar xs (y:ys)) else y : (juntar (x:xs) ys)
