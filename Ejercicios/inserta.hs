inserta :: (Ord a) => a-> [a] -> [a]
inserta x [] = [x]
inserta x (y:ys) = if x<=y then x:y:ys else y: inserta x ys
                                       
