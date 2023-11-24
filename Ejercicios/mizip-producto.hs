miZip :: [a]->[b]->[(a,b)]
miZip _ [] = []
miZip [] _ = []
miZip (x:xs) (y:ys) = (x,y): (miZip xs ys)



suma :: [Float] -> Float
suma [] = 0
suma (x:xs) = x + suma xs

productosescalar :: [Float] -> [Float] -> Float
productosescalar ys zs = suma ([y*z | (y,z) <- miZip ys zs])
