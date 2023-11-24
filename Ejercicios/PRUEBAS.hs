--LAMBDA (sirve para evitar darle nombre a funciones que se utilizan una unica vez) EJEMPLOS 

odds n = map f [0..n-1]
        where
            f x = x * 2 + 1

odds' n = map (\x->x * 2 + ) [0..n-1]


