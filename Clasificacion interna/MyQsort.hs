mimaximun [] = error "No puedo hacer nada chaval"
mimaximun [x] = x

mimaximun (x:xs)
        | x > t = x
        | otherwise = mtail
        where t = mimaximun xs

mireplicate n x 
        | n <= 0 = []
        | otherwise = x:mireplicate (n-1) x

mitake n _
    | n <= 0 = []
mitake _ []  = []
mitake n (x:xs) = x : mitake (n-1) xs  

----Selection Sort---------------------------

delete y [] = []
delete y l = [x | x <-l, x /= y]

minimo [x] = x
minimo (x:y:t) = if x < y then minimo (x:t)
                 else minimo (y:t) 

ssort [] = []
ssort [x] = [x]
ssort l = m:ssort l'
        where  
        m = minimo l
        l'= delete m l
---------------------------------------------
------Insertion Sort-------------------------
insertion x [] = [x]
insertion x (y:t) = if x < y then (x:y:t)
                    else y:insertion x t

insort [] = []
insort [x] = [x]
insort (x:t) = insertion x (insort t)
---------------------------------------------
--------QuickSort

qsort [] = []
qsort [x] = [x]
qsort (x:t) = qsort menores ++ [x] ++ qsort mayores
                where 
                menores = [j | j <- t, j <= x]
                mayores = [j | j <- t, j > x ]

-----MergeSort-------------------------------------
-- SPLIT divide al conjunto 

split [] = ([],[])
split [x] = ([x],[])
split (x:y:z) = (x:a, y:b)
                where   
                (a,b) = split z

merge [] [] = []
merge a [] = a
merge [] b = b
merge (x:xs) (y:ys)= if x < y then (x:merge xs (y:ys))
                     else (y:merge (x:xs) ys)

msort [] = []
msort [x] = [x]
msort lista =
        let
        (i,j) = split lista;
         i' = msort i;
         j' = msort j
         in
         merge i' j'
----------------------------------------------------
-- Si son iguales dos listas------------------------
iguales :: Eq a => [a] ->[a] -> Bool
iguales [] [] = True
iguales [] _ = False
iguales _ [] = False
iguales (x:xs) (y:ys) = (x:xs) == (y:ys)
----------------------------------------------------

