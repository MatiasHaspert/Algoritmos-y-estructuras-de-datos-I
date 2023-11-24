--Reescribir cada una de las siguientes definiciones sin usar let, where o if:
--a) f x = let (y,z) = (x,x) in y
--b) greater (x,y) = if x > y then True else False
--c) f (x,y) = let z = x + y in g (z,y) where g (a,b) = a − b

--a)

f x = let (y,z) = (x,x) in y 
f2 x = x

--b)

greater (x,y)
    | x > y = True
    | otherwise = False 

--c)

f3 (x,y) = let z = x + y in g (z,y) where g (a,b) = a - b

f4 (x,y) = x 