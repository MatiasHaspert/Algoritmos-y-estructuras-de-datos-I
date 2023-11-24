a :: (Int -> Int) -> Int
a f = 2

b :: Int -> (Int -> Int)
b x y = x+y

c :: (Int -> Int) -> (Int -> Int)
c f = f 

d :: Int -> Bool
d x
 | x <= 0 = False
 | otherwise = True

e :: Bool -> (Bool -> Bool)
e a = \x -> True

f :: (Int,Char) -> Bool
f (a,b) = True

g :: (Int,Int) -> Int
g (a,b) = a+b

h :: Int -> (Int,Int)
h a = (a,a)

i :: a -> Bool
i x = True

j :: a -> a
j a = a