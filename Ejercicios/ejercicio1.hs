module Test where

--a) five, que dado cualquier valor, devuelve 5

--five x = 5    
--five x = x + 5 - x    

--five :: a -> Int
five x = x * 5 / x

--b) apply, que toma una funcion y un valor, y devuelve el resultado de aplicar la funcional valor dado

apply f x = f x 

--c) id, la funcion identidad

identidad f x = x 

--d) first, que toma un par ordenado, y devuelve su primera componente

f (a,_) = a   

--e) derive, que aproxima la derivada de una funcion dada en un punto dado

derive f x dx = (f (x+dx)- f x) / dx 

--f)sign, la funcion signo

--sign x = if x > 0 then 1 else -1

--h) pot, que toma un entero y un numero, y devuelve el resultado de elevar el segundo a la 
--potencia dada por el primero

pot 0 b = 1
pot a b = b^a 

--i) max3, que toma tres numeros enteros y devuelve el maximo entre ellos

mimax3 x y z 
    | x > y && x > z = x
    | y > x && y > z = y
    | otherwise = z 

--j) swap, que toma un par y devuelve el par con sus componentes invertidas

mswap (x,y) = (y,x)










