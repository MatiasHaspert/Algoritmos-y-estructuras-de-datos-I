--5. Definir una funcion que determine si un año es bisiesto o no, de acuerdo a la siguiente
--definicion:
--año bisiesto 1. m. El que tiene un dıa mas que el ano comun, anadido al mes de febrero. Se
--repite cada cuatro años, a excepcion del ultimo de cada siglo cuyo numero de centenas no
--sea multiplo de cuatro

bisiesto :: Int -> Bool
bisiesto x = ((x `mod` 4) == 0) && (((x `mod` 100) /= 0) || ((x `mod` 400) == 0))