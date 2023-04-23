doubleMe :: (Num t) => t -> t
doubleMe x = x + x


-- EJERCICIO 1 --

f :: Int -> Int  -- o usar Integer
f 1 = 8
f 4 = 131
f 16 = 16

g :: Int -> Int
g 8 = 16
g 16 = 4
g 131 = 1

h :: Int -> Int
h x = f (g x)

k :: Int -> Int
k x = g (f x)


-- EJERCICIO 2 --

absoluto :: Int -> Int
absoluto x | x >= 0 = x
           | otherwise = -x 

maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto a b  | absoluto a >= absoluto b = absoluto a
                    | otherwise = absoluto b

maximo3 :: Int -> Int -> Int -> Int
maximo3 a b c   | (a >= b && a >= c) = a  -- a >= b >= c
                | b >= c = b
                | otherwise = c

-- Con guardas
--algunoEs0 :: Float -> Float -> Bool
--algunoEs0 a b   | (a == 0 || b == 0) = True
--                | otherwise = False

-- Con pattern matching
algunoEs0 :: Float -> Float -> Bool
algunoEs0 0 _ = True
algunoEs0 _ 0 = True
algunoEs0 _ _ = False

-- Con guardas
--ambosSon0 :: Float -> Float -> Bool
--ambosSon0 a b   | (a == 0 && b == 0) = True
--                | otherwise = False

-- Con pattern matching
ambosSon0 :: Float -> Float -> Bool
ambosSon0 0 0 = True
ambosSon0 _ _ = False

mismoIntervalo :: Float -> Float -> Bool
mismoIntervalo a b  | a <= 3 && b <= 3 = True
                    | 3 < a && a <= 7 && 3 < b && b <= 7 = True
                    | a < 7 && b < 7 = True
                    | otherwise = False

-- Opcion 1:
--sumaDistintos :: Int -> Int -> Int -> Int
--sumaDistintos a b c | a == b && b == c = a -- Si a=b=c entonces devuelve a
--                    | a == b && a /= c = a + c -- Si a=b entonces devuelve a + c
--                    | a == c && b /= c = b + c -- Si a=c entonces devuelve b + c
--                    | b == c && a /= c = a + c -- Si b=c entonces devuelve a + c
--                    | a /= b && b /= c = a + b + c -- Si todos sin distIntos entonces sumo todos entre si

--O Opción 2:
sumaDistintosTupla :: Int -> Int -> Int
sumaDistintosTupla x y | x == y = x
                       | otherwise = x + y

sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos a b c | a == b || a == c = sumaDistintosTupla b c
                    | b == c           = sumaDistintosTupla a b
                    | otherwise        = a+b+c

-- Ambos hacen lo mismo
--esMultiploDe :: Int -> Int -> Bool
--esMultiploDe a b | mod a b == 0 = True
--                 | otherwise = False

esMultiploDe :: Int -> Int -> Bool
esMultiploDe a b = mod a b == 0

digitoUnidades :: Int -> Int
-- digitoUnidades a = a - (div a 10) * 10
digitoUnidades a = mod a 10

digitoDecenas :: Int -> Int
-- digitoDecenas a = div (a - (div a 100) * 100) 10
digitoDecenas a = div (mod a 100) 10



-- EJERCICIO 3 --

estanRelacionados :: Int -> Int -> Bool
-- Requiere que a/= 0 y b/=0
-- El problema se puede traducir a True si y solo si a * a es multiplo de a * b
-- OBS: a*a = a*b*k <=> a*a - a*b*k = 0 <=> a*a + a*b*(-k) = 0
-- estanRelacionados a b = mod (a*a) (a*b) == 0
-- OBS: Si divido a ambos lados por a tengo que a*a + a*b*(-k) = 0 <=> a + b*(-k) = 0. Entonces es equivalente:
estanRelacionados a b = mod a b == 0


-- EJERCICIO 4 --

-- prodInt :: (Num t) => (t, t) -> (t, t) -> t
prodInt :: (Float, Float) -> (Float, Float) -> Float
-- prodInt v w = ((fst v) * (fst w)) + ((snd v) * (snd w))
prodInt (vx, vy) (wx, wy) = ((vx) * (wx)) + ((vy) * (wy))

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
-- todoMenor v w = ((fst v) < (fst w)) && ((snd v) < (snd w))
todoMenor (vx, vy) (wx, wy) = (vx < wx) && (vy < wy)

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
-- distanciaPuntos v w = sqrt(((fst w - fst v)^2) + ((snd w - snd v)^2))
distanciaPuntos (vx, vy) (wx, wy) = sqrt(((wx - vx)^2) + ((wy - vy)^2))

sumaTerna :: (Float, Float, Float) -> Float
sumaTerna (x, y, z) = x + y + z

-- Forma ineficiente escribiendo por casos:
{- sumarSoloMultiplos :: (Int, Int, Int) -> Int -> Int
-- Requiere: n perteneciente a los naturales (n>0)
sumarSoloMultiplos (a, b, c) n | esMultiploDe a n  && esMultiploDe b n  && esMultiploDe c n  = a + b + c
                               | esMultiploDe a n  && esMultiploDe b n                       = a + b
                               |                      esMultiploDe b n  && esMultiploDe c n  =     b + c
                               | esMultiploDe a n  &&                      esMultiploDe c n  = a     + c
                               | esMultiploDe a n                                            = a
                               |                      esMultiploDe b n                       =     b
                               |                                           esMultiploDe c n  =         c
                               | otherwise                                                   = 0 -}

esMultiploDeBinario :: Int -> Int -> Int
esMultiploDeBinario a b | esMultiploDe a b = 1
                        | otherwise = 0

sumarSoloMultiplos :: (Int, Int, Int) -> Int -> Int
-- Requiere: n perteneciente a los naturales (n>0)
sumarSoloMultiplos (a, b, c) n = (a*(esMultiploDeBinario a n)) + (b*(esMultiploDeBinario b n)) + (c*(esMultiploDeBinario c n))

posPrimerPar ::  (Int, Int, Int) -> Int
-- dada una terna de enteros, devuelve la posicion del primer numero par si es que hay alguno, y devuelve 4 si son todos impares.
-- Interpreto que la posición se empieza a contar desde 0
posPrimerPar (a, b, c) | esMultiploDe a 2 = 0
                       | esMultiploDe b 2 = 1
                       | esMultiploDe c 2 = 2
                       | otherwise = 4

crearPar :: t1 -> t2 -> (t1, t2)
crearPar a b = (a, b)

invertir :: (t1, t2) -> (t2, t1)
invertir (a, b) = (b, a)


-- EJERCICIO 5 --

todosMenores :: (Int, Int, Int) -> Bool
todosMenores (a, b, c) = (funcion_f a) > (funcion_g a) && (funcion_f b) > (funcion_g b) && (funcion_f) c > (funcion_g c)

funcion_f :: Int -> Int
funcion_f n | n <= 7 = n^2
            | otherwise = 2*n - 1

funcion_g :: Int -> Int
funcion_g n | esMultiploDe n 2 = div n 2
            | otherwise = 3*n + 1


-- EJERCICIO 6 --

bisiesto :: Int -> Bool
bisiesto year | not (esMultiploDe year 4) || (esMultiploDe year 100 && not (esMultiploDe year 400)) = False
             | otherwise = True

-- Otra manera más corta pero menos clara:
-- bisiesto year = not(not(esMultiploDe year 4) || (esMultiploDe year 100 && not(esMultiploDe year 400)))


-- EJERCICIO 7 --

distanciaManhattan :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distanciaManhattan (p1, p2, p3) (q1, q2, q3) = abs (p1 - q1) + abs (p2 - q2) + abs (p3 - q3)


-- EJERCICIO 8 --

-- Siguiendo a pie de la letra la especificación:
-- sumaUltimosDosDigitos :: Int -> Int
-- sumaUltimosDosDigitos x = (mod x 10) + (mod (floor (x / 10)) (10))
-- Floor no funciona para enteros. No le gusto a Haskell mezclar operadores de distintos tipos de numeros.

sumaUltimosDosDigitos :: Int -> Int
sumaUltimosDosDigitos x = digitoUnidades x + digitoDecenas x

comparar a b | sumaUltimosDosDigitos a < sumaUltimosDosDigitos b = 1
             | sumaUltimosDosDigitos a > sumaUltimosDosDigitos b = -1
             | otherwise = 0  -- sumaUltimosDosDigitos a = sumaUltimosDosDigitos b
