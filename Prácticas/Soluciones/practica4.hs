-- EJERCICIO 1 --

fibonacci :: Integer -> Integer
-- Requiere: n perteneciente a los naturales con el cero incluido.

fibonacci n | n == 0 = 0
            | n == 1 = 1
            | otherwise = (fibonacci (n - 1)) + (fibonacci (n - 2))

{- Alternativa:
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = (fibonacci (n - 1)) + (fibonacci (n - 2)) -}


-- EJERCICIO 2 --

{- Version que hace lo que dice el nombre, pero no cumple con la especificacion con x negativo
parteEntera :: Float -> Integer
parteEntera x | (x>(-1) && x<=0) || (x>=0 && x<1) = 0
              | x>=1 = parteEntera (x - 1) + 1
              | x<=(-1) = parteEntera (x + 1) - 1 -}

-- La espeficación pide en realidad ceiling function:
parteEntera :: Float -> Integer
parteEntera x | x>=0 && x<1 = 0
              | x>=1 = parteEntera (x - 1) + 1
              | x<(0) = parteEntera (x + 1) - 1



-- EJERCICIO 3 --

esDivisible ::  Integer -> Integer -> Bool
-- No usar mod ni div. Quiero saber si a es divisible por b --
-- Requiere: a, b perteneciente a los naturales.

esDivisible a b | a == b = True
                | a < b = False
                | otherwise = esDivisible (a-b) b


-- EJERCICIO 4 --

sumaImpares :: Integer -> Integer
-- Requiere: n perteneciente a los naturales sin el cero.

sumaImpares n | n == 1 = 1
              | otherwise = sumaImpares (n-1) + n*2 - 1


-- EJERCICIO 5 --

medioFact :: Integer -> Integer
-- Requiere: n perteneciente a los naturales con el cero incluido.

medioFact n | (n == 0 || n == 1) = 1
            |  otherwise = medioFact (n-2) * n


-- EJERCICIO 6 --

sumaDigitos :: Integer -> Integer
-- Requiere: n perteneciente a los naturales con el cero incluido.

sumaDigitos n | div n 10 == 0 = mod n 10
              | otherwise = sumaDigitos (div n 10) + mod n 10


-- EJERCICIO 7 --

todosDigitosIguales :: Integer -> Bool
-- Requiere: n perteneciente a los naturales sin el cero.

{- Versión vieja con funciones auxiliares especificas:
todosDigitosIguales n | div n 10 == 0 = True
                      | otherwise = (todosDigitosIguales (div n 10) && (digitoUnidades n == digitoDecenas n)) -}

todosDigitosIguales n | div n 10 == 0 = True
                      | otherwise = (todosDigitosIguales (div n 10) && (iesimoDigitoOrdenado n 1 == iesimoDigitoOrdenado n 2))

-- Funciones auxiliares.
digitoUnidades :: Integer -> Integer
digitoUnidades a = a - (div a 10) * 10

digitoDecenas :: Integer -> Integer
digitoDecenas a = div (a - (div a 100) * 100) 10


-- EJERCICIO 8 --

iesimoDigito :: Integer -> Integer -> Integer
-- Require n >= 0 && 1 <= i <= cantDigitos n

-- OBS: es el iesimoDigito empezando desde la derecha y yendo hacia la izquierda. No empieza por las unidades.
iesimoDigito n i = mod (div n (10^((cantDigitos (n)) - (i)))) (10)


cantDigitos :: Integer -> Integer
-- Requiere: n perteneciente a los naturales con el cero incluido.

cantDigitos n | (0 <= n && n < 10) = 1
              | otherwise = cantDigitos (div n 10) + 1


-- EJERCICIO 8 BIS --

iesimoDigitoOrdenado :: Integer -> Integer -> Integer
-- iesimoDigitoOrdenado es una funcion devuelve el iesimo digito contando desde las unidades y subiendo.
-- (De derecha a izquierda)
-- Require n >= 0 && 1 <= i <= cantDigitos n

iesimoDigitoOrdenado n i = iesimoDigito n ((cantDigitos n) - (i) + 1)


-- EJERCICIO 9 --

esCapicua :: Integer -> Bool
-- Requiere: n perteneciente a los naturales con el cero incluido.

esCapicua n | cantDigitos n == 1 = True
            | otherwise = checkeoSimetricoCapicua n (1) && esCapicua (eliminarPrimerYUltimoDigito n)

eliminarDigitoUninidad :: Integer -> Integer
-- Require n >= 0

eliminarDigitoUninidad n = (div n 10)


eliminarUltimoDigito :: Integer -> Integer
-- Require n >= 0

eliminarUltimoDigito n = n - (((div n (10^((cantDigitos (n)) - 1)))) * ((10^((cantDigitos (n)) - 1))))


eliminarPrimerYUltimoDigito :: Integer -> Integer
-- Require n >= 0

eliminarPrimerYUltimoDigito n = eliminarUltimoDigito (eliminarDigitoUninidad (n))


checkeoSimetricoCapicua :: Integer -> Integer -> Bool
-- Checkea que dado un n, i. Que para ese i especifico, los digitos sean identidos tanto por derecha como por izquierda.
-- Require n >= 0 && 1 <= i <= cantDigitos n

checkeoSimetricoCapicua n i = iesimoDigito n i == iesimoDigitoOrdenado n i


-- EJERCICIO 10 --

f1 :: Integer -> Integer
-- Requiere: n perteneciente a los naturales con el cero incluido.
f1 n = 2 ^(n+1)-1

g1 :: Integer -> Integer
-- Requiere: n perteneciente a los naturales con el cero incluido.
g1 n | n == 0 = 1
     | otherwise = g1 (n-1) + 2^n

f2 :: Integer -> Float -> Float
-- Requiere: n perteneciente a los naturales sin el cero.
f2 n q | q/=1 = (q^(n+1)-q) / (q-1)
       | otherwise = 1

g2 :: Integer -> Float -> Float
-- Requiere: n perteneciente a los naturales sin el cero.
g2 n q | n == 1 = q
       | otherwise = g2 (n-1) q + (q^n)

{- f3 :: Integer -> Float -> Float
-- Requiere: n perteneciente a los naturales con el cero incluido.
f3 n q | abs (n) == 1 = fromIntegral (n) + 1
       | otherwise = ((q^(2*n+1)) - 1) / ((q^2) - 1)

g3 :: Integer -> Float -> Float
-- Requiere: n perteneciente a los naturales con el cero incluido.
g3 n q | n == 1 = q
       |  -}

-- EJERCICIO 11 --

eAprox :: Integer -> Float
-- Requiere: n perteneciente a los naturales con el cero incluido.
eAprox n | n == 0 = 1
         | otherwise = (1 / (fromIntegral (factorial (n)))) + (eAprox (n-1)) 

e :: Float
e = eAprox 10

factorial :: Integer -> Integer
-- Requiere: n perteneciente a los naturales con el cero incluido.

factorial n | n == 0 = 1
            | otherwise = factorial (n-1) * n


-- EJERCICIO 12 --

raizDe2Aprox :: Integer -> Float
-- Requiere: n perteneciente a los naturales sin el cero.

raizDe2Aprox n = (auxiliarRaizDe2Aprox n) - 1

auxiliarRaizDe2Aprox :: Integer -> Float
-- Requiere: n perteneciente a los naturales sin el cero.

auxiliarRaizDe2Aprox n | n == 1 = 2
                       | otherwise = (2) + ((1) / (auxiliarRaizDe2Aprox (n-1)))


-- EJERCICIO 13 --

