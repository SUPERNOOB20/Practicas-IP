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
-- digitoUnidades a = a - (div a 10) * 10
digitoUnidades a = mod a 10

digitoDecenas :: Integer -> Integer
-- digitoDecenas a = div (a - (div a 100) * 100) 10
digitoDecenas a = div (mod a 100) 10


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
-- Requiere: n perteneciente a los naturales sin el cero, q real.
f2 n q | q/=1 = (q^(n+1)-q) / (q-1)
       | otherwise = 1

g2 :: Integer -> Float -> Float
-- Requiere: n perteneciente a los naturales sin el cero, q real.
g2 n q | n == 1 = q
       | otherwise = g2 (n-1) q + (q^n)

f3 :: Integer -> Float -> Float
-- Requiere: n perteneicente a los naturales con el cero incluido, q Real.
f3 n q = f2 (2*n) q

f4 :: Integer -> Float -> Float
f4 n q = (f3 n q) - (f2 (n-1) q)


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

sumatoriaEj13 :: Integer -> Integer -> Integer
-- Requiere: n, m perteneciente a los naturales sin el cero.
-- Sumatoria de i=1 hasta n de auxiliarSumatoriaEj13.
sumatoriaEj13 n m | n == 1 = auxiliarSumatoriaEj13 1 m
              | otherwise = sumatoriaEj13 (n-1) m + auxiliarSumatoriaEj13 n m

auxiliarSumatoriaEj13 :: Integer -> Integer -> Integer
-- Requiere: m perteneciente a los naturales sin el cero.
-- Sumatoria de j=1 hasta m de i^j. i esta fijo (cte) y se itera la j.
auxiliarSumatoriaEj13 i m | m == 1 = i^1
                      | otherwise = auxiliarSumatoriaEj13 i (m-1) + (i^m)


-- EJERCICIO 14 --

sumaPotencias :: Integer -> Integer -> Integer -> Integer
-- Requiere: q natural incluiyendo al cero. n, m naturales sin el cero.
-- Sumatoria de a=1 hasta n de auxiliarSumaPotencias.
sumaPotencias q n m | n == 1 = auxiliarSumaPotencias q 1 m
                    | otherwise = sumaPotencias q (n-1) m + auxiliarSumaPotencias q n m


auxiliarSumaPotencias :: Integer -> Integer -> Integer -> Integer
-- Requiere: q natural incluiyendo al cero. a, m naturales sin el cero.
-- Sumatoria de b=1 hasta m de (q^a) * (q^b). a esta fijo (cte) y se itera la b
auxiliarSumaPotencias q a m | m == 1 = (q^a) * (q^1)
                            | otherwise = auxiliarSumaPotencias q a (m-1) + (q^a) * (q^m)


-- EJERCICO 15 --

sumaRacionales :: Integer -> Integer -> Float
-- Requiere: n, m pertenecientes a los naturales sin el cero.

sumaRacionales n m | n == 1 = auxiliarSumaRacionales 1 m
                   | otherwise = sumaRacionales (n-1) m + auxiliarSumaRacionales n m


auxiliarSumaRacionales :: Integer -> Integer -> Float
-- Requiere: m perteneciente a los naturales sin el cero.
-- Sumatoria de q=1 hasta m de p/q. p esta fijo (cte) y se itera la q
auxiliarSumaRacionales p m | m == 1 = (fromIntegral p) / 1
                           | otherwise = (auxiliarSumaRacionales p (m-1)) + ((fromIntegral p) / (fromIntegral m))

-- CONSULTAR SOBRE fromIntegral !!!!!! --
{- testeando :: Integer -> Integer -> Float
testeando a b = fromIntegral(a) / fromIntegral(b) -}


-- EJERCICIO 16 --

       -- ITEM A

menorDivisor :: Integer -> Integer
-- Requiere: n natural sin incluir al cero.
-- Busca el menor divisor de n mayor o igual a 2.
menorDivisor 1 = 1
menorDivisor n = menorDivisorAPartirDe n 2

menorDivisorAPartirDe :: Integer -> Integer -> Integer
-- Requiere: n, i natural sin incluir al cero y 1<=i<=n.
-- Busca el menor divisor a partir de 2.
menorDivisorAPartirDe n i | i == n = n
                          | otherwise = if mod n i == 0 then i else menorDivisorAPartirDe n (i+1)


       -- ITEM B --

esPrimo :: Integer -> Bool
-- Requiere: n perteneciente a los naturales sin incluir al cero.
esPrimo n = if menorDivisor n == n then True else False


       -- ITEM C --

sonCoprimos :: Integer -> Integer -> Bool
-- Requiere: a, b naturales sin incluir al cero.
sonCoprimos a b = auxiliarSonCoprimos a b (maximoEntre a b)

auxiliarSonCoprimos :: Integer -> Integer -> Integer -> Bool
-- Requiere: a, b, i naturales sin incluir al cero.
auxiliarSonCoprimos a b i | i == 1 = True
                          | otherwise = not(mod a i == 0 && mod b i == 0) && auxiliarSonCoprimos a b (i-1)

maximoEntre :: (Ord t) => t -> t -> t
maximoEntre a b  | a >= b = a
                 | otherwise = b


       -- ITEM D --

nEsimoPrimo :: Integer -> Integer
-- Requiere: n perteneciente a los naturales sin incluir al cero.
nEsimoPrimo n = auxiliarNEsimoPrimo 1 n


auxiliarNEsimoPrimo :: Integer -> Integer -> Integer
-- Requiere: numero, contador naturales.
-- Retorna el n-esimo (contador) primo contando desde (numero).
auxiliarNEsimoPrimo numero contador | contador == 0 = numero - 1
                                    | otherwise = if esPrimo numero
                                          then (auxiliarNEsimoPrimo (numero + 1) (contador - 1))
                                          else (auxiliarNEsimoPrimo (numero + 1) (contador))


-- EJERCICIO 17 --

esFibonacci :: Integer -> Bool
-- Requiere n perteneciente a los naturales incluyendo al cero.
-- OBS: Funciona pero es lento.
esFibonacci n = auxiliarEsFibonacci n (n+1)

auxiliarEsFibonacci :: Integer -> Integer -> Bool
-- Requiere n, i perteneciente a los naturales incluyendo al cero.
auxiliarEsFibonacci n i | ((n /= 0) && (i == 0)) = False
                        | n == fibonacci i = True
                        | otherwise = auxiliarEsFibonacci n (i-1)


-- EJERCICIO 18 --

mayorDigitoPar :: Integer -> Integer
-- Requiere: n perteneciente a los naturales sin el cero.
mayorDigitoPar 0 = 0
mayorDigitoPar n = auxiliarMayorDigitoEspecial n 8 (-2)


tieneDigitoIgualAi :: Integer -> Integer -> Bool
-- Requiere: n perteneciente a los naturales incluyendo al cero. i un numero natural incluyendo al cero de un digito.
tieneDigitoIgualAi n i | n == 0 = False
                       | otherwise = if (digitoUnidades n) == i then True else (tieneDigitoIgualAi (eliminarDigitoUninidad n) (i))

auxiliarMayorDigitoEspecial :: Integer -> Integer -> Integer -> Integer
-- Requiere: n perteneciente a los naturales incluyendo al cero. i numero natural inclyendo al cero y par. rSalto (reduccion) entero negativo.
-- n es el numero a evaluar. i es el primer digito a verificar igualdad. rSalto es un numero negativo que va a ir restando al i. 
auxiliarMayorDigitoEspecial n i rSalto | i <= rSalto = (-1)
                                       | otherwise = if (tieneDigitoIgualAi n i) then i else (auxiliarMayorDigitoEspecial n (i + rSalto) rSalto)


-- EJERCICIO 19 --

-- EJERCICIO 20 --

-- EJERCICIO 21 --

--pitagoras :: Integer -> Integer -> Integer -> Integer
-- Requiere m, n, r perteneciente a los naturales incluyendo al cero.

-- ARREGLAR !!!! ---

pitagoras :: Integer -> Integer -> Integer -> Integer
-- Requiere: Requiere m, n, r perteneciente a los naturales incluyendo al cero.
-- Sumatoria de p=0 hasta n de auxiliarPitagoras.
pitagoras n m r | n == 0 = auxiliarPitagoras 0 m r
                | otherwise = pitagoras (n-1) m r + auxiliarPitagoras n m r


auxiliarPitagoras :: Integer -> Integer -> Integer -> Integer
-- Requiere: Requiere m, n, r perteneciente a los naturales incluyendo al cero.
-- Sumatoria de q=0 hasta m de (if (p^2) + (q^2) >= (r^2) then 1 else 0) . p esta fijo (cte) y se itera la q respecto a la m. r constante.
auxiliarPitagoras p m r | m == 0 = if ((p^2) + (0^2)) <= (r^2) then 1 else 0
                        | otherwise = (auxiliarPitagoras p (m-1) (r^2)) + (if (((p^2) + (m^2)) <= (r^2)) then 1 else 0)

-- ARREGLAR !!!! ---
