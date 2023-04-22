-- No editar esta parte
main :: IO()
main = do {
  x <- readLn ;
  print(sumaPrimerosNImpares(x ::(Integer)))
  }

sumaPrimerosNImpares :: Integer -> Integer
-- Completar la definición de la función

-- Requiere: n perteneciente a los naturales sin incluir al cero.

sumaPrimerosNImpares n = auxiliarSumaPrimerosNImpares ((2*n)-(1))

-- Pueden agregan las funciones que consideren necesarias

auxiliarSumaPrimerosNImpares :: Integer -> Integer

-- Requiere: n perteneciente a los naturales sin incluir al cero.

auxiliarSumaPrimerosNImpares n | n == 1 = 2*1 + 2 -- mod 1 2 /= 0
                               | otherwise = auxiliarSumaPrimerosNImpares (n-1) + if mod n 2 == 0 then 0 else (2*n)+(2)

{- Alternativa, no tengo if-then-else pero requiere que n sea impar.
OBS: n impar se cumple siempre que auxiliarSumaPrimerosNImpares sea llamada por sumaPrimerosNImpares porque para todo n, 2*n-1 es impar
                               | otherwise = auxiliarSumaPrimerosNImpares (n-2) + (2*n) + (2) -}
