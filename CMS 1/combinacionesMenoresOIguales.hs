-- No editar esta parte
main :: IO()
main = do {
  x <- readLn ;
  print(combinacionesMenoresOiguales(x ::(Integer)))
  }

combinacionesMenoresOiguales :: Integer -> Integer
-- Completar la definición de la función
-- Requiere: n natural sin incluir al cero.
combinacionesMenoresOiguales n = iteradorI n n

-- Pueden agregan las funciones que consideren necesarias

iteradorI :: Integer -> Integer -> Integer
-- i variable que va desde i=1 hasta i=n. n fijo.
-- Sumatoria desde i=1 hasta i=n de iteradorJ.
iteradorI i n | i == 1 = iteradorJ 1 n n
                                 | otherwise = (iteradorI (i-1) (n)) + (iteradorJ i n n)


iteradorJ :: Integer -> Integer -> Integer -> Integer
-- i fijo, j variable que va desde desde j=1 hasta j=n. n fijo.
-- Sumatoria desde j=1 hasta j=n de (if i * j <= n then 1 else 0).
iteradorJ i j n | j == 1 = if i * 1 <= n then 1 else 0
               | otherwise = (if i * j <= n then 1 else 0) + iteradorJ i (j-1) n
