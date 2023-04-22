-- No editar esta parte
main :: IO()
main = do {
  x <- readLn ;
  print(prod(x ::(Integer)))
  }

prod :: Integer -> Integer
-- Completar la definición de la función

-- Requiere n perteneciente a loas naturales sin incluir al cero.
-- Productoria de 1 hasta 2*n de (i^2 + 2*i)
prod n = auxiliarProd (2*n)


-- Pueden agregan las funciones que consideren necesarias

auxiliarProd :: Integer -> Integer
-- Requiere n perteneciente a loas naturales sin incluir al cero.
-- Define la productoria de 1 hasta n de (i^2 + 2*i)
auxiliarProd n | n == 1 = (1^2) + (2*1)
               | otherwise = auxiliarProd (n-1) * ((n^2) +(2*n))