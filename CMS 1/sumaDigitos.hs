-- No editar esta parte
main :: IO()
main = do {
  x <- readLn ;
  print(sumaDigitos(x ::(Int)))
  }

sumaDigitos :: Int -> Int
-- Completar la definición de la función

-- Requiere: n perteneciente a los naturales incluido el cero.
sumaDigitos n | cantDigitos n == 1 = n
              | otherwise = (sumaDigitos (eliminarDigitoUninidad n)) + (digitoUnidades (n))

-- Pueden agregan las funciones que consideren necesarias

cantDigitos :: Int -> Int
-- Requiere: n perteneciente a los naturales con el cero incluido.
cantDigitos n | (0 <= n && n < 10) = 1
              | otherwise = cantDigitos (eliminarDigitoUninidad n) + 1

eliminarDigitoUninidad :: Int -> Int
-- Require: n perteneciente a los naturales con el cero incluido.
eliminarDigitoUninidad n = (div n 10)

digitoUnidades :: Int -> Int
-- Require: n perteneciente a los naturales con el cero incluido.
digitoUnidades a = mod a 10
