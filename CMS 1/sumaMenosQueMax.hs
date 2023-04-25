-- No editar esta parte
main :: IO ()
main = do
  x <- readLn
  print (sumaMenosQueMax (x :: (Int, Int, Int)))

sumaMenosQueMax :: (Int, Int, Int) -> Bool

-- Completar acá la definición de la función

sumaMenosQueMax (a, b, c) = (maximoEntreTres x y z) > ((minimoEntreTres x y z) + (medio x y z))
                          where x = auxiliarCurrificacion (a, b, c) 0
                                y = auxiliarCurrificacion (a, b, c) 1
                                z = auxiliarCurrificacion (a, b, c) 2

-- Pueden agregan las funciones que consideren necesarias

auxiliarCurrificacion :: (Int, Int, Int) -> Int -> Int
auxiliarCurrificacion (a, b, c) 0 = a
auxiliarCurrificacion (a, b, c) 1 = b
auxiliarCurrificacion (a, b, c) 2 = c

maximoEntre :: (Ord t) => t -> t -> t
maximoEntre a b  | a >= b = a
                 | otherwise = b

maximoEntreTres :: (Ord t) => t -> t -> t -> t
maximoEntreTres a b c = maximoEntre (maximoEntre a b) c


minimoEntre :: (Ord t) => t -> t -> t
minimoEntre a b  | a <= b = a
                 | otherwise = b

minimoEntreTres :: (Ord t) => t -> t -> t -> t
minimoEntreTres a b c = minimoEntre (minimoEntre a b) c

medio :: (Ord t) => t -> t -> t -> t
medio a b c = maximoEntreTres (minimoEntre a b) (minimoEntre a c) (minimoEntre b c)
