{- Notas varias: (Después borrar)

lista ++ lista concatenar.
elemento : lista agregar.

ultimo x | (longitud x) == 1 = head x -- primera alternativa para hablar de una lista con un solo elemento. Sin patter matching
que la lista sólo tiene un solo elemento se puede escribir con patter matching con:
ultimo [x] = ...

patter matching (x:y:xs) x primer elemento, y segundo elemento, xs la lista del resto de elementos. -}



-- EJERCICIO 1 --

    -- ITEM 1 --

longitud :: [t] -> Integer
-- Requiere: True
longitud [] = 0
longitud (x:xs) = (longitud xs) + 1


    -- ITEM 2 --

ultimo :: [t] -> t
-- Requiere: Lista no vacía.
ultimo [a] = a
ultimo (x:xs) = ultimo xs


    -- ITEM 3 --

principio :: [t] -> [t]
-- Requiere: Lista no vacía.
-- Devuelve una lista con todos los elementos menos el último.
principio [a] = []
principio (x:xs) = x : (principio xs)


    -- ITEM 4--

reverso :: [t] -> [t]
-- Requiere: True
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]


-- EJERCICIO 2 --

    -- ITEM 1 --

pertenece :: (Eq t) => t -> [t] -> Bool
-- Requiere: True
pertenece _ [] = False
pertenece e (x:xs) = if e == x then True else pertenece e xs
-- Alternativa: pertenece e (x:xs) = e == x || pertenece e xs


    -- ITEM 2 --

todosIguales :: (Eq t) => [t] -> Bool
-- Requiere: True
todosIguales [] = True
todosIguales (x:xs) = (x == ultimo (x:xs)) && (todosIguales xs)


    -- ITEM 3 --

todosDistintos :: (Eq t) => [t] -> Bool
-- Requiere: True
todosDistintos [] = True
todosDistintos (x:xs) = (not (pertenece x xs)) && (todosDistintos xs)


    -- ITEM 4 --

hayRepetidos :: (Eq t) => [t] -> Bool
-- Requiere: True
hayRepetidos [] = False
hayRepetidos (x:xs) = (pertenece x xs) || (hayRepetidos xs)


    -- ITEM 5 --

-- quitar :: (Eq t) => t -> [t] -> [t]
