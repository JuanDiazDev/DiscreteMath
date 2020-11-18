module Practica2 where

--fibonacci. Regresa el n-ésimo elemento de una serie de fibonacci
--fibonacci 3 = 2
fibonacci :: Int -> Int
fibonnaci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x-1) + fibonacci (x-2)

--andy. Dada una lista de booleanos, regresa si todos los elementos de la lista se evalúan a verdadero.
--andy [True, True, False, True] -> False
andy :: [Bool] -> Bool
andy [] = True
andy (x:xs) = if x == True then (andy xs) else False

--concate. Concatena una lista de listas.
--concate [[1,2],[3,4],[5]] = [1,2,3,4,5]
concate :: [[a]] -> [a]
concate [[x]]=[x]
concate [[]] = []
concate [xs,ys] = xs ++ concate [ys]

--esElemento. Regresa si un elemento pertenece a una lista o no.
--esElemento 5 [1,2,3,4] = False
esElemento :: Eq a => a -> [a] -> Bool
esElemento a [] = False
esElemento a (x:xs) = if a == x then True else (esElemento a xs)

--merge. Concatena dos listas y la lista output corresponde a una lista ordenada.
--merge [3,5,1] [4,2] = [1,2,3,4,5]
merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) [] = isort(x:xs)
merge [] [] = []
merge [] (x:xs) = isort(x:xs)
merge (x:xs) (y:ys) = isort([x] ++ (merge (xs) (y:ys)))

--inversa. Dada una lista de tuplas, regresa otra lista donde el primer y segundo valor de cada tupla se intercambia de posición.
--inversa [(1,2),(1,2)] = [(2,1),(2,1)]
inversa [] = []
inversa list = [(d,c)|(c,d)<-list]

--elimRep. Elimina todos los elementos repetidos de una lista.
--elimRep [1,1,2,2,3,3,4,4] -> [1,2,3,4]
elimRep :: (Eq a) => [a] -> [a]
elimRep (x:xs) = x : elimRep (filter (/= x) xs)
elimRep [] = []

--eliminaElem. Elimina todas las ocurrencias de un elemento en una lista.
--eliminaElem 4 [1,2,3,4,4] -> [1,2,3]
eliminaElem :: Eq a => a -> [a] -> [a]
eliminaElem _ [] = []
eliminaElem x (y:ys) = iguales x y ++ eliminaElem x ys
iguales :: Eq a => a -> a-> [a]
iguales x y | x == y = []
            | otherwise = [y]

						
--cuenta. Dado una entrada con un elemento y una lista, regresa el número de veces que aparece el elemento en la lista.
--cuenta 3 [1,2,3,3,4] -> 2
cuenta :: Eq a => a -> [a] -> Int
cuenta a [] = 0
cuenta a (x:xs) = if a == x then (1 + (cuenta a xs)) else (0 + (cuenta a xs))

--frecuencia. Dada una lista, regresa una lista de tuplas donde el primer valor de cada tupla corresponde a un elemento de la lista inicial y el segundo valor a la cantidad de veces que aparece tal elemento.
--frecuencia [1,2,2,3,4,4] -> [(1,1),(2,2),(3,1),(4,2)]
frecuencia :: Eq a => [a] -> [(a,Int)]
frecuencia [] = []
frecuencia (x:xs) = [(x, cuenta x (x:xs))] ++ (frecuencia (filter (/= x) xs))

--inserta. Dado un elemento y una lista, el elemento se inserta en la lista de acuerdo a su valor.
--inserta. 2 [1,3,4] -> [1,2,3,4]
inserta :: Ord a => a -> [a] -> [a]
inserta x [] = [x]
inserta x (y:ys) = if x <= y then (x:y:ys) else (y:inserta x ys)

--isort. Dada una lista, devuelve una lista ordenada con los mismos elementos
--isort [3,6,8,4,1,5,2,7,9] -> [1,2,3,4,5,6,7,8,9]
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = inserta x (isort xs)












