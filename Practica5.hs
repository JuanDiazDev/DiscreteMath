-- | Estructuras Discretas 2020-1
-- | Práctica 5: Gráficas y relaciones
-- | Profesora: Pilar Selene Linares Arévalo
-- | Laboratorio: Adrián Felipe Vélez Rivera
-- | Integrantes: 420004365 juanmanuel32@ciencias.unam.mx

module Practica5 where

type Graph a = ([a],[(a,a)]) 

ingrado :: Eq a => a -> Graph a -> Int
ingrado a ((x:xs), list) = ocurrencias a (destino ((x:xs), list))

exgrado :: Eq a => a -> Graph a -> Int
exgrado a ((x:xs), list) = ocurrencias a (origen ((x:xs), list)) 

esReflexiva :: Eq a => Graph a -> Bool
esReflexiva ((x:xs), list) = length(elimRep (origen ((x:xs), list))) == compara (origen ((x:xs), list)) (destino ((x:xs), list))

esSimetrica :: Eq a => Graph a -> Bool
esSimetrica (list, []) = True
esSimetrica (list, (x:xs)) = if elem (swap x) (x:xs) then esSimetrica (list, (xs)) else False

esAntisimetrica :: Eq a => Graph a -> Bool
esAntisimetrica (list, (x:xs)) = esSimetrica(list, (x:xs)) && esReflexiva(list, (x:xs))

--swap. Regresa una tupla de dos elementos con el orden invertido.
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

--Origen. Devuelve una lista con los vértices origen de cada arco en una digráfica.
origen :: Graph a -> [a]
origen (_, list) = primero list

--Destino. Devuelve una lista con los vértices destino de cada arco en una digráfica.
destino :: Graph a -> [a]
destino (_, list) = segundo list

--Primero. Devuelve una lista con el primer elemento de cada tupla en una lista de tuplas.
primero :: [(a,b)] -> [a]
primero list = [a | (a,b) <- list]

--Segundo. Devuelve una lista con el segundo elemento de cada tupla en una lista de tuplas.
segundo :: [(a,b)] -> [b]
segundo list = [b | (a,b) <- list]

--Ocurrencias. Devuelve el número de ocasiones que un elemento aparece en una lista.
ocurrencias :: Eq a => a -> [a] -> Int
ocurrencias a [] = 0
ocurrencias a (x:xs) = if a == x then 1 + ocurrencias a (xs) else ocurrencias a (xs)

--elimRep. Dada una lista, regresa una lista con los mismos elementos pero sin ser repetidos.
elimRep :: (Eq a) => [a] -> [a]
elimRep [] = []
elimRep (x:xs) = x : elimRep (filter (/= x) xs)

--compara. Regresa el número de ocasiones que dos listas coinciden con el mismo elemento en el mismo índice.
compara :: Eq a => [a] -> [a] -> Int
compara [] [a] = 0
compara [a] [] = 0
compara (x:xs) [y] = if x == y then 1 else 0
compara [x] (y:ys) = if x == y then 1 else 0
compara (x:xs) (y:ys) = if x == y then 1 + compara (xs) (ys) else compara (xs) (ys)

