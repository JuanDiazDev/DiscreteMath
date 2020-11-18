module Practica4 where

data Nat = Zero | Suc Nat deriving Show
data BTree a = Void | Node (BTree a) a (BTree a) deriving Show
data SnocList a = Empty | Snoc (SnocList a) a deriving Show

-- | igualNat. Decide si dos números naturales son iguales o no.
igualNat :: Nat -> Nat -> Bool
igualNat Zero Zero = True
igualNat (Suc x) Zero = False
igualNat (Suc x) (Suc y) = igualNat x y

-- | natToInt. Convierte números naturales a integers.
natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Suc x) = 1 + natToInt x

-- | intToNat. Convierte integers a números naturales.
intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat x = Suc(intToNat (x-1))

-- | restaNat. Hace una resta de números naturales.
restaNat :: Nat -> Nat -> Nat
restaNat x Zero = x
restaNat Zero x = error "Resultado negativo"
restaNat (Suc x) (Suc y) = restaNat x y

-- | preorden. Regresa una lista de elementos de un árbol binario en recorrido de preorden.
preorden :: BTree a -> [a]
preorden Void = []
preorden (Node t1 e t2) = [e] ++ preorden t1 ++ preorden t2

-- | inorden. Regresa una lista de elementos de un árbol binario en recorrido de inorden.
inorden :: BTree a -> [a]
inorden Void = []
inorden (Node t1 e t2) = inorden t1 ++ [e] ++ inorden t2

-- | postorden. Regresa una lista de elementos de un árbol binario en recorrido de postorden.
postorden :: BTree a -> [a]
postorden Void = []
postorden (Node t1 e t2) = postorden t1 ++ postorden t2 ++ [e]

agregaOrd:: Ord a => a -> BTree a -> BTree a
agregaOrd x Void = Node Void x Void
agregaOrd x (Node t1 a t2)
   | x == a = Node t1 x t2
   | x < a  = Node (agregaOrd x t1) a t2
   | x > a  = Node t1 a (agregaOrd x t2)

-- | reflejo. Regresa un árbol binario reflejado en un espejo.
reflejo :: BTree a -> BTree a
reflejo Void = Void
reflejo (Node t1 e t2) = Node (reflejo t2) e (reflejo t1)

-- | peso. Devuelve la longitud de la rama más larga de un árbol binario.
peso :: BTree a -> Int
peso Void = 0
peso (Node t1 e t2) = 1 + (max (peso t1) (peso t2))

-- | size. Devuelve el número de nodos de un árbol binario.
size :: BTree a -> Int
size Void = 0
size (Node t1 _ t2) = 1 + size t1 + size t2

-- | lastSnoc. Devuelve el último elemento de una lista snoc.
lastSnoc :: SnocList a -> a
lastSnoc Empty = error "lista vacía"
lastSnoc (Snoc a b) = b

-- | takeSnoc. Regresa los primeros n elementos de una lista snoc.
takeSnoc :: Int -> SnocList a -> SnocList a
takeSnoc _ Empty = error "lista vacía"
takeSnoc 1 (Snoc a b) = Snoc Empty (firstSnoc (Snoc a b))
takeSnoc x (Snoc a b) = Snoc (takeSnoc (x-1) (a)) b

-- | dropSnoc. Regresa una lista snoc sin los primeros n elementos.
dropSnoc :: Int -> SnocList a -> SnocList a
dropSnoc _ Empty = error "lista vacía"
dropSnoc 0 (Snoc a b) = Snoc a b
dropSnoc 1 (Snoc a b) = a
dropSnoc x (Snoc a b) = Snoc (dropSnoc ((longSnoc (Snoc a b)) - x) a) b

-- | reversaSnoc. Regresa la reversa de una lista snoc.
reversaSnoc :: SnocList a -> SnocList a
reversaSnoc Empty = Empty
reversaSnoc (Snoc Empty b) = Snoc Empty b
reversaSnoc (Snoc a b) = Snoc (reversaSnoc a) (firstSnoc (Snoc a b))


-- | firstSnoc. Regresa el primer elemento de una lista snoc.
firstSnoc :: SnocList a -> a
firstSnoc Empty = error "lista vacía"
firstSnoc (Snoc Empty b) = b
firstSnoc (Snoc (Snoc a b) c) = firstSnoc (Snoc a b)

-- | longSnoc. Regresa la longitud de una lista snoc.
longSnoc :: SnocList a -> Int
longSnoc Empty = 0
longSnoc (Snoc a b) = 1 + longSnoc(a)

