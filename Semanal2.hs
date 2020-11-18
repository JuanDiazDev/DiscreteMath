module Semanal2 where

data Nat = Zero | Suc Nat deriving Show
data Btree a = Void | Node (Btree a) a (Btree a) deriving Show
data SnocList a = Empty | Snoc (SnocList a) a deriving Show

-- sumaNat. Realiza la suma de dos números naturales.
sumaNat :: Nat -> Nat -> Nat
sumaNat Zero a = a
sumaNat (Suc x) (Suc b) = Suc(sumaNat x b)

-- multNat. Realiza la multiplicación de dos números naturales
multNat :: Nat -> Nat -> Nat
multNat Zero a = Zero
multNat (Suc x) (Suc b) = sumaNat (Suc x) (multNat (Suc x) b)

-- menorNat. Dados dos números naturales, te regresa si el primero es menor que el segundo.
menorNat :: Nat -> Nat -> Bool
menorNat Zero (Suc x) = True
menorNat (Suc x) Zero = False
menorNat Zero Zero = False
menorNat (Suc x) (Suc b) = menorNat x b 

-- mayorNat. Dados dos números naturales, te regresa si el primeros es mayor que el segundo.
mayorNat :: Nat -> Nat -> Bool
mayorNat Zero (Suc x) = False
mayorNat (Suc x) Zero = True
mayorNat Zero Zero = False
mayorNat (Suc x) (Suc b) = mayorNat x b

-- inTree. Te regresa si un elemento está contenido en un árbol binario.
inTree :: Ord a => a -> Btree a -> Bool 
inTree _ Void = False
inTree x (Node t1 a t2)  
	| x == a = True
	| x < a = inTree x t1 
	| x > a = inTree x t2

-- maximo. Te regresa el valor máximo de un árbol binario.
maximo :: Ord a => Btree a -> a
maximo Void = error "árbol vacío"
maximo (Node Void e Void) = e
maximo (Node t1 e t2) = max e (max (maximo t1) (maximo t2))

-- minimo. Te regresa el valor mínimo de un árbol binario.
minimo :: Ord a => Btree a -> a
minimo Void = error "árbol vacío"
minimo (Node Void e Void) = e
minimo (Node t1 e t2) = min e (min (minimo t1) (minimo t2))

-- firstSnoc. Te regresa el primer valor de una lista snoc.
firstSnoc :: SnocList a -> a
firstSnoc Empty = error "lista vacía"
firstSnoc (Snoc a b) = b

-- lengthSnoc. Te regresa la longitud de una lista snoc.
lengthSnoc :: SnocList a -> Int
lengthSnoc Empty = 0
lengthSnoc (Snoc a x) = 1 + lengthSnoc(a) 

-- concatSnoc. Concatena dos listas snoc.
concatSnoc :: SnocList a -> SnocList a -> SnocList a
concatSnoc Empty l= l 
concatSnoc a (Snoc b c) = Snoc (concatSnoc a b) c 







