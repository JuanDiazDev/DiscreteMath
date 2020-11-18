module Practica3 where

data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show,Eq)
data Formula = Prop Var
     	     | Verdadero
	     | Falso
	     | Neg Formula
	     | Formula :&: Formula
	     | Formula :|: Formula
	     | Formula :=>: Formula
	     | Formula :<=>: Formula deriving (Show, Eq)

infixl 9 :&:
infixl 9 :|:
infixr 8 :=>:
infixl 7 :<=>:

type Estado = [Var]

negar :: Formula -> Formula
negar (Verdadero)=Verdadero
negar (Falso)=Falso
negar (Prop x)=(Prop x)
negar (a :&: b)=negar (a) :&: negar (b)
negar (a :|: b)=negar (a) :|: negar (b)
negar (Neg a)= case a of
      	       (c :&: b)->Neg (negar (c)) :|: Neg(negar (b))
	       (c :|: b)->Neg(negar (c)) :&: Neg(negar (b))
	       (c :=>: b)->negar (c) :=>: negar (b)
	       (c :<=>: b)->negar (c) :<=>: negar (b)
	       _ -> Neg a
negar (a :=>: b)=negar (a) :=>: negar (b)
negar (a:<=>: b)=negar (a) :<=>: negar (b)

eliminacion :: Formula -> Formula
eliminacion (Verdadero)=Verdadero
eliminacion (Falso)=Falso
eliminacion (Prop x)=Prop x
eliminacion (Neg a)= case a of
	    	     (c :&: b)->Neg (negar(eliminacion(a) :&: eliminacion (b)))
	       	     (c :|: b)->Neg (negar(eliminacion(a) :|: eliminacion (b)))
	      	     (c :=>: b)->Neg (negar(Neg (eliminacion(a)) :|: eliminacion (b)))
	      	     (c :<=>: b)->Neg (negar(eliminacion(a :=>: b) :&: eliminacion(b :=>: a)))
		     _ -> Neg a

eliminacion (a :&: b)= negar(eliminacion(a) :&: eliminacion (b))
eliminacion (a :|: b)= negar(eliminacion(a) :|: eliminacion (b))
eliminacion (a :=>: b) = negar(Neg (eliminacion(a)) :|: eliminacion (b))
eliminacion (a :<=>: b) = negar(eliminacion(a :=>: b) :&: eliminacion(b :=>: a))

vars :: Formula -> [Var]
vars (Verdadero) = []
vars (Falso) = []
vars (Prop x) = [x]
vars (Neg a) = case a of
               (b :|: c) -> vars b ++ vars c
	       (b :&: c) -> vars b ++ vars c
	       (b :=>: c) -> vars b ++ vars c
	       (b :<=>: c) -> vars b ++ vars c
vars (a :|: b) = vars a ++ vars b
vars (a :&: b) = vars a ++ vars b
vars (a :=>: b) = vars a ++ vars b
vars (a :<=>: b) = vars a ++ vars b

interp :: Estado -> Formula -> Bool
interp [x] (Verdadero) = True
interp [x] (Falso) = False
interp list (Neg a) = case a of
       	    	      Prop y -> subList (vars (Prop y)) list
       	    	      (b :&: c) -> if (subList (vars (a)) list && subList (vars(b)) list) then False else True
		      (b :|: c) -> subList (vars (b)) list == False && subList (vars(c)) list == False
		      (b :=>: c) -> subList (vars (b)) list  && subList (vars(c)) list == False
		      (b :<=>: c) -> interp list (b :=>: c) == False || interp list (b :=>: c) == False
interp list (Prop y) = (vars (Prop y)) == list
interp list (a :|: b) = subList (vars (a)) list || subList (vars(b)) list
interp list (a :&: b) = subList (vars (a)) list && subList (vars(b)) list
interp list (a :=>: b) = subList (vars (a)) list == False || subList (vars(b)) list
interp list (a :<=>: b) = interp list (a :=>: b) && interp list (b :=>: a)

conjPotencia :: [a] -> [[a]]
conjPotencia [] = [[]]
conjPotencia (x:xs) = [x:ys | ys <- conjPotencia xs] ++ conjPotencia xs

estados :: Formula -> [Estado]
estados (Verdadero) = []
estados (Falso) = []
estados (Prop x) = conjPotencia (vars (Prop x))
estados (Neg a) = case a of
	     	  (b :&: c) -> conjPotencia (vars (a :|: b))
		  (b :|: c) -> conjPotencia (vars (a :|: b))
		  (b :=>: c) -> conjPotencia (vars (a :|: b))
		  (b :<=>: c) -> conjPotencia (vars (a :|: b))
estados (a :|: b) = conjPotencia (vars (a :|: b))
estados (a :&: b) = conjPotencia (vars (a :|: b))
estados (a :=>: b) = conjPotencia (vars (a :|: b))
estados (a :<=>: b) = conjPotencia (vars (a :|: b))

tautologia :: Formula -> Bool
tautologia (Verdadero) = True
tautologia (Falso) = False
tautologia (Prop y) = andy [interp (x) (Prop y)|x<-(estados (Prop y)), x /= []]
tautologia (a :&: b) = andy [interp (x) (a :&: b)|x<-(estados (a :&: b)), x /= []]
tautologia (a :|: b) = andy [interp (x) (a :|: b)|x<-(estados (a :|: b))]
tautologia (a :=>: b) = andy [interp (x) (a :=>: b)|x<-(estados (a :=>: b))]
tautologia (a :<=>: b) = andy [interp (x) (a :<=>: b)|x<-(estados (a :<=>: b))]

contradiccion :: Formula -> Bool
contradiccion (Verdadero) = False
contradiccion (Falso) = True
contradiccion (Prop y) = (andy [interp (x) (Prop y)|x<-(estados (Prop y))]) == False
contradiccion (Neg a) = case a of
	      	      	(c :&: b) -> (andy [interp (x) (c :&: b)|x<-(estados (c :&: b))]) 
	      	      	(c :|: b) -> (andy [interp (x) (c :|: b)|x<-(estados (c :|: b))])
			(c :=>: b) -> (andy [interp (x) (c :=>: b)|x<-(estados (c :=>: b))])
			(c :<=>: b) -> (andy [interp (x) (c :<=>: b)|x<-(estados (c :<=>: b))])
			(Prop y) -> (andy [interp (x) (Prop y)|x<-(estados (Prop y)),x/=[]])
contradiccion (a :&: b) = (andy [interp (x) (a :&: b)|x<-(estados (a :&: b)),x/=[]]) == False
contradiccion (a :|: b) = (andy [interp (x) (a :|: b)|x<-(estados (a :|: b)), x/=[]]) == False
contradiccion (a :=>: b) = (andy [interp (x) (a :=>: b)|x<-(estados (a :=>: b)), x/=[]]) == False
contradiccion (a :<=>: b) = (andy [interp (x) (a :<=>: b)|x<-(estados (a :<=>: b))]) == False

contingencia :: Formula -> Bool
contingencia (Verdadero) = False
contingencia (Falso) = False
contingencia (Prop y) = contradiccion (Prop y) == False && tautologia (Prop y) == False
contingencia (a :&: b) = contradiccion (a :&: b) == False && tautologia (a :&: b) == False
contingencia (a :|: b) = contradiccion (a :|: b) == False && tautologia (a :|: b) == False
contingencia (a :=>: b) = contradiccion (a :=>: b) == False && tautologia (a :=>: b) == False
contingencia (a :<=>: b) = contradiccion (a :<=>: b) == False && tautologia (a :<=>: b) == False

subList :: Eq a => [a] -> [a] -> Bool
subList [] [] = True
subList _ []    = False
subList [] _    = True
subList (x:xs) (y:ys) 
    | x == y    = subList xs ys   
    | otherwise = subList (x:xs) ys

andy :: [Bool] -> Bool
andy [] = True
andy (x:xs) = if x == True then (andy xs) else False
