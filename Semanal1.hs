media3 :: (Float,Float,Float) -> Float
media3 (a,b,c) = (a + b + c)/3

absoluto :: Int -> Int
absoluto n = if (n<0) then n * (-1) else n

distanciaPuntos :: (Float,Float) -> (Float,Float) -> Float
distanciaPuntos (a,b) (c,d) = sqrt((d - b) ** 2 + (c - a) ** 2)

sumaCom :: (Double,Double) -> (Double,Double) -> (Double,Double)
sumaCom (a,b) (c,d) = (a + c, b + d)

mulCom :: (Double,Double) -> (Double,Double) -> (Double,Double)
mulCom (a,b) (c,d) = (a * c - b, b * d + a)

esPar :: Int -> Bool
esPar n = if ((mod n 2) == 1) then False else True

areaCirculo :: Float -> Float
areaCirculo n = pi * n * n

rex :: Int -> Bool -> String
rex n b = if (((12 <= n) && (n <= 20) && True) || ((21 <= n) && (n <= 28) && False))
 		then "Rex sale a jugar" else "Rex no sale a jugar"
		
sumaGauss :: Int -> Int
sumaGauss n = (n * (n + 1)) `div` 2

calculadora :: String -> (Int,Int) -> Int
calculadora s (a,b) = if (s == "first") then a
			else if (s == "last") then b
 			else if (s == "sum") then a + b
			else if (s == "subs") then a -  b
			else if (s == "mul") then a * b
			else if (s == "div") then a `div` b
                        else if (s == "pow") then a^b else 0
                  
