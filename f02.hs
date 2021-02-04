module Ficha2 where
import Data.Char
import Data.List

dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = 2*h : dobros t

numOcorre :: Char -> String -> Int
numOcorre c (h:t)
    | c == h = 1 + numOcorre c t
    | c /= h = numOcorre c t
numOcorre _ [] = 0
numOcorre c l = if elem c l == False then 0 else numOcorre c l


positivos :: [Int] -> Bool
positivos (h:t)
    | h < 0 = False
    | otherwise = positivos t
positivos [] = False

soPos :: [Int] -> [Int]
soPos (h:t) 
    | h > 0 = h : soPos t
    | otherwise = soPos t
soPos [] = []

somaNeg :: [Int] -> Int
somaNeg (h:t)
    | h < 0 = h + somaNeg t
    | otherwise = somaNeg t
somaNeg [] = 0

tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt l@(_:t)
    | length l <= 3 = l
    | otherwise = tresUlt t

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((_,b):c) = b : segundos c 

nosPrimeiros :: (Eq a) =>  a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros a ((q,_):e) 
    | a == q = True
    | otherwise = nosPrimeiros a e

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos = sumTriplos2 (0,0,0) 

sumTriplos2 :: (Num a, Num b, Num c) => (a,b,c) -> [(a,b,c)] -> (a,b,c)
sumTriplos2 acc [] = acc
sumTriplos2 (a,b,c) ((x,y,z):t) = sumTriplos2 (a + x, b + y, c + z) t

type Polinomio = [Monomio]
type Monomio = (Float,Int)

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((_,y):z)
    | n == y = 1 + conta n z
    | otherwise = conta n z

grau :: Polinomio -> Int
grau [] = 0
grau ((_,y):t) =
  let maiorGrauEmT = grau t
  in if y > maiorGrauEmT then y else maiorGrauEmT

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((x,y):t) = ((fromIntegral y)*x,y-1) : deriv t 

calcula :: Float -> Polinomio -> Float
calcula f ((x,y):t) = (f^y)*x + calcula f t
calcula _ [] = 0

simp :: Polinomio -> Polinomio
simp [] = []
simp ((x,y):t)
    | x == 0 = simp t
    | otherwise = (x,y) : simp t

mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (a,b) ((x,y):t) = (a*x,b+y) : mult (a,b) t

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza (h:t) = x : normaliza y
    where 
        (x,y) = normaliza' (h,[]) t

normaliza' :: (Monomio, Polinomio) -> Polinomio -> (Monomio, Polinomio)
normaliza' acc [] = acc
normaliza' ((a1,g1), p1) ((a2,g2):p2) 
    | g1 == g2 = normaliza' ((a1+a2, g1), p1) p2 
    | otherwise = normaliza' ((a1,g1),p1 ++ [(a2,g2)]) p2 


produto ::  Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto _ [] = []
produto ((a,b):p1) ((c,d):p2) = (a*c,b+d) : produto p1 p2

insere :: Monomio -> Polinomio -> Polinomio
insere (a,b) ((c,d):ps) 
    | b<d = (a,b) : (c,d) : ps 
    | otherwise = (c,d) : insere (a,b) ps
insere m [] = [m]

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena (h:t) = insere h (ordena t)

equiv :: Polinomio -> Polinomio -> Bool
equiv [] [] = True
equiv p1 p2
    | a == c && b == d = equiv x y
    | otherwise = False 
    where
        ((a,b):x) = ordena $ normaliza p1
        ((c,d):y) = ordena $ normaliza p2
equiv _ _ = False