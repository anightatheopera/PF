module Ficha01 where
import Data.Char


type Hora = (Int,Int)



perimetro :: Double -> Double
perimetro r = 2*pi*r

dist :: (Double, Double) -> (Double, Double) -> Double
dist (a,b) (c,d) = sqrt((c-a)^2 + (d-b)^2)

primUlt :: [a] -> (a,a)
primUlt l = (head l, last l)

multiplo :: Int -> Int -> Bool
multiplo m n 
    | mod m n == 0 = True 
    | otherwise = False

truncaImpar :: [a] -> [a]
truncaImpar l
    | mod (length l) 3 == 0 = tail l
    | otherwise = l 

nRaizes :: Double -> Double -> Double -> Double
nRaizes a b c 
    | (b^2)-(4*a*c) > 0 = 2
    | (b^2)-(4*a*c) == 0 = 1
    | otherwise = 0

raizes :: Double -> Double -> Double -> [Double]
raizes a b c 
    | nRaizes a b c == 2 = [(-b + sqrt(b^2 - 4*a*c))/(2*a), (-b - sqrt(b^2 - 4*a*c))/(2*a)]
    | nRaizes a b c == 1 = [(-b + sqrt(b^2 - 4*a*c))/(2*a)]
    | otherwise = []

validHour :: (Int, Int) -> Bool
validHour (a,b) 
    | a >= 0  && a <= 23 && b >= 0 && b <= 59 = True
    | otherwise = False

laterOrNot :: (Int, Int) -> (Int, Int) -> Bool
laterOrNot p@(a,b) s@(c,d) 
    | validHour p && validHour s && a > c = True
    | validHour p && validHour s && a == c && b > d = True
    | otherwise = False

convertToMinutes :: (Int, Int) -> Int
convertToMinutes (a,b) = a*60 + b

convertToHours :: (Int, Int) -> Double
convertToHours (a,b) = fromIntegral((div b 60) + a) 

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

nextC :: Semaforo -> Semaforo
nextC c = case c of 
               Verde -> Amarelo
               Amarelo -> Vermelho
               Vermelho -> Verde

stop :: Semaforo -> Bool
stop c = case c of 
              Verde -> False
              Amarelo -> False 
              Vermelho -> True

safe :: Semaforo -> Semaforo -> Bool
safe s1 s2 
    | s1 == Vermelho && (s2 == Verde || s2 == Amarelo || s2 == Vermelho) = True 
    | s2 == Vermelho && (s1 == Verde || s1 == Amarelo || s1 == Vermelho) = True 
    | otherwise = False

data Ponto = Cartesiano Double Double | Polar Double Double
             deriving (Show,Eq)

posx :: Ponto -> Double
posx (Cartesiano x _) = x
posx (Polar d a) = d*cos(a)

posy :: Ponto -> Double
posy (Cartesiano _ y) = y
posy (Polar d a) = d*sin(a)

raio :: Ponto -> Double
raio (Polar d _) = d 
raio (Cartesiano x y) = x^2 + y^2

angulo :: Ponto -> Double
angulo (Polar _ a) = a
angulo (Cartesiano x y) = sin(x/y)

dist :: Ponto -> Ponto -> Double
dist (Cartesiano a b) (Cartesiano c d) =  