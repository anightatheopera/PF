module Ficha4 where
import Data.Char
import Data.List

digitAlpha :: String -> (String,String)
digitAlpha x = ([xs | xs <- x, isAlpha xs], [xs | xs <- x, isDigit xs])

nzp :: [Int] -> (Int,Int,Int)
nzp x = (length [x | xs <- x, xs<0],length [x | xs <- x, xs==0], length [x | xs <- x, xs>0])

accFoo :: (Int, Int, Int) -> [Int] -> (Int, Int, Int)
accFoo (a,b,c) (h:t) 
    | h < 0 = accFoo (a+1,b,c) t 
    | h == 0 = accFoo (a, b+1,c) t 
    | h > 0 = accFoo (a,b,c+1) t
    | otherwise = accFoo (a,b,c) t
accFoo acc [] = acc

