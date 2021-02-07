module Ficha5 where
import Data.Char
import Data.List

any2 :: (a -> Bool) -> [a] -> Bool
any2 _ [] = False
any2 f (h:t) 
    | f h  = True
    | otherwise = any2 f t

zipWithh :: (a->b->c) -> [a] -> [b] -> [c]
zipWithh f (a:b) (c:d) = (f a c) : zipWithh f b d 
zipWithh _ _ _ = []

takeWhilee :: (a->Bool) -> [a] -> [a]
takeWhilee _ [] = []
takeWhilee f (h:t) 
    | f h = h : takeWhilee f t
    | otherwise = []

dropWhilee :: (a->Bool) -> [a] -> [a]
dropWhilee _ [] = []
dropWhilee f (h:t) 
    | f h = dropWhilee f t
    | otherwise = (h:t)

spany :: (a-> Bool) -> [a] -> ([a],[a])
spany _ [] = ([],[])
spany f (h:t)
    | f h = (h:s1, s2)
    | otherwise = ([], h:t)
    where (s1,s2) = spany f t

deleteByy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteByy f n (h:t)
    | f n h = t
    | otherwise = h : deleteByy f n t
deleteByy _ _ [] = []

type Mat a = [[a]]

dimOK :: Mat a -> Bool
dimOK [] = True
dimOK [_] = True
dimOK (a:b:c) 
    | (length a) == (length b) = dimOK (b:c)
    | otherwise = False 

dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat a@(m:_) = (length m, length a)

transpose :: Mat a -> Mat a
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

