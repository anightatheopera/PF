module Ficha3 where
import Data.Char
import Data.List

data Contacto = Casa Integer
                | Trab Integer
                | Tlm Integer
                | Email String
                deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email ((x,y):t) 
    | nome == x = (nome, y ++ [Email email]) : t
    | otherwise = acrescEmail nome email t
acrescEmail n e [] = [(n, [Email e])]

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails n [] = Nothing
verEmails nome ((x,y):t)
    | nome == x = Just (filterEmails y)
    | otherwise = verEmails nome t 

filterEmails :: [Contacto] -> [String]
filterEmails [] = []
filterEmails (Email m : ms) = m : filterEmails ms
filterEmails (_:cs) = filterEmails cs

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs c = filterPhones c 

filterPhones :: [Contacto] -> [Integer]
filterPhones (Trab c:cs) = c : filterPhones cs 
filterPhones (Tlm c:cs) = c : filterPhones cs 
filterPhones (Casa c:cs) = c : filterPhones cs 
filterPhones (_:cs) = filterPhones cs
filterPhones [] = []

casa :: Nome -> Agenda -> Maybe Integer
casa _ [] = Nothing
casa nome ((x,c):t)
    | nome == x = Just (filterCasa c)
    | otherwise = casa nome t


filterCasa :: [Contacto] -> Integer
filterCasa (Casa c: _) = c
filterCasa (_:cs) = filterCasa cs 
filterCasa [] = 0 

type Dia = Int
type Mes = Int
type Ano = Int

data Data = D Dia Mes Ano
    deriving Show

type TabDN = [(Nome,Data)]

procura :: Nome -> TabDN -> Maybe Data
procura _ [] = Nothing
procura nome ((x,y):s) 
    | nome == x = Just y
    | otherwise = procura nome s

idade :: Data -> Nome -> TabDN -> Maybe Int
idade _ _ [] = Nothing
idade date@(D _ _ a) nome ((x,D _ _ a1):s)
    | nome == x = Just (a-a1)
    | otherwise = idade date nome s 

anterior :: Data -> Data -> Bool
anterior (D d1 m1 a1) (D d2 m2 a2)
    | a1 > a2 = False
    | a1 == a2 && m1 > m2 = False
    | a1 == a2 && m1 == m2 && d1 > d2 = False
    | otherwise = True

ordena :: TabDN -> TabDN
ordena ((a,b):(c,d):t)
    | anterior b d = (c,d) : (a,b) : ordena t
    | otherwise = (a,b) : (c,d) : ordena t
ordena [] = []
    
data Movimento = Credito Float | Debito Float
    deriving Show
    
data Extracto = Ext Float [(Data, String, Movimento)]
    deriving Show