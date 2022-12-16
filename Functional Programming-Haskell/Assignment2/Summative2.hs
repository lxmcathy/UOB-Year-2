-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Summative2 (encodeWord , encodeWords , encodeText ,
                   decodeText ,
                   decodeTextWithTree ,
                   ramify ,
                   tabulate ,
                   tree ,
                   simplify , cf , infFrac , eseq , eApproximate) where

import Types

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------
import Data.List
import Data.Maybe

{- Question 1 -}
encodeWord :: Table -> String -> Code
encodeWord a [] = []
encodeWord a [b] = case ( lookup b a ) of
             Nothing -> []
             Just c -> c

encodeWord a (b:bs) = case ( lookup b a ) of
               Nothing -> []
               Just c -> c ++ shortGap ++ encodeWord a ( bs )


encodeWords :: Table -> [String] -> Code
encodeWords a [] = []
encodeWords a [b] = encodeWord a b
encodeWords a (b:bs) = encodeWord a ( b ) ++ mediumGap ++ encodeWords a ( bs )


helper :: Eq a => [a] -> [a] -> Maybe Int
helper a b = findIndex (isPrefixOf a) (tails b)

split :: Eq a => [a] -> [a] -> [[a]]
split a b = case (helper a b) of
        Nothing -> [b]
        Just c -> [(take c b)] ++ split a (drop (c + (length a)) b)

encodeText :: Table -> String -> Code
encodeText a [] = []
encodeText a b = encodeWords a (split " " b)

{- Question 2 -}
split' :: Eq a => [a] -> [a] -> [[a]]
split' a b = case (helper a b) of
         Nothing -> [b]
         Just c -> [(take c b)] ++ split' a (drop (c + (length a) -1) b)

helper1 :: [Code] -> Table -> String
helper1 [] t = []
helper1 (a:as) t = [head [ x | b <- t, let (x,y) = b, y ==a]] ++ helper1 as t

func :: [Code] -> Table -> String
func [] t = []
func (a:as) t = helper1 (split' [Silence,Silence,Beep] a) t ++ " " ++ func as t

decodeText :: Table -> Code -> String
decodeText t s = func as t
        where as = split' [Silence,Silence,Silence,Silence,Silence,Silence,Beep] s

{- Question 3 -}
matchPrefix :: Tree ->Tree -> Code -> String
matchPrefix Empty root _ = []
matchPrefix (Branch a l r) root []  = case a of
                          Nothing -> []
                          Just a -> [a]
matchPrefix (Branch a l r) root s
      | isPrefixOf dit s = matchPrefix l root (drop (length dit) s)
      | isPrefixOf dah s = matchPrefix r root (drop (length dah) s)
      | otherwise = if isPrefixOf [Silence,Silence,Beep] s
                  then (case a of
                     Nothing -> []
                     Just a -> [a] ++ decodeTextWithTree root (drop (length shortGap) s))
                  else (case a of
                      Nothing -> []
                      Just a -> [a] ++ " " ++ decodeTextWithTree root (drop (length mediumGap) s))


decodeTextWithTree :: Tree -> Code -> String
decodeTextWithTree t c = matchPrefix t t c

{- Question 4 -}
insert' :: Char -> Code -> Tree -> Tree
insert' c s Empty
    | isPrefixOf dit s = Branch Nothing (insert' c (drop (length dit) s) Empty) Empty
    | isPrefixOf dah s = Branch Nothing Empty (insert' c (drop (length dah) s) Empty)
    | otherwise  = Branch (Just c) Empty Empty
insert' c s (Branch a l r)
    | isPrefixOf dit s = Branch a (insert' c (drop (length dit) s) l) r
    | isPrefixOf dah s = Branch a l (insert' c (drop (length dah) s) r)
    | otherwise  = Branch (Just c) l r

loop1 :: Table -> Tree -> Tree
loop1 [] tree = tree
loop1 (a:ax) tree = let (x,y) = a in loop2 ax (insert' x y tree)

loop2 :: Table -> Tree -> Tree
loop2 [] tree = tree
loop2 (a:ax) tree = let (x,y) = a in loop1 ax (insert' x y tree)

ramify :: Table -> Tree
ramify table = loop1 table (Branch Nothing Empty Empty)

{- Question 5 -}
func1 :: Tree -> Code -> Table
func1 Empty code = []
func1 (Branch c l r) code = case c of
                   Nothing -> (func1 l (code ++ dit)) ++ (func1 r (code ++ dah))
                   Just c -> [(c , code)] ++ (func1 l (code ++ dit)) ++ (func1 r (code ++ dah))

tabulate :: Tree -> Table
tabulate tree = func1 tree []

{- Question 6 -}
brackets :: Bracket -> String
brackets (Round ts) = "(" ++ concat [brackets t | t <- ts] ++ ")"
brackets (Curly ts) = "{" ++ concat [brackets t | t <- ts] ++ "}"

tree :: String -> Maybe Bracket
tree = undefined

isWellBracketed :: String -> Bool
isWellBracketed xs = case tree xs of
                      Nothing -> False
                      Just _  -> True

{- Question 7 -}


exprExample :: Expr
exprExample = ((Lit 1) :+: (Lit 2)) :+: OneOver ((Lit 5) :+: OneOver (Lit 3))

simplify :: Expr -> Frac
simplify (Lit n) = n :/: 1
simplify (OneOver expr) = m :/: n where (n :/: m) = (simplify expr)
simplify (expr1 :+: expr2) = q :/: p
    where (b :/: a) = (simplify expr1)
          (d :/: c) = (simplify expr2)
          (n :/: m) = ((b * c + a * d) :/: (a * c))
          g = (gcd m n)
          q = (n `quot` g)
          p = (m `quot` g)

cf :: Integer -> [Integer] -> Expr
cf a [] = Lit a
cf a (b:bs) = (Lit a) :+: OneOver (cf b bs)

infFrac :: Integer -> [Integer] -> Int -> Expr
infFrac n ms = cf n . (flip take) ms

eseq :: [Integer]
eseq = concat [[1, 2 * i, 1] | i <- [1..]]

eApproximate :: Int -> Frac
eApproximate = simplify . infFrac 2 eseq

compute :: Frac -> Double
compute (m :/: n) = (fromIntegral m) / (fromIntegral n)