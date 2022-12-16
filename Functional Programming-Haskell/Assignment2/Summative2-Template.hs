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
{- Question 1 -}
encodeWord :: Table -> String -> Code
encodeWord a []  = []
encodeWord a [b] = case (lookup b a) of
                        Nothing -> []
                        Just c -> c 
encodeWord a (b:bx) = case (lookup b a) of
                        Nothing -> []
                        Just c -> c ++ shortGap ++ (encodeWord a bx)



encodeWords :: Table -> [String] -> Code
encodeWords a []  = []
encodeWords a [b] = encodeWord a b
encodeWords a (b:bx) = encodeWord a b ++ mediumGap ++ encodeWords a bx
            
split :: Eq a => [a] -> [a] -> [[a]]
split a b = case (helper a b) of
             Nothing -> [b]
             Just c -> [(take c b)] ++ split a (drop (c + (length a)) b)
             where 
                 helper :: Eq a =>[a] -> [a] -> Maybe Int
                 helper a b = findIndex (isPrefixOf a) (tails b)
             




encodeText :: Table -> String -> Code
encodeText a b = encodeWords a (split " " b)

{- Question 2 -}

split' :: Eq a => [a] -> [a] -> [[a]]
split' a b = case (helper a b) of
             Nothing -> [b]
             Just c -> [(take c b)] ++ split' a (drop (c + (length a) - 1) b)
             where 
                 helper :: Eq a =>[a] -> [a] -> Maybe Int
                 helper a b = findIndex (isPrefixOf a) (tails b)


func1 :: [Code] -> Table -> String
func1 [] t = []
func1 (a:ax) t = fhelper (split' [Silence,Silence,Beep] a) t ++ " " ++ func1 ax t


fhelper :: [Code] -> Table -> String
fhelper [] t = []
fhelper (a:ax) t = [head [ x | b <- t, let (x,y) = b, y == a]] ++ fhelper ax t


decodeText :: Table -> Code -> String
decodeText t s = func1 as t
    where
        as = split' [Silence,Silence,Silence,Silence,Silence,Silence,Beep] s
        


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
ramify :: Table -> Tree
ramify table = loop1 table (Branch Nothing Empty Empty)

loop1 :: Table -> Tree -> Tree
loop1 [] tree = tree
loop1 (a:ax) tree = let (x,y) = a in loop2 ax (insert' x y tree)
                     

loop2 :: Table -> Tree -> Tree
loop2 [] tree = tree
loop2 (a:ax) tree = let (x,y) = a in loop1 ax (insert' x y tree)
      
insert' :: Char -> Code -> Tree -> Tree
insert' c s Empty 
    | isPrefixOf dit s = Branch Nothing (insert' c (drop (length dit) s) Empty) Empty
    | isPrefixOf dah s = Branch Nothing Empty (insert' c (drop (length dah) s) Empty)
    | otherwise  = Branch (Just c) Empty Empty
insert' c s (Branch a l r) 
    | isPrefixOf dit s = Branch a (insert' c (drop (length dit) s) l) r
    | isPrefixOf dah s = Branch a l (insert' c (drop (length dah) s) r)
    | otherwise  = Branch (Just c) l r


{- Question 5 -}
tabulate :: Tree -> Table
tabulate tree = func2 tree []

func2 :: Tree -> Code -> Table
func2 Empty code = []
func2 (Branch c l r) code = case c of 
                             Nothing -> (func2 l (code ++ dit)) ++ (func2 r (code ++ dah)) 
                             Just c -> [(c , code)] ++ (func2 l (code ++ dit)) ++ (func2 r (code ++ dah))

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
simplify :: Expr -> Frac
simplify = undefined

cf :: Integer -> [Integer] -> Expr
cf = undefined

infFrac :: Integer -> [Integer] -> Int -> Expr
infFrac = undefined

eseq :: [Integer]
eseq = undefined

eApproximate :: Int -> Frac
eApproximate = undefined

compute :: Frac -> Double
compute (m :/: n) = (fromIntegral m) / (fromIntegral n)
