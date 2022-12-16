-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Summative3 (experiments ,
                   gameAnnotated ,
                   game ,
                   odds ,
                   oneOf ,
                   noun , verb , pronoun , properNoun , determiner , preposition ,
                   nominal ,
                   np ,
                   vp ,
                   pp ,
                   sent) where

import Types
import Parsing
import Data.Char

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------
import Data.List
{- Monads and a game of chance -}

-- An example you can use for testing
headsOrTails :: ChanceMonad m => m Outcome
headsOrTails = do
  c <- toss
  if c == H then return Win else return Lose


{- Exercise 1.1 -}
experiments :: ChanceMonad m => m a -> Integer -> m [a]
experiments xm 1 = 
          xm >>= (\x ->
          return([x]))
experiments xm y = 
          xm >>= (\x ->
          experiments xm (y-1) >>= (\y ->
          return([x]++y)))
                                      

{- Exercise 1.2 -}

helper :: ChanceMonad m => Int -> m ([Coin],Int)
helper 1 = do
  c <- toss
  if c == H then return([c],1) else return([c],0)
helper a  = do
  c <- toss
  (list,a) <- helper (a-1)
  if c == H then return(([c]++list),a+1) else return(([c]++list,a))

calc :: Die -> Int -> Outcome
calc D1 n = if 1 >= n then Win else Lose
calc D2 n = if 2 >= n then Win else Lose
calc D3 n = if 3 >= n then Win else Lose
calc D4 n = if 4 >= n then Win else Lose
calc D5 n = if 5 >= n then Win else Lose
calc D6 n = if 6 >= n then Win else Lose

gameAnnotated :: ChanceMonad m => m ([Coin],Die,Outcome)
gameAnnotated = do
          (list,a) <- helper 6
          die <- roll
          return((list,die,(calc die a)))

{- Exercise 1.3 -}
game :: ChanceMonad m => m Outcome
game = do
  (list,a) <- helper 6
  die <- roll
  return(calc die a)

{- Exercise 1.4 -}

support :: [Outcome] -> Float
support [] = 0
support (a:ax) = if a == Win then ((support ax) + 1) else support ax

length' :: [Outcome] -> Float
length' [] = 0
length' (a:ax) = 1 + length' ax


odds :: [Outcome] -> Float
odds result = (support result) / (length' result)



{- Parsing English -}


parseTest :: Parser Tree -> String -> Maybe Tree
parseTest p s = case (parse p s) of
                  [(t,"")] -> Just t
                  _        -> Nothing

{- Exercise 2.1 -}

find' :: String -> [String] -> String
find' a [] = ""
find' a (b:bx) = if (isPrefixOf b a) then b else find' a bx

oneOf :: [String] -> Parser String
oneOf a = P (\inp -> case inp of
                            []     -> []
                            x      -> do
                                         let result = find' inp a
                                         if result == "" then [] else [(result,(drop (length result) x))])

{- Exercise 2.2 -}
noun :: Parser Tree
noun = P (\inp -> case inp of
          []     -> []
          x      -> do
               let result = find' inp nouns
               if result == "" then [] else [(Leaf Noun result,(drop (length result) x))])
verb :: Parser Tree
verb = P (\inp -> case inp of
  []     -> []
  x      -> do
       let result = find' inp verbs
       if result == "" then [] else [(Leaf Verb result,(drop (length result) x))])
pronoun :: Parser Tree
pronoun = P (\inp -> case inp of
  []     -> []
  x      -> do
       let result = find' inp pronouns
       if result == "" then [] else [(Leaf Pronoun result,(drop (length result) x))])
properNoun :: Parser Tree
properNoun = P (\inp -> case inp of
  []     -> []
  x      -> do
       let result = find' inp properNouns
       if result == "" then [] else [(Leaf ProperNoun result,(drop (length result) x))])
determiner :: Parser Tree
determiner = P (\inp -> case inp of
  []     -> []
  x      -> do
       let result = find' inp determiners
       if result == "" then [] else [(Leaf Determiner result,(drop (length result) x))])
preposition :: Parser Tree
preposition = P (\inp -> case inp of
  []     -> []
  x      -> do
       let result = find' inp prepositions
       if result == "" then [] else [(Leaf Preposition result,(drop (length result) x))])

{- Exercise 2.3 -}

-- branch :: Sort -> [Parser Tree] -> Parser Tree
-- branch a [] = P (\inp -> case inp of
--                 []     -> []
--                 x      -> do
--                 let result = find' inp properNouns
--                 if result == "" then [] else [(Branch Nominal b,(drop (length result) x))])

branch :: Sort -> Parser Tree
branch a = P (\inp -> case inp of
                    []     -> []
                    x      -> do
                    let rst = parse noun x
                    if rst == [] then [] else (do
                                              let (m,n) = head rst
                                              let temp = parse space' n
                                              if temp == [] then [(Branch a [m],n)] else (do 
                                                                                        let (x,y) = head temp
                                                                                        let result = parse noun y
                                                                                        if result == [] then [(Branch a [m],n)] else ( do
                                                                                                                                        let (p,q) = head (parse (branch a) y)
                                                                                                                                        [(Branch a ([m]++[p]),q)]))))

nominal :: Parser Tree
nominal = P (\inp -> case inp of
          []     -> []
          x      -> do
          parse (branch Nominal) inp)
          


space' :: Parser ()
space' = do some (sat isSpace)
            return ()

{- Exercise 2.4 -}

select :: [Parser Tree] -> Parser Tree
select [] = P (\inp -> case inp of
              []     -> []
              x      -> do
              [])
select (a:as) = P (\inp -> case inp of
                  []     -> []
                  x      -> do
                  let rst = parse a x
                  if rst == [] then parse (select as) x else rst)

-- getLabel :: Parser Tree -> Sort
-- getLabel 

branch' :: Sort -> [Parser Tree] -> Parser [Tree]
branch' a list = P (\inp -> case inp of
                    []     -> []
                    x      -> do
                    let rst = parse (select list) x
                    if rst == [] then [] else (do
                                              let (m,n) = head rst
                                              let temp = parse space' n
                                              if temp == [] then [([m],n)] else (do 
                                                                                        let (x,y) = head temp
                                                                                        if parse (select list) y == [] then [([m],n)] else ( do
                                                                                                                                        let (p,q) = head (parse (branch' a list) y)
                                                                                                                                        [(([m]++p),q)]))))
                                                                                                                                        
np :: Parser Tree
np = P (\inp -> case inp of
        []     -> []
        x      -> do
        let rst = parse (branch' NP [pronoun,properNoun,determiner,nominal]) inp
        if rst == [] then [] else (do
                                    let (m,n) = head rst
                                    [(Branch NP m,n)]))
{- Exercise 2.5 -}
vp :: Parser Tree
vp = P (\inp -> case inp of
  []     -> []
  x      -> do
  let rst = parse (branch' VP [verb,np,np,pp]) inp
  if rst == [] then [] else (do
                              let (m,n) = head rst
                              [(Branch VP m,n)]))

{- Exercise 2.6 -}
pp :: Parser Tree
pp = P (\inp -> case inp of
  []     -> []
  x      -> do
  let rst = parse (branch' PP [preposition,np]) inp
  if rst == [] then [] else (do
                              let (m,n) = head rst
                              [(Branch PP m,n)]))


{- Exercise 2.7 -}
sent :: Parser Tree
sent = P (\inp -> case inp of
  []     -> []
  x      -> do
  let rst = parse (branch' Sentence [np,vp]) inp
  if rst == [] then [] else (do
                              let (m,n) = head rst
                              if length n > 0 then [] else [(Branch Sentence m,n)]))

parseSentence :: String -> Maybe Tree
parseSentence = parseTest sent
