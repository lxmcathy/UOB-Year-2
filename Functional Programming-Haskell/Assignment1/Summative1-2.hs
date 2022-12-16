module Summative1 (catYrs , golfScorer , majority , luhn , bankCardNumbers , encrypt) where
import Data.Char

catYrs :: Integer -> Integer
catYrs  x | x <= 0 = 0
      | x == 1 = 15
      | x == 2 = 24
      | x >= 3 = (24 + 4 * (x - 2))



golfScorer :: Integer -> Integer -> Integer
golfScorer p s | ((p-s) <=(-2)) && s /= 1 = 0
          | ((p-s) ==(-1)) && s /= 1 = 1
          | ((p-s) == 0) && s /= 1 = 2
          | ((p-s) == 1) && s /= 1 = 3
          | ((p-s) >= 2) && s /= 1 = 4
          | s==1 = 5



majority :: (Bool,Bool,Bool) -> Bool
majority (a,b,c) | a == True && b == True && c == False = True
           | a == True && b == False && c == True = True
           | a == False && b == True && c == True = True
           | a == True && b == True && c == True = True
           | otherwise = False



luhnDouble :: Int -> Int
luhnDouble x | x<5 = x*2
         | otherwise = x*2-9

sumOdd :: [Int] -> Int
sumOdd [] = 0
sumOdd [x] = x
sumOdd (a:b:bx) = a + luhnDouble(b) + (sumOdd bx)

sumEven :: [Int] -> Int
sumEven [] = 0
sumEven [x] = x
sumEven (a:b:bx) = luhnDouble(a) + b + (sumEven bx)

luhn :: [Int] -> Bool
luhn [] = False
luhn a = if ((length a)`mod`2 == 0)
      then (if ((sumEven a)`mod`10 == 0)
          then True
          else False)
      else (if ((sumOdd a)`mod`10 ==0)
          then True
          else False)



cal :: [Int] -> [[Int]]
cal a = [ b | n <- [0..9], let b= (a ++ [n])]

helper :: [[Int]] -> [[Int]]
helper [] = []
helper [a] = cal a
helper (a:ax) = (cal a) ++ (helper ax)

list :: Int -> [[Int]]
list 0 = []
list 1 = [[0],[1],[2],[3],[4],[5],[6],[7],[8],[9]]
list n = helper (list (n-1))

bankCardNumbers :: Int -> [[Int]]
bankCardNumbers n = [ result | result <- list n , luhn result]



let2int :: Char -> Int
let2int c = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr(n + ord 'A' )

shift :: Char -> Char -> Char
shift n c | isUpper c = int2let((let2int n + let2int c)`mod`26)
       | otherwise = c

encrypt :: String -> String -> String
encrypt keyword message = zipWith(shift) rp message
    where rp = cycle keyword

