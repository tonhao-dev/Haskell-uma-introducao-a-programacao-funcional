module Exericios where
import Data.Char (chr )

-- A
exercicioA :: [Int]
exercicioA = [11^x | x <- [0..6]]

-- B
exercicioB :: [Int]
exercicioB = [1..39]

-- C
exercicioC :: [String]
exercicioC = ["A" ++ [['a'..'z'] !! x] ++ "BB" | x <- [0..6]]

-- E
exercicioE :: Fractional a => [a]
exercicioE = [1.0 / 2.0^n | n <- [0..5]]

exercicioF :: [Int] 
exercicioF = [1 + (n - 1) * 9 | n <- [1..8]]

exercicioG :: [Int]
exercicioG = [x | x <- [2..30], mod x 2 == 0]

exercicioH :: [Char]
exercicioH = [chr  x | x <- [64..76]]

lengthIsOdd :: String -> Bool
lengthIsOdd x = mod (length x) 2 == 0

reverseStringList :: [String] -> [String]
reverseStringList xs = [reverse x | x <- xs]

lengthOfString :: [String] -> [Int]
lengthOfString xs = [length x | x <- xs, mod (length x) 2 /= 0]

myHead :: [Int] -> Int
myHead xs = xs !! 0

isPalindrome :: String -> Bool
isPalindrome xs = xs == reverse xs

sequenceOfNumber :: Int -> (Int, Int, Int, Int)
sequenceOfNumber x = (x*2, x*3, x*4, x*5)