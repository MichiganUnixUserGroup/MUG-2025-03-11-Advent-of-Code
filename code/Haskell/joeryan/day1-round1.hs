{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ParallelListComp #-}
import System.IO
import Data.List (sort)

main = do
  file1contents <- readFile "sample1.txt"
  let firstColLines = sort (convertToInt (lines file1contents))
  file2contents <- readFile "sample2.txt"
  let secondColLines = sort (convertToInt (lines file2contents))

  let result = sum [ calculateDistance x y | x <- firstColLines | y <- secondColLines]

  print result

convertToInt :: [String] -> [Integer]
convertToInt = map read

calculateDistance :: Integer -> Integer -> Integer
calculateDistance x y = abs (x - y)

