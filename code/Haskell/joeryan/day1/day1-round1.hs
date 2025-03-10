{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ParallelListComp #-}
import System.IO
import Data.List (sort)

main = do
  -- read each file into a sorted list of integers
  file1contents <- readFile "sample1.txt"
  let firstColLines = sort (convertToInt (lines file1contents))
  file2contents <- readFile "sample2.txt"
  let secondColLines = sort (convertToInt (lines file2contents))
  
  -- calculate the sum of distances using parallel list comprehension
  let result = sum [ calculateDistance x y | x <- firstColLines | y <- secondColLines]

  print result

-- simple function to convert list of strings to integers
convertToInt :: [String] -> [Integer]
convertToInt = map read

-- return absolute distance between two integers
calculateDistance :: Integer -> Integer -> Integer
calculateDistance x y = abs (x - y)

