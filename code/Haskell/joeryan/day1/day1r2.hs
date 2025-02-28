{-# LANGUAGE GHC2021 #-}
import System.IO
import System.Environment (getArgs)
import Data.List (sort)

main = do
  -- get file name and read in contents
  args <- getArgs
  let inFile = head args
  file1contents <- readFile inFile
  
  -- create a sorted list of tuples from the file contents
  let colLines = convertLines (lines file1contents)

  -- sum the absolute values of each tuple using list comprehension
  let result = sum [ abs (x - y)| (x,y) <- colLines]
  print result


-- function to take the lines from the file and return an list of sorted tuples
convertLines :: [String] -> [(Integer, Integer)]
convertLines inputList = do
  let firstList = sort [ read x | x:xs <- map words inputList]
  let secondList = sort [ read (head xs) | x:xs <- map words inputList]
  -- return a list of tuples created with an element from each sorted list
  zip firstList secondList

