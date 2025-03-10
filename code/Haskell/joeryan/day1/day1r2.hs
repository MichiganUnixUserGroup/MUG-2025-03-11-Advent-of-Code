{-# LANGUAGE GHC2021 #-}
import System.IO
import System.Environment (getArgs)
import Data.List (nub, sort)
import Data.Map as Map

main = do
  -- get file name and read in contents
  args <- getArgs
  let inFile = head args
  file1contents <- readFile inFile
  
  -- create a sorted list of tuples from the file contents
  let colLines = convertLines (lines file1contents)
  -- sum the absolute values of each tuple using list comprehension
  let result = sum [ abs (x - y)| (x,y) <- colLines]
  print ("Sum for part 1: " ++ show result)
  

  -- create two lists of integers from the file contents
  let unzipList = unzip colLines
  -- create zero value map from firstList
  let firstList = getFirstList (unzip colLines)
  let secondList = getSecondList (unzip colLines)
  -- print ("List: " ++ show firstList ++ show secondList)
    
  -- multiply each element by the count of occurrances in secondList add to sum 
  let similarity = sum [ x * (length (Prelude.filter (\target -> target == x) secondList )) | x <- firstList]
  print ("Similarity: " ++ show similarity)


-- function to take the lines from the file and return an list of sorted tuples
convertLines :: [String] -> [(Int, Int)]
convertLines inputList = do
  let firstList = Data.List.sort [ read x | x:xs <- Prelude.map words inputList]
  let secondList = Data.List.sort [ read (head xs) | x:xs <- Prelude.map words inputList]
  -- return a list of tuples created with an element from each sorted list
  zip firstList secondList

getFirstList :: ([Int], [Int]) -> [Int]
getFirstList (x, _) = x

getSecondList :: ([Int], [Int]) -> [Int]
getSecondList (_, x) = x

