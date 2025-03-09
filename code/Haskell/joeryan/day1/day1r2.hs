{-# LANGUAGE GHC2021 #-}
import System.IO
import System.Environment (getArgs)
import Data.List (nub, sort)

main = do
  -- get file name and read in contents
  args <- getArgs
  let inFile = head args
  file1contents <- readFile inFile
  
  -- create a sorted list of tuples from the file contents
  let colLines = convertLines (lines file1contents)
  -- sum the absolute values of each tuple using list comprehension
  let result = sum [ abs (x - y)| (x,y) <- colLines]
  print "sum for part 1"
  print result

  -- create two lists of integers from the file contents
  let unzipList = unzip colLines
  -- create zero value map from firstList
  let firstList = getFirst unzipList
  -- print ("map: " ++ show mapList)
  
  -- for each entry in secondList, if in first list then add to result
  let resultList = [x | x <- getSecond unzipList, x `elem` firstList  ]
  print resultList
  -- take multiply each k,v and add to sum 
  
  -- print similarity

-- function to take the lines from the file and return an list of sorted tuples
convertLines :: [String] -> [(Integer, Integer)]
convertLines inputList = do
  let firstList = Data.List.sort [ read x | x:xs <- map words inputList]
  let secondList = Data.List.sort [ read (head xs) | x:xs <- map words inputList]
  -- return a list of tuples created with an element from each sorted list
  zip firstList secondList

getFirst :: ([Integer], [Integer]) -> [Integer]
getFirst (x, _) = x

getSecond :: ([Integer], [Integer]) -> [Integer]
getSecond (x, _) = x
