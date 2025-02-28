{-# LANGUAGE GHC2021 #-}
import System.IO
import System.Environment (getArgs)
import Data.List (sort)

main = do
  args <- getArgs

  let inFile = head args
  file1contents <- readFile inFile
  let colLines = convertLines (lines file1contents)

  let result = sum [ abs (x - y)| (x,y) <- colLines]

  print result

convertLines :: [String] -> [(Integer, Integer)]
convertLines inputList = do
  let firstList = sort [ read x | x:xs <- map words inputList]
  let secondList = sort [ read (unwords xs) | x:xs <- map words inputList]
  zip firstList secondList

