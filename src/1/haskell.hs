{-# LANGUAGE ParallelListComp #-}

import System.Environment   
import Data.List.Split
import Data.Char (isDigit)
import Data.Char(digitToInt)

openInput :: IO String
openInput = do
  args <- getArgs
  contents <- readFile (head args)
  return contents

firstAndLastToInt :: [Char] -> Int
firstAndLastToInt [] = 0
firstAndLastToInt [x] =  (10 * digitToInt x) + (digitToInt x)
firstAndLastToInt (x:xs) = (10 * digitToInt x) + (digitToInt (last xs))

solutionPart1 :: String -> Int
solutionPart1 contents = sum [firstAndLastToInt (filter isDigit lines) | lines <- splitOn "\n" contents]

main :: IO ()
main = do
  contents <- openInput
  print (solutionPart1 contents)
