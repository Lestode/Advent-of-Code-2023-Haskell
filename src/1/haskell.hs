{-# LANGUAGE ParallelListComp #-}

import System.Environment   
import Data.List.Split
import Data.Char (isDigit)
import Data.Char(digitToInt)
import Text.Regex.Posix
import Data.Array

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

numbersStrToInt :: String -> Int
numbersStrToInt "one" = 1
numbersStrToInt "two" = 2
numbersStrToInt "three" = 3
numbersStrToInt "four" = 4
numbersStrToInt "five" = 5
numbersStrToInt "six" = 6
numbersStrToInt "seven" = 7
numbersStrToInt "eight" = 8
numbersStrToInt "nine" = 9
numbersStrToInt [c] = digitToInt c
numbersStrToInt _ = 0

extractNumber :: [String] -> Int
extractNumber [] = 0
extractNumber [x] =  (10 * numbersStrToInt x) + (numbersStrToInt x)
extractNumber (x:xs) = (10 * numbersStrToInt x) + (numbersStrToInt (last xs))


findAllNumbersOverlapping :: String -> [String]
findAllNumbersOverlapping input = go input []
  where
    go :: String -> [String] -> [String]
    pattern = "(one|two|three|four|five|six|seven|eight|nine)|[0-9]"
    go [] acc = acc
    go s acc = 
      case s =~ pattern :: (String, String, String) of
        (nomatch, [], []) -> go [] acc
        (_before, [char], after) -> go after (acc ++ [[char]])
        (_before, (x:xs), after) -> go (xs ++ after) (acc ++ [[x] ++ xs])

solutionPart2 contents = sum [extractNumber $ findAllNumbersOverlapping lines | lines <- splitOn "\n" contents]

main :: IO ()
main = do
  contents <- openInput
  print (solutionPart2 contents)
