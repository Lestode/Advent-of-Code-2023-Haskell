{-# LANGUAGE ParallelListComp #-}

import System.Environment   
import Data.List.Split
import Data.Char (isDigit)
import Data.Char(digitToInt)
import Text.Regex.Posix
import Data.Array
import Debug.Trace

openInput :: IO String
openInput = do
  args <- getArgs
  contents <- readFile (head args)
  return contents

makeGrid :: String -> [[Char]]
makeGrid contents = [line | line <- splitOn "\n" contents]

checkGridCharacterIsActive :: [[Char]] -> (Int, Int) -> Bool
checkGridCharacterIsActive grid (row, col) = if row < 0 || col < 0 || row >= length grid || col >= length (grid !! 0) then
    False
else
    let elem = grid !! row !! col in
        (not (isDigit elem)) && (elem /= '.')

isTrue :: Bool -> Bool
isTrue True = True
isTrue False = False

hasActiveCharacter :: [[Char]] -> (Int, Int) -> Bool
hasActiveCharacter grid (row, col) = any isTrue [checkGridCharacterIsActive grid (currentRow, currentCol) | currentCol <- [col-1..col+1], currentRow <- [row-1..row+1]] 

readNumbersOnRow :: Int -> [Char] -> [[Char]] -> [Int]
readNumbersOnRow rowNumber row grid = go (zip [0..] row) 0 False []
    where
        go :: [(Int, Char)] -> Int -> Bool -> [Int] -> [Int]
        go [] accCurrent isValid accNumbers = if isValid then
            [accCurrent] ++ accNumbers
        else
            accNumbers
        go ((colNumber, current):rest) accCurrent isValid accNumbers = if isDigit current then
            go rest (accCurrent * 10 + digitToInt current) (isValid || (hasActiveCharacter grid (rowNumber, colNumber))) accNumbers
        else if isValid then
            go rest 0 False [accCurrent] ++ accNumbers
        else
            go rest 0 False accNumbers

solution grid = sum [sum $ readNumbersOnRow rowNumber row grid | (rowNumber, row) <- zip[0..] grid]

main :: IO ()
main = do
  contents <- openInput
  let grid = makeGrid contents
  print $ solution grid
