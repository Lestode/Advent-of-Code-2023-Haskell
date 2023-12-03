{-# LANGUAGE ParallelListComp #-}

import System.Environment   
import Data.List.Split
import Data.Char (isDigit)
import Data.Char(digitToInt)
import Text.Regex.Posix
import Data.Array
import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map as Map

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
        (elem == '*')

isTrue :: Bool -> Bool
isTrue True = True
isTrue False = False

findNearbyStars :: [[Char]] -> (Int, Int) -> Set.Set (Int, Int)
findNearbyStars grid (row, col) = Set.fromList [(currentCol, currentRow) | currentCol <- [col-1..col+1], currentRow <- [row-1..row+1], checkGridCharacterIsActive grid (currentRow, currentCol)] 

addNumberToNearbyStars :: Int -> Set.Set (Int, Int) -> Map.Map (Int, Int) [Int] -> Map.Map (Int, Int) [Int]
addNumberToNearbyStars number starsToNumber accNumbers = foldr (\star acc -> Map.insertWith (++) star [number] acc) accNumbers starsToNumber

readNumbersOnRow :: Int -> [Char] -> [[Char]] -> Map.Map (Int, Int) [Int]
readNumbersOnRow rowNumber row grid = go (zip [0..] row) 0 Set.empty Map.empty
    where
        go :: [(Int, Char)] -> Int -> Set.Set (Int, Int) -> Map.Map (Int, Int) [Int] -> Map.Map (Int, Int) [Int]
        go [] accCurrent nearbyStars starsToNumber = addNumberToNearbyStars accCurrent nearbyStars starsToNumber
        go ((colNumber, current):rest) accCurrent nearbyStars starsToNumber = if isDigit current then
            go rest (accCurrent * 10 + digitToInt current) (Set.union nearbyStars (findNearbyStars grid (rowNumber, colNumber))) starsToNumber
        else
            go rest 0 Set.empty (addNumberToNearbyStars accCurrent nearbyStars starsToNumber)

getStarMap grid = Map.unionsWith (++) [readNumbersOnRow rowNumber row grid | (rowNumber, row) <- zip[0..] grid]

main :: IO ()
main = do
  contents <- openInput
  let grid = makeGrid contents
  let starMap = getStarMap grid
  print $ sum $ Map.elems $ Map.map (\numbers -> (numbers !! 0) * (numbers !! 1)) $ Map.filter (\numbers -> length numbers == 2) starMap
