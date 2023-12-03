{-# LANGUAGE ParallelListComp #-}

import System.Environment   
import Data.List.Split
import Data.Char (isDigit)
import Data.Char(digitToInt)
import Text.Regex.Posix
import Data.Array

maxRed = 12
maxGreen = 13
maxBlue = 14

openInput :: IO String
openInput = do
  args <- getArgs
  contents <- readFile (head args)
  return contents

removeGameNumber :: String -> Maybe String
removeGameNumber line = case line =~ pattern :: (String, String, String) of
    (nomatch, [], []) -> Nothing
    (_before, matched, after) -> Just $ matched ++ after
    where
        pattern = "Game [0-9]+: "

data GameColors = GameColors {
    red :: Int,
    green :: Int,
    blue :: Int
} deriving (Show)

addRed :: GameColors -> Int -> GameColors
addRed gameColors value = gameColors { red = value + red gameColors }

addGreen :: GameColors -> Int -> GameColors
addGreen gameColors value = gameColors { green = value + green gameColors }

addBlue :: GameColors -> Int -> GameColors
addBlue gameColors value = gameColors { blue = value + blue gameColors }

getGamesOnLine :: Maybe String -> [String]
getGamesOnLine line = case line of
    Nothing -> []
    Just line -> splitOn ";" line

extractCurrentGame :: [String] -> [GameColors]
extractCurrentGame [] = [GameColors 0 0 0]
extractCurrentGame games = map getOneGameColor games

colorStrsToGameColors :: [String] -> GameColors
colorStrsToGameColors [] = GameColors 0 0 0
colorStrsToGameColors strings = go (GameColors 0 0 0) strings
    where
        go :: GameColors -> [String] -> GameColors
        go gameColors [] = gameColors
        go gameColors (current:rest) = case current =~ pattern :: (String, String, String) of
            (nomatch, [], []) -> go gameColors rest
            (_before, number, "red") -> go (addRed gameColors (read number :: Int)) rest
            (_before, number, "green") -> go (addGreen gameColors (read number :: Int)) rest
            (_before, number, "blue") -> go (addBlue gameColors (read number :: Int)) rest
            (_before, number, any) -> go gameColors rest
        pattern = "[0-9]+ "

getOneGameColor :: String -> GameColors
getOneGameColor gameStr = colorStrsToGameColors $ splitOn "," gameStr

isValidGame :: Int -> [GameColors] -> Int
isValidGame id [] = id
isValidGame id (current:rest) = if (red current <= maxRed) && 
                                    (green current <= maxGreen) && 
                                    (blue current <= maxBlue)
                                then isValidGame id rest
                                else 0

-- solutionPart1 :: String -> Int
solutionPart1 contents = sum [isValidGame id $ extractCurrentGame $ getGamesOnLine $ removeGameNumber line | (id, line) <- zip [1..] (splitOn "\n" contents)]

main :: IO ()
main = do
  contents <- openInput
  print (solutionPart1 contents)
