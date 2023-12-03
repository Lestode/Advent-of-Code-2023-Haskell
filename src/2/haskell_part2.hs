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

gameColorsPower :: [GameColors] -> Int
gameColorsPower [] = 0
gameColorsPower [last] = (red last) * (green last) * (blue last)
gameColorsPower (current:rest) = go current rest
    where
        go :: GameColors -> [GameColors] -> Int
        go acc (current:rest) = go 
                            (acc { red = max (red acc) (red current), 
                                green = max (green acc) (green current), 
                                blue = max (blue acc) (blue current) })
                            rest
        go acc [] = gameColorsPower [acc]

-- solutionPart1 :: String -> Int
solutionPart2 contents = sum [gameColorsPower $ extractCurrentGame $ getGamesOnLine $ removeGameNumber line | line <- splitOn "\n" contents]

main :: IO ()
main = do
  contents <- openInput
  print (solutionPart2 contents)
