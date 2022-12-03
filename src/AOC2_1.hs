{-# LANGUAGE LambdaCase #-}

module AOC2_1(solve) where

import Data.List.Split (splitOn)
import Data.List (sort)

solve :: String -> Int
solve = sum . map (calcScore . wordPair) . lines

wordPair :: String -> (String, String)
wordPair = (\[a, b] -> (a, b)) . words

calcScore :: (String, String) -> Int
calcScore shapes = outcomeScore shapes + shapeScore (snd shapes)

outcomeScore :: (String, String) -> Int
outcomeScore shapes =
  case shapes of
    ("A", "X") -> 3
    ("A", "Y") -> 6
    ("A", "Z") -> 0
    ("B", "X") -> 0
    ("B", "Y") -> 3
    ("B", "Z") -> 6
    ("C", "X") -> 6
    ("C", "Y") -> 0
    ("C", "Z") -> 3

shapeScore :: String -> Int
shapeScore shape =
  case shape of
    "X" -> 1
    "Y" -> 2
    "Z" -> 3