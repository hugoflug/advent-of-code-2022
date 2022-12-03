{-# LANGUAGE LambdaCase #-}

module AOC2_2(solve) where

import Data.List.Split (splitOn)
import Data.List (sort)

solve :: String -> Int
solve = sum . map (calcScore . wordPair) . lines

wordPair :: String -> (String, String)
wordPair = (\[a, b] -> (a, b)) . words

calcScore :: (String, String) -> Int
calcScore shapes = outcomeScore (snd shapes) + shapeScore shapes

outcomeScore :: String -> Int
outcomeScore shape =
  case shape of
    "X" -> 0
    "Y" -> 3
    "Z" -> 6

shapeScore :: (String, String) -> Int
shapeScore shapes = case shapes of
  ("A", "X") -> 3
  ("A", "Y") -> 1
  ("A", "Z") -> 2
  ("B", "X") -> 1
  ("B", "Y") -> 2
  ("B", "Z") -> 3
  ("C", "X") -> 2
  ("C", "Y") -> 3
  ("C", "Z") -> 1