{-# LANGUAGE LambdaCase #-}

module AOC3_1 where

import Data.Char (ord)
import Data.List.Split (splitOn)
import Data.List (intersect)

solve :: String -> Int
solve = sum . map (priority . head . uncurry intersect . splitInHalf) . lines

priority :: Char -> Int
priority c =
  if c >= 'a' && c <= 'z' then
    ord c - 96
  else
    ord c - 65 + 27

splitInHalf :: [a] -> ([a], [a])
splitInHalf l = let half = length l `div` 2 in
  (take half l, drop half l)