{-# LANGUAGE LambdaCase #-}

module AOC3_2 where

import Data.Char (ord)
import Data.List.Split (chunksOf)
import Data.List (intersect, foldl1')

solve :: String -> Int
solve = sum . map (priority . head . foldl1' intersect) . chunksOf 3 . lines

priority :: Char -> Int
priority c =
  if c >= 'a' && c <= 'z' then
    ord c - 96
  else
    ord c - 65 + 27
