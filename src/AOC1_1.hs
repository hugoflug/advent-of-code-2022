module AOC1_1(solve) where

import Data.List.Split (splitOn)

solve :: String -> Int
solve = maximum . map (sum . map read) . splitOn [""] . lines