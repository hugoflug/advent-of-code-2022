module AOC1_2(solve) where

import Data.List.Split (splitOn)
import Data.List (sort)

solve :: String -> Int
solve = sum . take 3 . reverse . sort . map (sum . map read) . splitOn [""] . lines