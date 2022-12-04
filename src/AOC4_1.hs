module AOC4_1 where

import Data.List.Split (splitOn)
import Data.Bifunctor (Bifunctor, bimap)

solve :: String -> Int
solve = length . filter overlap . map parseLine . lines

both :: Bifunctor p => (a -> b) -> p a a -> p b b
both f = bimap f f

splitIn2 :: Eq a => [a] -> [a] -> ([a], [a])
splitIn2 splitter input = case splitOn splitter input of [a, b] -> (a, b)

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine = both (both read . splitIn2 "-") . splitIn2 ","

overlap :: ((Int, Int), (Int, Int)) -> Bool
overlap ((a, b), (c, d)) = 
  a >= c && b <= d || c >= a && d <= b