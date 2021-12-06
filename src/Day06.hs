{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day06 where

import Data.List.Split (splitOn)

main :: IO ()
main = interact (unlines . sequence [part1, part2] . map read . splitOn ",")

part1, part2 :: [Int] -> String
part1 = ("Part 1: " ++) . show . day 80
part2 = ("Part 2: " ++) . show . day 256

day :: Int -> [Int] -> Int
day n = (!! n) . map sum . iterate step . counts
  where
    counts xs = [length $ filter (== i) xs | i <- [0 .. 8]]
    step [i0, i1, i2, i3, i4, i5, i6, i7, i8] =
      [i1, i2, i3, i4, i5, i6, i7 + i0, i8, i0]
