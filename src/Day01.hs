module Day01 where

import Data.List (tails)

main :: IO ()
main = interact (unlines . sequence [part1, part2] . map read . lines)

part1 :: [Int] -> String
part1 = (++) "Part 1: " <$> show . countIncreasing

part2 :: [Int] -> String
part2 = (++) "Part 2: " <$> show . countIncreasing . map (sum . take 3) . tails

countIncreasing :: [Int] -> Int
countIncreasing l = length $ filter id $ zipWith (>) (tail l) l
