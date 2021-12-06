module Day06 where

import Data.List (group, sort)
import Data.List.Split (splitOn)

type State = [Int]

main :: IO ()
main = interact (unlines . sequence [part1, part2] . map read . splitOn "," . head . lines)

part1 :: [Int] -> String
part1 = (++) "Part 1: " <$> show . sum . day 80

part2 :: [Int] -> String
part2 = (++) "Part 2: " <$> show . sum . day 256

day :: Int -> [Int] -> State
day n = (!! n) . iterate step . initialize

indices :: [Int]
indices = [0 .. 8]

initialize :: [Int] -> State
initialize = map (pred . length) . group . sort . (++ indices)

step :: State -> State
step s = map count indices
  where
    count 6 = head s + s !! 7
    count n = s !! (succ n `mod` length indices)
