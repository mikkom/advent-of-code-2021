module Day07 where

import Data.List.Split (splitOn)

main :: IO ()
main = interact (unlines . sequence [part1, part2] . map read . splitOn ",")

part1, part2 :: [Int] -> String
part1 = ("Part 1: " ++) . show . minFuel id
part2 = ("Part 2: " ++) . show . minFuel fuelCost
  where
    fuelCost x = x * (x + 1) `div` 2

minFuel :: (Int -> Int) -> [Int] -> Int
minFuel fuelCost xs = minimum $ map fuelConsumption [minimum xs .. maximum xs]
  where
    fuelConsumption pos = sum $ map (fuelCost . abs . (-) pos) xs
