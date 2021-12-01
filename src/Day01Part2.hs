module Day01Part2 where

import Data.List

readInt :: String -> Int
readInt = read

readData = fmap readInt . lines

windowed n xs = unfoldr f xs
  where
    f xs
      | length xs < n = Nothing
      | otherwise = Just (take n xs, tail xs)

solve input = length $ filter (> 0) $ uncurry (-) <$> pairs
  where
    nums = readData input
    windowSums = sum <$> windowed 3 nums
    pairs = tail windowSums `zip` windowSums
