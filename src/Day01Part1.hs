module Day01Part1 where

readInt :: String -> Int
readInt = read

readData = fmap readInt . lines

solve input = length $ filter (> 0) $ uncurry (-) <$> pairs
  where
    nums = readData input
    pairs = tail nums `zip` nums
