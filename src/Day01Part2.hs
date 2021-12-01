module Day01Part2
  ( solve,
  )
where

readInt :: String -> Int
readInt = read

readData = fmap readInt . lines

windowed :: Int -> [a] -> [[a]]
windowed n xs
  | length xs < n = []
  | otherwise = take n xs : windowed n (tail xs)

solve input = length $ filter (> 0) $ uncurry (-) <$> pairs
  where
    nums = readData input
    windowSums = sum <$> windowed 3 nums
    pairs = tail windowSums `zip` windowSums
