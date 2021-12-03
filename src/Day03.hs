module Day03 where

import Data.List (foldl')

type Binary = [Int]

main :: IO ()
main = interact (unlines . sequence [part1, part2] . map parse . lines)

part1 :: [Binary] -> String
part1 = (++) "Part 1: " <$> show . powerConsumption

part2 :: [Binary] -> String
part2 = (++) "Part 2: " <$> show . lifeSupportRating

powerConsumption :: [Binary] -> Int
powerConsumption xs = product $ map (parseBinary . (\f -> f xs)) [gammaRate, epsilonRate]

lifeSupportRating :: [Binary] -> Int
lifeSupportRating xs = product $ map (`getRating` xs) [gammaRate, epsilonRate]

parse :: String -> Binary
parse = map (read . return)

gammaRate :: [Binary] -> Binary
gammaRate = bitFreqs True

epsilonRate :: [Binary] -> Binary
epsilonRate = bitFreqs False

bitFreqs :: Bool -> [Binary] -> Binary
bitFreqs bit xs = map (fromEnum . f) ones
  where
    ones = foldr1 (zipWith (+)) xs
    f oneCount = case compare (2 * oneCount) (length xs) of
      LT -> not bit
      _ -> bit

getRating :: ([Binary] -> Binary) -> [Binary] -> Int
getRating f xs = parseBinary $ go 0 xs
  where
    go _ [x] = x
    go n xs = go (n + 1) (filter (\x -> x !! n == f xs !! n) xs)

parseBinary :: Binary -> Int
parseBinary = foldl' (\acc n -> 2 * acc + n) 0
