module Day10 where

import Control.Monad (foldM)
import Data.Either (lefts, rights)
import Data.List (elemIndex, foldl', sort)
import Data.Maybe (fromJust)

main :: IO ()
main = interact (unlines . sequence [part1, part2] . lines)

part1, part2 :: [String] -> String
part1 = ("Part 1: " ++) . show . sum . map score . lefts . map parse
part2 = ("Part 2: " ++) . show . middle . map score' . rights . map parse

parse :: String -> Either Char String
parse = foldM f []
  where
    f stack c
      | c `elem` open = Right (c : stack)
      | take 1 stack == [matchingOpen c] = Right (tail stack)
      | otherwise = Left c
    matchingOpen c = fromJust $ lookup c $ zip closed open

score :: Char -> Int
score c = fromJust $ lookup c $ zip closed [3, 57, 1197, 25137]

score' :: String -> Int
score' = foldl' (\acc c -> 5 * acc + scoreCh c) 0
  where
    scoreCh c = succ . fromJust $ elemIndex c open

middle :: Ord a => [a] -> a
middle xs = sort xs !! (length xs `div` 2)

open, closed :: String
(open, closed) = ("([{<", ")]}>")
