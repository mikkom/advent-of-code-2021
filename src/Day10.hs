module Day10 where

import Control.Monad (foldM)
import Data.Either (lefts, rights)
import Data.List (elemIndex, foldl', sort)
import Data.Maybe (listToMaybe, mapMaybe)

main :: IO ()
main = interact (unlines . sequence [part1, part2] . map parse . lines)

part1, part2 :: [Either Char String] -> String
part1 = ("Part 1: " ++) . show . sum . mapMaybe score . lefts
part2 = ("Part 2: " ++) . show . middle . sort . map score' . rights

parse :: String -> Either Char String
parse = foldM f []
  where
    f stack c
      | c `elem` open = Right (c : stack)
      | listToMaybe stack == matchingOpen c = Right (tail stack)
      | otherwise = Left c
    matchingOpen = (`lookup` zip closed open)

score :: Char -> Maybe Int
score = (`lookup` zip closed [3, 57, 1197, 25137])

score' :: String -> Int
score' = foldl' (\acc i -> 5 * acc + i + 1) 0 . mapMaybe (`elemIndex` open)

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

open, closed :: String
(open, closed) = ("([{<", ")]}>")
