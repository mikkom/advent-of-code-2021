{-# LANGUAGE TupleSections #-}

module Day14 where

import Control.Arrow ((&&&))
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.List (sort, tails)
import Data.List.Split (splitOn)
import Data.Map.Monoidal (MonoidalMap, (!))
import qualified Data.Map.Monoidal as M
import Data.Monoid (Sum (Sum, getSum))

type Pair = (Char, Char)

type Rules = MonoidalMap Pair Char

type Input = (String, Rules)

type Counts a = MonoidalMap a (Sum Int)

main :: IO ()
main = interact (unlines . sequence [part1, part2] . parse)

part1, part2 :: Input -> String
part1 = ("Part 1: " ++) . show . quantityScore . (!! 10) . letterCounts
part2 = ("Part 2: " ++) . show . quantityScore . (!! 40) . letterCounts

parse :: String -> Input
parse input = (template, parseRules rulePart)
  where
    [template, rulePart] = splitOn "\n\n" input
    parseRules = M.fromList . map parseRule . filter (/= "") . splitOn "\n"
    parseRule = bimap listToPair head . listToPair . splitOn " -> "
    listToPair [x, y] = (x, y)
    listToPair _ = error "Invalid input"

quantityScore :: Counts Char -> Int
quantityScore = score . map getSum . M.elems
  where
    score xs = maximum xs - minimum xs

letterCounts :: Input -> [Counts Char]
letterCounts (template, rules) =
  map counts $ iterate (step rules) (foldMap single $ pairs template)
  where
    pairs xs = zip xs (tail xs)
    counts = (<> single (head template)) . countLetters
    countLetters = foldMap (uncurry M.singleton . first snd) . M.assocs

step :: Rules -> Counts Pair -> Counts Pair
step rules = foldMap (uncurry M.singleton) . concatMap insert . M.assocs
  where
    insert (pair, n) =
      let ch = rules ! pair in map (,n) [(fst pair, ch), (ch, snd pair)]

single :: (Ord a) => a -> MonoidalMap a (Sum Int)
single x = M.singleton x (Sum 1)
