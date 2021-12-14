{-# LANGUAGE TupleSections #-}

module Day14 where

import Data.Bifunctor (Bifunctor (first))
import Data.List (sort, tails)
import Data.List.Split (splitOn)
import Data.Map.Monoidal (MonoidalMap, (!))
import qualified Data.Map.Monoidal as M
import Data.Monoid (Sum (Sum, getSum))

type Rules = MonoidalMap String Char

type Input = (String, Rules)

type StringCounts = MonoidalMap String (Sum Int)

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
    parseRule = listToPair . splitOn " -> "
    listToPair [x, y] = (x, head y)
    listToPair _ = error "Invalid input"

quantityScore :: StringCounts -> Int
quantityScore = score . sort . map getSum . M.elems
  where
    score xs = last xs - head xs

letterCounts :: Input -> [StringCounts]
letterCounts (template, rules) =
  map counts $ iterate (step rules) (foldMap single $ pairs template)
  where
    pairs = filter ((== 2) . length) . map (take 2) . tails
    counts = (<> single [head template]) . countLetters
    countLetters = foldMap (uncurry M.singleton . first tail) . M.assocs

step :: Rules -> StringCounts -> StringCounts
step rules = foldMap (uncurry M.singleton) . concatMap insert . M.assocs
  where
    insert (pair, n) = map (,n) [[head pair, ch], ch : tail pair]
      where
        ch = rules ! pair

single :: (Ord a) => a -> MonoidalMap a (Sum Int)
single x = M.singleton x (Sum 1)
