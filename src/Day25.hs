module Day25 where

import Control.Arrow ((&&&))
import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import qualified Data.Map as M
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

type Point = (Int, Int)

data Direction = East | South deriving (Eq, Show)

type Input = (Map Point Direction, (Int, Int))

main :: IO ()
main = interact (unlines . sequence [part1, part2] . parse)

part1, part2 :: Input -> String
part1 = ("Part 1: " ++) . show . succ . length . takeUntilDuplicate . iterate step
part2 = ("Part 2: " ++) . show . const ""

takeUntilDuplicate :: (Eq a) => [a] -> [a]
takeUntilDuplicate = map fst . takeWhile (uncurry (/=)) . pairs
  where
    pairs xs = zip xs (tail xs)

step :: Input -> Input
step input@(m, (rows, cols)) = first go input
  where
    go = stepDir South nextSouth . stepDir East nextEast
    nextEast = wrap . second succ
    nextSouth = wrap . first succ
    wrap = bimap (`mod` rows) (`mod` cols)

stepDir :: (Ord k, Eq a) => a -> (k -> k) -> Map k a -> Map k a
stepDir dir nextPos m = foldr move m (movers m)
  where
    movers = filter hasSpace . M.keys . M.filter (== dir)
    move pos = M.insert (nextPos pos) dir . M.delete pos
    hasSpace pos = not $ M.member (nextPos pos) m

parse :: String -> Input
parse = (&&&) parseRows getSize . splitOn "\n"
  where
    getSize = (&&&) (pred . length) (length . head)
    parseRows = M.fromList . concat . zipWith parseRow [0 ..]
    parseRow i = catMaybes . zipWith (fmap . index i) [0 ..] . map parseChar
    index i j e = ((i, j), e)
    parseChar '>' = Just East
    parseChar 'v' = Just South
    parseChar _ = Nothing
