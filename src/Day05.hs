{-# LANGUAGE TupleSections #-}

module Day05 where

import Control.Arrow ((&&&))
import Data.List.Split (splitOn)
import qualified Data.Map.Monoidal as M
import Data.Monoid (Sum (Sum))

type Int' = Sum Int

type Point = (Int', Int')

type Line = (Point, Point)

type PointMap = M.MonoidalMap Point Int'

main :: IO ()
main = interact (unlines . sequence [part1, part2] . map parseLine . lines)

part1 :: [Line] -> String
part1 = (++) "Part 1: " <$> show . countOverlaps . map (toPointMap True)

part2 :: [Line] -> String
part2 = (++) "Part 2: " <$> show . countOverlaps . map (toPointMap False)

countOverlaps :: [PointMap] -> Int
countOverlaps = M.size . M.filter (> 1) . mconcat

toPointMap :: Bool -> Line -> PointMap
toPointMap skipDiagonal = M.fromList . (map (,1) . points)
  where
    points (p1, p2)
      | skipDiagonal && dx /= 0 && dy /= 0 = []
      | otherwise = range (<> (dx, dy)) p1 p2
      where
        (dx, dy) = (delta fst, delta snd)
        delta f = signum $ f p2 - f p1

parseLine :: String -> Line
parseLine = (&&&) (parsePoint . head) (parsePoint . last) . words
  where
    parsePoint = (&&&) (head . coords) (last . coords)
    coords = map (Sum . read) . splitOn ","

range :: Eq t => (t -> t) -> t -> t -> [t]
range step start end = takeWhile (/= step end) $ iterate step start
