{-# LANGUAGE TupleSections #-}

module Day05 where

import Control.Arrow ((&&&))
import Data.List.Split (splitOn)
import qualified Data.Map as M

type Point = (Int, Int)

type Line = (Point, Point)

type PointMap = M.Map Point Int

main :: IO ()
main = interact (unlines . sequence [part1, part2] . map parseLine . lines)

part1 :: [Line] -> String
part1 = (++) "Part 1: " <$> show . countOverlaps . map (toPointMap True)

part2 :: [Line] -> String
part2 = (++) "Part 2: " <$> show . countOverlaps . map (toPointMap False)

countOverlaps :: [PointMap] -> Int
countOverlaps = M.size . M.filter (> 1) . M.unionsWith (+)

toPointMap :: Bool -> Line -> PointMap
toPointMap skipDiagonal = M.fromList . (map (,1) . newPoints)
  where
    newPoints ((x1, y1), (x2, y2))
      | x1 == x2 = map (x1,) ys
      | y1 == y2 = map (,y1) xs
      | skipDiagonal = []
      | (x1 < x2) == (y1 < y2) = zip xs ys
      | otherwise = zip xs (reverse ys)
      where
        xs = [min x1 x2 .. max x1 x2]
        ys = [min y1 y2 .. max y1 y2]

parseLine :: String -> Line
parseLine = (&&&) (parsePoint . head) (parsePoint . last) . words
  where
    parsePoint = (&&&) (read . head . split) (read . last . split)
    split = splitOn ","
