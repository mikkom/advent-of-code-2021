{-# LANGUAGE TupleSections #-}

module Day05 where

import Data.List.Split (splitOn)
import qualified Data.Map.Monoidal as M
import Data.Monoid (Sum)

type Point = (Int, Int)

data Line = Line Point Point deriving (Eq, Show)

type PointMap = M.MonoidalMap Point (Sum Int)

main :: IO ()
main = interact (unlines . sequence [part1, part2] . map parseLine . lines)

part1 :: [Line] -> String
part1 = (++) "Part 1: " <$> show . countOverlaps . map toPointMap . filter isStraight

part2 :: [Line] -> String
part2 = (++) "Part 2: " <$> show . countOverlaps . map toPointMap

countOverlaps :: [PointMap] -> Int
countOverlaps = M.size . M.filter (> 1) . mconcat

toPointMap :: Line -> PointMap
toPointMap = M.fromList . (map (,1) . points)
  where
    points (Line (x1, y1) (x2, y2)) = zip (range x1 x2) (range y1 y2)
    range a b = [a, a + signum (b - a) .. b]

isStraight :: Line -> Bool
isStraight (Line (x1, y1) (x2, y2)) = x1 == x2 || y1 == y2

parseLine :: String -> Line
parseLine = uncurry Line . toPair . map toPoint . splitOn " -> "
  where
    toPoint = toPair . map read . splitOn ","
    toPair [x, y] = (x, y)
    toPair _ = error "Invalid input"
