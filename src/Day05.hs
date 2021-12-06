{-# LANGUAGE TupleSections #-}

module Day05 where

import Data.Group ((~~))
import Data.List.Split (splitOn)
import qualified Data.Map.Monoidal as M
import Data.Monoid (Sum (Sum))

type Point = (Sum Int, Sum Int)

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
    points (Line p1 p2) = range p1 (mapPair signum (p2 ~~ p1)) p2
    mapPair f (x, y) = (f x, f y)

isStraight :: Line -> Bool
isStraight (Line (x1, y1) (x2, y2)) = x1 == x2 || y1 == y2

parseLine :: String -> Line
parseLine = uncurry Line . toPair . map toPoint . splitOn " -> "
  where
    toPoint = toPair . map (Sum . read) . splitOn ","
    toPair [x, y] = (x, y)
    toPair _ = error "Invalid input"

range :: (Eq m, Monoid m) => m -> m -> m -> [m]
range start step end = takeWhile (/= end <> step) $ iterate (<> step) start
