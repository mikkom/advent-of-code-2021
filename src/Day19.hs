{-# LANGUAGE TupleSections #-}

module Day19 where

import Data.Foldable (find)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.Set (Set, (\\))
import qualified Data.Set as S
import Debug.Trace (trace)

type Point = (Int, Int, Int)

type Scanner = Set Point

type Orientation = Point -> Point

threshold = 12

main :: IO ()
main = interact (unlines . sequence [part1, part2] . parse)

part1, part2 :: [Scanner] -> String
part1 = ("Part 1: " ++) . show . length . fst . findBeacons
part2 = ("Part 2: " ++) . show . maxDistance . ((0, 0, 0) :) . snd . findBeacons

todo = error "TODO"

parse :: String -> [Scanner]
parse = map parseScanner . splitOn "\n\n"
  where
    parseScanner = S.fromList . map parsePoint . tail . lines
    parsePoint = listToPoint . map read . splitOn ","
    listToPoint [x, y, z] = (x, y, z)
    listToPoint _ = error "Invalid input"

maxDistance :: [Point] -> Int
maxDistance = maximum . map (uncurry manhattan) . pairs
  where
    manhattan p1 p2 = let (x, y, z) = delta p1 p2 in abs x + abs y + abs z
    pairs [] = []
    pairs (x : xs) = map (x,) xs ++ pairs xs

type Data = (Scanner, [Scanner], [Point])

findBeacons :: [Scanner] -> (Scanner, [Point])
findBeacons [] = (S.empty, [])
findBeacons (x : xs) = let (s, _, deltas) = combine S.empty (x, xs, []) in (s, deltas)

combine :: Set Data -> Data -> Data
combine tried (s, [], ds) = trace "Nothing to combine" (s, [], ds)
combine tried p@(s, x : xs, ds) = case catMaybes combinations of
  ((delta, f) : _) ->
    trace
      ("Found delta " ++ show delta ++ ", remaining " ++ show (length xs))
      combine
      tried'
      (S.union s $ S.map (add delta . f) x, xs, delta : ds)
  [] ->
    trace
      "Miss"
      ( if S.member p tried
          then let (s', xs', ds') = combine tried' (x, xs, ds) in combine tried' (s, s' : xs', ds')
          else combine tried' (s, xs ++ [x], ds)
      )
  where
    combinations = map (\f -> align S.empty s f (S.toList x)) orientations
    tried' = S.insert p tried

align :: Set Point -> Scanner -> Orientation -> [Point] -> Maybe (Point, Orientation)
align tried s f [] = Nothing
align tried s f s'@(x : xs)
  | length s' < threshold = Nothing
  | otherwise = case find (`check` s') deltas of
    Just d -> Just (d, f)
    Nothing -> align (S.union tried deltas) s f xs
  where
    deltas = S.map (`delta` f x) s \\ tried
    check d = lengthAtLeast threshold . filter (`S.member` s) . map (add d . f)

add :: Point -> Point -> Point
add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

delta :: Point -> Point -> Point
delta (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

lengthAtLeast :: Int -> [a] -> Bool
lengthAtLeast 0 _ = True
lengthAtLeast _ [] = False
lengthAtLeast n (_ : xs) = lengthAtLeast (n - 1) xs

orientations :: [Point -> Point]
orientations =
  [ -- facing x
    \(x, y, z) -> (x, y, z),
    \(x, y, z) -> (x, - z, y),
    \(x, y, z) -> (x, - y, - z),
    \(x, y, z) -> (x, z, - y),
    -- facing -x
    \(x, y, z) -> (- x, y, - z),
    \(x, y, z) -> (- x, - z, - y),
    \(x, y, z) -> (- x, - y, z),
    \(x, y, z) -> (- x, z, y),
    -- facing y
    \(x, y, z) -> (- y, x, z),
    \(x, y, z) -> (z, x, y),
    \(x, y, z) -> (y, x, - z),
    \(x, y, z) -> (- z, x, - y),
    -- facing -y
    \(x, y, z) -> (- y, - x, z),
    \(x, y, z) -> (- z, - x, y),
    \(x, y, z) -> (y, - x, z),
    \(x, y, z) -> (z, - x, - y),
    -- facing z
    \(x, y, z) -> (- z, y, x),
    \(x, y, z) -> (- y, - z, x),
    \(x, y, z) -> (z, - y, x),
    \(x, y, z) -> (y, z, x),
    -- facing -z
    \(x, y, z) -> (z, y, - x),
    \(x, y, z) -> (y, - z, - x),
    \(x, y, z) -> (- z, - y, - x),
    \(x, y, z) -> (- y, z, - x)
  ]

-- Ideas
-- - the scanner area necessarily happens over some corner
--
-- Trying already
-- - use sets for scanner points
-- - skip already tried deltas
-- - stop when there aren't enough points left
--
-- Unclear
-- - whether uncombined scanner pairs form a connected graph, but probably yes
--   => checking combined pairs may (or may not) slow things down
