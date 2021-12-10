{-# LANGUAGE TupleSections #-}

module Day09 where

import Data.Array (Array, array, bounds, indices, (!), (//))
import Data.List (sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (maybeToList)

type Coord = (Int, Int)

type Input = Array Coord Int

type Input' = Array Coord (Int, Bool)

main :: IO ()
main = interact (unlines . sequence [part1, part2] . parse . lines)

part1, part2 :: Input -> String
part1 = ("Part 1: " ++) . show . sum . riskLevels
part2 = ("Part 2: " ++) . show . product . take 3 . sortBy (flip compare) . map length . basins

parse :: [String] -> Input
parse input = array ((0, 0), (rows - 1, cols - 1)) $ combineInput input
  where
    combineInput = zipWith assocs [0 ..] . concatMap (map readInt)
    assocs i e = (getIndex i, e)
    getIndex x = (x `div` cols, x `mod` cols)
    rows = length input
    cols = length $ head input

readInt :: Char -> Int
readInt = read . return

riskLevels :: Input -> [Int]
riskLevels arr = map (succ . (arr !)) $ lowPoints arr

lowPoints :: Input -> [Coord]
lowPoints arr = filter (isLowPoint arr) $ indices arr

basins :: Input -> [[Coord]]
basins arr = map (\l -> fst $ basinStep arr' [] [l]) ls
  where
    ls = lowPoints arr
    arr' = fmap (,False) arr

basinStep :: Input' -> [Coord] -> [Coord] -> ([Coord], Input')
basinStep arr acc [] = (acc, arr)
basinStep arr acc (c : cs) = basinStep arr' (c : acc) (cs' ++ cs)
  where
    (cs', arr') = expandBasin c
    expandBasin :: Coord -> ([Coord], Input')
    expandBasin coord = (coords, arr // nextAssocs)
      where
        coords = nextCoords coord
        nextAssocs = map (\c -> (c, (fst (arr ! c), True))) coords

    nextCoords coord = filter validPoint $ adjacentCoords arr coord
      where
        validPoint c = case arr ! c of
          (_, True) -> False
          (9, _) -> False
          (n, _) -> n >= fst (arr ! coord)

isLowPoint :: Input -> Coord -> Bool
isLowPoint arr coord = all (> (arr ! coord)) (adjacent arr coord)

adjacent :: Input -> Coord -> [Int]
adjacent arr = map (arr !) . adjacentCoords arr

adjacentCoords :: Array Coord a -> Coord -> [Coord]
adjacentCoords arr coord = map (add coord) deltas >>= check
  where
    deltas = [(0, 1), (0, -1), (1, 0), (-1, 0)]
    check coord
      | isValidCoord arr coord = [coord]
      | otherwise = []
    add (x, y) (dx, dy) = (x + dx, y + dy)

isValidCoord :: Array Coord a -> Coord -> Bool
isValidCoord arr (x, y) = x `elem` [minx .. maxx] && y `elem` [miny .. maxy]
  where
    ((minx, miny), (maxx, maxy)) = bounds arr
