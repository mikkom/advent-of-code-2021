{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Day20 where

import Control.Arrow ((&&&))
import Data.Bifunctor (Bifunctor (bimap))
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import qualified Data.Map as M

type Point = (Int, Int)

data Input = Input
  { bitmap :: Map Point Bool,
    bounds :: (Point, Point),
    void :: Bool,
    algorithm :: Map Int Bool
  }

main :: IO ()
main = interact (unlines . sequence [part1, part2] . parse)

part1, part2 :: Input -> String
part1 = ("Part 1: " ++) . show . litCount . (!! 2) . iterate enhance
part2 = ("Part 2: " ++) . show . litCount . (!! 50) . iterate enhance

parse :: String -> Input
parse input = Input {void = False, algorithm = parseAlgorithm algoInput, ..}
  where
    [algoInput, bitmapInput] = splitOn "\n\n" input
    parseAlgorithm = M.fromList . zip [0 ..] . map lit
    (bitmap, bounds) = (parseBitmap &&& getBounds) $ splitOn "\n" bitmapInput
    getBounds xs = ((0, 0),) (length xs, length $ head xs)
    parseBitmap = M.fromList . concat . zipWith parseRow [0 ..]
    parseRow i = zipWith (\j b -> ((i, j), b)) [0 ..] . map lit
    lit = (== '#')

litCount :: Input -> Int
litCount = M.size . M.filter id . bitmap

enhance :: Input -> Input
enhance Input {bitmap, bounds = ((imin, jmin), (imax, jmax)), void, algorithm} =
  Input
    { bitmap = M.fromList $ zip pixels $ map outputPixel pixels,
      bounds = ((imin - 1, jmin - 1), (imax + 1, jmax + 1)),
      void = nextVoid void,
      algorithm
    }
  where
    nextVoid = (algorithm !) . toDecimal . replicate 9
    pixels = [(i, j) | i <- [imin - 1 .. imax + 1], j <- [jmin - 1 .. jmax + 1]]
    outputPixel = (algorithm !) . toDecimal . map inputPixel . square
    toDecimal = foldl' (\acc b -> 2 * acc + fromEnum b) 0
    square (i, j) = [(i + di, j + dj) | di <- [-1 .. 1], dj <- [-1 .. 1]]
    inputPixel p = M.findWithDefault void p bitmap
