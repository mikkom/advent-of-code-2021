{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day13 where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Set (Set, (\\))
import qualified Data.Set as S
import Data.Tuple (uncurry)

type Point = (Int, Int)

type Sheet = Set Point

type Input = (Sheet, [Point])

main :: IO ()
main = interact (unlines . sequence [part1, part2] . parse)

part1, part2 :: Input -> String
part1 = ("Part 1: " ++) . show . length . (!! 1) . uncurry (scanl fold)
part2 = ("Part 2: " ++) . prettyPrint . last . uncurry (scanl fold)

parse :: String -> Input
parse input = (parseDots dots, parseFolds folds)
  where
    [dots, folds] = splitOn "\n\n" input
    parseDots = S.fromList . map (listToPair . map read . splitOn ",") . lines
    parseFolds = map parseFold . lines
    parseFold str = case parseFoldLine str of
      ["x", value] -> (read value, 0)
      ["y", value] -> (0, read value)
    parseFoldLine = splitOn "=" . last . words
    listToPair [x, y] = (x, y)

fold :: Sheet -> Point -> Sheet
fold sheet (x, 0) = foldx sheet x
fold sheet (0, y) = foldy sheet y

foldx :: Sheet -> Int -> Sheet
foldx sheet fx = S.union (sheet \\ folded) (S.map wrap folded)
  where
    folded = S.filter ((> fx) . fst) sheet
    wrap (x, y) = (2 * fx - x, y)

foldy :: Sheet -> Int -> Sheet
foldy sheet fy = S.union (sheet \\ folded) (S.map wrap folded)
  where
    folded = S.filter ((> fy) . snd) sheet
    wrap (x, y) = (x, 2 * fy - y)

prettyPrint :: Sheet -> String
prettyPrint sheet = '\n' : intercalate "\n" (map printRow [0 .. maxy])
  where
    printRow y = [if (x, y) `elem` sheet then '#' else '.' | x <- [0 .. maxx]]
    (maxx, maxy) = (maximum $ S.map fst sheet, maximum $ S.map snd sheet)
