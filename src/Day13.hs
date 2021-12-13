{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day13 where

import Data.Bifunctor (Bifunctor (first, second))
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple (uncurry)

type Point = (Int, Int)

type Sheet = Set Point

data Fold = FoldLeft Int | FoldUp Int

type Input = (Sheet, [Fold])

main :: IO ()
main = interact (unlines . sequence [part1, part2] . uncurry (scanl fold) . parse)

part1, part2 :: [Sheet] -> String
part1 = ("Part 1: " ++) . show . length . (!! 1)
part2 = ("Part 2: " ++) . prettyPrint . last

parse :: String -> Input
parse input = (parseDots dots, parseFolds folds)
  where
    [dots, folds] = splitOn "\n\n" input
    parseDots = S.fromList . map (listToPair . map read . splitOn ",") . lines
    parseFolds = map parseFold . lines
    parseFold str = case parseFoldLine str of
      ["x", value] -> FoldLeft (read value)
      ["y", value] -> FoldUp (read value)
    parseFoldLine = splitOn "=" . last . words
    listToPair [x, y] = (x, y)

fold :: Sheet -> Fold -> Sheet
fold sheet (FoldLeft x) = S.map (first $ foldCoord x) sheet
fold sheet (FoldUp y) = S.map (second $ foldCoord y) sheet

foldCoord :: Int -> Int -> Int
foldCoord foldPoint coord =
  if coord > foldPoint then 2 * foldPoint - coord else coord

prettyPrint :: Sheet -> String
prettyPrint sheet = '\n' : intercalate "\n" (map printRow [0 .. maxy])
  where
    printRow y = [if (x, y) `elem` sheet then '#' else '.' | x <- [0 .. maxx]]
    (maxx, maxy) = (maximum $ S.map fst sheet, maximum $ S.map snd sheet)
