{-# LANGUAGE TupleSections #-}

module Day25 where

import Control.Arrow ((&&&))
import Data.Bifunctor (first)
import Data.List (foldl', intercalate)
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import qualified Data.Map as M
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

type Point = (Int, Int)

data SeaCucumber = East | South deriving (Eq, Show)

type Input = (Map Point SeaCucumber, (Int, Int))

todo = error "TODO"

main :: IO ()
main = interact (unlines . sequence [part1, part2] . parse)

part1, part2 :: Input -> String
-- part1 = ("Part 1: \n" ++) . intercalate "\n\n" . map prettyPrint . take 59 . iterate step
part1 = ("Part 1: " ++) . show . succ . length . whileDiffer . iterate step
part2 = ("Part 2: " ++) . show . const ""

firstDuplicate :: (Eq a) => [a] -> a
firstDuplicate = fst . head . dropWhile (uncurry (/=)) . pairs
  where
    pairs xs = zip xs (tail xs)

whileDiffer :: (Eq a) => [a] -> [a]
whileDiffer = map fst . takeWhile (uncurry (/=)) . pairs
  where
    pairs xs = zip xs (tail xs)

stepEast :: Input -> Input
stepEast (m, (rows, cols)) = (foldr move m (movers m), (rows, cols))
  where
    movers = filter hasSpace . M.keys . M.filter (== East)
    move pos = M.insert (nextPos pos) East . M.delete pos
    nextPos (i, j) = (i, (j + 1) `mod` cols)
    hasSpace pos = not $ M.member (nextPos pos) m

stepSouth :: Input -> Input
stepSouth (m, (rows, cols)) = (foldr move m (movers m), (rows, cols))
  where
    movers = filter hasSpace . M.keys . M.filter (== South)
    move pos = M.insert (nextPos pos) South . M.delete pos
    nextPos (i, j) = ((i + 1) `mod` rows, j)
    hasSpace pos = not $ M.member (nextPos pos) m

step :: Input -> Input
step = stepSouth . stepEast

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

prettyPrint :: Input -> String
prettyPrint (m, (rows, cols)) = intercalate "\n" $ map (printRow m) [0 .. rows - 1]
  where
    printRow m i = map (print . (`M.lookup` m) . (i,)) [0 .. cols - 1]
    print (Just East) = '>'
    print (Just South) = 'v'
    print Nothing = '.'
