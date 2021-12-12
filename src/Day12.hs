module Day12 where

import Data.Char (isLower)
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Map (Map, empty, insertWith, (!))
import Data.Set (Set)
import qualified Data.Set as S

type Cave = String

type Input = Map Cave [Cave]

main :: IO ()
main = interact (unlines . sequence [part1, part2] . parse . lines)

part1, part2 :: Input -> String
part1 = ("Part 1: " ++) . show . length . paths (Just "start")
part2 = ("Part 2: " ++) . show . length . paths Nothing

parse :: [String] -> Input
parse = foldl' (\m (k, v) -> insertWith (++) k [v] m) empty . (>>= connection)
  where
    connection = listToPairs . splitOn "-"
    listToPairs [x, y] = [(x, y), (y, x)]
    listToPairs _ = error "Invalid input"

paths :: Maybe Cave -> Input -> [[Cave]]
paths twice input = findPaths (S.empty, twice) [] "start"
  where
    findPaths visited path "end" = [reverse ("end" : path)]
    findPaths visited path cave =
      concatMap (findPaths visited' (cave : path)) (nextCaves visited' (input ! cave))
      where
        visited' = updateVisited visited cave

    updateVisited visited cave = case visited of
      (caves, Nothing) | S.member cave caves -> (caves, Just cave)
      (caves, twice) | all isLower cave -> (S.insert cave caves, twice)
      _ -> visited

    nextCaves (_, Nothing) = filter (/= "start")
    nextCaves (caves, _) = filter (not . flip S.member caves)
