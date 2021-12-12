module Day12 where

import Data.Char (isLower)
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Map (Map, empty, insertWith, (!))
import Data.Set (Set)
import qualified Data.Set as S

type Cave = String

type Input = Map Cave [Cave]

type Visited = (Set Cave, Maybe Cave)

main :: IO ()
main = interact (unlines . sequence [part1, part2] . parse . lines)

part1, part2 :: Input -> String
part1 = ("Part 1: " ++) . show . countPaths (Just "start")
part2 = ("Part 2: " ++) . show . countPaths Nothing

parse :: [String] -> Input
parse = foldl' (\m (k, v) -> insertWith (++) k [v] m) empty . (>>= connection)
  where
    connection = listToPairs . splitOn "-"
    listToPairs [x, y] = [(x, y), (y, x)]
    listToPairs _ = error "Invalid input"

countPaths :: Maybe Cave -> Input -> Int
countPaths twice input = findPaths (S.empty, twice) "start"
  where
    findPaths visited "end" = 1
    findPaths visited cave =
      sum $ map (findPaths visited') (filterCaves visited' (input ! cave))
      where
        visited' = updateVisited visited cave

updateVisited :: Visited -> Cave -> Visited
updateVisited visited cave = case visited of
  (caves, Nothing) | S.member cave caves -> (caves, Just cave)
  (caves, twice) | all isLower cave -> (S.insert cave caves, twice)
  _ -> visited

filterCaves :: Visited -> [Cave] -> [Cave]
filterCaves (_, Nothing) = filter (/= "start")
filterCaves (caves, _) = filter (not . flip S.member caves)
