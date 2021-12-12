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

paths :: Maybe Cave -> Input -> Set [Cave]
paths twice input = S.fromList $ findPaths S.empty twice [] "start"
  where
    findPaths _ _ path "end" = [reverse ("end" : path)]
    findPaths visited twice path cave = case (twice, cave) of
      (_, "start") -> recurse visited' twice
      (Just _, _) -> recurse visited' twice
      _ -> recurse visited' Nothing ++ recurse visited (Just cave)
      where
        nextCaves = filter (\c -> not $ S.member c visited) (input ! cave)
        visited' = if all isLower cave then S.insert cave visited else visited
        recurse visited twice =
          concatMap (findPaths visited twice (cave : path)) nextCaves
