module Day12 where

import Data.Char (isLower)
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

type Cave = String

type Input = Map Cave [Cave]

main :: IO ()
main = interact (unlines . sequence [part1, part2] . parse . lines)

part1, part2 :: Input -> String
part1 = ("Part 1: " ++) . show . length . paths
part2 = ("Part 2: " ++) . show . length . paths'

paths :: Input -> [[Cave]]
paths input = findPaths input S.empty [] "start"

paths' :: Input -> Set [Cave]
paths' input = S.fromList $ findPaths' input S.empty Nothing [] "start"

parse :: [String] -> Input
parse = foldl' (\acc (k, v) -> M.insertWith (++) k [v] acc) M.empty . (>>= connection)
  where
    connection = listToPairs . splitOn "-"
    listToPairs [x, y] = [(x, y), (y, x)]
    listToPairs _ = error "Invalid input"

findPaths :: Input -> Set Cave -> [Cave] -> Cave -> [[Cave]]
findPaths _ _ path "end" = [reverse ("end" : path)]
findPaths input visited path cave =
  concatMap (findPaths input visited' (cave : path)) nextCaves
  where
    nextCaves = filter (\c -> not $ S.member c visited) (input ! cave)
    visited' = if all isLower cave then S.insert cave visited else visited

findPaths' :: Input -> Set Cave -> Maybe Cave -> [Cave] -> Cave -> [[Cave]]
findPaths' _ _ _ path "end" = [reverse ("end" : path)]
findPaths' input visited twice path cave = case (twice, cave) of
  (_, "start") -> concatMap (findPaths' input visited' twice (cave : path)) nextCaves
  (Nothing, _) ->
    concatMap (findPaths' input visited' Nothing (cave : path)) nextCaves
      ++ concatMap (findPaths' input visited (Just cave) (cave : path)) nextCaves
  _ -> concatMap (findPaths' input visited' twice (cave : path)) nextCaves
  where
    nextCaves = filter (\c -> not $ S.member c visited) (input ! cave)
    visited' = if all isLower cave then S.insert cave visited else visited
