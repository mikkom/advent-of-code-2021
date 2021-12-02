module Day02 where

import Data.List (foldl')
import Data.Monoid (Sum (Sum, getSum))

type Position = (Sum Int, Sum Int)

type PositionWithAim = (Position, Sum Int)

main :: IO ()
main = interact (unlines . sequence [part1, part2] . map (parse . words) . lines)

part1 :: [Position] -> String
part1 = (++) "Part 1: " <$> show . getSum . uncurry (*) . mconcat

part2 :: [Position] -> String
part2 = (++) "Part 2: " <$> show . getSum . uncurry (*) . fst . foldl' move mempty

parse :: [String] -> Position
parse cmd = let (x, y) = p cmd in (Sum x, Sum y)
  where
    p ["forward", n] = (read n, 0)
    p ["down", n] = (0, read n)
    p ["up", n] = (0, - read n)
    p _ = error "Invalid command"

move :: PositionWithAim -> Position -> PositionWithAim
move (pos, aim) (Sum 0, dy) = (pos, aim + dy)
move (pos, aim) (dx, _) = (pos <> (dx, dx * aim), aim)
