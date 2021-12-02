module Day02 where

import Data.List (foldl')

type Command = (String, Int)

type Position = (Int, Int)

type PositionWithAim = (Int, Int, Int)

main :: IO ()
main = interact (unlines . sequence [part1, part2] . map parseCommand . lines)

part1 :: [Command] -> String
part1 = (++) "Part 1: " <$> show . uncurry (*) . getEndPos

part2 :: [Command] -> String
part2 = (++) "Part 2: " <$> show . uncurry (*) . getEndPos'

parseCommand :: String -> Command
parseCommand s = let [cmd, num] = words s in (cmd, read num)

-- Part 1

getEndPos :: [Command] -> Position
getEndPos = foldl' move (0, 0)

move :: Position -> Command -> Position
move (x, y) ("forward", n) = (x + n, y)
move (x, y) ("down", n) = (x, y + n)
move (x, y) ("up", n) = (x, y - n)
move _ _ = error "Invalid direction"

-- Part 2

getEndPos' :: [Command] -> Position
getEndPos' cmds =
  let (x, y, _) = foldl' move' (0, 0, 0) cmds
   in (x, y)

move' :: PositionWithAim -> Command -> PositionWithAim
move' (x, y, a) ("forward", n) = (x + n, y + n * a, a)
move' (x, y, a) ("down", n) = (x, y, a + n)
move' (x, y, a) ("up", n) = (x, y, a - n)
move' _ _ = error "Invalid direction"
