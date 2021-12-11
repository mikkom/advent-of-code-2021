module Day11 where

import Data.Array (Array, array, assocs, elems, (!), (//))
import Data.Char (digitToInt)
import Data.List (find, unfoldr)

type Coord = (Int, Int)

type Input = Array Coord Int

(size, threshold) = (10, 10)

main :: IO ()
main = interact (unlines . sequence [part1, part2] . parse . lines)

part1, part2 :: Input -> String
part1 = ("Part 1: " ++) . show . solve . iterate step
  where
    solve = sum . map flashCount . take 100 . tail
part2 = ("Part 2: " ++) . show . solve . iterate step
  where
    solve = fst . head . filter (allFlashing . snd) . zip [0 ..]

parse :: [String] -> Input
parse = array ((0, 0), (size - 1, size - 1)) . pairs . map (map digitToInt)
  where
    pairs = concatMap f . zip [0 ..] . map (zip [0 ..])
    f (i, xs) = map (\(j, e) -> ((i, j), e)) xs

flashCount :: Input -> Int
flashCount = length . filter (0 ==) . elems

allFlashing :: Input -> Bool
allFlashing = all (0 ==) . elems

step :: Input -> Input
step = zero . head . filter (null . newFlashes) . iterate flashStep . fmap succ
  where
    zero = fmap (\e -> if e >= threshold then 0 else e)

flashStep :: Input -> Input
flashStep arr = case newFlashes arr of
  [] -> arr
  (c : cs) -> arr // ((c, threshold + 1) : newAssocs)
    where
      newAssocs = map (\c -> (c, update $ arr ! c)) (adjacent c)
      update n = if n == threshold then threshold else n + 1

newFlashes :: Input -> [Coord]
newFlashes = map fst . filter ((== threshold) . snd) . assocs

adjacent :: Coord -> [Coord]
adjacent coord = [c | x <- ds, y <- ds, c <- check $ add coord (x, y)]
  where
    ds = [-1 .. 1]
    check c
      | c == coord = []
      | isValid c = [c]
      | otherwise = []
    add (x, y) (dx, dy) = (x + dx, y + dy)

isValid :: Coord -> Bool
isValid (x, y) = check x && check y
  where
    check c = c >= 0 && c < size
