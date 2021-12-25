module Day22 where

import Control.Arrow (Arrow (first), (&&&))
import Data.Bifunctor (Bifunctor (bimap))
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import qualified Data.Map as M

type Interval = (Int, Int)

type Cuboid = (Interval, Interval, Interval)

type Input = (Bool, Cuboid)

main :: IO ()
main = interact (unlines . sequence [part1, part2] . map parse . lines)

part1, part2 :: [Input] -> String
part1 = ("Part 1: " ++) . show . foldr step M.empty . filter inBounds
part2 = ("Part 2: " ++) . show

inBounds :: Input -> Bool
inBounds (_, (ix, iy, iz)) = all (within (-50, 50)) [ix, iy, iz]
  where
    within (min, max) (from, to) = from >= min && to <= max

todo = error "TODO"

parse :: String -> (Bool, Cuboid)
parse = bimap (== "on") parseCuboid . listToPair . splitOn " "
  where
    parseCuboid = listToTriple . map parseRange . splitOn ","
    parseRange = listToPair . map read . splitOn ".." . drop 2
    listToPair [x, y] = (x, y)
    listToPair _ = error "Invalid input"
    listToTriple [x, y, z] = (x, y, z)
    listToTriple _ = error "Invalid input"

type YZIntervals = Map Interval (Map Interval ())

type Intervals = Map Interval YZIntervals

-- invariant: no overlapping intervals

stepThree :: Bool -> Interval -> Map Interval () -> Map Interval ()
stepThree on iz acc = case findOverlaps iz acc of
  [] | not on -> acc
  [] -> M.insert iz () acc
  overlaps -> foldr (handleOverlap on iz f) acc overlaps
  where
    f () acc
      | on = todo

stepTwo :: Bool -> (Interval, Interval) -> YZIntervals -> YZIntervals
stepTwo on (iy, iz) acc = case findOverlaps iy acc of
  [] | not on -> acc
  [] -> M.insert iy (M.singleton iz ()) acc
  overlaps -> foldr (handleOverlap on iy f) acc overlaps
    where
      f = todo -- stepThree on iz

step :: Input -> Intervals -> Intervals
step (on, (ix, iy, iz)) acc = case findOverlaps ix acc of
  [] | not on -> acc
  [] -> M.insert ix (M.singleton iy $ M.singleton iz ()) acc
  overlaps -> foldr (handleOverlap on ix f) acc overlaps
  where
    f = todo -- stepTwo on (iy, iz)

data Split = Keep | Combine | Add

handleOverlap :: (Monoid a) => Bool -> Interval -> (a -> Map Interval a -> Map Interval a) -> Interval -> Map Interval a -> Map Interval a
handleOverlap on i combine i' acc =
  foldr handle (M.delete i' acc) $ split on i i'
  where
    handle (Keep, i) = M.insert i (acc ! i')
    handle (Add, i) = combine mempty
    handle (Combine, i) = combine (acc ! i')

split :: Bool -> Interval -> Interval -> [(Split, Interval)]
split on i i' = filter (isValid . snd) $ go on i i'
  where
    -- it may be unnecessary to be so exact with these cases, probably
    -- step two could at least ditch removals of empty space
    go False (from, to) (from', to') =
      [ (Keep, (from', from)),
        (Combine, (max from from', min to to')),
        (Keep, (to, to'))
      ]
    go True (from, to) (from', to') =
      [ (Add, (from, from')),
        (Keep, (from', from)),
        (Combine, (max from from', min to to')),
        (Keep, (to, to')),
        (Add, (to', to))
      ]
    isValid (from, to) = from <= to

-- TODO: rather use binary search
findOverlaps :: Interval -> Map Interval v -> [Interval]
findOverlaps interval = filter (overlap interval) . M.keys
  where
    overlap (from, to) (from', to') = from <= to' && to >= from'
