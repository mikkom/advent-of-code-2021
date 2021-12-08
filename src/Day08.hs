module Day08 where

import Control.Arrow (Arrow (second))
import Data.Function (on)
import Data.List (permutations, sort)
import Data.List.Split (splitOn)
import Data.Map (Map, fromList, fromListWith, toList, (!))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple (swap)

type Entry = ([String], [String])

type SignalMap = Map Char Char

type CountMap = Map Int [Int]

main :: IO ()
main = interact (unlines . sequence [part1, part2] . map parse . lines)

parse :: String -> Entry
parse = listToPair . map words . splitOn " | "
  where
    listToPair [x, y] = (x, y)
    listToPair _ = error "Invalid input"

part1, part2 :: [Entry] -> String
part1 = ("Part 1: " ++) . show . sum . map countDigits
  where
    countDigits = length . filter ((`elem` [2, 3, 4, 7]) . length) . snd
part2 = ("Part 2: " ++) . show . map outputDigits

outputDigits :: Entry -> Int
outputDigits (patterns, outputs) = read $ concatMap (show . getDigit) outputs
  where
    getDigit s = (invertMap segmentMap !) $ sort $ map (signalMap !) s
    (signalMap, _) = (head . filter (null . snd) . findMappings) patterns

findMappings :: [String] -> [(SignalMap, Set Int)]
findMappings [] = error "Invalid input"
findMappings [x] = possibleMaps x (S.fromList [0 .. 9])
findMappings (x : xs) =
  [(cs, ds') | (m, ds) <- findMappings xs, (m', ds') <- possibleMaps x ds, cs <- combineMaps m m']

combineMaps :: SignalMap -> SignalMap -> [SignalMap]
combineMaps m m'
  | M.intersection m m' /= M.intersection m' m = []
  | otherwise = [M.union m rm | rm <- remainingMaps m m']

remainingMaps :: SignalMap -> SignalMap -> [SignalMap]
remainingMaps m m' = map (fromList . (`zip` vs)) (permutations ks)
  where
    (ks, vs) = remainingAssocs m m'

remainingAssocs :: SignalMap -> SignalMap -> (String, String)
remainingAssocs m = unzip . M.assocs . (`M.difference` m)

possibleMaps :: String -> Set Int -> [(SignalMap, Set Int)]
possibleMaps str digits =
  [(fromList $ zip i o, ds) | (o, ds) <- outputs digits str, i <- permutations str]
  where
    outputs :: Set Int -> String -> [(String, Set Int)]
    outputs digits str = map (\d -> (segmentMap ! d, S.delete d digits)) validDigits
      where
        validDigits = filter (`S.member` digits) $ digitMap ! length str

digitMap :: CountMap
digitMap = fromList [(2, [1]), (3, [7]), (4, [4]), (5, [2, 3, 5]), (6, [0, 6, 9]), (7, [8])]

segmentMap :: Map Int String
segmentMap =
  fromList
    [ (0, "abcefg"),
      (1, "cf"),
      (2, "acdeg"),
      (3, "acdfg"),
      (4, "bcdf"),
      (5, "abdfg"),
      (6, "abdefg"),
      (7, "acf"),
      (8, "abcdefg"),
      (9, "abcdfg")
    ]

-- invert :: (Ord k, Ord v) => Map k v -> Map v [k]
-- invert m = fromListWith (++) pairs
--   where
--     pairs = [(v, [k]) | (k, v) <- toList m]

invertMap :: (Ord k, Ord a) => Map k a -> Map a k
invertMap = fromList . map swap . toList
