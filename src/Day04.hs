{-# LANGUAGE TupleSections #-}

module Day04 where

import Data.List (find, groupBy, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, isNothing, listToMaybe, mapMaybe)

type BingoBoard = [[(Int, Bool)]]

type GameState = ([Int], [BingoBoard])

main :: IO ()
main = interact (unlines . sequence [part1, part2] . parse . lines)

part1 :: GameState -> String
part1 = (++) "Part 1: " <$> show . playBingo

part2 :: GameState -> String
part2 = (++) "Part 2: " <$> show . playLosingBingo

parse :: [String] -> GameState
parse ss = (nums, boards)
  where
    nums :: [Int]
    nums = map read $ splitOn "," $ head ss
    boardLines = groupBy (\_ line -> line /= "") (tail ss)
    boards = map parseBoard boardLines

parseBoard :: [String] -> BingoBoard
parseBoard ss = map parseRow rows
  where
    rows = tail ss
    parseRow :: String -> [(Int, Bool)]
    parseRow = map ((,False) . read) . words

playBingo :: GameState -> Int
playBingo ([], boards) = error "No bingo :("
playBingo (n : ns, boards) = case finalScore scores of
  Just score -> score
  Nothing -> playBingo (ns, newBoards)
  where
    (newBoards, scores) = unzip $ map (playRound n) boards
    finalScore = listToMaybe . catMaybes

playLosingBingo :: GameState -> Int
playLosingBingo ([], boards) = error "No bingo :("
playLosingBingo (n : ns, boards) = case map snd newState of
  [Just score] -> score
  _ -> playLosingBingo (ns, remainingBoards newState)
  where
    newState = map (playRound n) boards
    remainingBoards = map fst . filter (isNothing . snd)

playRound :: Int -> BingoBoard -> (BingoBoard, Maybe Int)
playRound n board
  | newBoard == board = (newBoard, Nothing)
  | otherwise = (newBoard, checkBoard newBoard n)
  where
    newBoard = updateBoard board
    updateBoard = map (map updateElem)
    updateElem elem
      | fst elem == n = (n, True)
      | otherwise = elem

checkBoard :: BingoBoard -> Int -> Maybe Int
checkBoard board n = check board `orElse` check (transpose board)
  where
    check rows = (listToMaybe . mapMaybe checkRow) rows
    checkRow row
      | all snd row = Just (n * unmarkedSum board)
      | otherwise = Nothing

unmarkedSum :: BingoBoard -> Int
unmarkedSum = sum . map fst . filter (not . snd) . concat

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse _ y = y
