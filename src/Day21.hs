{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Day21 where

import Control.Arrow ((&&&))
import Control.Monad.State.Lazy (MonadState)
import qualified Control.Monad.State.Lazy as MS
import Data.Function (fix)
import Data.List (group, sort)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (Sum (Sum, getSum))

data PlayerState = PlayerState
  { position :: Int,
    score :: Int
  }
  deriving (Eq, Ord, Show)

type Input = (PlayerState, PlayerState)

type GameState = (Input, [Int], Int)

type DiracState = (Input, Bool)

type WinCounts = (Sum Int, Sum Int)

main :: IO ()
main = interact (unlines . sequence [part1, part2] . parse)

part1, part2 :: Input -> String
part1 = ("Part 1: " ++) . show . answer . play . (,cycle dieSums,0)
  where
    dieSums = [6, 5, 4, 3, 2, 1, 0, 9, 8, 7]
    answer ((PlayerState {score}, _), _, rolls) = score * rolls
part2 = ("Part 2: " ++) . show . getSum . uncurry max . memoizedDirac . (,False)

parse :: String -> Input
parse input = (p1, p2)
  where
    [p1, p2] = map parsePlayer $ lines input
    parsePlayer = getPlayer . read . last . splitOn ": "
    getPlayer position = PlayerState {score = 0, position}

play :: GameState -> GameState
play = head . dropWhile (not . finished) . iterate turn
  where
    finished ((_, PlayerState {score}), _, _) = score >= 1000

turn :: GameState -> GameState
turn (players, throw : dieSums, rolls) =
  (updatePlayers players throw, dieSums, rolls + 3)
turn (_, [], _) = error "Found the end of an infinite list"

updatePlayers :: Input -> Int -> Input
updatePlayers (PlayerState {position, score}, player) throw =
  (player, player')
  where
    player' = PlayerState {position = position', score = score'}
    position' = case mod (position + throw) 10 of
      0 -> 10
      n -> n
    score' = score + position'

memoizedDirac :: DiracState -> WinCounts
memoizedDirac n = MS.evalState (fix (memoize . dirac) n) M.empty

memoize :: (Ord k) => (MonadState (Map k v) m) => (k -> m v) -> k -> m v
memoize f x = do
  v <- MS.gets (M.lookup x)
  case v of
    Just y -> return y
    _ -> do
      y <- f x
      MS.modify $ M.insert x y
      return y

dirac :: (Monad m) => (DiracState -> m WinCounts) -> DiracState -> m WinCounts
dirac f state@((PlayerState {score}, player), toggle)
  | checkWin player = return $ if toggle then (1, 0) else (0, 1)
  | otherwise = mconcat <$> sequence recursions
  where
    checkWin PlayerState {score} = score >= 21
    recursions = map (\(c, s) -> times c <$> recurse s) $ counts dieSums
    recurse = f . updateState state
    times n (w1, w2) = (Sum n * w1, Sum n * w2)
    counts = map ((&&&) length head) . group . sort
    dieSums = [r1 + r2 + r3 | r1 <- [1 .. 3], r2 <- [1 .. 3], r3 <- [1 .. 3]]

updateState :: DiracState -> Int -> DiracState
updateState (players, toggle) throw = (updatePlayers players throw, not toggle)
