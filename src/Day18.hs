{-# LANGUAGE DeriveFunctor #-}

module Day18 where

import Data.Bifunctor (Bifunctor (first))
import Data.Char (isDigit)
import Data.List (foldl1')
import Data.Maybe (fromJust)

data Tree a = Regular a | Node (Tree a) (Tree a) deriving (Eq, Functor, Show)

type Number = Tree Int

main :: IO ()
main = interact (unlines . sequence [part1, part2] . map parseNumber . lines)

part1, part2 :: [Number] -> String
part1 = ("Part 1: " ++) . show . magnitude . foldl1' add
part2 = ("Part 2: " ++) . show . maximum . map magnitude . sums

magnitude :: Number -> Int
magnitude (Regular x) = x
magnitude (Node l r) = 3 * magnitude l + 2 * magnitude r

add :: Number -> Number -> Number
add x y = firstDuplicate $ iterate reduce $ Node x y

sums :: [Number] -> [Number]
sums [] = []
sums (x : xs) = [s | y <- xs, s <- [add x y, add y x]] ++ sums xs

firstDuplicate :: (Eq a) => [a] -> a
firstDuplicate = fst . head . dropWhile (uncurry (/=)) . pairs
  where
    pairs xs = zip xs (tail xs)

reduce :: Number -> Number
reduce t
  | t /= et = et
  | t /= st = st
  | otherwise = t
  where
    et = explode t
    st = split t

explode :: Number -> Number
explode = fmap fromJust . handleExplosion . go 0 Nothing . fmap Just
  where
    go _ (Just pair) node = (node, Just pair)
    go _ Nothing (Regular x) = (Regular x, Nothing)
    go depth Nothing node@(Node (Regular (Just l)) (Regular (Just r)))
      | depth >= 4 = (Regular Nothing, Just (l, r)) -- BOOM!
      | otherwise = (node, Nothing)
    go depth Nothing (Node l r) = case go (depth + 1) Nothing l of
      (l', Just pair) -> (Node l' r, Just pair)
      _ -> let (r', pair) = go (depth + 1) Nothing r in (Node l r', pair)

    handleExplosion (tree, Nothing) = tree
    handleExplosion (tree, Just (x, y)) = handleLeft x $ handleRight y tree

    handleLeft n = fst . go False
      where
        go _ (Regular Nothing) = (Regular (Just 0), True)
        go False (Regular x) = (Regular x, False)
        go True (Regular (Just x)) = (Regular (Just (x + n)), False)
        go b (Node l r) =
          let (r', b') = go b r
              (l', b'') = go b' l
           in (Node l' r', b'')

    handleRight n = fst . go False
      where
        go _ (Regular Nothing) = (Regular Nothing, True)
        go False (Regular x) = (Regular x, False)
        go True (Regular (Just x)) = (Regular (Just (x + n)), False)
        go b (Node l r) =
          let (l', b') = go b l
              (r', b'') = go b' r
           in (Node l' r', b'')

split :: Number -> Number
split = fst . go False
  where
    f = Regular . (`div` 2)
    go True (Regular x) = (Regular x, True)
    go False (Regular x)
      | x < 10 = (Regular x, False)
      | otherwise = (Node (f x) (f (x + 1)), True)
    go b (Node l r) =
      let (l', b') = go b l
          (r', b'') = go b' r
       in (Node l' r', b'')

parseNumber :: String -> Number
parseNumber = check . parse
  where
    check (num, []) = num
    check (_, rest) = error $ "Leftovers from parsing: " ++ rest

parse :: String -> (Number, String)
parse [] = error "Empty input"
parse s = case span isDigit s of
  ([], _) ->
    let (left, rest) = (parse . eat '[') s
        (right, rest') = (parse . eat ',') rest
     in (Node left right, eat ']' rest')
  pair -> first (Regular . read) pair

eat :: Char -> String -> String
eat ch [] = error "Empty input"
eat ch (x : xs)
  | ch == x = xs
  | otherwise = error $ "Expected " ++ [ch] ++ " but got " ++ [x]

printNumber :: Number -> String
printNumber (Regular x) = show x
printNumber (Node l r) = '[' : printNumber l ++ ',' : printNumber r ++ "]"
