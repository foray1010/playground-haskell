module Homework4.Homework4 where

import Data.List ((\\))

-- ex1 start
fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 =
  sum .
  filter even .
  takeWhile (> 1) .
  iterate (\x -> if even x then div x 2 else 3 * x + 1)
-- ex1 end

-- ex2 start
data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

split :: [a] -> ([a], [a])
split xs = splitAt (div (length xs + 1) 2) xs

foldTree :: [a] -> Tree a
foldTree (x:xs) = Node
  (floor . logBase 2 . fromIntegral $ length xs + 1)
  (foldTree . fst . split $ xs)
  x
  (foldTree . snd . split $ xs)
foldTree _ = Leaf
-- ex2 end

-- ex3 start
xor :: [Bool] -> Bool
xor = foldr (\x y -> if x then not y else y) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x:y) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base . reverse
-- ex3 end

-- ex4 start
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  map (\x -> 2 * x + 1) .
  (\\) [1..n] .
  map (\(i, j) -> i + j + 2 * i * j) $
  cartProd [1..n] [1..n]
-- ex4 end
