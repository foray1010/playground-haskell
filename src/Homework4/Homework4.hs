module Homework4.Homework4 where

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

foldTree :: [a] -> Tree a
foldTree _ = Leaf
-- ex2 end

-- ex3 start
-- ex3 end

-- ex4 start
-- ex4 end
