module Homework6.Homework6 where

-- ex1 start
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]
-- ex1 end

-- ex2 start
prependFib :: [Integer] -> [Integer]
prependFib (x1:x2:xs) = (x1 + x2):x1:x2:xs
prependFib (x:xs) = [1, 0]

fibs2 :: [Integer]
fibs2 = map head . iterate prependFib $ [0]
-- ex2 end

-- ex3 start
-- ex3 end

-- ex4 start
-- ex4 end

-- ex5 start
-- ex5 end

-- ex6 start
-- ex6 end

-- ex7 start
-- ex7 end
