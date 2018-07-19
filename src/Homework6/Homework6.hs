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
appendFibs :: Integer -> Integer -> [Integer]
appendFibs x y = x:appendFibs y (x + y)

fibs2 :: [Integer]
fibs2 = appendFibs 0 1
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
