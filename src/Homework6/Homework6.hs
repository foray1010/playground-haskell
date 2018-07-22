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
data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons x y) = x:streamToList y
-- ex3 end

-- ex4 start
streamRepeat :: a -> Stream a
streamRepeat n = Cons n (streamRepeat n)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x y) = Cons (f x) (streamMap f y)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons (f x) (streamFromSeed f (f x))
-- ex4 end

-- ex5 start
nats :: Stream Integer
nats = streamFromSeed (+1) (-1)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

rulerBy :: Integer -> Stream Integer
rulerBy n = foldr (interleaveStreams . streamRepeat) (streamRepeat n) [0..n - 1]

ruler :: Stream Integer
ruler = rulerBy 4
-- ex5 end

-- ex6 start
-- ex6 end

-- ex7 start
-- ex7 end
