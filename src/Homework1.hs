module Homework1 (
  doubleEveryOther,
  sumDigits,
  toDigits,
  toDigitsRev,
  validate,
) where

-- exercise1 start
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [mod n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits
-- exercise1 end

-- exercise2 start
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) (cycle [1,2]) . reverse
-- exercise2 end

-- exercise3 start
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | null xs = x
  | otherwise = sum (toDigits x) + sumDigits xs
-- exercise3 end

-- exercise4 start
validate :: Integer -> Bool
validate = (==) 0 . flip mod 10 . sumDigits . doubleEveryOther . toDigits
-- exercise4 end
