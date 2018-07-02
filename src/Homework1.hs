module Homework1 (
  doubleEveryOther,
  hanoi,
  sumDigits,
  toDigits,
  toDigitsRev,
  validate,
) where

-- ex1 start
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits
-- ex1 end

-- ex2 start
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) (cycle [1, 2]) . reverse
-- ex2 end

-- ex3 start
sumDigits :: [Integer] -> Integer
sumDigits = foldr ((+) . sum . toDigits) 0
-- ex3 end

-- ex4 start
validate :: Integer -> Bool
validate = (==) 0 . flip mod 10 . sumDigits . doubleEveryOther . toDigits
-- ex4 end

-- ex5 start
type Peg = String
type Move = (Peg,Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n src dest tmp
  | n == 0 = []
  | n == 1 = [(src, dest)]
  | otherwise =
      hanoi (n - 1) src tmp dest ++
      hanoi 1 src dest tmp ++
      hanoi (n - 1) tmp dest src
-- ex5 end

-- ex6 start
-- ex6 end
