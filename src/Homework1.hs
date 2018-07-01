module Homework1 (
  doubleEveryOther,
  hanoi,
  sumDigits,
  toDigits,
  toDigitsRev,
  validate,
) where

-- exercise1 start
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits
-- exercise1 end

-- exercise2 start
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) (cycle [1, 2]) . reverse
-- exercise2 end

-- exercise3 start
sumDigits :: [Integer] -> Integer
sumDigits = foldr ((+) . sum . toDigits) 0
-- exercise3 end

-- exercise4 start
validate :: Integer -> Bool
validate = (==) 0 . flip mod 10 . sumDigits . doubleEveryOther . toDigits
-- exercise4 end

-- exercise5 start
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
-- exercise5 end
