module Homework3.Homework3 where

import Data.List

withPosition :: [a] -> [(a, Integer)]
withPosition = flip zip [1..]

-- ex1 start
skips :: [a] -> [[a]]
skips xs =
  map (\(x, y) ->
    map fst .
    filter (\(_, b) -> mod b y == 0) .
    withPosition $ x
  ) .
  withPosition .
  replicate (length xs) $
  xs
-- ex1 end

-- ex2 start
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs) = [y | y > x && y > z] ++ localMaxima (y:z:zs)
localMaxima _ = []
-- ex2 end

-- ex3 start
histogram :: [Integer] -> String
histogram =
  flip (++) "==========\n0123456789\n" .
  unlines .
  map (\x ->
    map (\(y, z) -> if z - 1 `elem` x then '*' else y) .
    withPosition $
    "          "
  ) .
  reverse .
  transpose .
  group .
  sort
-- ex3 end
