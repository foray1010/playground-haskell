module Homework3.Homework3 where

withPosition :: [a] -> [(a, Int)]
withPosition xs = zip xs [1..length xs]

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
-- ex3 end
