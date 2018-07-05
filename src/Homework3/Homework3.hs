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
