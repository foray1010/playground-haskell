module Homework2.Homework2 where

import Homework2.Log

-- ex1 start
parseMessage :: String -> LogMessage
parseMessage n = case words n of
  ("E":level:ts:msg) -> LogMessage (Error (read level)) (read ts) (unwords msg)
  ("I":ts:msg) -> LogMessage Info (read ts) (unwords msg)
  ("W":ts:msg) -> LogMessage Warning (read ts) (unwords msg)
  _ -> Unknown n

parse :: String -> [LogMessage]
parse = map parseMessage . lines
-- ex1 end

-- ex2 start
-- ex2 end

-- ex3 start
-- ex3 end

-- ex4 start
-- ex4 end

-- ex5 start
-- ex5 end

-- ex6 start
-- ex6 end
