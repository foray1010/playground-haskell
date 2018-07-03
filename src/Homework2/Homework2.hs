{-# LANGUAGE LambdaCase #-}

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
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert log Leaf = Node Leaf log Leaf
insert
  log1@(LogMessage _ ts1 _)
  tree@(Node left log2@(LogMessage _ ts2 _) right)
  | ts1 > ts2 = Node left log2 (insert log1 right)
  | otherwise = Node (insert log1 left) log2 right
-- ex2 end

-- ex3 start
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf
-- ex3 end

-- ex4 start
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left log right) = inOrder left ++ [log] ++ inOrder right
-- ex4 end

-- ex5 start
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =
  map (\(LogMessage _ _ msg) -> msg) .
  inOrder .
  build .
  filter (\case
    LogMessage (Error level) _ _ -> level >= 50
    _ -> False
  )
-- ex5 end

-- ex6 start
-- ex6 end
