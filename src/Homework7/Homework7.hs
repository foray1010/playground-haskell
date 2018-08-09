{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, TypeSynonymInstances #-}
module Homework7.Homework7 where

import qualified Data.Char as Char

import qualified Homework7.Buffer as Buffer
import qualified Homework7.Sized as Sized

data JoinList m a = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- ex1 start
tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag _ = mempty

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y
-- ex1 end

-- ex2.1 start
getSize :: (Sized.Sized b) => b -> Int
getSize = Sized.getSize . Sized.size

indexJ :: (Sized.Sized b, Monoid b) =>
  Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ a) = Just a
indexJ i (Append m l r)
  | i < 0 || i >= size = Nothing
  | i < sizeL = indexJ i l
  | otherwise = indexJ (i - sizeL) r
  where
    size = getSize m
    sizeL = getSize . tag $ l
indexJ _ _ = Nothing
-- ex2.1 end

-- ex2.2 start
dropJ :: (Sized.Sized b, Monoid b) =>
  Int -> JoinList b a -> JoinList b a
dropJ 0 jl = jl
dropJ n (Single _ _) = Empty
dropJ n (Append m l r)
  | n >= size = Empty
  | n >= sizeL = dropJ (n - sizeL) r
  | otherwise = dropJ n l +++ r
  where
    size = getSize m
    sizeL = getSize . tag $ l
dropJ _ _ = Empty
-- ex2.2 end

-- ex2.3 start
takeJ :: (Sized.Sized b, Monoid b) =>
  Int -> JoinList b a -> JoinList b a
takeJ 0 _ = Empty
takeJ n jl@(Single _ _) = jl
takeJ n jl@(Append m l r)
  | n >= size = jl
  | n > sizeL = l +++ takeJ (n - sizeL) r
  | otherwise = takeJ n l
  where
    size = getSize m
    sizeL = getSize . tag $ l
takeJ _ _ = Empty
-- ex2.3 end

-- ex3 start
newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = Score 0

score :: Char -> Score
score c
  | lc `elem` "aeilnorstu" = Score 1
  | lc `elem` "dg" = Score 2
  | lc `elem` "bcmp" = Score 3
  | lc `elem` "fhvwy" = Score 4
  | lc `elem` "k" = Score 5
  | lc `elem` "jx" = Score 8
  | lc `elem` "qz" = Score 10
  | otherwise = Score 0
  where lc = Char.toLower c

scoreString :: String -> Score
scoreString = sum . map score

scoreLine :: String -> JoinList Score String
scoreLine l = Single (scoreString l) l
-- ex3 end

-- ex4 start
scoreLineWithSize :: String -> JoinList (Score, Sized.Size) String
scoreLineWithSize l = Single (scoreString l, Sized.Size 1) l

instance Buffer.Buffer (JoinList (Score, Sized.Size) String) where
  toString = unlines . jlToList

  fromString = foldr ((+++) . scoreLineWithSize) Empty . lines

  line = indexJ

  replaceLine n l jl = takeJ (n - 1) jl +++ Buffer.fromString l +++ dropJ n jl

  numLines = Sized.getSize . snd . tag

  value = read . show . fst . tag
-- ex4 end
