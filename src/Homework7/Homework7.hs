module Homework7.Homework7 where

import qualified Homework7.Sized as Sized

data JoinList m a = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

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
-- ex3 end

-- ex4 start
-- ex4 end

-- ex5 start
-- ex5 end

-- ex6 start
-- ex6 end

-- ex7 start
-- ex7 end
